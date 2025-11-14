#!/run/current-system/profile/bin/bash
set -euo pipefail

# Bootstrap script to build and run the Guix installer from source
# Verification strategy:
#   1. Git verifies the commit/tag integrity when cloning
#   2. Go verifies go.mod and go.sum (if external deps exist)
#   3. Source code is compiled locally before execution

REPO_OWNER="${GUIX_INSTALL_REPO:-durantschoon/cloudzy-guix-install}"
REPO_REF="${GUIX_INSTALL_REF:-main}"

# Parse channel arguments
CHANNEL_REPO=""
CHANNEL_BRANCH="main"
CHANNEL_PATH=""
PLATFORM=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --channel-repo)
            CHANNEL_REPO="$2"
            shift 2
            ;;
        --channel-branch)
            CHANNEL_BRANCH="$2"
            shift 2
            ;;
        --channel-path)
            CHANNEL_PATH="$2"
            shift 2
            ;;
        *)
            PLATFORM="$1"
            shift
            ;;
    esac
done

echo "=== Guix Installer Bootstrap ==="
echo "Repository: ${REPO_OWNER}"
echo "Reference: ${REPO_REF}"
if [[ -n "$CHANNEL_REPO" ]]; then
    echo "Channel Repository: ${CHANNEL_REPO}"
    echo "Channel Branch: ${CHANNEL_BRANCH}"
    echo "Channel Path: ${CHANNEL_PATH}"
fi
echo ""

# Test that stdin is working before proceeding
echo "Testing stdin availability..."
read -p "Press Enter to continue (or Ctrl+C to abort): " -r </dev/tty
echo ""

# Create temporary directory
WORK_DIR=$(mktemp -d)
trap "rm -rf $WORK_DIR" EXIT

cd "$WORK_DIR"

# Download repository tarball (avoids git HTTPS issues on Guix ISO)
echo "Downloading repository..."
TARBALL_URL="https://github.com/${REPO_OWNER}/archive/refs/heads/${REPO_REF}.tar.gz"
if ! curl -fsSL "$TARBALL_URL" -o repo.tar.gz; then
    echo "Error: Failed to download repository"
    exit 1
fi

# Extract tarball
echo "Extracting..."
tar -xzf repo.tar.gz
cd cloudzy-guix-install-*

# Show what we're building
echo ""
echo "Building from: ${REPO_OWNER} (${REPO_REF})"
echo ""

# Set up Go cache to use /tmp (more space on ISO)
# This needs to be done early as it's used for hash-to-words and main installer builds
mkdir -p /tmp/go-cache /tmp/go-tmp
export GOCACHE=/tmp/go-cache
export GOTMPDIR=/tmp/go-tmp

# Verify source manifest (ensures GitHub CDN has latest version)
if [[ -f SOURCE_MANIFEST.txt ]]; then
    echo "Verifying source file checksums..."
    echo ""

    # Extract and verify Go source files
    # Use process substitution to avoid stdin redirection issues
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue

        # Parse "hash  filepath" format
        expected_hash=$(echo "$line" | awk '{print $1}')
        filepath=$(echo "$line" | awk '{print $2}')

        # Skip if not a file (might be a header)
        [[ ! -f "$filepath" ]] && continue

        # Calculate actual hash
        actual_hash=$(shasum -a 256 "$filepath" | awk '{print $1}')

        if [[ "$actual_hash" != "$expected_hash" ]]; then
            echo "ERROR: Checksum mismatch for $filepath"
            echo "  Expected: $expected_hash"
            echo "  Got:      $actual_hash"
            echo ""
            echo "This likely means GitHub's CDN hasn't caught up with the latest push."
            echo "Wait a few minutes and try again, or check the repo on GitHub."
            exit 1
        fi

        echo "[OK] $filepath"
    done < <(cat SOURCE_MANIFEST.txt)

    echo ""
    echo "All source files verified!"
    echo ""

    # Build hash-to-words helper tool early (for manifest verification)
    # This is safe because we just verified the source files
    echo "Building hash-to-words helper tool for verification..."
    if go build -o hash-to-words ./cmd/hash-to-words 2>/dev/null; then
        chmod +x hash-to-words
        echo "[OK] hash-to-words tool built"
    else
        echo "[WARN] hash-to-words build failed (will use hex hash only)"
    fi
    echo ""

    # Calculate manifest hash and ask user to verify
    MANIFEST_HASH=$(shasum -a 256 SOURCE_MANIFEST.txt | awk '{print $1}')
    
    # Try to convert to words if hash-to-words is available
    MANIFEST_WORDS=""
    if [[ -f hash-to-words ]]; then
        MANIFEST_WORDS=$(echo "$MANIFEST_HASH" | ./hash-to-words 2>/dev/null)
    fi
    
    echo "================================================================"
    echo "MANIFEST HASH VERIFICATION"
    echo "================================================================"
    echo ""
    echo "The manifest hash for this download is:"
    echo "  Hash: $MANIFEST_HASH"
    if [[ -n "$MANIFEST_WORDS" ]]; then
        echo "  Words: $MANIFEST_WORDS"

        # Extract first 3 and last 3 words
        WORD_ARRAY=($MANIFEST_WORDS)
        FIRST_THREE="${WORD_ARRAY[0]} ${WORD_ARRAY[1]} ${WORD_ARRAY[2]}"
        LAST_IDX=$((${#WORD_ARRAY[@]} - 1))
        LAST_THREE="${WORD_ARRAY[$((LAST_IDX - 2))]} ${WORD_ARRAY[$((LAST_IDX - 1))]} ${WORD_ARRAY[$LAST_IDX]}"
        echo "  Quick: $FIRST_THREE ... $LAST_THREE"

        echo ""
        echo "You can verify using either:"
        echo "  - Compare the hex hash above"
        echo "  - Compare the words above (easier to read aloud)"
    fi
    echo ""
    echo "Before proceeding, you should verify this matches the expected hash"
    echo "from the repository documentation or a trusted source."
    echo ""

    if read -p "Does this hash match what you expect? [y/N] " -n 1 -r </dev/tty; then
        echo ""
    else
        echo ""
        echo "ERROR: Cannot read from stdin"
        exit 1
    fi

    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Installation aborted by user"
        exit 1
    fi
    echo ""
else
    echo "Warning: SOURCE_MANIFEST.txt not found. Skipping checksum verification."
    echo "  (This is okay but means GitHub CDN freshness isn't verified)"
    echo ""
fi

# Copy critical scripts to /root for manual recovery if needed
echo "Installing critical recovery scripts to /root..."
CRITICAL_SCRIPTS=(
    "clean-install.sh"
    "lib/verify-guix-install.sh"
    "lib/recovery-complete-install.sh"
)

for script in "${CRITICAL_SCRIPTS[@]}"; do
    if [[ -f "$script" ]]; then
        cp "$script" /root/
        chmod +x "/root/$(basename "$script")"
        echo "[OK] Copied $(basename "$script") to /root/"
    else
        echo "[WARN] $script not found (skipping)"
    fi
done
echo ""

# Verify go.mod exists
if [[ ! -f go.mod ]]; then
    echo "Error: go.mod not found. This doesn't appear to be a Go module."
    exit 1
fi

# Build the installer and helper tools
# Note: We have no external dependencies, so go.sum won't exist
# Go will still verify the build matches go.mod
# Go cache is already set up from earlier (for hash-to-words build)
echo "Building installer from source..."

if ! go build -o run-remote-steps .; then
    echo "Error: Build failed"
    exit 1
fi

# Verify the binary was created
if [[ ! -f run-remote-steps ]]; then
    echo "Error: Binary not created"
    exit 1
fi

# Note: hash-to-words was already built earlier for manifest verification
# If it failed then, we'll try again here as a fallback
if [[ ! -f hash-to-words ]]; then
    echo "Building hash-to-words helper tool..."
    if go build -o hash-to-words ./cmd/hash-to-words; then
        chmod +x hash-to-words
        echo "[OK] hash-to-words tool built"
    else
        echo "[WARN] hash-to-words build failed (not critical, continuing)"
    fi
fi

# Export channel info for Go installer
export GUIX_CHANNEL_REPO="$CHANNEL_REPO"
export GUIX_CHANNEL_BRANCH="$CHANNEL_BRANCH"
export GUIX_CHANNEL_PATH="$CHANNEL_PATH"

# Export platform for Go installer
export GUIX_PLATFORM="$PLATFORM"

# Run the installer
echo ""
echo "Starting installation..."
echo ""
./run-remote-steps
