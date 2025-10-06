#!bash
set -euo pipefail

# Bootstrap script to build and run the Guix installer from source
# Verification strategy:
#   1. Git verifies the commit/tag integrity when cloning
#   2. Go verifies go.mod and go.sum (if external deps exist)
#   3. Source code is compiled locally before execution

REPO_OWNER="${GUIX_INSTALL_REPO:-durantschoon/cloudzy-guix-install}"
REPO_REF="${GUIX_INSTALL_REF:-main}"

echo "=== Guix Installer Bootstrap ==="
echo "Repository: ${REPO_OWNER}"
echo "Reference: ${REPO_REF}"
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

    # Calculate manifest hash and ask user to verify
    MANIFEST_HASH=$(shasum -a 256 SOURCE_MANIFEST.txt | awk '{print $1}')
    echo "================================================================"
    echo "MANIFEST HASH VERIFICATION"
    echo "================================================================"
    echo ""
    echo "The manifest hash for this download is:"
    echo "  $MANIFEST_HASH"
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

# Verify go.mod exists
if [[ ! -f go.mod ]]; then
    echo "Error: go.mod not found. This doesn't appear to be a Go module."
    exit 1
fi

# Build the installer
# Note: We have no external dependencies, so go.sum won't exist
# Go will still verify the build matches go.mod
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

# Run the installer
echo ""
echo "Starting installation..."
echo ""
./run-remote-steps
