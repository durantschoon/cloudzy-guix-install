#!/run/current-system/profile/bin/bash
# Verify postinstall scripts against SOURCE_MANIFEST.txt
# Run this after downloading postinstall scripts to ensure integrity

set -euo pipefail

MANIFEST_URL="https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt"

echo "=== Postinstall Script Verification ==="
echo ""

# Download manifest
echo "Downloading manifest..."
if ! MANIFEST=$(curl -fsSL "$MANIFEST_URL"); then
    echo "Error: Failed to download manifest from GitHub"
    exit 1
fi

echo "Manifest downloaded successfully"
echo ""

# Function to verify a file
verify_file() {
    local file="$1"
    local section="$2"

    if [ ! -f "$file" ]; then
        echo "  ⊘ $file (not found, skipping)"
        return 0
    fi

    # Get expected hash from manifest
    local expected_hash=$(echo "$MANIFEST" | grep -A 100 "^## $section" | grep "$file" | awk '{print $1}' | head -1)

    if [ -z "$expected_hash" ]; then
        echo "  ⊘ $file (not in manifest, skipping)"
        return 0
    fi

    # Calculate actual hash
    local actual_hash=$(shasum -a 256 "$file" | awk '{print $1}')

    if [ "$actual_hash" = "$expected_hash" ]; then
        echo "  ✓ $file"
        return 0
    else
        echo "  ✗ $file (checksum mismatch!)"
        echo "    Expected: $expected_hash"
        echo "    Got:      $actual_hash"
        return 1
    fi
}

# Verify Guile library scripts
echo "Verifying Guile library scripts..."
verify_file "lib/guile-config-helper.scm" "Guile Library Scripts"
echo ""

# Verify postinstall customize scripts
echo "Verifying postinstall customize scripts..."
for platform in cloudzy framework framework-dual raspberry-pi; do
    if [ -f "$platform/postinstall/customize" ]; then
        verify_file "$platform/postinstall/customize" "Post-Install Customization Scripts"
    fi
done
echo ""

# Verify other library scripts
echo "Verifying library scripts..."
verify_file "lib/postinstall.sh" "Critical Shell Scripts"
echo ""

echo "=== Verification Complete ==="
echo ""
echo "If all files show ✓, your postinstall scripts are verified."
echo "If any show ✗, re-download those files or check for updates."
