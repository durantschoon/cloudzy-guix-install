#!/usr/bin/env bash
set -euo pipefail

# Generate manifest with checksums for all Go source files
# This lets users verify GitHub CDN has the latest version

echo "=== Generating source manifest ==="

MANIFEST_FILE="SOURCE_MANIFEST.txt"

cat > "$MANIFEST_FILE" <<EOF
# Source File Manifest
#
# Use this to verify GitHub's CDN has the latest version:
# Compare these checksums with what you see in the raw files on GitHub

EOF

# Checksum all Go source files
echo "## Go Source Files" >> "$MANIFEST_FILE"
echo "" >> "$MANIFEST_FILE"

find . -name "*.go" -not -path "./vendor/*" -not -path "./.git/*" | sort | while read -r file; do
    hash=$(shasum -a 256 "$file" | awk '{print $1}')
    echo "$hash  $file" >> "$MANIFEST_FILE"
done

echo "" >> "$MANIFEST_FILE"
echo "## Critical Shell Scripts" >> "$MANIFEST_FILE"
echo "" >> "$MANIFEST_FILE"

# Bootstrap installer (entry point)
hash=$(shasum -a 256 bootstrap-installer.sh | awk '{print $1}')
echo "$hash  bootstrap-installer.sh" >> "$MANIFEST_FILE"

# Cleanup script (prepare for fresh install)
hash=$(shasum -a 256 clean-install.sh | awk '{print $1}')
echo "$hash  clean-install.sh" >> "$MANIFEST_FILE"

# Verification script (diagnostic tool)
hash=$(shasum -a 256 verify-guix-install.sh | awk '{print $1}')
echo "$hash  verify-guix-install.sh" >> "$MANIFEST_FILE"

# Recovery script (installation recovery tool)
if [ -f recovery-complete-install.sh ]; then
    hash=$(shasum -a 256 recovery-complete-install.sh | awk '{print $1}')
    echo "$hash  recovery-complete-install.sh" >> "$MANIFEST_FILE"
fi

# Post-install library
if [ -f lib/postinstall.sh ]; then
    hash=$(shasum -a 256 lib/postinstall.sh | awk '{print $1}')
    echo "$hash  lib/postinstall.sh" >> "$MANIFEST_FILE"
fi

echo "" >> "$MANIFEST_FILE"
echo "## Post-Install Customization Scripts" >> "$MANIFEST_FILE"
echo "" >> "$MANIFEST_FILE"

# Platform-specific customization tools
for customize in */postinstall/customize; do
    if [ -f "$customize" ]; then
        hash=$(shasum -a 256 "$customize" | awk '{print $1}')
        echo "$hash  $customize" >> "$MANIFEST_FILE"
    fi
done

echo ""
echo "Manifest written to $MANIFEST_FILE"
echo ""

# Calculate and display the manifest checksum itself
MANIFEST_HASH=$(shasum -a 256 "$MANIFEST_FILE" | awk '{print $1}')

# Convert hash to human-readable words
MANIFEST_WORDS=$(go run cmd/hash-to-words/main.go "$MANIFEST_HASH" 2>/dev/null)

echo "================================================================"
echo "MANIFEST CHECKSUM (verify this on Guix ISO before running):"
echo ""
echo "  Hash: $MANIFEST_HASH"
if [ -n "$MANIFEST_WORDS" ]; then
    echo "  Words: $MANIFEST_WORDS"
fi
echo ""
echo "On Guix ISO, verify with:"
echo "  curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt | shasum -a 256"
if [ -n "$MANIFEST_WORDS" ]; then
    echo ""
    echo "Or verify with words (easier to read aloud):"
    echo "  curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt | shasum -a 256 | awk '{print \$1}' | ./hash-to-words"
fi
echo ""
echo "If checksums match, GitHub CDN has the latest version."
echo "================================================================"
echo ""
echo "File checksums:"
cat "$MANIFEST_FILE"
