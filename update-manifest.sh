#!/usr/bin/env bash
set -euo pipefail

# Generate manifest with checksums for all Go source files
# This lets users verify GitHub CDN has the latest version

echo "=== Generating source manifest ==="

MANIFEST_FILE="SOURCE_MANIFEST.txt"

cat > "$MANIFEST_FILE" <<EOF
# Source File Manifest
# Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
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
echo "## Bootstrap Script" >> "$MANIFEST_FILE"
echo "" >> "$MANIFEST_FILE"
hash=$(shasum -a 256 bootstrap-installer.sh | awk '{print $1}')
echo "$hash  bootstrap-installer.sh" >> "$MANIFEST_FILE"

echo ""
echo "Manifest written to $MANIFEST_FILE"
echo ""
echo "File checksums:"
cat "$MANIFEST_FILE"
