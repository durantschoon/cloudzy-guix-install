#!/usr/bin/env bash
# update-sha256.sh - Generate SHA256 checksums and update run-remote-steps.go

set -euo pipefail

echo "=== Generating SHA256 checksums for warning and clean scripts ==="
echo ""

# Generate checksums for all warning and clean scripts across all platforms
declare -A checksums

# Define platforms that have install scripts
PLATFORMS=("cloudzy" "framework" "framework-dual")

for platform in "${PLATFORMS[@]}"; do
  echo "Processing $platform platform..."
  for file in "$platform/install"/*-warnings.sh "$platform/install"/*-clean.sh; do
    if [[ -f "$file" ]]; then
      checksum=$(shasum -a 256 "$file" | cut -d' ' -f1)
      basename_file=$(basename "$file")
      checksums["$platform/install/$basename_file"]="$checksum"
      echo "  $platform/install/$basename_file: $checksum"
    fi
  done
done

echo ""
echo "=== Updating run-remote-steps.go ==="

# Create the new checksums map
cat > /tmp/checksums_block.txt << 'EOF'
// Script checksums (generated from update-sha256.sh)
var checksums = map[string]string{
EOF

# Add scripts organized by platform
for platform in "${PLATFORMS[@]}"; do
  echo "	// $platform platform" >> /tmp/checksums_block.txt

  # Add warning scripts for this platform
  for file in "$platform/install"/*-warnings.sh; do
    if [[ -f "$file" ]]; then
      basename_file=$(basename "$file")
      full_path="$platform/install/$basename_file"
      if [[ -n "${checksums[$full_path]:-}" ]]; then
        echo "	\"$full_path\": \"${checksums[$full_path]}\"," >> /tmp/checksums_block.txt
      fi
    fi
  done

  # Add clean scripts for this platform
  for file in "$platform/install"/*-clean.sh; do
    if [[ -f "$file" ]]; then
      basename_file=$(basename "$file")
      full_path="$platform/install/$basename_file"
      if [[ -n "${checksums[$full_path]:-}" ]]; then
        echo "	\"$full_path\": \"${checksums[$full_path]}\"," >> /tmp/checksums_block.txt
      fi
    fi
  done

  echo "" >> /tmp/checksums_block.txt
done

echo "}" >> /tmp/checksums_block.txt

# Find the start and end of the current checksums block in run-remote-steps.go
start_line=$(grep -n "^// Script checksums (generated from update-sha256.sh)" run-remote-steps.go | cut -d: -f1)
end_line=$(grep -n "^var platformScripts = map" run-remote-steps.go | cut -d: -f1)

if [[ -z "$start_line" ]]; then
  echo "Error: Could not find checksums marker in run-remote-steps.go"
  exit 1
fi

if [[ -z "$end_line" ]]; then
  echo "Error: Could not find platformScripts marker in run-remote-steps.go"
  exit 1
fi

# End should be one line before platformScripts
end_line=$((end_line - 2))

echo "Found checksums block from line $start_line to $end_line"

# Safety check: make sure we're not going to truncate the file
total_lines=$(wc -l < run-remote-steps.go)
if [[ $end_line -ge $total_lines ]]; then
  echo "Error: End line $end_line is beyond file length $total_lines"
  echo "This would truncate the file. Aborting for safety."
  exit 1
fi

# Replace the checksums block
{
  head -n $((start_line - 1)) run-remote-steps.go
  cat /tmp/checksums_block.txt
  echo ""
  tail -n +$((end_line + 1)) run-remote-steps.go
} > run-remote-steps.go.new

# Verify the new file has reasonable length
new_lines=$(wc -l < run-remote-steps.go.new)
if [[ $new_lines -lt $((total_lines - 10)) ]]; then
  echo "Error: New file would be too short ($new_lines lines vs original $total_lines)"
  echo "This suggests the replacement would truncate the file. Aborting."
  rm -f run-remote-steps.go.new
  exit 1
fi

mv run-remote-steps.go.new run-remote-steps.go

# Clean up
rm -f /tmp/checksums_block.txt

echo "✓ Updated run-remote-steps.go with new SHA256 checksums"
echo ""
echo "Summary of checksums by platform:"
for platform in "${PLATFORMS[@]}"; do
  echo "  $platform platform:"
  for file in "$platform/install"/*-warnings.sh "$platform/install"/*-clean.sh; do
    if [[ -f "$file" ]]; then
      basename_file=$(basename "$file")
      full_path="$platform/install/$basename_file"
      if [[ -n "${checksums[$full_path]:-}" ]]; then
        echo "    $full_path: ${checksums[$full_path]}"
      fi
    fi
  done
  echo ""
done
