#!/usr/bin/env bash
# Interactive diff viewer for batch conversion results
# Pages through all converted scripts showing diffs between .sh and .scm versions

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CONVERSIONS_DIR="$REPO_ROOT/tools/converted-scripts"

if [ ! -d "$CONVERSIONS_DIR" ]; then
  echo "Error: Converted scripts directory not found: $CONVERSIONS_DIR"
  echo ""
  echo "Run retrieve-batch.sh first to extract converted scripts."
  exit 1
fi

# Check if less is available
if ! command -v less >/dev/null 2>&1; then
  echo "Error: 'less' command not found. Please install it to use this viewer."
  exit 1
fi

echo "Finding converted scripts..."
echo ""

# Find all .scm files in converted-scripts directory
mapfile -t converted_files < <(find "$CONVERSIONS_DIR" -name "*.scm" -type f | sort)

if [ ${#converted_files[@]} -eq 0 ]; then
  echo "No converted .scm files found in: $CONVERSIONS_DIR"
  exit 1
fi

echo "Found ${#converted_files[@]} converted script(s):"
echo ""

# Build list of original/converted pairs
pairs=()
for converted in "${converted_files[@]}"; do
  # Convert path: tools/converted-scripts/postinstall/recipes/add-spacemacs.scm
  # To original: postinstall/recipes/add-spacemacs.sh
  relative_path="${converted#$CONVERSIONS_DIR/}"
  
  # Handle old path format (add/spacemacs.scm) vs new format (add-spacemacs.scm)
  # Try new format first (correct format)
  original_path="$REPO_ROOT/${relative_path%.scm}.sh"
  
  # If not found, try old format (nested directories like add/spacemacs.scm)
  if [ ! -f "$original_path" ]; then
    # Check if this looks like old format (has nested add/ directory)
    if [[ "$relative_path" == */add/* ]]; then
      # Convert postinstall/recipes/add/spacemacs.scm -> postinstall/recipes/add-spacemacs.sh
      # Extract everything before /add/, then combine add- with nested filename
      base_dir=$(echo "$relative_path" | sed 's|/add/.*||')
      nested_part=$(echo "$relative_path" | sed 's|.*/add/||' | sed 's|/|-|g' | sed 's/.scm$//')
      old_format_path="${base_dir}/add-${nested_part}.sh"
      original_path="$REPO_ROOT/$old_format_path"
    fi
  fi
  
  if [ -f "$original_path" ]; then
    pairs+=("$original_path|$converted")
  else
    echo "  ⚠ Warning: Original not found for $relative_path"
    echo "    Tried: ${original_path#$REPO_ROOT/}"
  fi
done

if [ ${#pairs[@]} -eq 0 ]; then
  echo "Error: No matching original .sh files found for converted scripts."
  exit 1
fi

echo "Found ${#pairs[@]} matching pairs to review"
echo ""

# Generate diff output for all pairs
TEMP_DIFF=$(mktemp)
trap "rm -f $TEMP_DIFF" EXIT

for pair in "${pairs[@]}"; do
  IFS='|' read -r original converted <<< "$pair"
  relative_original="${original#$REPO_ROOT/}"
  relative_converted="${converted#$CONVERSIONS_DIR/}"
  
  echo "==================================================================================" >> "$TEMP_DIFF"
  echo "File: $relative_original -> $relative_converted" >> "$TEMP_DIFF"
  echo "==================================================================================" >> "$TEMP_DIFF"
  echo "" >> "$TEMP_DIFF"
  
  # Generate unified diff
  if diff -u "$original" "$converted" >> "$TEMP_DIFF" 2>&1; then
    echo "  (No differences)" >> "$TEMP_DIFF"
  fi
  
  echo "" >> "$TEMP_DIFF"
  echo "" >> "$TEMP_DIFF"
done

# Display with less
less -R "$TEMP_DIFF"

echo ""
echo "Review complete!"
echo ""
echo "Next steps:"
echo "  1. Copy converted scripts to their final locations"
echo "  2. Test converted scripts"
echo "  3. Update references in other scripts"
echo "  4. Commit changes"

