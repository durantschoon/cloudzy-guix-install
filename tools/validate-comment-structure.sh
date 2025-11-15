#!/usr/bin/env bash
# Validate that converted .scm scripts have matching comment sections with original .sh scripts
# This helps ensure diffs are readable by showing corresponding sections side-by-side

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

# Extract comment headers from a file
# Bash: lines starting with # followed by capital letter or space
# Guile: lines starting with ;;; or ;;
extract_comment_headers() {
  local file="$1"
  local lang="$2"  # "bash" or "guile"
  
  if [ "$lang" = "bash" ]; then
    # Extract lines starting with # followed by space and capital letter (section headers)
    # Or # followed by word starting with capital letter
    grep -E '^#[[:space:]]+[A-Z]' "$file" 2>/dev/null | sed 's/^#[[:space:]]*/# /' || true
  else
    # Extract lines starting with ;;; (major sections) or ;; (subsections)
    grep -E '^;;;[[:space:]]+[A-Z]|^;;[[:space:]]+[A-Z]' "$file" 2>/dev/null | sed 's/^;;;[[:space:]]*/;;; /' | sed 's/^;;[[:space:]]*/;; /' || true
  fi
}

# Normalize comment header text for comparison
normalize_header() {
  echo "$1" | sed 's/^[#;]*[[:space:]]*//' | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]//g'
}

echo "Validating comment structure matching..."
echo ""

# Find all .scm files and match with originals
mapfile -t converted_files < <(find "$CONVERSIONS_DIR" -name "*.scm" -type f | sort)

if [ ${#converted_files[@]} -eq 0 ]; then
  echo "No converted .scm files found in: $CONVERSIONS_DIR"
  exit 1
fi

MATCHES=0
MISMATCHES=0
NO_COMMENTS=0

for converted in "${converted_files[@]}"; do
  relative_path="${converted#$CONVERSIONS_DIR/}"
  
  # Handle old path format (add/spacemacs.scm) vs new format (add-spacemacs.scm)
  original_path="$REPO_ROOT/${relative_path%.scm}.sh"
  
  if [ ! -f "$original_path" ]; then
    # Try old format
    if [[ "$relative_path" == */add/* ]]; then
      base_dir=$(echo "$relative_path" | sed 's|/add/.*||')
      nested_part=$(echo "$relative_path" | sed 's|.*/add/||' | sed 's|/|-|g' | sed 's/.scm$//')
      old_format_path="${base_dir}/add-${nested_part}.sh"
      original_path="$REPO_ROOT/$old_format_path"
    fi
  fi
  
  if [ ! -f "$original_path" ]; then
    continue
  fi
  
  # Extract comment headers
  bash_headers=$(extract_comment_headers "$original_path" "bash")
  guile_headers=$(extract_comment_headers "$converted" "guile")
  
  # Normalize and compare
  bash_normalized=$(echo "$bash_headers" | while read -r line; do normalize_header "$line"; done)
  guile_normalized=$(echo "$guile_headers" | while read -r line; do normalize_header "$line"; done)
  
  relative_original="${original_path#$REPO_ROOT/}"
  
  if [ -z "$bash_headers" ] && [ -z "$guile_headers" ]; then
    echo "  ⚠ $relative_original - No comment sections found in either file"
    NO_COMMENTS=$((NO_COMMENTS + 1))
  elif [ -z "$bash_headers" ]; then
    echo "  ⚠ $relative_original - Original has no comment sections, converted has:"
    echo "$guile_headers" | sed 's/^/      /'
    NO_COMMENTS=$((NO_COMMENTS + 1))
  elif [ -z "$guile_headers" ]; then
    echo "  ✗ $relative_original - Converted missing comment sections (original has):"
    echo "$bash_headers" | sed 's/^/      /'
    MISMATCHES=$((MISMATCHES + 1))
  else
    # Compare normalized headers
    bash_count=$(echo "$bash_normalized" | grep -c . || echo "0")
    guile_count=$(echo "$guile_normalized" | grep -c . || echo "0")
    
    if [ "$bash_count" -eq "$guile_count" ]; then
      # Check if headers match (simple comparison)
      match=true
      while IFS= read -r bash_norm && IFS= read -r guile_norm <&3; do
        if [ "$bash_norm" != "$guile_norm" ]; then
          match=false
          break
        fi
      done < <(echo "$bash_normalized") 3< <(echo "$guile_normalized")
      
      if [ "$match" = true ]; then
        echo "  ✓ $relative_original - Comment sections match ($bash_count sections)"
        MATCHES=$((MATCHES + 1))
      else
        echo "  ⚠ $relative_original - Section count matches but names differ:"
        echo "    Original: $(echo "$bash_headers" | head -1)"
        echo "    Converted: $(echo "$guile_headers" | head -1)"
        MISMATCHES=$((MISMATCHES + 1))
      fi
    else
      echo "  ✗ $relative_original - Section count mismatch (original: $bash_count, converted: $guile_count)"
      echo "    Original sections:"
      echo "$bash_headers" | sed 's/^/      /'
      echo "    Converted sections:"
      echo "$guile_headers" | sed 's/^/      /'
      MISMATCHES=$((MISMATCHES + 1))
    fi
  fi
done

echo ""
echo "Summary:"
echo "  ✓ Matches: $MATCHES"
echo "  ✗ Mismatches: $MISMATCHES"
echo "  ⚠ No comments: $NO_COMMENTS"
echo ""

if [ $MISMATCHES -gt 0 ]; then
  echo "Tip: Update the conversion prompt to better preserve comment structure."
  echo "     See: tools/generate-batch-conversion.sh (COMMENT STRUCTURE PRESERVATION section)"
  exit 1
else
  echo "All converted scripts have matching comment structure! ✓"
  exit 0
fi

