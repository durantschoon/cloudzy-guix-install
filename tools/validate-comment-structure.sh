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
# Distinguishes between header/documentation comments and actual section headers
extract_comment_headers() {
  local file="$1"
  local lang="$2"  # "bash" or "guile"
  
  if [ "$lang" = "bash" ]; then
    # Extract section headers: comments that organize code structure
    # Skip header comments (first 20 lines typically contain file header/documentation)
    # Skip inline descriptive comments (comments with parentheses like "# Bootstrap installer (entry point)")
    # Skip comments immediately before single statements (like "# Post-install library" before "if [ -f ... ]")
    # Section headers are usually short, capitalized, and describe code blocks
    
    awk '
      NR <= 20 { next }  # Skip header section
      /^#[[:space:]]+[A-Z]/ {
        # Skip comments with parentheses (inline descriptive comments)
        if ($0 ~ /\(/) { next }
        # Skip very long comments (documentation, not sections)
        if (length($0) >= 60) { next }
        
        # Check if this comment is followed by a single statement (inline comment) vs code block (section header)
        # Read ahead to find next non-comment, non-blank line
        comment_line = NR
        next_code_line = ""
        for (i = NR + 1; i <= NR + 5; i++) {
          getline next_line < FILENAME
          if (next_line !~ /^[[:space:]]*#/ && next_line !~ /^[[:space:]]*$/) {
            next_code_line = next_line
            break
          }
        }
        
        # If next line is a single statement (if, hash=, echo, etc.), skip it (inline comment)
        # Section headers are followed by code blocks (loops, multiple statements, etc.)
        if (next_code_line ~ /^[[:space:]]*(if|hash=|echo|MANIFEST_|WORD_)/) {
          # This is likely an inline comment before a single statement, skip it
          next
        }
        
        # This looks like a section header
        print
      }
    ' "$file" 2>/dev/null | sed 's/^#[[:space:]]*/# /' || true
  else
    # Extract lines starting with ;;; (major sections) or ;; (subsections)
    # Skip header comments (first 20 lines)
    awk 'NR > 20 && /^;;;[[:space:]]+[A-Z]|^;;[[:space:]]+[A-Z]/ { print }' "$file" 2>/dev/null | sed 's/^;;;[[:space:]]*/;;; /' | sed 's/^;;[[:space:]]*/;; /' || true
  fi
}

# Check if original script has structured section headers (not just header comments)
# Scripts with only header documentation + 1-2 simple sections are considered "unstructured"
# Returns true if script has 5+ section headers that organize code structure
has_structured_sections() {
  local file="$1"
  local headers=$(extract_comment_headers "$file" "bash")
  local count=$(echo "$headers" | grep -c . || echo "0")
  
  # If original has 5+ section headers, consider it "structured"
  # Scripts with only header comments + 1-2 sections (like "# Colors", "# Run if called directly")
  # typically have 4 or fewer headers total and are considered unstructured
  # They're just documentation, not code organization
  [ "$count" -ge 5 ]
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
UNSTRUCTURED=0

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
  
  relative_original="${original_path#$REPO_ROOT/}"
  
  # Check if original has structured sections (not just header comments)
  if ! has_structured_sections "$original_path"; then
    # Original doesn't have structured sections - just verify converted has reasonable structure
    if [ -z "$guile_headers" ]; then
      echo "  ⚠ $relative_original - Original has no structured sections, converted also has none"
      UNSTRUCTURED=$((UNSTRUCTURED + 1))
    else
      guile_count=$(echo "$guile_headers" | grep -c . || echo "0")
      echo "  ✓ $relative_original - Original has no structured sections, converted added structure ($guile_count sections)"
      echo "    (This is expected - converted scripts add logical organization)"
      UNSTRUCTURED=$((UNSTRUCTURED + 1))
    fi
    continue
  fi
  
  # Original has structured sections - validate matching
  bash_normalized=$(echo "$bash_headers" | while read -r line; do normalize_header "$line"; done)
  guile_normalized=$(echo "$guile_headers" | while read -r line; do normalize_header "$line"; done)
  
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
      # Check if headers match in order (for structured scripts, order matters for diff readability)
      match=true
      mismatched_sections=()
      header_num=0
      while IFS= read -r bash_norm && IFS= read -r guile_norm <&3; do
        header_num=$((header_num + 1))
        if [ "$bash_norm" != "$guile_norm" ]; then
          match=false
          # Get original header text for this position
          bash_orig=$(echo "$bash_headers" | sed -n "${header_num}p")
          guile_orig=$(echo "$guile_headers" | sed -n "${header_num}p")
          mismatched_sections+=("  Section $header_num: '$bash_orig' vs '$guile_orig'")
        fi
      done < <(echo "$bash_normalized") 3< <(echo "$guile_normalized")
      
      if [ "$match" = true ]; then
        echo "  ✓ $relative_original - Comment sections match ($bash_count sections)"
        MATCHES=$((MATCHES + 1))
      else
        echo "  ✗ $relative_original - Section count matches but names/order differ:"
        printf '%s\n' "${mismatched_sections[@]}"
        echo "    (For structured scripts, sections must match exactly for diff readability)"
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
echo "  ℹ Unstructured originals (converted added structure): $UNSTRUCTURED"
echo ""

# Only fail on actual mismatches (not unstructured originals)
if [ $MISMATCHES -gt 0 ]; then
  echo "Tip: Scripts with structured sections in original should match in converted version."
  echo "     See: tools/generate-batch-conversion.sh (COMMENT STRUCTURE PRESERVATION section)"
  exit 1
else
  if [ $UNSTRUCTURED -gt 0 ]; then
    echo "Note: Some original scripts had no structured sections."
    echo "      Converted versions added logical organization (this is expected and beneficial)."
  fi
  echo "All converted scripts have appropriate comment structure! ✓"
  exit 0
fi

