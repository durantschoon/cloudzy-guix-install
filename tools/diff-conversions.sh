#!/usr/bin/env bash
# Interactive diff viewer for batch conversion results
# Pages through all converted scripts showing diffs between .sh and .scm versions
#
# Usage:
#   ./diff-conversions.sh                # Interactive colored diff viewer
#   ./diff-conversions.sh --magit        # Generate git diff output
#   ./diff-conversions.sh --patch        # Generate patch file
#   ./diff-conversions.sh --stage-for-magit  # Temporarily copy files for magit viewing

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CONVERSIONS_DIR="$REPO_ROOT/tools/converted-scripts"

# Parse arguments
MAGIT_MODE=false
PATCH_MODE=false
STAGE_MODE=false
RESTORE_MODE=false
if [[ "${1:-}" == "--magit" ]]; then
  MAGIT_MODE=true
elif [[ "${1:-}" == "--patch" ]]; then
  PATCH_MODE=true
elif [[ "${1:-}" == "--stage-for-magit" ]]; then
  STAGE_MODE=true
elif [[ "${1:-}" == "--restore-from-backup" ]]; then
  RESTORE_MODE=true
fi

if [ ! -d "$CONVERSIONS_DIR" ]; then
  echo "Error: Converted scripts directory not found: $CONVERSIONS_DIR"
  echo ""
  echo "Run retrieve-batch.sh first to extract converted scripts."
  exit 1
fi

# Handle restore mode early (before other checks)
if [ "$RESTORE_MODE" = true ]; then
  BACKUP_DIR="$REPO_ROOT/tools/.magit-diff-backup"
  if [ ! -d "$BACKUP_DIR" ]; then
    echo "No backup found at: $BACKUP_DIR"
    exit 1
  fi
  
  echo "Restoring files from backup..."
  cd "$REPO_ROOT"
  find "$BACKUP_DIR" -type f | while read -r backup_file; do
    relative_path="${backup_file#$BACKUP_DIR/}"
    original_file="$REPO_ROOT/$relative_path"
    
    if [ -f "$backup_file" ]; then
      cp "$backup_file" "$original_file"
      echo "  Restored: $relative_path"
    fi
  done
  
  echo ""
  echo "✓ Files restored"
  echo "You can now remove the backup directory if desired:"
  echo "  rm -rf $BACKUP_DIR"
  exit 0
fi

# Check if less is available (only needed for interactive mode)
if [ "$MAGIT_MODE" = false ] && [ "$PATCH_MODE" = false ] && [ "$STAGE_MODE" = false ]; then
  if ! command -v less >/dev/null 2>&1; then
    echo "Error: 'less' command not found. Please install it to use this viewer."
    exit 1
  fi
fi

# For stage mode, check if we're in a git repo
if [ "$STAGE_MODE" = true ]; then
  if ! git rev-parse --git-dir >/dev/null 2>&1; then
    echo "Error: Not in a git repository. This mode requires git."
    exit 1
  fi
fi

# Detect best diff command for colorized output
if command -v git >/dev/null 2>&1; then
  DIFF_CMD="git diff --no-index --color=always"
elif command -v colordiff >/dev/null 2>&1; then
  DIFF_CMD="colordiff -u"
else
  DIFF_CMD="diff -u"
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

# Handle stage-for-magit mode separately
if [ "$STAGE_MODE" = true ]; then
  BACKUP_DIR="$REPO_ROOT/tools/.magit-diff-backup"
  STAGED_FILES=()
  
  # Create backup directory
  mkdir -p "$BACKUP_DIR"
  
  echo "Staging files for magit viewing..."
  echo ""
  echo "This will temporarily copy .scm content to .sh file locations."
  echo "Original .sh files are backed up to: $BACKUP_DIR"
  echo ""
  
  for pair in "${pairs[@]}"; do
    IFS='|' read -r original converted <<< "$pair"
    relative_original="${original#$REPO_ROOT/}"
    
    # Backup original if it exists and isn't already backed up
    if [ -f "$original" ] && [ ! -f "$BACKUP_DIR/$relative_original" ]; then
      mkdir -p "$(dirname "$BACKUP_DIR/$relative_original")"
      cp "$original" "$BACKUP_DIR/$relative_original"
    fi
    
    # Copy converted .scm content to original .sh location
    # (keeping .sh extension so git tracks it as a modification)
    cp "$converted" "$original"
    STAGED_FILES+=("$original")
    
    echo "  Staged: $relative_original"
  done
  
  echo ""
  echo "✓ Files staged for magit viewing"
  echo ""
  echo "To view in magit:"
  echo "  1. Open magit: M-x magit-status"
  echo "  2. Navigate to unstaged changes"
  echo "  3. Press RET on files to view diffs"
  echo ""
  echo "To restore original files:"
  echo "  git checkout -- ${STAGED_FILES[*]}"
  echo ""
  echo "Or restore from backup:"
  echo "  ./tools/diff-conversions.sh --restore-from-backup"
  echo ""
  echo "Backup location: $BACKUP_DIR"
  exit 0
fi

# Generate diff output for all pairs
TEMP_DIFF=$(mktemp)
trap "rm -f $TEMP_DIFF" EXIT

for pair in "${pairs[@]}"; do
  IFS='|' read -r original converted <<< "$pair"
  relative_original="${original#$REPO_ROOT/}"
  relative_converted="${converted#$CONVERSIONS_DIR/}"
  
  if [ "$MAGIT_MODE" = true ] || [ "$PATCH_MODE" = true ]; then
    # For magit/patch mode, use git diff format
    echo "diff --git a/$relative_original b/$relative_converted" >> "$TEMP_DIFF"
    echo "--- a/$relative_original" >> "$TEMP_DIFF"
    echo "+++ b/$relative_converted" >> "$TEMP_DIFF"
  else
    # For interactive mode, use header format
    echo "==================================================================================" >> "$TEMP_DIFF"
    echo "File: $relative_original -> $relative_converted" >> "$TEMP_DIFF"
    echo "==================================================================================" >> "$TEMP_DIFF"
  fi
  echo "" >> "$TEMP_DIFF"
  
  # Generate unified diff with color support
  if [ "$MAGIT_MODE" = true ] || [ "$PATCH_MODE" = true ]; then
    # For magit, use git diff without color (magit will colorize)
    git diff --no-index --no-color "$original" "$converted" >> "$TEMP_DIFF" 2>&1 || true
  else
    # For interactive mode, use colorized diff
    if $DIFF_CMD "$original" "$converted" >> "$TEMP_DIFF" 2>&1; then
      echo "  (No differences)" >> "$TEMP_DIFF"
    fi
  fi
  
  echo "" >> "$TEMP_DIFF"
  echo "" >> "$TEMP_DIFF"
done

# Handle different output modes
if [ "$MAGIT_MODE" = true ]; then
  # Output git diff format (can be piped to a file or viewed directly)
  cat "$TEMP_DIFF"
  echo ""
  echo "To view in emacs:"
  echo "  1. Save to file: ./diff-conversions.sh --magit > conversions.patch"
  echo "  2. Open in emacs: C-x C-f conversions.patch"
  echo "     (Emacs will automatically use diff-mode with syntax highlighting)"
  echo ""
  echo "Or view directly with less:"
  echo "  ./diff-conversions.sh --magit | less -R"
elif [ "$PATCH_MODE" = true ]; then
  # Generate patch file
  PATCH_FILE="$REPO_ROOT/tools/conversions.patch"
  cp "$TEMP_DIFF" "$PATCH_FILE"
  echo "Patch file created: $PATCH_FILE"
  echo ""
  echo "To view in emacs:"
  echo "  1. Open the file: C-x C-f $PATCH_FILE"
  echo "     (Emacs will automatically use diff-mode with syntax highlighting)"
  echo ""
  echo "To view in magit (temporarily replace files to see diffs):"
  echo "  1. Run: ./tools/diff-conversions.sh --stage-for-magit"
  echo "  2. Open magit: M-x magit-status"
  echo "  3. View unstaged changes (original .sh files temporarily replaced)"
  echo "  4. Restore originals: git checkout -- <files>"
  echo "     Or: ./tools/diff-conversions.sh --restore-from-backup"
  echo ""
  echo "Or view directly:"
  echo "  cat $PATCH_FILE | less -R"
else
  # Interactive mode with colored output
  less -R "$TEMP_DIFF"
fi

echo ""
echo "Review complete!"
echo ""
echo "Next steps:"
echo "  1. Copy converted scripts to their final locations"
echo "  2. Test converted scripts"
echo "  3. Update references in other scripts"
echo "  4. Commit changes"

