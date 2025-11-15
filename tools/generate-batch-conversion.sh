#!/usr/bin/env bash
# Generate batch API requests for converting .sh scripts to .scm
# Uses Anthropic Batch API for cost-effective conversion
#
# The conversion prompt includes instructions to preserve comment structure,
# ensuring matching section headers between original and converted scripts.
# This makes diffs more readable. Use validate-comment-structure.sh to check.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_FILE="$REPO_ROOT/tools/batch-requests.jsonl"

# Read documentation files
KNOWLEDGE=$(cat "$REPO_ROOT/docs/GUILE_KNOWLEDGE.md")
BEST_PRACTICES=$(cat "$REPO_ROOT/docs/GUILE_BEST_PRACTICES.md")
GOTCHAS=$(cat "$REPO_ROOT/docs/GUILE_GOTCHAS.md")
CONVERSION_GUIDE=$(cat "$REPO_ROOT/docs/GUILE_CONVERSION.md")

# Combine all documentation
FULL_DOCS="# GUILE KNOWLEDGE BASE

$KNOWLEDGE

---

$BEST_PRACTICES

---

$GOTCHAS

---

$CONVERSION_GUIDE"

echo "Generating batch conversion requests..."
echo "Output: $OUTPUT_FILE"
echo ""

# Backup existing file with version number if it exists
if [ -f "$OUTPUT_FILE" ]; then
  VERSION=1
  BACKUP_FILE="${OUTPUT_FILE}.${VERSION}"
  while [ -f "$BACKUP_FILE" ]; do
    VERSION=$((VERSION + 1))
    BACKUP_FILE="${OUTPUT_FILE}.${VERSION}"
  done
  mv "$OUTPUT_FILE" "$BACKUP_FILE"
  echo "Backed up existing file to: $BACKUP_FILE"
  echo ""
fi

# Create new output file
> "$OUTPUT_FILE"

# Find all .sh scripts to convert (scripts that run on Guix OS)
# Exclude: tools/, archive/, test files, development scripts
SCRIPT_COUNT=0
cd "$REPO_ROOT"

# Find all .sh scripts that run on Guix
# - postinstall/recipes/*.sh (shared recipes)
# - lib/*.sh (library scripts used during/after install)
# - platform/postinstall/recipes/*.sh (platform-specific recipes)
# Exclude: tools/, archive/, test files, development scripts
while IFS= read -r script; do
  # Skip tools directory
  if [[ "$script" == tools/* ]]; then
    continue
  fi
  # Skip archive directory
  if [[ "$script" == archive/* ]]; then
    continue
  fi
  # Skip test files
  if [[ "$script" == *test*.sh ]] || [[ "$script" == *tests/*.sh ]]; then
    continue
  fi
  # Skip development-only scripts (not run on Guix OS)
  if [[ "$script" == update-manifest.sh ]] || [[ "$script" == run-tests.sh ]] || [[ "$script" == test-docker.sh ]]; then
    continue
  fi
  
  if [ -f "$script" ]; then
    SCRIPT_COUNT=$((SCRIPT_COUNT + 1))
    # Use full path as custom_id, replacing / with __SLASH__ to preserve structure
    CUSTOM_ID="convert-$(echo "$script" | sed 's|/|__SLASH__|g' | sed 's/.sh$//')"

    # Use temporary files to avoid command-line argument length limits
    # and ensure proper handling of special characters
    TEMP_DOCS=$(mktemp)
    TEMP_SCRIPT=$(mktemp)
    echo "$FULL_DOCS" > "$TEMP_DOCS"
    cat "$script" > "$TEMP_SCRIPT"

    # Create JSON request (properly escaped) and append to output file
    python3 <<EOF >> "$OUTPUT_FILE"
import sys
import json

# Read content from files to avoid command-line argument limits
with open('$TEMP_DOCS', 'r', encoding='utf-8') as f:
    docs = f.read()

with open('$TEMP_SCRIPT', 'r', encoding='utf-8') as f:
    script_content = f.read()

custom_id = '$CUSTOM_ID'
script_path = '$script'

request = {
    "custom_id": custom_id,
    "params": {
        "model": "claude-sonnet-4-5",
        "max_tokens": 8192,
        "messages": [
            {
                "role": "user",
                "content": """Convert this bash script to Guile Scheme following the documentation provided.

REFERENCE DOCUMENTATION:

{docs}

---

ORIGINAL SCRIPT PATH: {script_path}

ORIGINAL SCRIPT CONTENT:

{script_content}

---

CONVERSION REQUIREMENTS:

1. Use proper Guile shebang: #!/run/current-system/profile/bin/guile --no-auto-compile -s
2. Include all necessary use-modules (ice-9 popen, ice-9 rdelim, ice-9 format, srfi srfi-1)
3. Convert all bash functions to Guile procedures
4. Use Guile idioms: call-with-input-file, open-input-pipe, etc.
5. Maintain all functionality from the original script
6. Add comments explaining complex conversions
7. Follow naming conventions: predicates end in ?, mutators end in !
8. Use tail recursion where appropriate
9. Use GNU coreutils commands (sha256sum not shasum, etc.)

COMMENT STRUCTURE PRESERVATION:

CRITICAL: Preserve the logical structure and organization of the original script by matching comment sections.

**For scripts WITH structured section headers (5+ section headers):**
- Extract ALL comment headers/sections from the original bash script (lines starting with # that describe sections)
- Convert bash comment headers (# Section Name) to Guile comment headers (;;; Section Name)
- Maintain the EXACT SAME ORDER and EXACT SAME SECTION NAMES in the converted script
- Preserve these exact section names verbatim (e.g., "# Configuration" → ";;; Configuration")
- Use three semicolons (;;;) for major section headers in Guile
- Use two semicolons (;;) for subsection headers
- Place section headers immediately before the code they describe
- This ensures diffs between original and converted scripts show matching sections side-by-side

**For scripts WITHOUT structured section headers (fewer than 5 headers, mostly header comments):**
- Add logical section headers based on the code structure
- Keep them minimal and descriptive
- Organize code into logical sections (Configuration, Helpers, Main logic, etc.)

Example (structured original):
  Bash:  # Configuration
         # Helper functions  
         # Main logic
  Guile: ;;; Configuration
         ;;; Helper functions
         ;;; Main logic

**IMPORTANT:** If the original script has structured sections, you MUST preserve them exactly. The validation system will check for exact matching.

TEST GENERATION:

After converting the script, generate a corresponding test file. The test file should:
1. Be named: test-<script-name>.scm (e.g., test-add-development.scm for add-development.sh)
2. Use Guile's test framework (srfi-64) or simple assertion-based testing
3. Test key functions/procedures from the converted script
4. Use temporary files/directories for testing (create and clean up)
5. Test both success and error cases where applicable
6. For scripts that modify config.scm files, test with sample config files
7. Place test file in same directory structure as converted script (e.g., tools/converted-scripts/postinstall/recipes/test-add-development.scm)

Test file structure:
- Use-modules for testing (srfi-64 if available, or simple assertions)
- Load the script being tested (use load or include)
- Test helper functions/procedures
- Test main entry point if applicable
- Use temporary files for file operations
- Clean up after tests

OUTPUT FORMAT:

Provide TWO files:

1. The converted Guile script (complete .scm file content starting with shebang)
2. The test file (complete test-<script-name>.scm file content)

Separate the two files with a clear delimiter:
---TEST FILE---
Then provide the test file content.

If the script is too simple to warrant tests, include a minimal test file that at least verifies the script can be loaded without errors.
""".format(docs=docs, script_path=script_path, script_content=script_content)
            }
        ]
    }
}

# Output compact JSON (one line per request)
print(json.dumps(request, ensure_ascii=False))
EOF

    # Clean up temp files
    rm -f "$TEMP_DOCS" "$TEMP_SCRIPT"

    echo "  [$SCRIPT_COUNT] $script"
  fi
done < <(find . -name "*.sh" -type f | grep -v "^\./tools/" | grep -v "^\./archive/" | sort)

echo ""
echo "Generated $SCRIPT_COUNT batch requests"
echo "Output written to: $OUTPUT_FILE"
echo ""
echo "Next steps:"
echo "  1. Review batch-requests.jsonl"
echo "  2. Submit batch: ./tools/submit-batch.sh"
echo "  3. Wait 24 hours for results"
echo "  4. Retrieve results: ./tools/retrieve-batch.sh <batch-id>"
echo "  5. Review and test converted scripts"
