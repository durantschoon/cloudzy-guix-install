#!/usr/bin/env bash
# Generate batch API requests for converting customize scripts to .scm
# These scripts are bash but don't have .sh extension, so they need special handling

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_FILE="$REPO_ROOT/tools/batch-requests-customize.jsonl"

# Read documentation files
KNOWLEDGE=$(cat "$REPO_ROOT/docs/GUILE_KNOWLEDGE.md")
BEST_PRACTICES=$(cat "$REPO_ROOT/docs/GUILE_BEST_PRACTICES.md")
GOTCHAS=$(cat "$REPO_ROOT/docs/GUILE_GOTCHAS.md")
CONVERSION_GUIDE=$(cat "$REPO_ROOT/docs/GUILE_CONVERSION.md")
POSTINSTALL_DEV=$(cat "$REPO_ROOT/docs/POSTINSTALL_DEV.md")

# Combine all documentation
FULL_DOCS="# GUILE KNOWLEDGE BASE

$KNOWLEDGE

---

$BEST_PRACTICES

---

$GOTCHAS

---

$CONVERSION_GUIDE

---

# POSTINSTALL DEVELOPMENT GUIDE

$POSTINSTALL_DEV"

echo "Generating batch conversion requests for customize scripts..."
echo "Output: $OUTPUT_FILE"
echo ""

# Archive existing file if it exists
ARCHIVE_DIR="$REPO_ROOT/archive/batch"
mkdir -p "$ARCHIVE_DIR"

if [ -f "$OUTPUT_FILE" ]; then
  TIMESTAMP=$(date +%Y%m%d-%H%M%S)
  ARCHIVE_FILE="$ARCHIVE_DIR/batch-requests-customize-${TIMESTAMP}.jsonl"
  mv "$OUTPUT_FILE" "$ARCHIVE_FILE"
  echo "Archived existing file to: $ARCHIVE_FILE"
  echo ""
fi

# Create new output file
> "$OUTPUT_FILE"

# List of customize scripts to convert
CUSTOMIZE_SCRIPTS=(
  "framework/postinstall/customize"
  "framework-dual/postinstall/customize"
  "cloudzy/postinstall/customize"
  "raspberry-pi/postinstall/customize"
)

SCRIPT_COUNT=0
cd "$REPO_ROOT"

for script in "${CUSTOMIZE_SCRIPTS[@]}"; do
  if [ -f "$script" ]; then
    SCRIPT_COUNT=$((SCRIPT_COUNT + 1))
    # Use full path as custom_id, replacing / with _ and limiting to 64 chars
    # Pattern must match: ^[a-zA-Z0-9_-]{1,64}$
    # Strip leading ./ if present, then sanitize
    CLEAN_PATH=$(echo "$script" | sed 's|^\./||')
    CUSTOM_ID="convert-$(echo "$CLEAN_PATH" | sed 's|/|_|g' | sed 's/[^a-zA-Z0-9_-]//g')"
    # Truncate to 64 chars if needed
    if [ ${#CUSTOM_ID} -gt 64 ]; then
      CUSTOM_ID="${CUSTOM_ID:0:64}"
    fi

    # Use temporary files to avoid command-line argument length limits
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
                "content": """Convert this bash customize script to Guile Scheme following the documentation provided.

REFERENCE DOCUMENTATION:

{docs}

---

ORIGINAL SCRIPT PATH: {script_path}

ORIGINAL SCRIPT CONTENT:

{script_content}

---

CONVERSION REQUIREMENTS:

1. Use proper Guile shebang: #!/run/current-system/profile/bin/guile --no-auto-compile -s
2. Include all necessary use-modules (ice-9 popen, ice-9 rdelim, ice-9 format, srfi srfi-1, ice-9 textual-ports)
3. Convert all bash functions to Guile procedures
4. Use Guile idioms: call-with-input-file, open-input-pipe, etc.
5. Maintain all functionality from the original script
6. Add comments explaining complex conversions
7. Follow naming conventions: predicates end in ?, mutators end in !
8. Use tail recursion where appropriate
9. Use GNU coreutils commands (sha256sum not shasum, etc.)

SPECIAL CONSIDERATIONS FOR CUSTOMIZE SCRIPTS:

**CRITICAL: These scripts source postinstall/lib.sh which provides shared functions.**
**You MUST convert the shared functions from postinstall/lib.sh into Guile equivalents.**

Key functions to convert from postinstall/lib.sh:
- msg(), warn(), err(), info(), success() - colored output functions
- ask_yes() - yes/no prompts
- backup_config() - backup /etc/config.scm
- guile_add_service() - add services using guile-config-helper.scm
- safe_edit_config() - safe config editing
- add_ssh(), add_desktop(), add_packages() - service/package addition
- add_nonguix_info() - nonguix channel info
- reconfigure() - system reconfigure
- edit_config(), view_config() - config manipulation
- clear_screen() - clear terminal

**IMPORTANT:**
- The script uses INSTALL_ROOT environment variable (set to ~/guix-customize)
- Use (getenv "INSTALL_ROOT") or default to (string-append (getenv "HOME") "/guix-customize")
- Path to guile-config-helper.scm: INSTALL_ROOT/lib/guile-config-helper.scm
- Path to postinstall/lib.sh functions: These should be converted inline or loaded as a Guile module
- CONFIG_FILE is /etc/config.scm
- BACKUP_DIR is ~/.config/guix-customize/backups

**Platform-specific functions:**
- framework/postinstall/customize: add_framework_hardware()
- framework-dual/postinstall/customize: add_networkmanager(), add_framework_hardware()
- cloudzy/postinstall/customize: (no platform-specific functions)
- raspberry-pi/postinstall/customize: (check for platform-specific functions)

**Menu system:**
- Convert the menu loop to a Guile recursive procedure
- Use read-line for user input
- Use case/cond for menu option handling
- Maintain the same menu structure and options

**File operations:**
- Use call-with-input-file / call-with-output-file for file I/O
- Use system* for external commands (sudo, guix system reconfigure, etc.)
- Handle file permissions correctly (sudo may be needed for /etc/config.scm)

COMMENT STRUCTURE PRESERVATION:

CRITICAL: Preserve the logical structure and organization of the original script by matching comment sections.

**For scripts WITH structured section headers:**
- Extract ALL comment headers/sections from the original bash script (lines starting with # that describe sections)
- Convert bash comment headers (# Section Name) to Guile comment headers (;;; Section Name)
- Maintain the EXACT SAME ORDER and EXACT SAME SECTION NAMES in the converted script
- Preserve these exact section names verbatim (e.g., "# Configuration" → ";;; Configuration")
- Use three semicolons (;;;) for major section headers in Guile
- Use two semicolons (;;) for subsection headers
- Place section headers immediately before the code they describe

**For scripts WITHOUT structured section headers:**
- Add logical section headers based on the code structure
- Keep them minimal and descriptive
- Organize code into logical sections (Configuration, Helper functions, Platform-specific functions, Main menu, etc.)

TEST GENERATION:

After converting the script, generate a corresponding test file. The test file should:
1. Be named: test-<script-name>.scm (e.g., test-customize.scm for customize)
2. Use Guile's test framework (srfi-64) or simple assertion-based testing
3. Test key functions/procedures from the converted script
4. Use temporary files/directories for testing (create and clean up)
5. Test both success and error cases where applicable
6. For scripts that modify config.scm files, test with sample config files
7. Place test file in same directory structure as converted script

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
  else
    echo "  ⚠ Warning: Script not found: $script"
  fi
done

echo ""
echo "Generated $SCRIPT_COUNT batch conversion requests"
echo "Output file: $OUTPUT_FILE"
echo ""
echo "Next steps:"
echo "  1. Review the requests: tools/view-jsonl.sh $OUTPUT_FILE"
echo "  2. Submit batch: tools/submit-batch.sh $OUTPUT_FILE"
echo "  3. Check status: tools/check-batch-status.sh"
echo "  4. Retrieve results: tools/retrieve-batch.sh"

