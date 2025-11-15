#!/usr/bin/env bash
# Generate batch API requests for converting .sh scripts to .scm
# Uses Anthropic Batch API for cost-effective conversion

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

# Find all .sh scripts to convert (exclude tools directory itself)
SCRIPT_COUNT=0
cd "$REPO_ROOT"

# Recipe scripts in postinstall/recipes/
for script in postinstall/recipes/*.sh; do
  if [ -f "$script" ]; then
    SCRIPT_COUNT=$((SCRIPT_COUNT + 1))
    CUSTOM_ID="convert-$(echo "$script" | tr '/' '-' | sed 's/.sh$//')"

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

OUTPUT FORMAT:

Provide ONLY the complete converted Guile script. No explanations, no markdown formatting, just the raw .scm file content starting with the shebang.
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
done

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
