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

# Clear output file
> "$OUTPUT_FILE"

# Find all .sh scripts to convert (exclude tools directory itself)
SCRIPT_COUNT=0
cd "$REPO_ROOT"

# Recipe scripts in postinstall/recipes/
for script in postinstall/recipes/*.sh; do
  if [ -f "$script" ]; then
    SCRIPT_COUNT=$((SCRIPT_COUNT + 1))
    SCRIPT_CONTENT=$(cat "$script")
    CUSTOM_ID="convert-$(echo "$script" | tr '/' '-' | sed 's/.sh$//')"

    # Create JSON request (properly escaped)
    python3 - "$CUSTOM_ID" "$script" "$FULL_DOCS" "$SCRIPT_CONTENT" <<'EOF'
import sys
import json

custom_id = sys.argv[1]
script_path = sys.argv[2]
docs = sys.argv[3]
script_content = sys.argv[4]

request = {
    "custom_id": custom_id,
    "params": {
        "model": "claude-sonnet-4-5",
        "max_tokens": 8192,
        "messages": [
            {
                "role": "user",
                "content": f"""Convert this bash script to Guile Scheme following the documentation provided.

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
"""
            }
        ]
    }
}

print(json.dumps(request))
EOF

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
