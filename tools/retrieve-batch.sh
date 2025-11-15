#!/usr/bin/env bash
# Retrieve and extract batch conversion results

set -euo pipefail

BATCH_ID="${1:-}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/batch-results.jsonl"
CONVERSIONS_DIR="$REPO_ROOT/tools/converted-scripts"

if [ -z "$BATCH_ID" ]; then
  echo "Usage: $0 <batch-id>"
  exit 1
fi

# Check for API key
if [ -z "${ANTHROPIC_API_KEY:-}" ]; then
  echo "Error: ANTHROPIC_API_KEY environment variable not set"
  exit 1
fi

echo "Retrieving batch results..."
echo "Batch ID: $BATCH_ID"
echo ""

# Download results
curl -s "https://api.anthropic.com/v1/messages/batches/$BATCH_ID/results" \
  --header "x-api-key: $ANTHROPIC_API_KEY" \
  --header "anthropic-version: 2023-06-01" \
  > "$RESULTS_FILE"

echo "Results downloaded to: $RESULTS_FILE"
echo ""

# Create conversions directory
mkdir -p "$CONVERSIONS_DIR"

# Extract converted scripts
echo "Extracting converted scripts..."
python3 - "$RESULTS_FILE" "$CONVERSIONS_DIR" "$REPO_ROOT" <<'EOF'
import sys
import json
import os
from pathlib import Path

results_file = sys.argv[1]
conversions_dir = sys.argv[2]
repo_root = sys.argv[3]

success_count = 0
error_count = 0

with open(results_file, 'r') as f:
    for line in f:
        result = json.loads(line)
        custom_id = result['custom_id']

        # Extract original path from custom_id (e.g., "convert-postinstall-recipes-add-spacemacs")
        # Convert back to path: postinstall/recipes/add-spacemacs.scm
        path_parts = custom_id.replace('convert-', '').split('-')
        original_path = '/'.join(path_parts) + '.scm'

        output_path = os.path.join(conversions_dir, original_path)
        os.makedirs(os.path.dirname(output_path), exist_ok=True)

        # Check if successful
        if result['result']['type'] == 'succeeded':
            # Extract converted script from response
            content = result['result']['message']['content'][0]['text']

            # Write to file
            with open(output_path, 'w') as out:
                out.write(content)

            # Make executable
            os.chmod(output_path, 0o755)

            print(f"  ✓ {original_path}")
            success_count += 1
        else:
            # Handle error
            error_type = result['result'].get('error', {}).get('type', 'unknown')
            error_msg = result['result'].get('error', {}).get('message', 'No message')

            print(f"  ✗ {original_path}")
            print(f"    Error: {error_type}")
            print(f"    Message: {error_msg}")
            error_count += 1

print()
print(f"Extraction complete!")
print(f"  Succeeded: {success_count}")
print(f"  Errored: {error_count}")
print()
print(f"Converted scripts written to: {conversions_dir}")
EOF

echo ""
echo "Next steps:"
echo "  1. Review converted scripts: ls -la $CONVERSIONS_DIR"
echo "  2. Compare with originals"
echo "  3. Copy to appropriate locations for testing"
echo "  4. Run tests: ./run-tests.sh"
echo "  5. Update GUILE_GOTCHAS.md if new patterns discovered"
echo ""
echo "Example review workflow:"
echo "  diff postinstall/recipes/add-spacemacs.sh $CONVERSIONS_DIR/postinstall/recipes/add-spacemacs.scm"
