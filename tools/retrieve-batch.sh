#!/usr/bin/env bash
# Retrieve and extract batch conversion results

set -euo pipefail

BATCH_ID="${1:-}"
RESULTS_FILE_NAME="${2:-batch-results.jsonl}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/$RESULTS_FILE_NAME"
CONVERSIONS_DIR="$REPO_ROOT/tools/converted-scripts"

if [ -z "$BATCH_ID" ]; then
  echo "Usage: $0 <batch-id> [results-file]"
  echo ""
  echo "Examples:"
  echo "  $0 msgbatch_01HJGK7MZ3X5QR8W9P2N4V6B7D"
  echo "  $0 msgbatch_01HJGK7MZ3X5QR8W9P2N4V6B7D batch-results-customize.jsonl"
  echo ""
  echo "Arguments:"
  echo "  batch-id      - The batch ID from submit-batch.sh output"
  echo "  results-file  - Optional: Output filename (default: batch-results.jsonl)"
  exit 1
fi

# Validate and normalize batch ID format
# Anthropic API requires msgbatch_ prefix
if [[ "$BATCH_ID" =~ ^batch_ ]]; then
  # Auto-fix: replace batch_ with msgbatch_
  BATCH_ID="msgbatch_${BATCH_ID#batch_}"
  echo "⚠️  Auto-corrected batch ID prefix: batch_ → msgbatch_"
  echo "   Using: $BATCH_ID"
  echo ""
elif [[ ! "$BATCH_ID" =~ ^msgbatch_ ]]; then
  echo "Error: Invalid batch ID format"
  echo ""
  echo "Batch ID must start with 'msgbatch_' prefix"
  echo "Received: $BATCH_ID"
  echo ""
  echo "If you have a batch ID starting with 'batch_', it will be auto-corrected."
  echo "Otherwise, please check your batch ID from the submit-batch.sh output."
  exit 1
fi

# Load .env file - check repo root first, then tools directory
if [ -f "$REPO_ROOT/.env" ]; then
  set -a  # automatically export all variables
  source "$REPO_ROOT/.env"
  set +a
elif [ -f "$SCRIPT_DIR/.env" ]; then
  set -a  # automatically export all variables
  source "$SCRIPT_DIR/.env"
  set +a
fi

# Check for API key
if [ -z "${ANTHROPIC_API_KEY:-}" ]; then
  echo "Error: ANTHROPIC_API_KEY environment variable not set"
  echo ""
  echo "Set your API key using one of these methods:"
  echo ""
  echo "1. Create a .env file in repo root (recommended):"
  echo "   cp tools/.env.example .env"
  echo "   # Edit .env and add your API key"
  echo ""
  echo "2. Create a .env file in tools directory:"
  echo "   cp tools/.env.example tools/.env"
  echo "   # Edit tools/.env and add your API key"
  echo ""
  echo "3. Export as environment variable:"
  echo "   export ANTHROPIC_API_KEY='your-api-key-here'"
  exit 1
fi

echo "Retrieving batch results..."
echo "Batch ID: $BATCH_ID"
echo ""

# Archive existing results file if it exists
ARCHIVE_DIR="$REPO_ROOT/archive/batch"
mkdir -p "$ARCHIVE_DIR"

if [ -f "$RESULTS_FILE" ]; then
  TIMESTAMP=$(date +%Y%m%d-%H%M%S)
  # Extract base name without extension for archive filename
  BASE_NAME=$(basename "$RESULTS_FILE_NAME" .jsonl)
  ARCHIVE_FILE="$ARCHIVE_DIR/${BASE_NAME}-${TIMESTAMP}.jsonl"
  mv "$RESULTS_FILE" "$ARCHIVE_FILE"
  echo "Archived existing results to: $ARCHIVE_FILE"
  echo ""
fi

# Download results (compact JSONL format)
curl -s "https://api.anthropic.com/v1/messages/batches/$BATCH_ID/results" \
  --header "x-api-key: $ANTHROPIC_API_KEY" \
  --header "anthropic-version: 2023-06-01" \
  > "$RESULTS_FILE"

echo "Results downloaded to: $RESULTS_FILE"
echo ""

# Check if results file contains an error
if grep -q '"type":"error"' "$RESULTS_FILE"; then
  echo "Error in batch results:"
  python3 -m json.tool < "$RESULTS_FILE"
  exit 1
fi

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

# Parse pretty-printed JSONL (multi-line JSON objects)
with open(results_file, 'r', encoding='utf-8') as f:
    content = f.read()
    
    # Try parsing as single-line JSONL first
    current_obj = []
    brace_count = 0
    obj_num = 0
    
    for line in content.split('\n'):
        brace_count += line.count('{') - line.count('}')
        current_obj.append(line)
        
        # When braces are balanced, we have a complete JSON object
        if brace_count == 0 and current_obj:
            obj_str = '\n'.join(current_obj).strip()
            if obj_str:
                obj_num += 1
                try:
                    result = json.loads(obj_str)
                except json.JSONDecodeError as e:
                    print(f"  ✗ Object {obj_num}: Invalid JSON: {e}", file=sys.stderr)
                    error_count += 1
                    current_obj = []
                    continue
        
                # Check if custom_id exists
                if 'custom_id' not in result:
                    print(f"  ✗ Object {obj_num}: Missing 'custom_id' field", file=sys.stderr)
                    print(f"    Result keys: {list(result.keys())}", file=sys.stderr)
                    error_count += 1
                    current_obj = []
                    continue
                
                custom_id = result['custom_id']

                # Extract original path from custom_id
                # Format: "convert-postinstall__SLASH__recipes__SLASH__add-spacemacs"
                # Convert back to path: postinstall/recipes/add-spacemacs.scm
                path_with_slashes = custom_id.replace('convert-', '').replace('__SLASH__', '/')
                original_path = path_with_slashes + '.scm'

                output_path = os.path.join(conversions_dir, original_path)
                os.makedirs(os.path.dirname(output_path), exist_ok=True)

                # Check if result field exists
                if 'result' not in result:
                    print(f"  ✗ {original_path}: Missing 'result' field")
                    print(f"    Result keys: {list(result.keys())}")
                    error_count += 1
                    current_obj = []
                    continue
                
                # Check if successful
                if result['result'].get('type') == 'succeeded':
                    # Extract converted script from response
                    if 'message' not in result['result']:
                        print(f"  ✗ {original_path}: Missing 'message' field in result")
                        error_count += 1
                        current_obj = []
                        continue
                    
                    message = result['result']['message']
                    if 'content' not in message or not message['content']:
                        print(f"  ✗ {original_path}: Missing or empty 'content' field")
                        error_count += 1
                        current_obj = []
                        continue
                    
                    content = message['content'][0]['text']

                    # Strip markdown wrapping if present (defensive fallback)
                    # Claude may still wrap in markdown despite instructions
                    lines = content.split('\n')

                    # Check if first line is markdown header
                    if lines and lines[0].strip().startswith('# '):
                        # Remove first line (markdown header)
                        lines = lines[1:]

                    # Check if next line is empty and following is code fence
                    if len(lines) >= 2 and not lines[0].strip() and lines[1].strip().startswith('```'):
                        # Remove empty line and code fence
                        lines = lines[2:]
                    elif lines and lines[0].strip().startswith('```'):
                        # Remove code fence
                        lines = lines[1:]

                    # Check if last line is closing code fence
                    if lines and lines[-1].strip() == '```':
                        lines = lines[:-1]

                    # Rejoin content
                    content = '\n'.join(lines)

                    # Check if content contains test file delimiter
                    if '---TEST FILE---' in content:
                        # Split into script and test file
                        parts = content.split('---TEST FILE---', 1)
                        script_content = parts[0].strip()
                        test_content = parts[1].strip() if len(parts) > 1 else None
                        
                        # Write converted script
                        with open(output_path, 'w', encoding='utf-8') as f:
                            f.write(script_content)
                        os.chmod(output_path, 0o755)
                        
                        # Write test file if present
                        if test_content:
                            # Generate test file path: test-<script-name>.scm
                            script_name = os.path.basename(original_path).replace('.scm', '')
                            test_filename = f"test-{script_name}.scm"
                            test_path = os.path.join(os.path.dirname(output_path), test_filename)
                            
                            with open(test_path, 'w', encoding='utf-8') as f:
                                f.write(test_content)
                            os.chmod(test_path, 0o755)
                            print(f"  ✓ {original_path} + {test_filename}")
                        else:
                            print(f"  ✓ {original_path}")
                    else:
                        # No test file, just write script
                        with open(output_path, 'w', encoding='utf-8') as f:
                            f.write(content)
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
                
                current_obj = []

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
