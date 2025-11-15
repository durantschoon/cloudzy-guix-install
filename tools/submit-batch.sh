#!/usr/bin/env bash
# Submit batch conversion requests to Anthropic API

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Accept optional batch file argument, default to batch-requests.jsonl
BATCH_FILE="${1:-$SCRIPT_DIR/batch-requests.jsonl}"
# If relative path, make it relative to script directory
if [[ "$BATCH_FILE" != /* ]]; then
  BATCH_FILE="$SCRIPT_DIR/$BATCH_FILE"
fi

# Load .env file if it exists (in tools/ directory)
if [ -f "$SCRIPT_DIR/.env" ]; then
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
  echo "1. Create a .env file (recommended):"
  echo "   cp tools/.env.example tools/.env"
  echo "   # Edit tools/.env and add your API key"
  echo ""
  echo "2. Export as environment variable:"
  echo "   export ANTHROPIC_API_KEY='your-api-key-here'"
  echo ""
  echo "Get your API key from: https://console.anthropic.com/settings/keys"
  exit 1
fi

# Check batch file exists
if [ ! -f "$BATCH_FILE" ]; then
  echo "Error: Batch requests file not found: $BATCH_FILE"
  echo ""
  echo "Generate batch requests first:"
  echo "  ./tools/generate-batch-conversion.sh"
  exit 1
fi

echo "Submitting batch conversion requests..."
echo "Batch file: $BATCH_FILE"
echo ""

# Validate and parse JSONL format (handles pretty-printed multi-line JSON)
echo "Validating JSONL format..."
TEMP_JSON=$(mktemp)
VALIDATION_ERRORS=0
python3 <<'PYTHON' > "$TEMP_JSON"
import json
import sys

requests = []
current_obj = []
brace_count = 0
obj_count = 0

with open('$BATCH_FILE', 'r', encoding='utf-8') as f:
    for line in f:
        # Count braces to detect complete JSON objects
        brace_count += line.count('{') - line.count('}')
        current_obj.append(line)
        
        # When braces are balanced, we have a complete JSON object
        if brace_count == 0 and current_obj:
            obj_str = ''.join(current_obj).strip()
            if obj_str:
                try:
                    obj = json.loads(obj_str)
                    requests.append(obj)
                    obj_count += 1
                except json.JSONDecodeError as e:
                    print(f"  ✗ Object {obj_count + 1}: Invalid JSON: {e}", file=sys.stderr)
                    VALIDATION_ERRORS += 1
            current_obj = []

# Handle any remaining object
if current_obj:
    obj_str = ''.join(current_obj).strip()
    if obj_str:
        try:
            obj = json.loads(obj_str)
            requests.append(obj)
            obj_count += 1
        except json.JSONDecodeError as e:
            print(f"  ✗ Object {obj_count + 1}: Invalid JSON: {e}", file=sys.stderr)
            VALIDATION_ERRORS += 1

if VALIDATION_ERRORS > 0:
    print(f"", file=sys.stderr)
    print(f"Error: Found {VALIDATION_ERRORS} invalid JSON object(s) in batch file", file=sys.stderr)
    sys.exit(1)

# API expects {"requests": [...]} format
batch_request = {"requests": requests}
json.dump(batch_request, sys.stdout, ensure_ascii=False)
PYTHON

if [ $? -ne 0 ]; then
  echo ""
  echo "Error: Failed to parse batch file"
  rm -f "$TEMP_JSON"
  exit 1
fi

REQUEST_COUNT=$(python3 -c "import json; print(len(json.load(open('$TEMP_JSON'))['requests']))")
echo "  ✓ Found $REQUEST_COUNT valid JSON objects"
echo "  ✓ Converted to JSON array format"
echo ""

# Submit batch
# Note: Anthropic Batch API expects JSON array format
RESPONSE=$(curl -s https://api.anthropic.com/v1/messages/batches \
  --header "x-api-key: $ANTHROPIC_API_KEY" \
  --header "anthropic-version: 2023-06-01" \
  --header "content-type: application/json" \
  --data-binary @"$TEMP_JSON")

# Clean up temp file
rm -f "$TEMP_JSON"

# Check for errors
if echo "$RESPONSE" | grep -q '"type":"error"'; then
  echo "Error submitting batch:"
  echo "$RESPONSE" | python3 -m json.tool
  exit 1
fi

# Extract batch ID
BATCH_ID=$(echo "$RESPONSE" | python3 -c "import sys, json; print(json.load(sys.stdin)['id'])")

echo "✓ Batch submitted successfully!"
echo ""
echo "Batch ID: $BATCH_ID"
echo ""
echo "Response:"
echo "$RESPONSE" | python3 -m json.tool
echo ""
echo "Next steps:"
echo "  1. Wait ~24 hours for processing"
echo "  2. Check status: ./tools/check-batch-status.sh $BATCH_ID"
echo "  3. Retrieve results: ./tools/retrieve-batch.sh $BATCH_ID"
echo ""
echo "Save this batch ID: $BATCH_ID"
