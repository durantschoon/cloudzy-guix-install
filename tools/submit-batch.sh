#!/usr/bin/env bash
# Submit batch conversion requests to Anthropic API

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BATCH_FILE="$SCRIPT_DIR/batch-requests.jsonl"

# Check for API key
if [ -z "${ANTHROPIC_API_KEY:-}" ]; then
  echo "Error: ANTHROPIC_API_KEY environment variable not set"
  echo ""
  echo "Set your API key:"
  echo "  export ANTHROPIC_API_KEY='your-api-key-here'"
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

# Count requests
REQUEST_COUNT=$(wc -l < "$BATCH_FILE")
echo "Submitting $REQUEST_COUNT conversion requests"
echo ""

# Submit batch
RESPONSE=$(curl -s https://api.anthropic.com/v1/messages/batches \
  --header "x-api-key: $ANTHROPIC_API_KEY" \
  --header "anthropic-version: 2023-06-01" \
  --header "content-type: application/json" \
  --data-binary @"$BATCH_FILE")

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
