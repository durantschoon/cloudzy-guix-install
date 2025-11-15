#!/usr/bin/env bash
# Check status of a batch conversion job

set -euo pipefail

BATCH_ID="${1:-}"

if [ -z "$BATCH_ID" ]; then
  echo "Usage: $0 <batch-id>"
  echo ""
  echo "Example:"
  echo "  $0 batch_01234567890abcdef"
  exit 1
fi

# Check for API key
if [ -z "${ANTHROPIC_API_KEY:-}" ]; then
  echo "Error: ANTHROPIC_API_KEY environment variable not set"
  exit 1
fi

echo "Checking batch status..."
echo "Batch ID: $BATCH_ID"
echo ""

# Get batch status
RESPONSE=$(curl -s "https://api.anthropic.com/v1/messages/batches/$BATCH_ID" \
  --header "x-api-key: $ANTHROPIC_API_KEY" \
  --header "anthropic-version: 2023-06-01")

# Check for errors
if echo "$RESPONSE" | grep -q '"type":"error"'; then
  echo "Error checking batch:"
  echo "$RESPONSE" | python3 -m json.tool
  exit 1
fi

# Display status
echo "$RESPONSE" | python3 -m json.tool

# Extract key info
PROCESSING_STATUS=$(echo "$RESPONSE" | python3 -c "import sys, json; print(json.load(sys.stdin)['processing_status'])")
REQUEST_COUNT=$(echo "$RESPONSE" | python3 -c "import sys, json; d=json.load(sys.stdin); print(d['request_counts']['total'])")
SUCCEEDED=$(echo "$RESPONSE" | python3 -c "import sys, json; d=json.load(sys.stdin); print(d['request_counts']['succeeded'])")
ERRORED=$(echo "$RESPONSE" | python3 -c "import sys, json; d=json.load(sys.stdin); print(d['request_counts']['errored'])")

echo ""
echo "Summary:"
echo "  Status: $PROCESSING_STATUS"
echo "  Total requests: $REQUEST_COUNT"
echo "  Succeeded: $SUCCEEDED"
echo "  Errored: $ERRORED"
echo ""

if [ "$PROCESSING_STATUS" = "ended" ]; then
  echo "✓ Batch processing complete!"
  echo ""
  echo "Retrieve results:"
  echo "  ./tools/retrieve-batch.sh $BATCH_ID"
else
  echo "⏳ Batch still processing..."
  echo ""
  echo "Check again later:"
  echo "  ./tools/check-batch-status.sh $BATCH_ID"
fi
