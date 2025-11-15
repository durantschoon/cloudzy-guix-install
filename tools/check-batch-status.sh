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

# Check for errors first
if echo "$RESPONSE" | grep -q '"type":"error"'; then
  echo "Error checking batch:"
  echo "$RESPONSE" | python3 -m json.tool
  exit 1
fi

# Display formatted JSON
echo "$RESPONSE" | python3 -m json.tool

# Extract key info (use a temp file to avoid stdin consumption issues)
TEMP_FILE=$(mktemp)
echo "$RESPONSE" > "$TEMP_FILE"

read -r PROCESSING_STATUS REQUEST_COUNT SUCCEEDED ERRORED <<< "$(python3 <<PYTHON
import json
import sys

with open('$TEMP_FILE', 'r') as f:
    data = json.load(f)

counts = data['request_counts']
# Calculate total if not present
total = counts.get('total', 
    counts.get('succeeded', 0) + 
    counts.get('errored', 0) + 
    counts.get('canceled', 0) + 
    counts.get('expired', 0) + 
    counts.get('processing', 0))

print(f"{data['processing_status']} {total} {counts['succeeded']} {counts['errored']}")
PYTHON
)"

rm -f "$TEMP_FILE"

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
