#!/usr/bin/env bash
# Check status of a batch conversion job

set -euo pipefail

BATCH_ID="${1:-}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if [ -z "$BATCH_ID" ]; then
  echo "Usage: $0 <batch-id>"
  echo ""
  echo "Example:"
  echo "  $0 msgbatch_01HJGK7MZ3X5QR8W9P2N4V6B7D"
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
