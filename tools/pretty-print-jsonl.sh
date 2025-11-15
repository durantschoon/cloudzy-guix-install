#!/usr/bin/env bash
# Pretty-print a JSONL file for human readability
# Each JSON object is formatted with indentation

set -euo pipefail

INPUT_FILE="${1:-}"
OUTPUT_FILE="${2:-}"

if [ -z "$INPUT_FILE" ]; then
  echo "Usage: $0 <input.jsonl> [output.jsonl]"
  echo "  If output is not specified, input file is modified in place"
  exit 1
fi

if [ ! -f "$INPUT_FILE" ]; then
  echo "Error: File not found: $INPUT_FILE"
  exit 1
fi

# Use temporary file if modifying in place
if [ -z "$OUTPUT_FILE" ]; then
  TEMP_FILE=$(mktemp)
  OUTPUT_FILE="$TEMP_FILE"
  IN_PLACE=1
else
  IN_PLACE=0
fi

# Pretty-print each line
python3 <<EOF
import json
import sys

input_file = '$INPUT_FILE'
output_file = '$OUTPUT_FILE'

with open(input_file, 'r', encoding='utf-8') as infile, \
     open(output_file, 'w', encoding='utf-8') as outfile:
    for line_num, line in enumerate(infile, 1):
        line = line.strip()
        if not line:
            outfile.write('\n')
            continue
        
        try:
            obj = json.loads(line)
            # Pretty-print with 2-space indentation
            json.dump(obj, outfile, indent=2, ensure_ascii=False)
            outfile.write('\n\n')  # Add blank line between objects for readability
        except json.JSONDecodeError as e:
            print(f"Error parsing line {line_num}: {e}", file=sys.stderr)
            # Write original line if it can't be parsed
            outfile.write(line)
            outfile.write('\n')
EOF

# Replace original if modifying in place
if [ "$IN_PLACE" -eq 1 ]; then
  mv "$TEMP_FILE" "$INPUT_FILE"
  echo "Pretty-printed: $INPUT_FILE"
else
  echo "Pretty-printed output written to: $OUTPUT_FILE"
fi

