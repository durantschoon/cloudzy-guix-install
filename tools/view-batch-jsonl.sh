#!/usr/bin/env bash
# View JSONL files with proper formatting and text blob handling
# Replaces \n in text fields with actual newlines for readability

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="${1:-}"

if [ -z "$INPUT_FILE" ]; then
  echo "Usage: $0 <jsonl-file>"
  echo ""
  echo "Examples:"
  echo "  $0 batch-requests.jsonl"
  echo "  $0 batch-results.jsonl"
  exit 1
fi

if [ ! -f "$INPUT_FILE" ]; then
  echo "Error: File not found: $INPUT_FILE"
  exit 1
fi

# Check if less is available
if ! command -v less >/dev/null 2>&1; then
  echo "Error: 'less' command not found. Please install it to use this viewer."
  exit 1
fi

# Process and display JSONL file
python3 - "$INPUT_FILE" <<'PYTHON' | less -R
import json
import sys
import re

input_file = sys.argv[1]

def replace_escaped_newlines(text):
    """Replace \\n with actual newlines in text strings"""
    if not isinstance(text, str):
        return text
    return text.replace('\\n', '\n')

def format_value(value, indent=0, max_depth=10, key_name=""):
    """Recursively format JSON values, handling text blobs"""
    if max_depth <= 0:
        return "... (max depth reached)"
    
    indent_str = "  " * indent
    
    if isinstance(value, dict):
        if not value:
            return "{}"
        lines = ["{"]
        for i, (key, val) in enumerate(value.items()):
            comma = "," if i < len(value) - 1 else ""
            formatted_val = format_value(val, indent + 1, max_depth - 1, key)
            lines.append(f"{indent_str}  {json.dumps(key)}: {formatted_val}{comma}")
        lines.append(f"{indent_str}}}")
        return "\n".join(lines)
    elif isinstance(value, list):
        if not value:
            return "[]"
        # Special handling for content arrays (common in API responses)
        if len(value) == 1 and isinstance(value[0], dict) and 'text' in value[0]:
            # This is likely a content array with text - format the text nicely
            text_content = value[0].get('text', '')
            if '\\n' in text_content or len(text_content) > 200:
                processed = replace_escaped_newlines(text_content)
                lines = ["["]
                lines.append(f"{indent_str}  {{")
                lines.append(f'{indent_str}    "type": {json.dumps(value[0].get("type", ""))},')
                lines.append(f'{indent_str}    "text": """')
                for line in processed.split('\n'):
                    lines.append(f"{indent_str}      {line}")
                lines.append(f'{indent_str}    """')
                lines.append(f"{indent_str}  }}")
                lines.append(f"{indent_str}]")
                return "\n".join(lines)
        # Regular list
        lines = ["["]
        for i, item in enumerate(value):
            comma = "," if i < len(value) - 1 else ""
            formatted_item = format_value(item, indent + 1, max_depth - 1)
            lines.append(f"{indent_str}  {formatted_item}{comma}")
        lines.append(f"{indent_str}]")
        return "\n".join(lines)
    elif isinstance(value, str):
        # Check if it's a large text blob (like script content)
        # Special handling for 'text' fields which often contain code
        if key_name == "text" or len(value) > 200 or '\\n' in value:
            # Replace escaped newlines
            processed = replace_escaped_newlines(value)
            # If it has newlines, format it nicely
            if '\n' in processed:
                lines = ['"""']
                for line in processed.split('\n'):
                    lines.append(f"{indent_str}  {line}")
                lines.append(f'{indent_str}"""')
                return "\n".join(lines)
        # Regular string - truncate if very long
        if len(value) > 200:
            return json.dumps(value[:197] + "...")
        return json.dumps(value)
    else:
        return json.dumps(value)

def format_json_object(obj, obj_num, total):
    """Format a single JSON object for display"""
    output = []
    output.append("=" * 80)
    output.append(f"Object {obj_num} of {total}")
    output.append("=" * 80)
    output.append("")
    
    # Format the entire object
    formatted = format_value(obj, indent=0, key_name="")
    output.append(formatted)
    output.append("")
    output.append("=" * 80)
    output.append("")
    
    return "\n".join(output)

# Parse JSONL file (handles both single-line and multi-line pretty-printed)
objects = []
with open(input_file, 'r', encoding='utf-8') as f:
    content = f.read()
    
    # Try parsing as multi-line pretty-printed JSON first
    current_obj = []
    brace_count = 0
    
    for line in content.split('\n'):
        brace_count += line.count('{') - line.count('}')
        current_obj.append(line)
        
        # When braces are balanced, we have a complete JSON object
        if brace_count == 0 and current_obj:
            obj_str = '\n'.join(current_obj).strip()
            if obj_str:
                try:
                    obj = json.loads(obj_str)
                    objects.append(obj)
                except json.JSONDecodeError:
                    pass  # Skip invalid JSON
            current_obj = []

# If no objects found, try single-line JSONL
if not objects:
    with open(input_file, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
                objects.append(obj)
            except json.JSONDecodeError:
                pass

# Display all objects
total = len(objects)
if total == 0:
    print("No valid JSON objects found in file.")
    sys.exit(1)

for i, obj in enumerate(objects, 1):
    print(format_json_object(obj, i, total))
PYTHON

