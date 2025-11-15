#!/usr/bin/env bash
# Interactive JSONL viewer - prompts to select a file, then pages through it
# Works with both batch-requests.jsonl and batch-results.jsonl

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if less is available
if ! command -v less >/dev/null 2>&1; then
  echo "Error: 'less' command not found. Please install it to use this viewer."
  exit 1
fi

# If file path provided as argument, use it directly
if [ $# -gt 0 ]; then
  INPUT_FILE="$1"
  if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: File not found: $INPUT_FILE"
    exit 1
  fi
  # Skip to file processing
else
  # Find all .jsonl files in tools directory (where script lives) and current directory
  # Prefer tools directory since that's where batch files are typically located
  mapfile -t tools_files < <(find "$SCRIPT_DIR" -maxdepth 1 -name "*.jsonl" -type f 2>/dev/null | sort)
  mapfile -t current_files < <(find "${PWD}" -maxdepth 1 -name "*.jsonl" -type f 2>/dev/null | sort)
  
  # Combine and deduplicate (tools directory files take precedence)
  declare -A seen
  jsonl_files=()
  for file in "${tools_files[@]}" "${current_files[@]}"; do
    if [ -n "$file" ] && [ -z "${seen["$file"]:-}" ]; then
      jsonl_files+=("$file")
      seen["$file"]=1
    fi
  done
  
  if [ ${#jsonl_files[@]} -eq 0 ]; then
    echo "No .jsonl files found in:"
    echo "  - $SCRIPT_DIR"
    echo "  - ${PWD}"
    echo ""
    echo "Usage: $0 [jsonl-file-path]"
    exit 1
  fi

  # If only one file, use it automatically
  if [ ${#jsonl_files[@]} -eq 1 ]; then
    INPUT_FILE="${jsonl_files[0]}"
    echo "Found one .jsonl file: $(basename "$INPUT_FILE")"
    echo ""
  else
    # Display menu
    echo "Found ${#jsonl_files[@]} .jsonl file(s):"
    echo ""
    for i in "${!jsonl_files[@]}"; do
      file="${jsonl_files[$i]}"
      if [[ "$file" == "$SCRIPT_DIR"/* ]]; then
        echo "  $((i + 1)). $(basename "$file") (tools/)"
      else
        echo "  $((i + 1)). $(basename "$file") ($(basename "$(dirname "$file")")/)"
      fi
    done
    echo ""
    
    # Prompt for selection
    while true; do
      read -p "Select file number [1-${#jsonl_files[@]}]: " selection
      if [[ "$selection" =~ ^[0-9]+$ ]] && [ "$selection" -ge 1 ] && [ "$selection" -le ${#jsonl_files[@]} ]; then
        INPUT_FILE="${jsonl_files[$((selection - 1))]}"
        break
      else
        echo "Invalid selection. Please enter a number between 1 and ${#jsonl_files[@]}."
      fi
    done
    echo ""
  fi
fi

# Check if jq is available for colorization
USE_JQ=false
if command -v jq >/dev/null 2>&1; then
  USE_JQ=true
fi

# Process and display JSONL file
python3 - "$INPUT_FILE" "$USE_JQ" <<'PYTHON' | less -R
import json
import sys
import re

input_file = sys.argv[1]
use_jq = sys.argv[2] == "true"

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

def extract_text_blobs(obj, path=""):
    """Extract text blobs that need special formatting, return (modified_obj, text_blobs)"""
    text_blobs = {}
    
    if isinstance(obj, dict):
        modified = {}
        for key, val in obj.items():
            current_path = f"{path}.{key}" if path else key
            if isinstance(val, str) and ('\n' in val or '\\n' in val):
                # Store text blob and replace with placeholder
                text_blobs[current_path] = val.replace('\\n', '\n')
                modified[key] = f"__TEXT_BLOB_{current_path}__"
            elif isinstance(val, (dict, list)):
                sub_modified, sub_blobs = extract_text_blobs(val, current_path)
                modified[key] = sub_modified
                text_blobs.update(sub_blobs)
            else:
                modified[key] = val
        return modified, text_blobs
    
    elif isinstance(obj, list):
        modified = []
        for i, item in enumerate(obj):
            current_path = f"{path}[{i}]" if path else f"[{i}]"
            if isinstance(item, dict) and 'text' in item:
                text_content = item.get('text', '')
                if '\n' in text_content or '\\n' in text_content:
                    # Store text blob
                    text_blobs[current_path] = text_content.replace('\\n', '\n')
                    # Keep structure but mark text field
                    modified_item = item.copy()
                    modified_item['text'] = f"__TEXT_BLOB_{current_path}__"
                    modified.append(modified_item)
                else:
                    sub_modified, sub_blobs = extract_text_blobs(item, current_path)
                    modified.append(sub_modified)
                    text_blobs.update(sub_blobs)
            elif isinstance(item, (dict, list)):
                sub_modified, sub_blobs = extract_text_blobs(item, current_path)
                modified.append(sub_modified)
                text_blobs.update(sub_blobs)
            else:
                modified.append(item)
        return modified, text_blobs
    
    return obj, text_blobs

def detect_code_language(text):
    """Detect programming language from text content"""
    text_lower = text.lower()
    first_lines = text.split('\n')[:5]
    first_100_chars = text[:100].lower()
    
    # Check for shebangs
    if text.startswith('#!/'):
        if 'python' in first_lines[0] or 'py' in first_lines[0]:
            return 'python'
        elif 'bash' in first_lines[0] or 'sh' in first_lines[0]:
            return 'bash'
        elif 'guile' in first_lines[0]:
            return 'scheme'
        elif 'ruby' in first_lines[0]:
            return 'ruby'
        elif 'perl' in first_lines[0]:
            return 'perl'
        elif 'node' in first_lines[0]:
            return 'javascript'
    
    # Check for common patterns
    if 'use-modules' in first_100_chars or '(define ' in first_100_chars or '(use-modules' in first_100_chars:
        return 'scheme'
    if 'def ' in first_100_chars and ('import ' in first_100_chars or 'from ' in first_100_chars):
        return 'python'
    if 'function ' in first_100_chars or 'const ' in first_100_chars or 'let ' in first_100_chars:
        return 'javascript'
    if 'package ' in first_100_chars and 'import ' in first_100_chars:
        return 'go'
    if 'fn ' in first_100_chars or 'let ' in first_100_chars and 'mut ' in first_100_chars:
        return 'rust'
    if 'class ' in first_100_chars and ('public ' in first_100_chars or 'private ' in first_100_chars):
        return 'java'
    if '<?php' in first_100_chars:
        return 'php'
    if '<!DOCTYPE' in first_100_chars or '<html' in first_100_chars:
        return 'html'
    if 'SELECT ' in text or 'INSERT ' in text or 'CREATE TABLE' in text:
        return 'sql'
    if '```' in text:
        # Try to extract language from markdown code blocks
        import re
        lang_match = re.search(r'```(\w+)', text)
        if lang_match:
            return lang_match.group(1)
    
    return None

def highlight_code(text, language=None):
    """Apply syntax highlighting to code using pygmentize if available"""
    if language is None:
        language = detect_code_language(text)
    
    if language is None:
        return None
    
    # Try pygmentize first (most common)
    import subprocess
    for cmd in ['pygmentize', 'pygmentize-3', 'pygmentize-3.11']:
        try:
            process = subprocess.Popen(
                [cmd, '-l', language, '-f', 'terminal256', '-O', 'style=monokai'],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            stdout, stderr = process.communicate(input=text, timeout=5)
            if process.returncode == 0 and stdout:
                return stdout
        except (FileNotFoundError, subprocess.TimeoutExpired):
            continue
    
    # Try bat as fallback
    try:
        process = subprocess.Popen(
            ['bat', '--language', language, '--style', 'plain', '--color', 'always', '--decorations', 'never'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdout, stderr = process.communicate(input=text, timeout=5)
        if process.returncode == 0 and stdout:
            return stdout
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass
    
    return None

def insert_text_blobs(formatted_json, text_blobs, use_syntax_highlighting=True):
    """Insert formatted text blobs into JSON output, handling jq color codes and syntax highlighting"""
    lines = formatted_json.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check if this line contains a text blob placeholder (handle ANSI codes)
        placeholder_found = False
        import re
        for path, text_content in text_blobs.items():
            placeholder = f'__TEXT_BLOB_{path}__'
            if placeholder in line:
                # Extract indentation (strip ANSI codes first)
                ansi_stripped = re.sub(r'\x1b\[[0-9;]*m', '', line)
                # Find key name before the placeholder
                # Pattern: "key": "placeholder"
                key_match = re.search(r'(\s+)"([^"]+)"\s*:\s*"[^"]*' + re.escape(placeholder) + r'[^"]*"', ansi_stripped)
                if key_match:
                    indent = key_match.group(1)
                    key = key_match.group(2)
                    
                    # Try syntax highlighting if enabled
                    highlighted = None
                    if use_syntax_highlighting:
                        highlighted = highlight_code(text_content)
                    
                    # Replace the entire line with key and multi-line text
                    # Preserve ANSI codes for the key part
                    ansi_reset = '\x1b[0m'
                    ansi_key = '\x1b[1;34m'  # Blue for keys (jq default)
                    ansi_string = '\x1b[0;32m'  # Green for strings (jq default)
                    ansi_normal = '\x1b[1;39m'  # Normal color
                    
                    result_lines.append(f'{indent}{ansi_key}"{key}"{ansi_normal}: {ansi_string}|{ansi_reset}')
                    
                    if highlighted:
                        # Use syntax-highlighted version
                        highlighted_lines = highlighted.rstrip().split('\n')
                        for hl_line in highlighted_lines:
                            result_lines.append(f'{indent}  {hl_line}')
                    else:
                        # Use plain text with color
                        for text_line in text_content.split('\n'):
                            result_lines.append(f'{indent}  {ansi_string}{text_line}{ansi_reset}')
                    
                    placeholder_found = True
                    i += 1
                    break
        
        if not placeholder_found:
            result_lines.append(line)
            i += 1
    
    return "\n".join(result_lines)

for i, obj in enumerate(objects, 1):
    # Print header
    print("=" * 80)
    print(f"Object {i} of {total}")
    print("=" * 80)
    print("")
    
    if use_jq:
        # Extract text blobs, get JSON structure, colorize with jq, then insert text blobs
        modified_obj, text_blobs = extract_text_blobs(obj)
        json_str = json.dumps(modified_obj, ensure_ascii=False, indent=2)
        
        import subprocess
        jq_process = subprocess.Popen(['jq', '-C', '.'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        jq_output, _ = jq_process.communicate(input=json_str)
        
        # Check if syntax highlighting tools are available
        use_syntax_highlighting = False
        for cmd in ['pygmentize', 'bat']:
            try:
                subprocess.run([cmd, '--version'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=1)
                use_syntax_highlighting = True
                break
            except (FileNotFoundError, subprocess.TimeoutExpired):
                continue
        
        # Insert formatted text blobs with optional syntax highlighting
        final_output = insert_text_blobs(jq_output.rstrip(), text_blobs, use_syntax_highlighting)
        print(final_output)
    else:
        # Use Python formatting
        formatted = format_value(obj, indent=0, key_name="")
        print(formatted)
    
    print("")
    print("=" * 80)
    print("")
PYTHON

