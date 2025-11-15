# Batch Conversion Best Practices

## Pre-Conversion Checklist

Before converting bash scripts to Guile Scheme, follow these best practices:

### 1. Add Structured Comments First ⭐ **RECOMMENDED**

**Why:** Adding structured section headers to bash scripts before conversion ensures:
- Better organization and maintainability of original scripts
- Converted scripts will have matching structure (easier diffs)
- AI doesn't have to guess what structure to add
- Validation passes automatically

**How:**
1. Review the script and identify logical sections
2. Add section headers using `# Section Name` format
3. Use consistent naming (e.g., `# Configuration`, `# Helper Functions`, `# Main Logic`)
4. Place headers immediately before the code they describe
5. Aim for 5+ section headers for scripts with substantial logic

**Example:**
```bash
#!/usr/bin/env bash

# Configuration
VAR1="value1"
VAR2="value2"

# Helper Functions
helper_function() {
    # ...
}

# Main Logic
main() {
    # ...
}
```

**Validation:**
After adding structured comments, run:
```bash
./tools/validate-comment-structure.sh
```

This will verify your script has structured sections (5+ headers).

### 2. Review Script Organization

Before conversion, ensure:
- Functions are logically grouped
- Related code is together
- Clear separation of concerns
- No dead code or commented-out blocks

### 3. Document Complex Logic

Add comments explaining:
- Why something is done (not just what)
- Non-obvious workarounds
- Platform-specific behavior
- Dependencies or requirements

### 4. Test Original Script

Ensure the bash script works correctly before conversion:
- Run it in a test environment
- Verify all edge cases
- Check error handling

### 5. Generate Conversion Request

Once the script is well-organized:
```bash
./tools/generate-batch-conversion.sh
```

The conversion will preserve your structured comments automatically.

## Post-Conversion Validation

After conversion, verify:
1. **Comment Structure:** `./tools/validate-comment-structure.sh`
2. **Syntax:** `guile --no-auto-compile -s converted-script.scm`
3. **Functionality:** Run tests (if available)
4. **Diffs:** `./tools/diff-conversions.sh` to review changes

## Retroactive Application

For scripts already converted:
- You can still add structured comments to originals
- Re-convert if needed (though this costs API credits)
- Or accept the AI-added structure if it's reasonable

## Scripts That Need Structured Comments

Run this to see which scripts lack structured sections:
```bash
./tools/validate-comment-structure.sh | grep "no structured sections"
```

These scripts would benefit from adding structured comments before conversion.

