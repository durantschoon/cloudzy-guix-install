# Batch Conversion System Generalization Plan

## Current State: Bash → Guile Specific

The current system is **well-structured for generalization** but has language-specific hardcoding.

## Architecture Analysis

### ✅ Fully Generalizable (No Changes Needed)

These components work for any language pair:

1. **Batch API Interaction**
   - `submit-batch.sh` - Generic API submission
   - `check-batch-status.sh` - Generic status checking
   - `retrieve-batch.sh` - Generic result retrieval (core logic)
   - `.env` support - Generic configuration

2. **File Handling**
   - JSONL generation/parsing
   - Temporary file handling
   - Path reconstruction with `__SLASH__` separator
   - Backup/versioning logic

3. **Viewing & Diffing**
   - `view-jsonl.sh` - Generic JSONL viewer
   - `diff-conversions.sh` - Generic diff viewer (just needs file extension config)

4. **Verification Framework**
   - `verify-setup.sh` - Structure is general (just needs config)

### 🔧 Needs Parameterization (Easy to Generalize)

These need configuration but are well-structured:

1. **File Discovery** (`generate-batch-conversion.sh`)
   - **Current**: Hardcoded `.sh` extension, specific exclusions
   - **Generalized**: Config file with:
     ```bash
     SOURCE_EXT=".sh"
     TARGET_EXT=".scm"
     EXCLUDE_PATTERNS=("tools/" "archive/" "*test*")
     ```

2. **Documentation Loading** (`generate-batch-conversion.sh`)
   - **Current**: Hardcoded `GUILE_*.md` files
   - **Generalized**: Config array:
     ```bash
     DOC_FILES=(
       "docs/TARGET_LANG_KNOWLEDGE.md"
       "docs/TARGET_LANG_BEST_PRACTICES.md"
       "docs/TARGET_LANG_GOTCHAS.md"
       "docs/SOURCE_TO_TARGET_CONVERSION.md"
     )
     ```

3. **Conversion Prompt** (`generate-batch-conversion.sh`)
   - **Current**: Hardcoded "bash → Guile" instructions
   - **Generalized**: Template file or config:
     ```bash
     PROMPT_TEMPLATE="docs/conversion-prompt-template.md"
     # Or config variables:
     SOURCE_LANG="bash"
     TARGET_LANG="Guile Scheme"
     ```

4. **Test File Extraction** (`retrieve-batch.sh`)
   - **Current**: Hardcoded `test-*.scm` pattern
   - **Generalized**: Config:
     ```bash
     TEST_FILE_PREFIX="test-"
     TEST_FILE_EXT=".scm"
     TEST_DELIMITER="---TEST FILE---"
     ```

5. **Comment Validation** (`validate-comment-structure.sh`)
   - **Current**: Hardcoded `#` → `;;;` patterns
   - **Generalized**: Config:
     ```bash
     SOURCE_COMMENT_PATTERN='^#[[:space:]]+[A-Z]'
     TARGET_COMMENT_PATTERN='^;;;[[:space:]]+[A-Z]|^;;[[:space:]]+[A-Z]'
     ```

6. **Test Runner Integration** (`run-tests.sh`)
   - **Current**: Hardcoded Guile test execution
   - **Generalized**: Config:
     ```bash
     TEST_RUNNER_CMD="guile --no-auto-compile -s"
     # Or for Python: "python3 -m pytest"
     # Or for Go: "go test"
     ```

### 📋 Configuration File Structure

Proposed `tools/conversion-config.sh`:

```bash
#!/usr/bin/env bash
# Configuration for batch conversion system
# Copy this file and customize for your language pair

# Language pair
SOURCE_LANG="bash"
TARGET_LANG="Guile Scheme"
SOURCE_EXT=".sh"
TARGET_EXT=".scm"

# File discovery
SOURCE_PATTERN="*.sh"
EXCLUDE_PATTERNS=(
  "tools/"
  "archive/"
  "*test*"
  "update-manifest.sh"
  "run-tests.sh"
  "test-docker.sh"
)

# Documentation files (relative to repo root)
DOC_FILES=(
  "docs/GUILE_KNOWLEDGE.md"
  "docs/GUILE_BEST_PRACTICES.md"
  "docs/GUILE_GOTCHAS.md"
  "docs/GUILE_CONVERSION.md"
)

# Conversion prompt template
PROMPT_TEMPLATE=""  # If empty, uses built-in template
# Or specify custom template file:
# PROMPT_TEMPLATE="docs/custom-conversion-prompt.md"

# Test configuration
TEST_FILE_PREFIX="test-"
TEST_FILE_EXT=".scm"
TEST_DELIMITER="---TEST FILE---"
TEST_RUNNER_CMD="guile --no-auto-compile -s"

# Comment patterns for validation
SOURCE_COMMENT_PATTERN='^#[[:space:]]+[A-Z]'
TARGET_COMMENT_PATTERN='^;;;[[:space:]]+[A-Z]|^;;[[:space:]]+[A-Z]'

# Output directories
OUTPUT_DIR="tools/converted-scripts"
BATCH_REQUESTS_FILE="tools/batch-requests.jsonl"
BATCH_RESULTS_FILE="tools/batch-results.jsonl"
```

## Generalization Strategy

### Phase 1: Extract Configuration (Low Risk)
1. Create `conversion-config.sh` template
2. Move hardcoded values to config
3. Update scripts to source config
4. Keep backward compatibility (defaults to bash→Guile)

### Phase 2: Template System (Medium Effort)
1. Create prompt template system
2. Support custom prompt templates
3. Allow per-language customization

### Phase 3: Language Packs (Future)
1. Create `language-packs/` directory
2. Each pack contains:
   - `config.sh` - Language-specific config
   - `prompt-template.md` - Conversion prompt
   - `validation-rules.sh` - Comment/pattern validation
   - `test-runner.sh` - How to run tests
3. Example packs:
   - `language-packs/bash-to-guile/`
   - `language-packs/python-to-rust/`
   - `language-packs/javascript-to-typescript/`

## Current Readiness Assessment

### ✅ Ready for Generalization: **85%**

**Strengths:**
- Clean separation of concerns
- Well-structured scripts
- Generic API interaction
- Good error handling
- Comprehensive documentation

**What Needs Work:**
- Extract ~15 hardcoded values to config
- Create configuration system
- Template prompt system
- Language pack structure

**Estimated Effort:**
- **Phase 1** (Config extraction): 2-3 hours
- **Phase 2** (Templates): 4-6 hours  
- **Phase 3** (Language packs): 8-12 hours

## Recommendation

**The system is in excellent shape for generalization!**

The architecture is already well-separated:
- API interaction is generic
- File handling is generic
- Only the "conversion prompt generation" is language-specific

**Next Steps:**
1. Create `conversion-config.sh` with current bash→Guile values
2. Refactor scripts to source config (with defaults)
3. Test that bash→Guile still works
4. Create example config for different language pair
5. Document language pack creation process

**This would make it a reusable tool for any language conversion!**

