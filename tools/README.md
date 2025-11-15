# Batch Conversion Tools

Tools for converting bash scripts to Guile Scheme using the Anthropic Batch API.

## Overview

These scripts automate the conversion of `.sh` files to `.scm` files using Claude's batch processing feature. This provides:

- **50% cost savings** vs interactive conversion
- **Consistent quality** - all conversions use the same documentation
- **Parallel processing** - all scripts converted simultaneously
- **Reproducible** - can re-run with updated documentation

## Prerequisites

1. **Anthropic API Key**: Get from https://console.anthropic.com/settings/keys
2. **Python 3**: For JSON processing
3. **curl**: For API requests
4. **API Key Setup** (choose one method):

   **Option 1: .env file (recommended)**
   ```bash
   cp tools/.env.example tools/.env
   # Edit tools/.env and add your actual API key
   ```

   **Option 2: Environment variable**
   ```bash
   export ANTHROPIC_API_KEY='your-api-key-here'
   ```

   The scripts will automatically load from `tools/.env` if it exists, otherwise they'll use the environment variable.

## Workflow

### 1. Generate Batch Requests

```bash
./tools/generate-batch-conversion.sh
```

**What it does:**
- Finds all `.sh` scripts that run on Guix OS:
  - `postinstall/recipes/*.sh` - Shared recipe scripts
  - `lib/*.sh` - Library scripts used during/after installation
  - Platform-specific scripts (if any)
- Excludes development tools (`tools/`, `archive/`, test files, `update-manifest.sh`, etc.)
- Reads all Guile documentation (KNOWLEDGE, BEST_PRACTICES, GOTCHAS, CONVERSION)
- Creates batch request for each script
- Outputs to `tools/batch-requests.jsonl`

**Output:**
```
Generating batch conversion requests...
  [1] ./lib/postinstall.sh
  [2] ./postinstall/recipes/add-development.sh
  [3] ./postinstall/recipes/add-doom-emacs.sh
  [4] ./postinstall/recipes/add-fonts.sh
  [5] ./postinstall/recipes/add-spacemacs.sh
  [6] ./postinstall/recipes/add-vanilla-emacs.sh

Generated 6 batch requests
```

**Note:** The script finds all `.sh` files that run on Guix OS, including:
- `postinstall/recipes/*.sh` - Shared recipe scripts
- `lib/*.sh` - Library scripts used during/after installation
- Platform-specific scripts (if any)

It excludes development tools in `tools/` and archived files in `archive/`.

### 2. Submit Batch

```bash
./tools/submit-batch.sh
```

**What it does:**
- Uploads batch requests to Anthropic API
- Starts background processing (24-hour turnaround)
- Returns batch ID for tracking

**Output:**
```
✓ Batch submitted successfully!

Batch ID: batch_01HJGK7MZ3X5QR8W9P2N4V6B7D

Save this batch ID: batch_01HJGK7MZ3X5QR8W9P2N4V6B7D
```

**Save the batch ID!** You'll need it to check status and retrieve results.

### 3. Check Status (Optional)

```bash
./tools/check-batch-status.sh batch_01HJGK7MZ3X5QR8W9P2N4V6B7D
```

**What it does:**
- Queries batch processing status
- Shows progress (total, succeeded, errored)
- Indicates when ready for retrieval

**Output:**
```
Summary:
  Status: in_progress
  Total requests: 3
  Succeeded: 0
  Errored: 0

⏳ Batch still processing...
```

Or when complete:
```
Summary:
  Status: ended
  Total requests: 3
  Succeeded: 3
  Errored: 0

✓ Batch processing complete!
```

### 4. Retrieve Results

```bash
./tools/retrieve-batch.sh batch_01HJGK7MZ3X5QR8W9P2N4V6B7D
```

**What it does:**
- Downloads conversion results from API
- Saves raw results to `tools/batch-results.jsonl`
- **Extracts `.scm` files** from results to `tools/converted-scripts/`
- Makes scripts executable
- Reports success/error counts

**Output:**
```
Results downloaded to: tools/batch-results.jsonl

Extracting converted scripts...
  ✓ postinstall/recipes/add-spacemacs.scm
  ✓ postinstall/recipes/add-development.scm
  ✓ postinstall/recipes/add-fonts.scm

Extraction complete!
  Succeeded: 3
  Errored: 0

Converted scripts written to: tools/converted-scripts
```

**Important:** This step extracts the converted `.scm` files from the batch results. The files are now ready for review and testing in `tools/converted-scripts/`.

### 5. View Results (Interactive Viewer) - Optional

**You can use this tool with existing batch files from previous jobs:**

```bash
cd tools
./view-jsonl.sh
```

**Works with:**
- Existing `batch-requests.jsonl` files (from previous batch generations)
- Existing `batch-results.jsonl` files (from previous batch retrievals)
- No need to run a new batch job - works with any JSONL file in the current directory

**You can view the raw JSONL files before or after extraction:**

```bash
cd tools
./view-jsonl.sh
```

**View batch files with syntax highlighting:**

```bash
cd tools
./view-jsonl.sh
```

**What it does:**
- Prompts you to select which `.jsonl` file to view (`batch-requests.jsonl` or `batch-results.jsonl`)
- Automatically detects and uses `jq` for JSON colorization (if available)
- Detects code blocks and applies syntax highlighting using `pygmentize` or `bat`
- Formats text blobs with actual newlines (replaces `\n` with real line breaks)
- Uses `less` for paging through multiple objects

**Features:**
- **Colorized JSON structure** - Keys, values, and structure are colorized
- **Syntax-highlighted code** - Code blocks (Scheme, Python, Bash, etc.) are highlighted
- **Readable text** - Multi-line text displays with proper formatting
- **Interactive paging** - Navigate with arrow keys, Page Up/Down, spacebar
- **Auto-detection** - Automatically detects language from shebangs and patterns

**Example output:**
```
Found 2 .jsonl file(s) in current directory:

  1. batch-requests.jsonl
  2. batch-results.jsonl

Select file number [1-2]: 2

================================================================================
Object 1 of 5
================================================================================

{
  "custom_id": "convert-postinstall-recipes-add-development",
  "result": {
    "type": "succeeded",
    "message": {
      "content": |
        #!/run/current-system/profile/bin/guile --no-auto-compile -s
        !#
        
        (use-modules (ice-9 popen)
                     (ice-9 rdelim)
                     ...)
        [syntax-highlighted Scheme code]
```

**Prerequisites for best experience:**
- `jq` - For JSON colorization (optional but recommended)
- `pygmentize` or `bat` - For code syntax highlighting (optional but recommended)
- `less` - For paging (required)

**Installation:**
```bash
# macOS
brew install jq pygments bat

# Guix
guix install jq python-pygments bat

# Ubuntu/Debian
sudo apt install jq python3-pygments bat
```

**Without syntax highlighting tools:**
The viewer still works but displays plain text without colorization. All functionality remains available.

**When to use the viewer:**
- **Before extraction** - Review what was converted before extracting files
- **After extraction** - Compare extracted `.scm` files with what's in the JSONL
- **Debugging** - Check error messages in failed conversions
- **Quality review** - See the full API response including metadata

**Note:** The viewer shows the raw JSONL content. The actual `.scm` files are extracted in step 4 above and saved to `tools/converted-scripts/`.

### 6. Review and Test

**Interactive diff viewer (recommended):**
```bash
./tools/diff-conversions.sh
```

**Works with existing converted scripts:**
- No need to run a new batch job
- Works with any `.scm` files already in `tools/converted-scripts/`
- Automatically finds and matches them with original `.sh` files

**Features:**
- **Colorized output** - Uses `git diff` or `colordiff` for syntax highlighting
- **Interactive paging** - Navigate with arrow keys, Page Up/Down, spacebar
- **Handles both path formats** - Works with old nested structure and new flat structure

**View in Emacs (diff-mode):**
```bash
# Generate patch file
./tools/diff-conversions.sh --patch

# Then in emacs:
# C-x C-f tools/conversions.patch
# (Emacs automatically uses diff-mode with syntax highlighting)
```

**View in magit (temporarily stage files):**
```bash
# Temporarily replace .sh files with .scm content
./tools/diff-conversions.sh --stage-for-magit

# Then in emacs:
# M-x magit-status
# Navigate to unstaged changes and press RET on files to view diffs

# When done, restore original files:
git checkout -- <files>
# Or use the restore command:
./tools/diff-conversions.sh --restore-from-backup
```

**Note:** The `--stage-for-magit` mode backs up your original `.sh` files to `tools/.magit-diff-backup/` before temporarily replacing them with the converted `.scm` content. This allows magit to show them as modified files with full diff viewing capabilities.

This script:
- Finds all converted `.scm` files in `tools/converted-scripts/`
- Matches them with their original `.sh` files (handles both old and new path formats)
- Shows unified diffs for each pair
- Pages through all changes interactively with `less` (or outputs git diff format for magit)

**Manual diff (alternative):**
```bash
# Visual diff
diff postinstall/recipes/add-spacemacs.sh \
     tools/converted-scripts/postinstall/recipes/add-spacemacs.scm

# Side-by-side
diff -y postinstall/recipes/add-spacemacs.sh \
        tools/converted-scripts/postinstall/recipes/add-spacemacs.scm
```

**Validate comment structure matching:**
```bash
./tools/validate-comment-structure.sh
```

This script checks if converted `.scm` scripts have matching comment section headers with their original `.sh` scripts. Matching comment sections make diffs much more readable by showing corresponding sections side-by-side.

**What it checks:**
- Extracts comment headers from both original and converted scripts
- Compares section names and order
- Reports mismatches or missing sections

**Tip:** The conversion prompt now includes instructions to preserve comment structure. If original scripts have structured comments (e.g., `# Configuration`, `# Helper functions`), they'll be converted to matching Guile comments (`;;; Configuration`, `;;; Helper functions`). This ensures diffs show corresponding sections together.

**Manual review checklist:**
- [ ] Shebang correct: `#!/run/current-system/profile/bin/guile --no-auto-compile -s`
- [ ] All `use-modules` present (ice-9, srfi-1, etc.)
- [ ] Functions converted to procedures
- [ ] Bash commands → Guile equivalents
- [ ] GNU coreutils used (sha256sum not shasum)
- [ ] Comments explain complex conversions
- [ ] Naming conventions followed (?, !, ->)
- [ ] Comment sections match original structure (use `validate-comment-structure.sh`)

**Test converted scripts:**

The batch conversion process now automatically generates test files (`test-*.scm`) alongside converted scripts. Tests are extracted and placed in the same directory structure.

**Run all tests (including converted script tests):**
```bash
# From repo root - runs Go tests and Guile tests (including converted scripts)
./run-tests.sh

# Or run in Docker (includes Guile)
./test-docker.sh
```

**Run only converted script tests:**
```bash
# Find and run all test-*.scm files
find tools/converted-scripts -name "test-*.scm" -exec guile --no-auto-compile -s {} \;
```

**Test structure:**
- Test files are named `test-<script-name>.scm`
- Placed in same directory as converted script
- Use Guile's test framework (srfi-64) or simple assertions
- Test key functions/procedures
- Use temporary files for file operations
- Tests are automatically extracted from batch results

**Copy converted scripts to final locations:**
```bash
# Copy to test location (from repo root)
cp tools/converted-scripts/postinstall/recipes/add-spacemacs.scm \
   postinstall/recipes/add-spacemacs.scm

# Or copy library scripts
cp tools/converted-scripts/lib/postinstall.scm \
   lib/postinstall.scm

# Run tests
./run-tests.sh

# Test specific script
framework-dual/postinstall/tests/run-guile-tests.sh
```

**If tests pass:**
```bash
# Copy converted scripts to their final locations (from repo root)
cp tools/converted-scripts/postinstall/recipes/*.scm postinstall/recipes/
cp tools/converted-scripts/lib/*.scm lib/

# Update manifest
./update-manifest.sh

# Commit
git add postinstall/recipes/*.scm lib/*.scm SOURCE_MANIFEST.txt
git commit -m "Convert scripts from bash to Guile (batch conversion)"
```

**If issues found:**
1. Note patterns in GUILE_GOTCHAS.md
2. Update documentation
3. Fix manually or re-run batch with improved docs

## Cost Estimate

**For this repo (3 recipe scripts):**
- Input: ~15K tokens per script × 3 = 45K tokens = $0.07
- Output: ~2K tokens per script × 3 = 6K tokens = $0.05
- **Total: ~$0.12**

Compare to interactive conversion: ~$0.24 (100% more expensive)

## Files Generated

```
tools/
├── batch-requests.jsonl         # Input: Batch API requests (compact JSONL)
├── batch-results.jsonl          # Output: Raw results from API (compact JSONL)
├── view-jsonl.sh               # Interactive viewer with syntax highlighting
├── pretty-print-jsonl.sh       # Utility to pretty-print JSONL files
└── converted-scripts/           # Output: Extracted .scm files
    └── postinstall/
        └── recipes/
            ├── add-spacemacs.scm
            ├── add-development.scm
            └── add-fonts.scm
```

**Note:** Batch files are stored in compact JSONL format (one JSON object per line) for efficiency. Use `view-jsonl.sh` to view them with proper formatting and syntax highlighting.

## Pre-Flight Check

Before running a batch conversion, verify everything is set up:

```bash
./tools/verify-setup.sh
```

This checks:
- ✅ All required scripts are present and executable
- ✅ API key is configured (.env or environment variable)
- ✅ Required dependencies (python3, curl, git)
- ✅ Documentation files exist
- ✅ .gitignore is properly configured
- ✅ Conversion prompt includes test generation and comment preservation
- ✅ Test extraction and runner are configured

**Fix any errors before proceeding!**

## Troubleshooting

### Error: API key not set
```bash
export ANTHROPIC_API_KEY='your-api-key-here'
```

### Error: Batch not found
- Wait 24 hours after submission
- Check batch ID is correct (starts with `batch_`)
- Verify API key has access to batches

### Error: Some conversions failed
1. **View results interactively:**
   ```bash
   cd tools
   ./view-jsonl.sh
   # Select batch-results.jsonl
   # Look for error messages in failed objects
   ```
2. Review which scripts failed (check `custom_id` field)
3. Fix documentation or convert manually
4. Can re-run batch for just failed scripts

### Error: Converted script has syntax errors
1. Use Guile REPL to test: `guile tools/converted-scripts/postinstall/recipes/add-spacemacs.scm`
2. Check error message
3. Common issues:
   - Missing SRFI import
   - Wrong command (shasum vs sha256sum)
   - Incorrect shebang
4. Add to GUILE_GOTCHAS.md and increment frequency count

## Advanced: Batch Specific Scripts

To convert just specific scripts (not all), edit `generate-batch-conversion.sh`:

```bash
# Replace the find command with specific files:
for script in \
  postinstall/recipes/add-spacemacs.sh \
  lib/postinstall.sh \
; do
  if [ -f "$script" ]; then
    # ... rest of loop
  fi
done
```

Or filter the find results:
```bash
# Only convert recipe scripts
find . -path "*/postinstall/recipes/*.sh" -type f | while read -r script; do
  # ... rest of loop
done
```

## Documentation Used

All conversions include context from:
- `docs/GUILE_KNOWLEDGE.md` - Quick reference and patterns
- `docs/GUILE_BEST_PRACTICES.md` - Design principles
- `docs/GUILE_GOTCHAS.md` - Common mistakes (frequency-sorted)
- `docs/GUILE_CONVERSION.md` - Conversion strategy

## Next Steps After Successful Conversion

1. ✅ Delete `.sh` versions (keep in git history)
2. ✅ Update references in other scripts
3. ✅ Update documentation (POSTINSTALL_DEV.md)
4. ✅ Mark conversion complete in CHECKLIST.md
5. ✅ Push to GitHub
6. ✅ Test on real Guix system

## See Also

- [GUILE_CONVERSION.md](../docs/GUILE_CONVERSION.md) - Overall conversion strategy
- [POSTINSTALL_DEV.md](../docs/POSTINSTALL_DEV.md) - Development workflow
- [Anthropic Batch API Docs](https://docs.anthropic.com/en/api/batch-processing)
