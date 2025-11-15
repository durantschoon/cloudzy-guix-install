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
4. **Environment variable**:
   ```bash
   export ANTHROPIC_API_KEY='your-api-key-here'
   ```

## Workflow

### 1. Generate Batch Requests

```bash
./tools/generate-batch-conversion.sh
```

**What it does:**
- Finds all `.sh` scripts in `postinstall/recipes/`
- Reads all Guile documentation (KNOWLEDGE, BEST_PRACTICES, GOTCHAS, CONVERSION)
- Creates batch request for each script
- Outputs to `tools/batch-requests.jsonl`

**Output:**
```
Generating batch conversion requests...
  [1] postinstall/recipes/add-spacemacs.sh
  [2] postinstall/recipes/add-development.sh
  [3] postinstall/recipes/add-fonts.sh

Generated 3 batch requests
```

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
- Downloads conversion results
- Extracts `.scm` files to `tools/converted-scripts/`
- Makes scripts executable
- Reports success/error counts

**Output:**
```
Extracting converted scripts...
  ✓ postinstall/recipes/add-spacemacs.scm
  ✓ postinstall/recipes/add-development.scm
  ✓ postinstall/recipes/add-fonts.scm

Extraction complete!
  Succeeded: 3
  Errored: 0

Converted scripts written to: tools/converted-scripts
```

### 5. Review and Test

**Compare conversions:**
```bash
# Visual diff
diff postinstall/recipes/add-spacemacs.sh \
     tools/converted-scripts/postinstall/recipes/add-spacemacs.scm

# Side-by-side
diff -y postinstall/recipes/add-spacemacs.sh \
        tools/converted-scripts/postinstall/recipes/add-spacemacs.scm
```

**Manual review checklist:**
- [ ] Shebang correct: `#!/run/current-system/profile/bin/guile --no-auto-compile -s`
- [ ] All `use-modules` present (ice-9, srfi-1, etc.)
- [ ] Functions converted to procedures
- [ ] Bash commands → Guile equivalents
- [ ] GNU coreutils used (sha256sum not shasum)
- [ ] Comments explain complex conversions
- [ ] Naming conventions followed (?, !, ->)

**Test converted scripts:**
```bash
# Copy to test location
cp tools/converted-scripts/postinstall/recipes/add-spacemacs.scm \
   postinstall/recipes/add-spacemacs.scm

# Run tests
./run-tests.sh

# Test specific script
framework-dual/postinstall/tests/run-guile-tests.sh
```

**If tests pass:**
```bash
# Update manifest
./update-manifest.sh

# Commit
git add postinstall/recipes/*.scm SOURCE_MANIFEST.txt
git commit -m "Convert recipe scripts from bash to Guile (batch conversion)"
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
├── batch-requests.jsonl         # Input: Batch API requests
├── batch-results.jsonl          # Output: Raw results from API
└── converted-scripts/           # Output: Extracted .scm files
    └── postinstall/
        └── recipes/
            ├── add-spacemacs.scm
            ├── add-development.scm
            └── add-fonts.scm
```

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
1. Check `batch-results.jsonl` for error messages
2. Review which scripts failed
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

To convert just specific scripts (not all):

```bash
# Edit generate-batch-conversion.sh
# Change the find pattern:
for script in postinstall/recipes/add-spacemacs.sh; do
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
