# Testing Results: Converted Recipe Scripts

**Test Date:** 2025-12-18
**Test Environment:** macOS with Guile 3.0.10
**Test Harness:** `test/test-recipe-script.sh`

---

## Test Infrastructure Created

### Test Fixtures ✅
- `test/fixtures/postinstall-recipes/minimal-config.scm` - Fresh install with `%base-packages`
- `test/fixtures/postinstall-recipes/existing-packages-config.scm` - Has some packages already
- `test/fixtures/postinstall-recipes/has-fonts-config.scm` - Has some fonts already

### Test Runner ✅
- `test/test-recipe-script.sh` - Automated test runner
  - Copies fixture to temp location
  - Runs recipe script
  - Validates output is valid Scheme
  - Shows diff of changes
  - Can test single fixture or all fixtures

---

## Issues Found During Testing

### 🚨 Issue #1: add-development.scm Entry Point BROKEN

**File:** `postinstall/recipes/add-development.scm:189-194`
**Severity:** CRITICAL - Script doesn't run at all

**Current Code (BROKEN):**
```scheme
(when (equal? (car (command-line))
              (string-append (dirname (dirname (getcwd)))
                           "/postinstall/recipes/add-development.scm"))
  (main (command-line)))
```

**Problem:**
- Checks if first command-line arg matches a specific absolute path
- Path construction uses `(dirname (dirname (getcwd)))` which is fragile
- Won't work when run from different directories
- Won't work with different installation paths

**Test Result:**
- ❌ Script executes without errors but **does not modify config**
- ❌ No output, no changes, fails silently

**Fix:**
Use `batch-mode?` like add-fonts.scm does:
```scheme
(when (batch-mode?)
  (add-development))
```

---

### 🚨 Issue #2: add-fonts.scm Regex Bug (CONFIRMED)

**File:** `postinstall/recipes/add-fonts.scm:77-88`
**Severity:** CRITICAL - Corrupts config.scm

**Status:** Bug confirmed in code review (see `docs/SCRIPT_REVIEW_add-fonts.md`)
**Not yet tested** because we need to test add-development.scm first

**Problem:**
- Regex splits lines incorrectly instead of appending
- Will create malformed Scheme code
- See detailed analysis in SCRIPT_REVIEW_add-fonts.md

---

### ⏳ Issue #3: Other Scripts Not Yet Tested

**Scripts Pending:**
- add-vanilla-emacs.scm (likely has same regex bug as add-fonts.scm)
- add-spacemacs.scm (uses shell commands, unknown status)
- add-doom-emacs.scm (uses shell commands, unknown status)

---

## Test Results Summary

| Script | Approach | Entry Point | Logic | Overall |
|--------|----------|-------------|-------|---------|
| add-development.scm | ✅ S-expr | ❌ BROKEN | ⏳ Not tested | ❌ BLOCKED |
| add-fonts.scm | ❌ Regex | ✅ Works | ❌ BROKEN | ❌ FAIL |
| add-vanilla-emacs.scm | ❌ Regex | ⏳ Unknown | ⏳ Not tested | ⏳ PENDING |
| add-spacemacs.scm | ⚠️ Shell | ⏳ Unknown | ⏳ Not tested | ⏳ PENDING |
| add-doom-emacs.scm | ⚠️ Shell | ⏳ Unknown | ⏳ Not tested | ⏳ PENDING |

---

## Immediate Action Items

### Priority 1: Fix Entry Points 🚨
All scripts need to use `batch-mode?` for entry point detection:

**Check all scripts:**
```bash
for f in postinstall/recipes/*.scm; do
    echo "=== $(basename $f) ==="
    tail -10 "$f" | grep -A 5 "when\|batch-mode"
done
```

**Fix pattern:**
```scheme
;; Old (BROKEN)
(when (equal? (car (command-line)) ...)
  (main (command-line)))

;; New (CORRECT)
(when (batch-mode?)
  (add-function-name))
```

### Priority 2: Fix Regex Logic 🚨
Rewrite add-fonts.scm and add-vanilla-emacs.scm to use S-expression approach like add-development.scm

### Priority 3: Test All Scripts ⏳
After fixing entry points and logic:
1. Test each script against all fixtures
2. Verify output is valid Scheme
3. Verify packages are added correctly
4. Verify duplicates are prevented

---

## Fix Strategy

### Option A: Minimal Fixes (Quick)
1. Fix all entry points to use `batch-mode?`
2. Fix regex bugs in add-fonts.scm and add-vanilla-emacs.scm
3. Test and deploy

**Pros:** Faster to implement
**Cons:** Still has mixed approaches, harder to maintain

### Option B: Standardize on S-expressions (Recommended)
1. Fix all entry points to use `batch-mode?`
2. Rewrite add-fonts.scm using S-expression approach
3. Rewrite add-vanilla-emacs.scm using S-expression approach
4. Extract common S-expression manipulation code into shared module
5. Test and deploy

**Pros:** Consistent, robust, maintainable
**Cons:** More work upfront

**Recommendation:** Use Option B for long-term quality

---

## Testing Workflow

### Step 1: Fix Entry Points
For each script, change the entry point to:
```scheme
(when (batch-mode?)
  (add-<function-name>))
```

### Step 2: Test Individual Scripts
```bash
cd test
./test-recipe-script.sh ../postinstall/recipes/add-development.scm all
./test-recipe-script.sh ../postinstall/recipes/add-fonts.scm all
# etc.
```

### Step 3: Fix Bugs Found
- If script doesn't modify config: check entry point
- If script corrupts config: check logic (likely regex bug)
- If script fails: check error messages

### Step 4: Iterate
Repeat test → fix → test cycle until all scripts pass

---

## Next Steps

1. ✅ Test harness created
2. ⏳ **Fix add-development.scm entry point** (CURRENT)
3. ⏳ Test add-development.scm again
4. ⏳ Fix add-fonts.scm (entry point + regex logic)
5. ⏳ Test add-fonts.scm
6. ⏳ Check and fix remaining scripts
7. ⏳ Test all scripts against all fixtures
8. ⏳ Deploy to cloudzy

---

## Lessons Learned

1. **Batch conversion quality varies** - Same tool produced different quality results
2. **Entry point detection is critical** - Scripts that don't run at all fail silently
3. **Regex on code is fragile** - S-expression manipulation is much safer
4. **Testing infrastructure needed** - Without test fixtures, bugs would only be found in production
5. **Standardization matters** - Mixed approaches make maintenance harder

---

## Conclusion

**Current Status:** Scripts are NOT ready for deployment

**Blocking Issues:**
1. add-development.scm: Entry point broken (script doesn't run)
2. add-fonts.scm: Entry point OK but regex logic corrupts config
3. Other scripts: Not yet tested

**Path Forward:**
1. Fix all entry points to use `batch-mode?`
2. Rewrite regex-based scripts to use S-expression approach
3. Test thoroughly before deploying

**Estimated Work:**
- Entry point fixes: ~30 minutes (straightforward)
- Logic fixes (regex → S-expr): ~2-3 hours (careful rewriting)
- Testing: ~1 hour (automated tests already created)

**Total:** ~4-5 hours to get all scripts working and tested
