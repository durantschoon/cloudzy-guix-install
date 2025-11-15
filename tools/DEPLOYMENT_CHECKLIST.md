# Deployment Checklist: Guile Scripts for Cloudzy

## Current State

**Converted Scripts Available:**
- ✅ `tools/converted-scripts/postinstall/recipes/add-development.scm`
- ✅ `tools/converted-scripts/postinstall/recipes/add-fonts.scm`
- ✅ `tools/converted-scripts/postinstall/recipes/add-spacemacs.scm`
- ✅ `tools/converted-scripts/postinstall/recipes/add-doom-emacs.scm`
- ✅ `tools/converted-scripts/postinstall/recipes/add-vanilla-emacs.scm`

**Missing:**
- ❌ No test files generated (batch conversion didn't include tests yet)
- ❌ Scripts not copied to final locations
- ❌ `cloudzy/postinstall/customize` still calls `.sh` versions

## Pre-Deployment Checklist

### 1. Review Converted Scripts ✅
- [x] Scripts extracted from batch results
- [x] Comment structure validated (`validate-comment-structure.sh`)
- [ ] Manual code review of each converted script
- [ ] Check for obvious syntax errors or logic issues

### 2. Test Converted Scripts Locally
- [ ] Run tests (if test files exist): `./run-tests.sh`
- [ ] Test each script manually in Guile REPL:
  ```bash
  guile --no-auto-compile -s tools/converted-scripts/postinstall/recipes/add-development.scm
  ```
- [ ] Verify scripts can be loaded without errors
- [ ] Test with sample config.scm file (dry-run mode if possible)

### 3. Update cloudzy/postinstall/customize
**Current:** Sources `.sh` files
```bash
source "$(dirname "$0")/../../postinstall/recipes/add-spacemacs.sh" && add_spacemacs
```

**Needs:** Call `.scm` files instead
```bash
guile --no-auto-compile -s "$(dirname "$0")/../../postinstall/recipes/add-spacemacs.scm"
```

**Files to update:**
- [ ] `cloudzy/postinstall/customize` - Update recipe calls (lines 237-239)
- [ ] Verify function names match (e.g., `add_spacemacs` vs script entry point)

### 4. Copy Scripts to Final Locations
- [ ] Copy converted scripts:
  ```bash
  cp tools/converted-scripts/postinstall/recipes/*.scm postinstall/recipes/
  ```
- [ ] Verify permissions (should be executable)
- [ ] Keep original `.sh` files as backup (or remove after testing)

### 5. Update Documentation
- [ ] Update `cloudzy/README.md` if it references recipe scripts
- [ ] Update `postinstall/CUSTOMIZATION.md` if it mentions recipe usage
- [ ] Note that scripts are now Guile Scheme

### 6. Integration Testing
- [ ] Test `cloudzy/postinstall/customize` menu options:
  - [ ] Option 's' (Spacemacs) - verify it calls .scm script correctly
  - [ ] Option 'd' (Development tools) - verify it works
  - [ ] Option 'f' (Fonts) - verify it works
- [ ] Test with actual config.scm file (backup first!)
- [ ] Verify config.scm modifications are correct
- [ ] Test system reconfigure after modifications

### 7. Safety Checks
- [ ] Backup existing `.sh` scripts before replacing
- [ ] Test on non-production system first
- [ ] Have rollback plan (restore .sh files if needed)
- [ ] Verify scripts don't require sudo (or document if they do)

### 8. Deployment Steps
- [ ] Commit converted scripts to git
- [ ] Update SOURCE_MANIFEST.txt if needed
- [ ] Test on Cloudzy VPS (fresh install)
- [ ] Document any issues found
- [ ] Update GUILE_GOTCHAS.md with any new discoveries

## Critical Questions to Answer

1. **Function naming:** Do the converted scripts export functions with same names?
   - Check: `add_spacemacs`, `add_development`, `add_fonts`
   - Or do they have different entry points?

2. **Script invocation:** How should customize script call them?
   - Option A: `guile --no-auto-compile -s script.scm` (if standalone)
   - Option B: `guile -c "(load \"script.scm\") (add_spacemacs)"` (if functions)
   - Option C: Source and call function (if compatible)

3. **Error handling:** What happens if script fails?
   - Does customize script handle errors gracefully?
   - Should scripts exit with error codes?

4. **Dependencies:** Do scripts need any Guile modules installed?
   - Check use-modules statements
   - Verify all modules are available on Guix system

## Recommended Order

1. **First:** Review converted scripts manually (check for obvious issues)
2. **Second:** Test scripts locally in Guile REPL
3. **Third:** Update customize script to call .scm versions
4. **Fourth:** Test customize script locally (with test config.scm)
5. **Fifth:** Deploy to Cloudzy VPS for real-world testing

## Rollback Plan

If issues arise:
1. Restore original `.sh` files from git
2. Revert customize script changes
3. Document issues in GUILE_GOTCHAS.md
4. Fix issues and retry
