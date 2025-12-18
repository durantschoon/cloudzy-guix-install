# Converted Scripts Analysis

**Analysis Date:** 2025-12-18
**Scope:** All postinstall recipe scripts converted from bash to Guile

---

## Summary

| Approach | Count | Scripts | Risk Level |
|----------|-------|---------|------------|
| **S-expression** (Guile native) | 1 | add-development.scm | ✅ LOW - Safe, idiomatic |
| **Shell commands** (system* + sed) | 2 | add-spacemacs.scm, add-doom-emacs.scm | ⚠️ MEDIUM - Depends on sed |
| **Regex** (string manipulation) | 2 | add-fonts.scm, add-vanilla-emacs.scm | 🚨 HIGH - Bug found |

**Total Scripts:** 5 recipe scripts

---

## Detailed Breakdown

### ✅ Category 1: S-Expression Approach (SAFE)

**Scripts:**
- `add-development.scm` (196 lines)

**Method:**
```scheme
;; Read config as S-expressions (data)
(define (read-all-exprs filepath)
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((exprs '()))
        (let ((expr (read port)))
          ...)))))

;; Use pattern matching to transform
(match expr
  (('operating-system fields ...)
    (transform-fields fields))
  ...)

;; Write back with pretty-print
(write-all-exprs filepath exprs)
```

**Pros:**
- ✅ Treats code as data (Lisp philosophy)
- ✅ Type-safe with pattern matching
- ✅ Preserves structure and formatting
- ✅ Least likely to introduce syntax errors
- ✅ Uses `ice-9 match` module for robust pattern matching

**Cons:**
- ⚠️ Requires understanding of Scheme/Lisp concepts
- ⚠️ More verbose code

**Status:** ✅ **RECOMMENDED APPROACH** - Should be safe to deploy after testing

**Example Usage:**
```scheme
(use-modules (ice-9 match)
             (ice-9 pretty-print))

(define (has-minimal-packages? exprs)
  (any (lambda (expr)
         (match expr
           (('operating-system fields ...)
            (any (lambda (field)
                   (match field
                     (('packages '%base-packages) #t)
                     (_ #f)))
                 fields))
           (_ #f)))
       exprs))
```

---

### ⚠️ Category 2: Shell Command Approach (MEDIUM RISK)

**Scripts:**
- `add-spacemacs.scm` (352 lines)
- `add-doom-emacs.scm` (392 lines)

**Method:**
```scheme
;; Call sed directly via system*
(define (add-packages-to-config! config-path packages)
  (system* "sed" "-i"
           "/(use-modules/a\\             (gnu packages emacs)"
           config-path)
  ...)
```

**Pros:**
- ✅ Familiar to bash scripters
- ✅ Uses well-tested sed commands
- ✅ Can leverage existing sed expertise

**Cons:**
- ⚠️ Depends on external `sed` command being available
- ⚠️ Less portable (sed variations across systems)
- ⚠️ Harder to debug than native Guile
- ⚠️ String escaping issues possible
- ⚠️ Error handling is limited (system* just returns exit code)

**Risk Assessment:**
- MEDIUM risk - sed is available on Guix systems
- Command syntax appears correct (based on manual review)
- Should work but not as elegant as S-expression approach

**Status:** ⚠️ **NEEDS TESTING** - Should work but verify sed commands are correct

**Recommendation:** Consider rewriting to use S-expression approach for consistency

---

### 🚨 Category 3: Regex Approach (HIGH RISK)

**Scripts:**
- `add-fonts.scm` (132 lines) - **CRITICAL BUG FOUND**
- `add-vanilla-emacs.scm` (332 lines) - **NEEDS REVIEW**

**Method:**
```scheme
;; Read file as string
(define content (call-with-input-file filepath get-string-all))

;; Use regexp-substitute/global to modify
(regexp-substitute/global
 #f
 "specification->package"
 content
 'pre
 (format #f "specification->package\n                (specification->package \"~a\")" pkg)
 'post
 1)

;; Write back as string
(call-with-output-file filepath
  (lambda (port) (put-string port new-content)))
```

**Pros:**
- ✅ Simple concept (find and replace)
- ✅ No external dependencies

**Cons:**
- ❌ Treats code as text (loses structure)
- ❌ Regex bugs can create malformed Scheme code
- ❌ **CRITICAL BUG FOUND in add-fonts.scm** (see below)
- ❌ Hard to maintain and debug
- ❌ No validation that output is valid Scheme
- ❌ Can corrupt config.scm if regex is wrong

**Status:** 🚨 **DO NOT DEPLOY** - Critical bug found

---

## Critical Bug: add-fonts.scm

**File:** `postinstall/recipes/add-fonts.scm:77-88`
**Function:** `add-font-to-existing-config`
**Severity:** CRITICAL - Will corrupt config.scm

### The Bug

**What it does (WRONG):**
```scheme
(regexp-substitute/global
 #f
 "specification->package"  ; Matches just the function name
 content
 'pre
 (format #f "specification->package\n                (specification->package \"~a\")" pkg)
 'post
 1)
```

**Input:**
```scheme
      (specification->package "emacs")
```

**Expected Output (what bash version does):**
```scheme
      (specification->package "emacs")
      (specification->package "font-fira-code")  ; New line AFTER
```

**Actual Output (what Guile version does):**
```scheme
      (specification->package
                (specification->package "font-fira-code") "emacs")  ; BROKEN
```

**Result:** Malformed Scheme code that will fail `guix system reconfigure`

### Root Cause

The regex matches `specification->package` without the surrounding context, so it splits the line in the middle rather than appending after it.

**Bash version (CORRECT):**
```bash
sed -i "/specification->package/a\                (specification->package \"$pkg\")" "$CONFIG_FILE"
```

This uses sed's `a\` command to **append after** the line, not replace within it.

---

## Suspected Bug: add-vanilla-emacs.scm

**Status:** NOT YET REVIEWED
**Likely Issue:** Same regex problem as add-fonts.scm

**Evidence:**
- Uses `regexp-substitute/global` (same as add-fonts.scm)
- Lines: 332 (more complex, more places to fail)
- Needs thorough review before deployment

---

## Recommendations by Priority

### Priority 1: Critical Fixes 🚨

1. **DO NOT DEPLOY** add-fonts.scm or add-vanilla-emacs.scm in current state
2. **Fix or rewrite** both scripts to use S-expression approach
3. **Add tests** to verify config.scm modifications are correct

### Priority 2: Testing ⚠️

1. **Test add-development.scm** - Should be safe but verify
2. **Test add-spacemacs.scm** - Verify sed commands work on Guix
3. **Test add-doom-emacs.scm** - Verify sed commands work on Guix

### Priority 3: Refactoring 🔄

1. **Standardize** on S-expression approach for all scripts
2. **Extract common code** into shared library module
3. **Add error handling** with `catch` or `guard`
4. **Add atomic file writes** (temp file + rename)

---

## Testing Checklist

Before deploying ANY converted script:

### Minimal Config Test
```scheme
;; Input
(packages %base-packages)

;; Expected Output
(packages
  (append
    (list
      (specification->package "emacs")
      (specification->package "git"))
    %base-packages))
```

### Existing Packages Test
```scheme
;; Input
(packages
  (append
    (list
      (specification->package "emacs"))
    %base-packages))

;; Expected Output
(packages
  (append
    (list
      (specification->package "emacs")
      (specification->package "font-fira-code")
      (specification->package "git"))
    %base-packages))
```

### Duplicate Prevention Test
```scheme
;; Input (already has package)
(packages
  (append
    (list
      (specification->package "font-fira-code"))
    %base-packages))

;; Expected Output (no duplicate)
(packages
  (append
    (list
      (specification->package "font-fira-code"))
    %base-packages))
```

---

## Recommended Fix Strategy

### Option A: Fix Regex (Quick)

Fix the regex in add-fonts.scm and add-vanilla-emacs.scm to match entire lines:

```scheme
(define (add-font-to-existing-config content pkg)
  (if (string-contains content (format #f "\"~a\"" pkg))
      content  ; Already present
      ;; Match entire specification->package line
      (regexp-substitute/global
       #f
       "(\\(specification->package \"[^\"]+\"\\))"
       content
       'pre 1 "\n                (specification->package \"" pkg "\")" 'post
       1)))
```

**Pros:** Faster fix
**Cons:** Still fragile, hard to maintain

### Option B: Rewrite with S-expressions (Best)

Rewrite both scripts to use the add-development.scm approach:

1. Use `read-all-exprs` to parse config.scm
2. Use `ice-9 match` to pattern-match and transform
3. Use `pretty-print` to write back

**Pros:** Robust, maintainable, safe
**Cons:** More work upfront

**Recommendation:** Use Option B for long-term maintainability

---

## Next Steps

1. ✅ Categorization complete (this document)
2. ⏳ **Review add-vanilla-emacs.scm** for same bug
3. ⏳ **Choose fix strategy** (regex fix vs rewrite)
4. ⏳ **Implement fixes** for add-fonts.scm and add-vanilla-emacs.scm
5. ⏳ **Test all scripts** with sample config.scm files
6. ⏳ **Deploy** only after testing passes
7. ⏳ **Update CHECKLIST.md** with findings

---

## Conclusion

The batch conversion produced **mixed results**:

- **1 script** uses best practice (S-expression approach) ✅
- **2 scripts** use acceptable approach (shell commands) ⚠️
- **2 scripts** use problematic approach (regex) with **critical bugs** 🚨

**Overall Status:** ⚠️ **NOT READY FOR DEPLOYMENT**

Critical bugs must be fixed before deploying ANY of these scripts to production.
