# Script Review: add-fonts.scm

**Review Date:** 2025-12-18
**Reviewer:** Claude Code
**Original:** `postinstall/recipes/add-fonts.sh` (70 lines)
**Converted:** `postinstall/recipes/add-fonts.scm` (133 lines)

## Overall Assessment

**Status:** ⚠️ **NEEDS FIXES** - Logic differs from bash version

**Conversion Quality:** 7/10
- ✅ Structure and organization good
- ✅ Proper Guile idioms used
- ✅ Module imports appropriate
- ⚠️ Different insertion logic than bash version
- ❌ Missing error handling
- ❌ Potential file safety issue

---

## Detailed Comparison

### 1. Shebang and Environment ✅

**Bash:**
```bash
#!/usr/bin/env bash
set -euo pipefail
```

**Guile:**
```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#
```

**Analysis:** ✅ **CORRECT**
- Guile shebang uses proper Guix system path
- `--no-auto-compile` flag prevents compilation overhead
- `-s` flag enables script mode

---

### 2. Helper Functions ✅

**Bash:**
```bash
info() { printf "  %s\n" "$*"; }
success() { printf "\n\033[1;32m[✓]\033[0m %s\n" "$*"; }
```

**Guile:**
```scheme
(define (info . args)
  (format #t "  ~a~%" (string-join (map ...) args) " ")))

(define (success . args)
  (format #t "~%\x1b[1;32m[✓]\x1b[0m ~a~%" ...))
```

**Analysis:** ✅ **CORRECT**
- Unicode checkmark OK for postinstall scripts (runs after boot)
- Variadic args handled correctly
- ANSI color codes properly escaped

---

### 3. Font Package List ✅

**Bash:**
```bash
local font_packages=(
  "font-fira-code"
  "font-jetbrains-mono"
  ...
)
```

**Guile:**
```scheme
(define font-packages
  '("font-fira-code"
    "font-jetbrains-mono"
    ...))
```

**Analysis:** ✅ **CORRECT**
- All 7 fonts present in same order
- Comments preserved

---

### 4. Minimal Config Insertion ✅

**Bash:**
```bash
sed -i "s|(packages %base-packages)|(packages\n  (append\n   (list\n${package_list}         )\n   %base-packages))|" "$CONFIG_FILE"
```

**Guile:**
```scheme
(regexp-substitute/global
 #f
 "\\(packages %base-packages\\)"
 content
 'pre
 (format #f "(packages\n  (append\n   (list\n~a\n         )\n   %base-packages))"
         package-list)
 'post)
```

**Analysis:** ✅ **CORRECT**
- Pattern matching equivalent (parentheses escaped)
- Replacement format matches bash version
- Proper use of regexp-substitute/global

---

### 5. Existing Config Insertion ⚠️ **ISSUE FOUND**

**Bash:**
```bash
for pkg in "${font_packages[@]}"; do
  if ! grep -q "\"$pkg\"" "$CONFIG_FILE"; then
    sed -i "/specification->package/a\                (specification->package \"$pkg\")" "$CONFIG_FILE"
  fi
done
```

This uses `sed`'s `a\` command to **append a line after** the first match.

**Guile:**
```scheme
(define (add-font-to-existing-config content pkg)
  (if (string-contains content (format #f "\"~a\"" pkg))
      content  ; Already present
      (regexp-substitute/global
       #f
       "specification->package"
       content
       'pre
       (format #f "specification->package\n                (specification->package \"~a\")" pkg)
       'post
       1)))  ; Only first occurrence
```

**Analysis:** ⚠️ **DIFFERENT LOGIC**

The Guile version:
1. Finds the FIRST occurrence of `specification->package`
2. **Replaces** it with `specification->package\n(specification->package "new-pkg")`

This **duplicates the first specification->package line**, which is different from bash's append-after behavior.

**Problem:**
If the config looks like:
```scheme
(packages
  (append
    (list
      (specification->package "emacs")
      (specification->package "git"))
    %base-packages))
```

Bash version adds AFTER first match:
```scheme
      (specification->package "emacs")
      (specification->package "font-fira-code")  ; <-- new
      (specification->package "git"))
```

Guile version REPLACES first match:
```scheme
      (specification->package "emacs")
      (specification->package "font-fira-code")  ; <-- duplicated + new
      (specification->package "git"))
```

Wait, let me re-read the Guile code... it's doing:
- 'pre = `(packages\n  (append\n    (list\n      (`
- replacement = `specification->package\n                (specification->package "font-fira-code")`
- 'post = `"emacs")\n...`

So it's actually including the original text, so it becomes:
```
specification->package
                (specification->package "font-fira-code")
```

Hmm, this is confusing. Let me think about regexp-substitute/global more carefully...

Actually, the replacement string includes `specification->package` at the beginning, so it's:
1. Match: `specification->package`
2. Replace with: `specification->package\n                (specification->package "font-fira-code")`

So the original line `(specification->package "emacs")` becomes:
```
      (specification->package
                (specification->package "font-fira-code") "emacs")
```

No wait, that's wrong too. Let me think about this more carefully.

The content is: `....(specification->package "emacs")....`
The pattern matches: `specification->package`
The 'pre is everything before: `....(`
The match is: `specification->package`
The 'post is everything after: ` "emacs")....`

The replacement is:
```
'pre + "specification->package\n                (specification->package \"font-fira-code\")" + 'post
```

So:
```
....( + specification->package
                (specification->package "font-fira-code") +  "emacs")....
```

Result:
```
....(specification->package
                (specification->package "font-fira-code") "emacs")....
```

This is BROKEN. It's splitting the original line incorrectly.

**Correct Fix Needed:**
The Guile version should find the first `specification->package` line and insert a NEW line after it, not split the existing line.

Proper approach:
1. Find the entire line: `\(specification->package "[^"]+"\)`
2. Replace with: same line + newline + new package line

OR use a different approach like bash's `/pattern/a\` append.

---

### 6. Error Handling ❌ **MISSING**

**Bash:**
```bash
set -euo pipefail  # Exit on error, undefined vars, pipe failures
```

**Guile:**
```scheme
# No equivalent error handling
```

**Analysis:** ❌ **MISSING**
- File operations could fail silently
- No validation that file write succeeded
- Should use `catch` or `guard` for error handling

**Recommendation:**
```scheme
(catch #t
  (lambda ()
    (write-file config-file new-content))
  (lambda (key . args)
    (format (current-error-port) "Error writing config: ~a~%" args)
    (exit 1)))
```

---

### 7. File Safety ⚠️ **CONCERN**

**Bash:**
```bash
sed -i ...  # In-place editing (creates backup on some systems)
```

**Guile:**
```scheme
(write-file config-file new-content)  # Overwrites file completely
```

**Analysis:** ⚠️ **RISK**
- If write fails midway, file could be corrupted or empty
- No atomic write or backup mechanism

**Recommendation:**
```scheme
(define (write-file-safe filepath content)
  "Write content to file atomically"
  (let ((temp-file (string-append filepath ".tmp")))
    (call-with-output-file temp-file
      (lambda (port) (put-string port content)))
    (rename-file temp-file filepath)))
```

---

### 8. Entry Point ✅

**Bash:**
```bash
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_fonts
fi
```

**Guile:**
```scheme
(when (batch-mode?)
  (add-fonts))
```

**Analysis:** ✅ **CORRECT**
- `batch-mode?` returns true when run as script (not loaded as module)
- Functionally equivalent to bash check

---

## Critical Issues

### 🚨 Issue #1: Broken Insert Logic

**File:** `add-fonts.scm:77-88`
**Function:** `add-font-to-existing-config`

**Problem:** The regex substitution splits existing lines incorrectly instead of appending new lines.

**Impact:** HIGH - Will produce malformed config.scm

**Fix Required:**
```scheme
(define (add-font-to-existing-config content pkg)
  "Add a single font package after first existing package"
  (if (string-contains content (format #f "\"~a\"" pkg))
      content  ; Already present
      ;; Find first complete specification->package line and insert after it
      (regexp-substitute/global
       #f
       "(\\(specification->package \"[^\"]+\"\\))"
       content
       'pre 1 "\n                (specification->package \"" pkg "\")" 'post
       1)))  ; Only first occurrence
```

OR better yet, use a more robust approach that finds the insertion point properly.

---

### ⚠️ Issue #2: No Error Handling

**Impact:** MEDIUM - Silent failures possible

**Fix Required:** Add `catch` blocks around file I/O operations

---

### ⚠️ Issue #3: No Atomic File Write

**Impact:** MEDIUM - Risk of file corruption on write failure

**Fix Required:** Use temp file + rename for atomic write

---

## Recommendations

### Priority 1: Fix Insert Logic ��
The current implementation will break config.scm. Must fix before deploying.

### Priority 2: Add Error Handling 🔶
Add proper error handling for file operations.

### Priority 3: Improve File Safety 🔶
Use atomic write operations.

### Priority 4: Add Tests 🔶
Create test cases to verify insertion logic works correctly.

---

## Test Plan

Before deploying, test with these scenarios:

1. **Minimal config:**
   ```scheme
   (packages %base-packages)
   ```
   Expected: Replace with full append/list structure

2. **Existing packages:**
   ```scheme
   (packages
     (append
       (list
         (specification->package "emacs"))
       %base-packages))
   ```
   Expected: Add fonts after emacs line

3. **Already has fonts:**
   ```scheme
   (packages
     (append
       (list
         (specification->package "font-fira-code"))
       %base-packages))
   ```
   Expected: Skip duplicate, add others

---

## Verdict

**Status:** ⚠️ **DO NOT DEPLOY** - Critical bug found

The conversion looks good structurally but has a critical logic error in the insert-into-existing-config code path. This must be fixed before deployment.

The minimal config path (replacing `%base-packages`) appears correct and could be tested independently.

---

## Next Steps

1. Fix the `add-font-to-existing-config` regex logic
2. Add error handling
3. Test with sample config.scm files
4. Add atomic file write
5. Create unit tests
6. Re-review after fixes
