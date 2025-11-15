# Guile/Scheme Gotchas and Common Mistakes

This document tracks mistakes we've actually made during development, sorted by frequency.

**Format**: Each gotcha includes:
- **Count**: Number of times we've hit this issue (kept up-to-date)
- **The Mistake**: What we did wrong
- **Why It Failed**: Technical explanation
- **The Fix**: Correct approach
- **When to Watch For**: Situations where this mistake is likely

---

## Gotcha Leaderboard (Sorted by Frequency)

### [Count: 3] Missing SRFI-1 Import

**The Mistake:**
```scheme
(use-modules (ice-9 pretty-print)
             (ice-9 match))

;; Later in code:
(let ((filtered (filter-map some-fn items)))  ; ERROR!
  ...)
```

**Why It Failed:**
```
ERROR: Unbound variable: filter-map
```

List processing functions like `filter-map`, `fold`, `take`, `take-right`, etc. are not part of core Guile - they come from SRFI-1.

**The Fix:**
```scheme
(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (srfi srfi-1))  ; REQUIRED for list functions
```

**When to Watch For:**
- Using any list function beyond basic `car`, `cdr`, `cons`, `append`
- Processing collections with `filter-map`, `fold`, `partition`
- Taking slices with `take`, `drop`, `take-right`, `drop-right`

**Frequency Notes:**
- 2024-11-14: Initial occurrence (guile-config-helper.scm)
- 2024-11-14: bootstrap-postinstall.scm (take/take-right)
- 2024-11-15: Future occurrence tracking starts here

---

### [Count: 2] Wrong Shebang for Piped Execution

**The Mistake:**
```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#
```

When piped: `curl ... | guile`

**Why It Failed:**
```
;;; In procedure primitive-load:
;;; No such file or directory: "/home/durant/-"
```

The `-s` flag tells Guile to expect a script filename argument. When piped, leftover `-` from arguments gets interpreted as a file path.

**The Fix:**
```scheme
#!/run/current-system/profile/bin/guile \
--no-auto-compile
!#
```

**When to Watch For:**
- Creating scripts meant to be piped via curl
- Scripts that should read from stdin
- Bootstrap/installer scripts

**Frequency Notes:**
- 2024-11-15: bootstrap-postinstall.scm initial shebang
- 2024-11-15: Fixed by removing -s flag

---

### [Count: 2] Using macOS Commands on Guix

**The Mistake:**
```scheme
(define (file-sha256 filepath)
  (let* ((port (open-input-pipe (format #f "shasum -a 256 ~s" filepath)))
         ...))
```

**Why It Failed:**
```
/gnu/store/.../bash: line 1: shasum: command not found
```

`shasum` is a BSD/macOS tool. Guix systems use GNU coreutils which provides `sha256sum`.

**The Fix:**
```scheme
(define (file-sha256 filepath)
  (let* ((port (open-input-pipe (format #f "sha256sum ~s" filepath)))
         ...))
```

**When to Watch For:**
- Porting scripts from macOS development to Guix deployment
- Using BSD-specific commands: `shasum`, `md5`, `gstat`, etc.
- Always use GNU coreutils equivalents: `sha256sum`, `md5sum`, `stat`

**Frequency Notes:**
- 2024-11-15: bootstrap-postinstall.scm checksum function
- 2024-11-15: Will likely happen again when porting more scripts

---

### [Count: 1] Reading Only First S-expression

**The Mistake:**
```scheme
(define (read-config filepath)
  (call-with-input-file filepath
    (lambda (port)
      (read port))))  ; ONLY READS FIRST FORM!
```

**Why It Failed:**
Guix config files have multiple top-level S-expressions:
```scheme
(use-modules (gnu))  ; First form
(use-modules (gnu services networking))  ; Second form
(operating-system ...)  ; Third form
```

Only got `(use-modules (gnu))`, missing the rest!

**The Fix:**
```scheme
(define (read-all-exprs filepath)
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((exprs '()))
        (let ((expr (read port)))
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))
```

**When to Watch For:**
- Reading Guix config files (always have multiple use-modules + operating-system)
- Processing Scheme files with multiple top-level definitions
- Any file that's more than a single S-expression

**Frequency Notes:**
- 2024-11-14: guile-config-helper.scm initial implementation

---

### [Count: 1] Wrong Invocation for Piped Scripts

**The Mistake:**
```bash
curl ... | guile --no-auto-compile -s -
```

**Why It Failed:**
The `-s -` combination causes Guile to look for a script file named `-`, leading to path resolution errors.

**The Fix:**
```bash
curl ... | guile
```

With the correct shebang (backslash continuation, no `-s`), Guile reads from stdin automatically.

**When to Watch For:**
- One-liner bootstrap commands
- Documentation examples for piped execution
- CI/CD scripts that download and run Guile code

**Frequency Notes:**
- 2024-11-15: bootstrap-postinstall.scm documentation

---

## Changelog

### 2024-11-15: Initial Creation
- Extracted gotchas from GUILE_KNOWLEDGE.md
- Established frequency tracking system
- Sorted by count (highest first)
- Added 5 initial gotchas with counts

### Frequency Tracking Protocol

When you encounter a gotcha:

1. **If it's already listed**: Increment the count, add date note
2. **If it's new**: Add it with `[Count: 1]`, insert in sorted position
3. **Periodically re-sort**: Keep highest-frequency gotchas at top
4. **Update notes**: Add specific occurrence details to help identify patterns

This ensures the most common mistakes are always visible first!
