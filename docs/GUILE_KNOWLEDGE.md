# Guile Development Knowledge Base

**Purpose**: Central index for Guile Scheme development knowledge in this project.

**Read this before**: Starting any new Guile script or modifying existing .scm files.

---

## Quick Navigation

### 📚 [GUILE_BEST_PRACTICES.md](GUILE_BEST_PRACTICES.md)
Best practices and design patterns for writing maintainable Guile code.

**Topics:**
- Module system usage
- Separation of concerns (REPL/Script/Library)
- Pure functional style
- Tail recursion patterns
- Incremental development
- Code readability and naming
- Abstraction levels
- Performance optimization
- Scheme conventions
- Error handling

**Read this**: When designing new functions or refactoring existing code.

---

### ⚠️  [GUILE_GOTCHAS.md](GUILE_GOTCHAS.md)
Common mistakes sorted by frequency (most common first).

**Current top gotchas:**
1. Missing SRFI-1 import (Count: 3)
2. Wrong shebang for piped execution (Count: 2)
3. Using macOS commands on Guix (Count: 2)
4. Reading only first S-expression (Count: 1)
5. Wrong invocation for piped scripts (Count: 1)

**Read this**: When you encounter an error, or before writing similar code patterns.

---

## Guile Quick Reference (This Project)

### Standard Shebang for Standalone Scripts

**For regular scripts (executed directly):**
```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#
```

**For scripts meant to be piped (bootstrap scripts):**
```scheme
#!/run/current-system/profile/bin/guile \
--no-auto-compile
!#
```

### Essential Imports

**Minimum for most scripts:**
```scheme
(use-modules (ice-9 popen)       ; Command execution
             (ice-9 rdelim)      ; Line reading
             (ice-9 format)      ; String formatting
             (srfi srfi-1))      ; List processing
```

**For S-expression manipulation:**
```scheme
(use-modules (ice-9 pretty-print) ; Pretty-print S-exprs
             (ice-9 match)        ; Pattern matching
             (srfi srfi-1))       ; List functions
```

### Common Patterns

**Reading all S-expressions from a file:**
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

**Executing shell command and capturing output:**
```scheme
(define (run-command cmd)
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (values output status)))
```

**Pattern matching on S-expressions:**
```scheme
(use-modules (ice-9 match))

(match expr
  (('operating-system fields ...)
   ;; Handle operating-system form
   ...)
  (('use-modules modules ...)
   ;; Handle use-modules form
   ...)
  (else
   ;; Default case
   ...))
```

---

## Testing Strategy

### REPL-Driven Development

1. Start Guile REPL: `guile`
2. Load your module: `(load "lib/my-script.scm")`
3. Test functions interactively
4. Fix issues and reload
5. Once working, extract to clean script

### Integration Testing

**For config manipulation:**
```bash
# Test with sample config file
framework-dual/postinstall/tests/run-guile-tests.sh
```

**For bootstrap scripts:**
```bash
# Test in Docker environment
./test-docker.sh test
```

---

## File I/O Reference

### Reading Files

```scheme
;; Read entire file as string
(call-with-input-file "path/to/file" read-string)

;; Read all S-expressions
(call-with-input-file "config.scm"
  (lambda (port)
    (let loop ((exprs '()))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (reverse exprs)
            (loop (cons expr exprs)))))))

;; Read line by line
(call-with-input-file "path/to/file"
  (lambda (port)
    (let loop ((lines '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines)))))))
```

### Writing Files

```scheme
;; Write string to file
(call-with-output-file "path/to/file"
  (lambda (port)
    (display "content" port)))

;; Pretty-print S-expression to file
(call-with-output-file "config.scm"
  (lambda (port)
    (pretty-print expr port)))
```

---

## Shell Integration

### Running Commands

```scheme
;; Simple command execution (return status)
(system "ls -la")  ; Returns exit code

;; Execute with proper error handling
(define (system* cmd)
  (let ((status (system cmd)))
    (zero? status)))

;; Capture output
(define (run-command cmd)
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (if (zero? status)
        output
        #f)))
```

### Platform-Specific Commands

**ALWAYS use GNU coreutils on Guix:**
- ✅ `sha256sum` (NOT `shasum`)
- ✅ `md5sum` (NOT `md5`)
- ✅ `stat` (NOT `gstat`)

See [GUILE_GOTCHAS.md](GUILE_GOTCHAS.md#count-2-using-macos-commands-on-guix) for details.

---

## When to Use Guile vs Bash

### Use Guile when:
- Manipulating S-expressions (config.scm files)
- Complex data transformations
- Need proper error handling
- Cross-platform script requirements

### Use Bash when:
- Simple file operations
- Gluing together CLI tools
- Quick one-liners
- Temporary helper scripts

### Use Guile from Bash:
```bash
# Call Guile helper from bash
guile_add_service() {
  local module="$1"
  local service="$2"

  guile --no-auto-compile -s lib/guile-config-helper.scm \
    add-service /etc/config.scm "$module" "$service"
}
```

---

## Resources

### Documentation
- [Guile Manual](https://www.gnu.org/software/guile/manual/)
- [SRFI-1 List Library](https://srfi.schemers.org/srfi-1/srfi-1.html)
- [Guix Manual](https://guix.gnu.org/manual/)

### Learning Resources
- [Guile Reference Manual - "Programming in Scheme"](https://www.gnu.org/software/guile/manual/guile.html#Programming-in-Scheme)
- [The Guile Hacker's Notebook](https://jeko.frama.io/)
- [Andy Wingo's Blog: "Wingolog"](https://wingolog.org/)
- [System Crafters Community](https://systemcrafters.net/)

### Project-Specific
- [GUILE_BEST_PRACTICES.md](GUILE_BEST_PRACTICES.md) - Design patterns and coding standards
- [GUILE_GOTCHAS.md](GUILE_GOTCHAS.md) - Common mistakes (frequency-sorted)
- [GUILE_CONVERSION.md](GUILE_CONVERSION.md) - Bash-to-Guile conversion strategy
- [POSTINSTALL_DEV.md](POSTINSTALL_DEV.md) - Postinstall development workflow

---

## Changelog

### 2024-11-15: Major Reorganization
- Split into three focused files:
  - GUILE_KNOWLEDGE.md (this file): Quick reference and index
  - GUILE_BEST_PRACTICES.md: Design patterns and principles
  - GUILE_GOTCHAS.md: Common mistakes with frequency tracking
- Streamlined content to be a practical quick reference
- Added navigation guide and file purposes

### 2024-11-14: Enhanced with Community Resources
- Added best practice themes (moved to GUILE_BEST_PRACTICES.md)
- Added recommended learning resources
- Documented common mistakes (moved to GUILE_GOTCHAS.md)

### 2024-11-14: Initial Creation
- Documented "read all S-expressions" mistake
- Documented SRFI-1 import requirement
- Added shebang best practices
- Added pattern matching guide
- Added file I/O patterns
- Added testing strategies
- Added shell integration patterns
