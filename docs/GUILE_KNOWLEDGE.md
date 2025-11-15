# Guile Development Knowledge Base

**Purpose**: Document hard-won lessons, common mistakes, and best practices for Guile Scheme development in this project.

**Read this before**: Starting any new Guile script or modifying existing .scm files.

---

## Critical Lessons Learned

### 1. Reading Multiple S-expressions from Files

**Problem**: Initial version of guile-config-helper.scm only read the first S-expression from config.scm (the `use-modules` form), missing the actual `operating-system` definition.

**Root Cause**: Used single `(read port)` instead of looping until EOF.

**Wrong:**
```scheme
(define (read-config config-file)
  (call-with-input-file config-file
    (lambda (port)
      (read port))))  ; Only reads first S-expression!
```

**Correct:**
```scheme
(define (read-config config-file)
  (call-with-input-file config-file
    (lambda (port)
      (let loop ((exprs '()))
        (let ((expr (read port)))
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))
```

**Why**: Guix config.scm files typically have multiple top-level forms:
1. `(use-modules ...)` - imports
2. `(operating-system ...)` - actual config

**Lesson**: Always loop until `eof-object?` when reading S-expressions from files.

---

### 2. Missing SRFI Imports

**Problem**: "Unbound variable: filter-map" error when trying to use `filter-map` function.

**Root Cause**: `filter-map` is in SRFI-1, not ice-9 modules.

**Fix**: Add `(srfi srfi-1)` to use-modules:
```scheme
(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1))  ; Required for filter-map, fold, etc.
```

**Common SRFI-1 functions we use:**
- `filter-map` - map and filter in one pass
- `fold` - reduce/accumulate over lists
- `any` - check if any element satisfies predicate
- `every` - check if all elements satisfy predicate

**Lesson**: Import `(srfi srfi-1)` by default in any script doing list processing.

---

### 3. Shebang for Guix System Scripts

**Critical**: All Guile scripts running on Guix systems must use this exact shebang:

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#
```

**Why `--no-auto-compile`?**
- Prevents Guile from trying to write compiled .go files to system directories
- Avoids permission errors when running as normal user
- Faster startup for short-lived scripts

**Why `/run/current-system/profile/bin/guile`?**
- Guix doesn't use FHS (Filesystem Hierarchy Standard)
- `/usr/bin/env` may not work reliably on Guix ISO
- This path is guaranteed to exist on Guix systems

**Don't use:**
- `#!/usr/bin/env guile` - may not work on Guix
- `#!/usr/bin/guile` - doesn't exist (no FHS)
- Missing `--no-auto-compile` - will cause permission errors

---

## Pattern Matching Best Practices

### Using `match` for S-expression Parsing

The `match` form is our primary tool for destructuring S-expressions:

```scheme
(use-modules (ice-9 match))

(match services-expr
  ;; Match: (append (list service1 service2) %base-services)
  (('append ('list services ...) base-services ...)
   `(append (list ,@services ,new-service) ,@base-services))

  ;; Match: %base-services (bare symbol)
  ('%base-services
   `(append (list ,new-service) %base-services))

  ;; Match: (list service1 service2)
  (('list services ...)
   `(list ,@services ,new-service))

  ;; Catch-all: unknown structure
  (_ services-expr))
```

**Key patterns:**
- `'symbol` - match exact symbol (e.g., `'%base-services`)
- `('append ...)` - match list starting with 'append
- `(var ...)` - match variable-length list, bind to var
- `_` - wildcard, matches anything

**Quasiquote for building S-expressions:**
- `` `(list ,new-item) `` - quasiquote with unquote
- `` ,@list-var `` - splice list contents (not the list itself)

---

## Common Mistakes to Avoid

### 1. Forgetting to Handle All Config Structures

Guix config.scm services can appear in many forms:
- `%base-services` (bare symbol)
- `(list service1 service2)`
- `(cons service1 %base-services)`
- `(append (list service1) %base-services)`
- `(modify-services %base-services ...)`

**Always provide a catch-all** in match expressions:
```scheme
(match services-expr
  ;; ... your patterns ...
  (_ services-expr))  ; Don't fail on unknown structures
```

### 2. Not Checking for Duplicates

Before adding a service, check if it's already present:

```scheme
(if (member service-expr services)
    services-expr  ; Already present, return unchanged
    `(list ,@services ,service-expr))  ; Add it
```

### 3. Modifying in Place Instead of Returning New Value

Scheme is functional - don't try to mutate S-expressions:

**Wrong (doesn't work in Scheme):**
```scheme
(set! services-expr (append services-expr new-service))
```

**Correct:**
```scheme
(define new-services (append services-expr (list new-service)))
```

### 4. Forgetting `!#` After Shebang

The shebang needs to be closed with `!#`:

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

(use-modules ...)
```

Without `!#`, Guile treats the shebang as part of the code.

---

## File I/O Patterns

### Reading Files

```scheme
;; Read entire file as string
(call-with-input-file filename
  (lambda (port)
    (read-string port)))

;; Read S-expressions one by one
(call-with-input-file filename
  (lambda (port)
    (let loop ((exprs '()))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (reverse exprs)
            (loop (cons expr exprs)))))))

;; Read lines
(use-modules (ice-9 rdelim))
(call-with-input-file filename
  (lambda (port)
    (let loop ((lines '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines)))))))
```

### Writing Files

```scheme
;; Write S-expressions with pretty-printing
(use-modules (ice-9 pretty-print))

(call-with-output-file filename
  (lambda (port)
    (for-each
      (lambda (expr)
        (pretty-print expr port))
      exprs)))

;; Write strings
(call-with-output-file filename
  (lambda (port)
    (display "Hello, world!\n" port)))
```

---

## Error Handling

### Checking File Existence

```scheme
;; Check before opening
(if (file-exists? config-file)
    (call-with-input-file config-file ...)
    (error "File not found" config-file))
```

### Validating S-expression Structure

```scheme
;; Find operating-system in list of expressions
(define os-expr
  (find (lambda (expr)
          (and (pair? expr)
               (eq? (car expr) 'operating-system)))
        exprs))

(if os-expr
    (process-os os-expr)
    (error "No operating-system found in config"))
```

### Providing Helpful Error Messages

```scheme
;; Bad: generic error
(error "Failed")

;; Good: specific error with context
(error "Failed to find services field in operating-system expression"
       "Config file:" config-file
       "Expression:" os-expr)
```

---

## Testing Guile Code

### Local Testing Pattern

```bash
# Test the helper directly
guile --no-auto-compile -s lib/guile-config-helper.scm \
  add-service test-config.scm "(gnu services ssh)" "(service openssh-service-type)"

# Check the result
guile --no-auto-compile -s lib/guile-config-helper.scm \
  check-config test-config.scm
```

### Shell Script Integration

```bash
# Call from bash with error handling
if guile --no-auto-compile -s "$guile_helper" add-service "$config" "$module" "$service"; then
    echo "Success"
else
    echo "Error: Guile script failed with exit code $?"
    return 1
fi
```

### Unit Test Structure

```bash
# framework-dual/postinstall/tests/test-*.sh pattern
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HELPER_SCRIPT="$SCRIPT_DIR/../../../lib/guile-config-helper.scm"
TEST_CONFIG="$SCRIPT_DIR/test-config.scm"

# Create test config
cat > "$TEST_CONFIG" <<'EOF'
(use-modules (gnu))
(operating-system
  (host-name "test")
  (services %base-services))
EOF

# Run test
if guile --no-auto-compile -s "$HELPER_SCRIPT" add-service "$TEST_CONFIG" ...; then
    echo "✓ Test passed"
else
    echo "✗ Test failed"
    exit 1
fi

# Verify result
result=$(guile --no-auto-compile -s "$HELPER_SCRIPT" check-config "$TEST_CONFIG")
if echo "$result" | grep -q "expected-pattern"; then
    echo "✓ Verification passed"
fi
```

---

## Command-Line Argument Parsing

### Simple Positional Arguments

```scheme
;; Get arguments (first is script name)
(define args (command-line))

;; Match expected arguments
(match args
  ((_ "add-service" config-file module service-expr)
   (add-service config-file module service-expr))

  ((_ "check-config" config-file)
   (check-config config-file))

  (_
   (display "Usage: script.scm <command> <args>\n" (current-error-port))
   (exit 1)))
```

### Using getopt-long for Complex Arguments

```scheme
(use-modules (ice-9 getopt-long))

(define option-spec
  '((help    (single-char #\h) (value #f))
    (verbose (single-char #\v) (value #f))
    (config  (single-char #\c) (value #t))))

(define options (getopt-long (command-line) option-spec))

(define verbose? (option-ref options 'verbose #f))
(define config-file (option-ref options 'config "/etc/config.scm"))
```

---

## Debugging Tips

### Print Debugging

```scheme
;; Print to stderr to avoid mixing with stdout
(format (current-error-port) "DEBUG: value = ~s\n" some-value)

;; Pretty-print complex S-expressions
(use-modules (ice-9 pretty-print))
(pretty-print expr (current-error-port))
```

### REPL Testing

```bash
# Start Guile REPL
guile

# Load your module
,use (ice-9 match)
,use (ice-9 pretty-print)

# Load your script (without executing main)
,load "lib/guile-config-helper.scm"

# Test functions interactively
(define test-expr '(append (list service1) %base-services))
(add-service-to-services test-expr '(service new-service))
```

---

## Performance Considerations

### When to Use `--no-auto-compile`

**Always** for short-lived scripts:
- Postinstall customization scripts
- Config manipulation tools
- One-off utilities

**Consider compiling** for:
- Long-running services
- Frequently-executed scripts
- Performance-critical code

### Avoiding Quadratic Behavior

```scheme
;; Bad: repeatedly appending to end of list (O(n²))
(let loop ((items input) (result '()))
  (if (null? items)
      result
      (loop (cdr items) (append result (list (car items))))))

;; Good: cons to front, then reverse (O(n))
(let loop ((items input) (result '()))
  (if (null? items)
      (reverse result)
      (loop (cdr items) (cons (car items) result))))

;; Even better: use built-in functions
(reverse input)
```

---

## Integration with Shell Scripts

### Calling Guile from Bash

```bash
# Pattern: Copy config, edit as user, copy back with sudo
guile_add_service() {
  local module="$1"
  local service="$2"
  local tmp_file=$(mktemp)

  # Copy to temp (user-writable)
  sudo cp "$CONFIG_FILE" "$tmp_file"
  sudo chown "$USER" "$tmp_file"

  # Edit with Guile
  if guile --no-auto-compile -s "$GUILE_HELPER" \
      add-service "$tmp_file" "$module" "$service"; then
    # Copy back with sudo
    sudo cp "$tmp_file" "$CONFIG_FILE"
    rm -f "$tmp_file"
    return 0
  else
    rm -f "$tmp_file"
    return 1
  fi
}
```

### Returning Status to Shell

```scheme
;; Success
(exit 0)

;; Failure
(exit 1)

;; Conditional exit
(if success?
    (exit 0)
    (begin
      (display "Error message\n" (current-error-port))
      (exit 1)))
```

---

## Module Organization

### When to Create a Module

For reusable code across multiple scripts:

```scheme
;; lib/my-module.scm
(define-module (my-module)
  #:use-module (ice-9 match)
  #:export (my-function other-function))

(define (my-function arg)
  ...)
```

### When to Use Standalone Scripts

For single-purpose tools (like our current approach):
- Easier to distribute (single file)
- No module path complications
- Simpler for users to understand

---

## Best Practice Themes for Guile/Scheme

Based on recommended resources and functional programming principles:

### 1. Use the Module System Properly

Structure code into modules with clear interfaces rather than one giant file:

```scheme
;; lib/my-module.scm
(define-module (my-module)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (public-function another-function))

(define (public-function arg)
  ;; Exported, available to other modules
  ...)

(define (private-helper arg)
  ;; Not exported, internal only
  ...)
```

**When to use modules:**
- Reusable code across multiple scripts
- Clear public/private API boundaries
- Logical grouping of related functions

**When to use standalone scripts:**
- Single-purpose tools (like our config helpers)
- Easier distribution (one file)
- Simpler for end users

### 2. Separate Concerns: REPL, Script, Library

- **Library code**: Pure functions, no I/O, easily testable
- **Script code**: Thin wrapper that handles I/O and calls library
- **REPL experimentation**: Interactive development, then extract to library

```scheme
;; Library: pure logic
(define (calculate-sum items)
  (fold + 0 items))

;; Script: I/O wrapper
(define (main args)
  (let ((numbers (read-numbers-from-file "input.txt")))
    (display (calculate-sum numbers))))
```

### 3. Prefer Pure Functional Definitions

Keep side-effects isolated (I/O, mutation) so bulk of logic is easy to reason about:

```scheme
;; Good: pure function
(define (add-service-to-list services new-service)
  (if (member new-service services)
      services
      (cons new-service services)))

;; Avoid: mutation
(define (add-service-mutating! services new-service)
  (set! services (cons new-service services)))  ; Don't do this
```

### 4. Use Tail Calls and Recursion Carefully

Scheme supports proper tail recursion - use it to avoid stack blow-ups:

```scheme
;; Good: tail recursive (constant stack)
(define (sum-list lst)
  (let loop ((items lst) (acc 0))
    (if (null? items)
        acc
        (loop (cdr items) (+ acc (car items))))))

;; Bad: not tail recursive (grows stack)
(define (sum-list-bad lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list-bad (cdr lst)))))
```

**Common tail-recursive patterns:**
- Named `let` for loops
- Accumulator parameter for results
- Always recurse as last operation

### 5. Write Tests and Develop Incrementally

- Write small modules
- Test via REPL
- Build up incrementally
- Keep examples in comments

```scheme
;; Example usage in comments:
;; (add-service '(service foo) '(service bar))
;; => '(service foo service bar)

(define (add-service services new-service)
  ...)
```

### 6. Code Readability and Naming

Scheme has minimal syntax - good naming matters even more:

```scheme
;; Good: descriptive names
(define (find-operating-system-expr exprs)
  (find (lambda (expr)
          (and (pair? expr)
               (eq? (car expr) 'operating-system)))
        exprs))

;; Bad: unclear names
(define (find-os x)
  (find (lambda (e) (and (pair? e) (eq? (car e) 'operating-system))) x))
```

**Naming conventions:**
- Predicates end in `?`: `empty?`, `file-exists?`
- Mutators end in `!`: `set!`, `delete-file!`
- Converters use `->`: `string->number`, `list->vector`
- Use full words, not abbreviations

### 7. Use Appropriate Abstraction Levels

Higher-order functions, closures, macros - use when they improve clarity:

```scheme
;; Good: clear intent with higher-order function
(filter (lambda (x) (> x 0)) numbers)

;; Overkill: macro where function suffices
(define-syntax when-positive  ; Don't do this for simple case
  (syntax-rules ()
    ((when-positive x body ...)
     (if (> x 0) (begin body ...)))))
```

### 8. Mind Performance When Needed

- Use tail recursion to avoid stack growth
- Avoid quadratic operations (repeated `append`)
- Consider compilation for frequently-run code
- Profile before optimizing

```scheme
;; Slow: O(n²) with repeated append
(let loop ((items input) (result '()))
  (if (null? items)
      result
      (loop (cdr items) (append result (list (car items))))))

;; Fast: O(n) with cons + reverse
(reverse
  (let loop ((items input) (result '()))
    (if (null? items)
        result
        (loop (cdr items) (cons (car items) result)))))
```

### 9. Follow Scheme Conventions

Even with Guile-specific features, stick to Scheme idioms:
- Lexical scope (let, let*, letrec)
- Immutability when possible
- Proper tail calls
- Closures for encapsulation

### 10. Handle Errors Gracefully

Provide helpful error messages with context:

```scheme
;; Bad: generic error
(if (not (file-exists? config-file))
    (error "File not found"))

;; Good: specific context
(if (not (file-exists? config-file))
    (error (format #f "Config file not found: ~a\nExpected at: ~a"
                   config-file
                   (dirname config-file))))
```

---

## Resources

### ✅ Recommended Learning Resources

#### 1. [Guile Reference Manual - "Programming in Scheme"](https://www.gnu.org/software/guile/manual/guile.html#Programming-in-Scheme)
**The official manual** for Guile, covering how to use it interactively, scripting, modules, debugging, etc.

**Why useful:** Gives you the "what" of Guile and how to use the language in practice (not just theory).

**Tip:** Use this as your baseline reference. Whenever you're unsure how to structure a module, script, or use a particular part of the API — go here.

#### 2. [The Guile Hacker's Notebook](https://jeko.frama.io/)
A resource dedicated specifically to Guile: covers concrete programming situations and brings best-practice thinking (TDD, clean code, architecture) into Scheme/Guile.

**Why useful:** Covers how to write maintainable code in Guile, not just syntax.

**Tip:** As you advance from basic syntax into "how to write maintainable code in Guile", this is very helpful.

#### 3. [Andy Wingo's Blog: "Wingolog"](https://wingolog.org/)
Blog posts and talks by a Guile maintainer. For example: ["Lessons Learned from Guile, the Ancient & Spry"](https://wingolog.org/archives/2016/01/11/lessons-learned-from-guile-the-ancient-and-spry)

**Why useful:** Meta-level thinking about how Guile is built and evolves - informs design decisions and best practices.

**Tip:** Use this for understanding "why" Guile works the way it does, which helps you make better architecture/style choices.

#### 4. [System Crafters Community Discussions](https://systemcrafters.net/)
Community discussions like "Is there a good learning material on Guile Scheme?"

**Why useful:** Honest user reflections - what they found difficult, what worked, common gotchas.

**Tip:** Browse these to see common patterns and pitfalls (especially with modules, REPL, embedding).

### Official Documentation
- [Guile Manual](https://www.gnu.org/software/guile/manual/)
- [SRFI-1 List Library](https://srfi.schemers.org/srfi-1/srfi-1.html)
- [Guix Manual](https://guix.gnu.org/manual/)

### Quick References
- Pattern matching: [ice-9 match](https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html)
- File I/O: [Ports and File Descriptors](https://www.gnu.org/software/guile/manual/html_node/File-Ports.html)
- Pretty-printing: [ice-9 pretty-print](https://www.gnu.org/software/guile/manual/html_node/Pretty_002dPrinting.html)
- Tail calls: [Expressions](https://www.gnu.org/software/guile/manual/html_node/Expressions.html)
- Modules: [Modules](https://www.gnu.org/software/guile/manual/html_node/Modules.html)

### Performance and Optimization
- [David Thompson: Optimizing Guile](https://dthompson.us/posts/optimize-guile.html)
- Guile compilation, bytecode, and performance considerations

---

## Changelog

### 2024-11-14: Enhanced with Community Resources
- Added "Best Practice Themes for Guile/Scheme" section with 10 key principles
- Added recommended learning resources:
  - Guile Reference Manual - "Programming in Scheme"
  - The Guile Hacker's Notebook (jeko.frama.io)
  - Andy Wingo's Wingolog (design philosophy and lessons learned)
  - System Crafters community discussions
- Added performance and optimization resources
- Expanded resources section with specific documentation links

### 2024-11-14: Initial Creation
- Documented "read all S-expressions" mistake
- Documented SRFI-1 import requirement
- Added shebang best practices
- Added pattern matching guide
- Added file I/O patterns
- Added testing strategies
- Added shell integration patterns

---

**Remember**: Read this document before starting any Guile development work. Add new lessons as we discover them!
