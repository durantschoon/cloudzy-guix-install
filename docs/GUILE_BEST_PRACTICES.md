# Guile/Scheme Best Practices

Best practices and design patterns for writing maintainable Guile code.

Based on:
- Community resources (Guile Hacker's Notebook, Wingolog, System Crafters)
- Functional programming principles
- Real-world experience from this project

For common mistakes, see [GUILE_GOTCHAS.md](GUILE_GOTCHAS.md).

---

## Table of Contents

- [Guile/Scheme Best Practices](#guilescheme-best-practices)
  - [Table of Contents](#table-of-contents)
  - [1. Use the Module System Properly](#1-use-the-module-system-properly)
  - [2. Separate Concerns: REPL, Script, Library](#2-separate-concerns-repl-script-library)
  - [3. Prefer Pure Functional Definitions](#3-prefer-pure-functional-definitions)
  - [4. Use Tail Calls and Recursion Carefully](#4-use-tail-calls-and-recursion-carefully)
  - [5. Write Tests and Develop Incrementally](#5-write-tests-and-develop-incrementally)
  - [6. Code Readability and Naming](#6-code-readability-and-naming)
  - [7. Use Appropriate Abstraction Levels](#7-use-appropriate-abstraction-levels)
  - [8. Mind Performance When Needed](#8-mind-performance-when-needed)
  - [9. Follow Scheme Conventions](#9-follow-scheme-conventions)
  - [10. Handle Errors Gracefully](#10-handle-errors-gracefully)
  - [Resources](#resources)
    - [Recommended Learning Resources](#recommended-learning-resources)
      - [1. Guile Reference Manual - "Programming in Scheme"](#1-guile-reference-manual---programming-in-scheme)
      - [2. The Guile Hacker's Notebook](#2-the-guile-hackers-notebook)
      - [3. Andy Wingo's Blog: "Wingolog"](#3-andy-wingos-blog-wingolog)
      - [4. System Crafters Community Discussions](#4-system-crafters-community-discussions)
    - [Official Documentation](#official-documentation)
    - [Quick References](#quick-references)
    - [Performance and Optimization](#performance-and-optimization)
  - [Changelog](#changelog)
    - [2024-11-15: Initial Creation](#2024-11-15-initial-creation)

---

## 1. Use the Module System Properly

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

---

## 2. Separate Concerns: REPL, Script, Library

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

---

## 3. Prefer Pure Functional Definitions

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

**Benefits:**
- Easier to test (no setup/teardown needed)
- Easier to reason about (no hidden state)
- Composable (pure functions combine cleanly)
- Parallelizable (no shared mutable state)

---

## 4. Use Tail Calls and Recursion Carefully

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

**When recursion is appropriate:**
- Processing lists or trees
- State machines
- Iterative algorithms

**When to use built-in functions instead:**
- `fold`, `map`, `filter` are already optimized
- More readable than manual recursion
- Less error-prone

---

## 5. Write Tests and Develop Incrementally

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

**Development workflow:**

1. Start in REPL with small test cases
2. Extract working code to functions
3. Add docstrings and examples
4. Create test files for regression testing
5. Refactor once tests pass

---

## 6. Code Readability and Naming

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
- Be consistent with Scheme stdlib naming

**Formatting:**
- Indent consistently (2 spaces per level)
- Align closing parens vertically
- Break long expressions across lines
- Group related functions together

---

## 7. Use Appropriate Abstraction Levels

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

**Abstraction ladder:**

1. **Direct code**: Simplest, most explicit
2. **Helper functions**: Reduce duplication
3. **Higher-order functions**: Parameterize behavior
4. **Macros**: Create new syntax (use sparingly!)

**When to create abstractions:**
- Pattern appears 3+ times
- Logic is complex and benefits from naming
- Future changes will be localized

**When NOT to abstract:**
- One-off code
- Abstraction obscures intent
- Too early (wait for real duplication)

---

## 8. Mind Performance When Needed

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

**Common performance pitfalls:**
- `append` on right side of list (O(n) per call)
- Deep recursion without tail calls (stack overflow)
- String concatenation in loops (use `string-append` with list, then join)
- Repeated `length` calls (cache it if needed)

**Optimization strategy:**

1. Write correct code first
2. Measure if performance matters
3. Identify bottlenecks (profiling)
4. Apply targeted optimizations
5. Verify correctness still holds

---

## 9. Follow Scheme Conventions

Even with Guile-specific features, stick to Scheme idioms:

**Lexical scope:**
- Use `let`, `let*`, `letrec` appropriately
- `let`: bindings don't see each other
- `let*`: sequential bindings (can reference earlier ones)
- `letrec`: recursive bindings

**Immutability:**
- Default to immutable data structures
- Return new values instead of mutating
- Use `!` suffix only when truly necessary

**Proper tail calls:**
- Scheme guarantees tail call optimization
- Rely on it for iteration
- No need for explicit loops (unless clearer)

**Closures:**
- Capture environment for encapsulation
- Create stateful objects functionally
- Return functions from functions

---

## 10. Handle Errors Gracefully

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

**Error handling strategies:**

**Guard against invalid input:**
```scheme
(define (divide a b)
  (if (zero? b)
      (error "Cannot divide by zero")
      (/ a b)))
```

**Validate early:**
```scheme
(define (process-config filepath)
  ;; Validate upfront
  (unless (file-exists? filepath)
    (error (format #f "Config not found: ~a" filepath)))
  (unless (access? filepath R_OK)
    (error (format #f "Config not readable: ~a" filepath)))

  ;; Now process
  ...)
```

**Provide recovery options:**
```scheme
(define (safe-read-config filepath fallback)
  (if (file-exists? filepath)
      (call-with-input-file filepath read)
      fallback))
```

---

## Resources

### Recommended Learning Resources

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
- [David Thompson: Optimizing Guile](https://dthompson.us/posts/optimizing-guile-scheme.html)
- Guile compilation, bytecode, and performance considerations

---

## Changelog

### 2024-11-15: Initial Creation
- Extracted best practices from GUILE_KNOWLEDGE.md
- Organized into 10 thematic sections
- Added practical examples for each principle
- Included learning resources and references
