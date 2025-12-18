# Guile Conversion Plan

## Overview

We're systematically converting all shell scripts that run on Guix systems to Guile Scheme. This provides:

- **Proper S-expression handling** for config.scm manipulation
- **Type safety** and better error handling
- **Consistency** with Guix's native language
- **Maintainability** through structured code
- **Future-proofing** for advanced Guix features

## Conversion Strategy

### Phase 1: Library Infrastructure (COMPLETED)
- ✅ Created `lib/guile-config-helper.scm` for S-expression manipulation
- ✅ Set up Guile testing in Docker
- ✅ Created test framework in `lib/tests/`
- ✅ Verified Guile integration with existing Go/shell tests

### Phase 2: Postinstall Scripts (IN PROGRESS)
Convert postinstall customization scripts to use Guile helper:

1. Update `framework-dual/postinstall/customize` to use Guile helper
2. Create similar for other platforms
3. Test GNOME, NetworkManager, SSH configurations

### Phase 3: Critical Installation Scripts (PLANNED)
Convert lib/*.sh scripts to Guile:

Priority order (by complexity and criticality):

1. **lib/postinstall.sh** (744B) - Simplest, good starting point
   - Channel generation utilities
   - Used by customize scripts

2. **lib/clean-install.sh** (3.0K) - Medium complexity
   - Clean installation wrapper
   - Calls into Go installer

3. **lib/verify-guix-install.sh** (8.2K) - Complex verification
   - Post-install verification
   - System checks

4. **lib/bootstrap-installer.sh** (7.6K) - Bootstrap logic
   - Downloads and verifies repo
   - Builds Go installer

5. **lib/recovery-complete-install.sh** (13K) - Most complex
   - Recovery scenarios
   - Multiple installation paths

6. **lib/channel-utils.sh** (6.8K) - Channel management
   - Mirror selection
   - Channel configuration

### Phase 4: Integration and Testing
- Update all platform READMEs
- Create comprehensive Guile test suite
- Docker testing for all conversions
- Update documentation

## Conversion Guidelines

### Shebang for Guile Scripts on Guix
```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#
```

### Module Structure
```scheme
(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26))  ; cut/cute for partial application
```

### Common Patterns

#### Bash → Guile

**Command execution:**
```bash
# Bash
output=$(command arg1 arg2)
if [ $? -eq 0 ]; then
    echo "Success"
fi
```

```scheme
;; Guile
(use-modules (ice-9 popen) (ice-9 rdelim))

(let* ((port (open-input-pipe "command arg1 arg2"))
       (output (read-string port))
       (status (close-pipe port)))
  (if (zero? status)
      (display "Success\n")))
```

**File operations:**
```bash
# Bash
if [ -f "$file" ]; then
    content=$(cat "$file")
fi
```

```scheme
;; Guile
(when (file-exists? file)
  (let ((content (call-with-input-file file read-string)))
    ...))
```

**Argument parsing:**
```bash
# Bash
while [[ $# -gt 0 ]]; do
    case $1 in
        --flag) FLAG=true; shift ;;
        *) ARG=$1; shift ;;
    esac
done
```

```scheme
;; Guile
(use-modules (ice-9 getopt-long))

(define option-spec
  '((flag (single-char #\f) (value #f))
    (arg (single-char #\a) (value #t))))

(let ((options (getopt-long (command-line) option-spec)))
  (define flag (option-ref options 'flag #f))
  (define arg (option-ref options 'arg #f))
  ...)
```

## Testing Requirements

Each converted script must have:

1. **Unit tests** in `lib/tests/test-<script-name>.scm`
2. **Integration tests** verifying behavior matches bash version
3. **Docker tests** ensuring Guile dependencies are available
4. **Documentation** explaining any behavior changes

## Status Tracking

| Script | Lines | Status | Tests | Notes |
|--------|-------|--------|-------|-------|
| guile-config-helper.scm | 169 | ✅ Done | ✅ Passing | S-expression manipulation |
| postinstall/recipes/*.scm | Various | 🔄 In Progress | ⏸️ Pending | Recipe scripts - fixing entry points and imports |
| postinstall.sh | 31 | 🔄 Next | ⏸️ Pending | Channel utilities |
| clean-install.sh | 134 | ⏸️ Planned | ⏸️ Pending | Installation wrapper |
| verify-guix-install.sh | 305 | ⏸️ Planned | ⏸️ Pending | Verification checks |
| bootstrap-installer.sh | 267 | ⏸️ Planned | ⏸️ Pending | Bootstrap logic |
| recovery-complete-install.sh | 458 | ⏸️ Planned | ⏸️ Pending | Recovery flows |
| channel-utils.sh | 235 | ⏸️ Planned | ⏸️ Pending | Channel/mirror selection |

## Current Work (Resume Here)

**Active Work**: Postinstall recipe scripts (`.scm` files)
- Fixing entry point logic: Using `batch-mode?` instead of `command-line` checks
- Module imports: Fixing missing imports (ice-9 match, ice-9 pretty-print)
- Code cleanup: Refactoring helper functions and improving structure

**Files being worked on**:
- `postinstall/recipes/add-development.scm` - Entry point fixes
- `postinstall/recipes/add-doom-emacs.scm` - Entry point fixes
- `postinstall/recipes/add-fonts.scm` - Module imports and code cleanup
- `postinstall/recipes/add-spacemacs.scm` - Entry point fixes
- `postinstall/recipes/add-vanilla-emacs.scm` - Entry point fixes

**Analysis Documents Created**:
- `docs/CONVERTED_SCRIPTS_ANALYSIS.md` - Analysis of all converted scripts, risk levels, approaches
- `docs/SCRIPT_REVIEW_add-fonts.md` - Detailed review of add-fonts.scm conversion
- `docs/TESTING_RESULTS.md` - Test results and issues found

**Test Infrastructure**:
- `test/test-recipe-script.sh` - Automated test runner for recipe scripts
- `test/fixtures/postinstall-recipes/` - Test fixtures (minimal-config.scm, existing-packages-config.scm, has-fonts-config.scm)

**Known Issues**:
- Entry point logic was broken (using fragile command-line path checks)
- Missing module imports in some scripts
- Need to verify scripts execute correctly after fixes

**Next steps when resuming**:
1. Review entry point changes (batch-mode? vs command-line checks) - IN PROGRESS
2. Verify module imports are correct - IN PROGRESS
3. Test scripts execute correctly using test infrastructure
4. Continue with remaining recipe scripts
5. Review analysis documents for any other issues found

## Migration Path

For each script:

1. **Analyze** - Understand current bash implementation
2. **Design** - Plan Guile equivalent with better structure
3. **Implement** - Write .scm version alongside .sh
4. **Test** - Comprehensive testing in Docker + real Guix
5. **Document** - Update all references and docs
6. **Deploy** - Switch references from .sh to .scm
7. **Deprecate** - Mark .sh as deprecated
8. **Remove** - Delete .sh after stable period

## Notes

- **Coexistence**: .sh and .scm versions will coexist during transition
- **Testing**: Both versions tested until .scm proven stable
- **Rollback**: Keep .sh versions until all platforms verified
- **Knowledge Base**: See [GUILE_KNOWLEDGE.md](GUILE_KNOWLEDGE.md) for lessons learned and best practices
- **Read First**: Review GUILE_KNOWLEDGE.md before starting any Guile development work
