# Development Notes for Claude Code

This document contains important notes for AI assistants (like Claude Code) working on this repository.

## Expected behavior when updating the code

- Do not remove code without being told explicitly to do so.
- It is fin and encouraged to flag code that you think should be removed and let the user decide after your updates to a section of code.
- Each line/block/unit of code should have at least one purpose (goal it is trying to achieve) and if you need to, create a file ending in `codefilename_purpose.txt` which follows the flow of the code, module by module, function by function, variable by variable etc. which states the justification the section. For example include reasoning why a certain setting is kept in the bare minimal config.scm file. You can also leave notes in the right areas of the text file to mention statements of omission, for example "although you might be tempted to include setting X, we leave it out because it causes problem Y" (the more specific the better, for example, in version ABC of guix).

## Important Constraints

### Guix ISO Terminal Limitations

**DO NOT use Unicode characters in scripts that run on the Guix ISO.**

The Guix ISO terminal has limited Unicode support and will display broken characters for:

- Checkmarks: ✓ ✅ ❌
- Emojis: 🚀 💻 🥧 ⚡ etc.
- Special symbols: ⚠️ 🛡️ etc.

**Affected files:**

- `bootstrap-installer.sh` - Runs on Guix ISO
- `run-remote-steps.go` - Runs on Guix ISO (compiled and executed during install)
- Any `**/install/*.sh` or `**/install/*.go` files

**Use instead:**

- `[OK]` instead of ✓
- `[ERROR]` instead of ❌
- `[WARN]` or `WARNING:` instead of ⚠️
- Plain text descriptions

**OK to use Unicode:**

- README.md and other documentation (viewed in browsers/editors)
- `**/postinstall/**` scripts (run after booting into installed Guix system)
- Scripts run on modern terminals outside the ISO

## Code Patterns

### Reading User Input

Always read from `/dev/tty` instead of stdin when prompting for user input:

**Bash:**

```bash
read -p "Continue? [Y/n] " -r </dev/tty
```

**Go:**

```go
tty, err := os.Open("/dev/tty")
if err != nil {
    // handle error
}
defer tty.Close()
reader := bufio.NewReader(tty)
answer, err := reader.ReadString('\n')
```

**Why:** stdin may be redirected by process substitution, pipes, or other operations. `/dev/tty` always points to the actual terminal.

### Process Substitution for File Reading

When reading files in loops, use process substitution to avoid consuming stdin:

**Correct:**

```bash
while IFS= read -r line; do
    # process line
done < <(cat SOURCE_MANIFEST.txt)
```

**Incorrect (breaks stdin for later reads):**

```bash
while IFS= read -r line; do
    # process line
done < SOURCE_MANIFEST.txt
```

### Language Choice for New Scripts

**CRITICAL: Always prefer Guile for scripts that run on the Guix system (ISO or fresh install).**

When creating new diagnostic, utility, or post-install scripts:

**Use Guile (`.scm`) by default for:**
- Scripts run on Guix ISO
- Scripts run on freshly installed Guix system
- Post-install customization scripts
- System configuration helpers
- Network diagnostics and fixes
- Any script that interacts with Guix-specific tools

**Use Bash (`.sh`) only for:**
- Bootstrap scripts that run before Guix is available
- Scripts that must run on non-Guix systems
- When explicitly required for compatibility

**Use Go (`.go`) for:**
- Installation steps that need compiled binaries
- Complex state management across steps
- When type safety and error handling are critical

**Rationale:**
- Guix is a Scheme-based system with excellent Guile integration
- Guile scripts can directly use Guix APIs and modules
- Guile is the native language of Guix System configuration
- Better integration with system services and package management
- More maintainable and idiomatic for Guix workflows

**Example:**
- `lib/fix-network.scm` - Network diagnostics (Guile, runs on Guix system)
- `lib/bootstrap-installer.sh` - Bootstrap (Bash, runs before Guix available)
- `lib/common.go` - Installation logic (Go, needs compilation and state management)

## Architecture

### Installation Flow

1. User downloads `bootstrap-installer.sh` from GitHub
2. Bootstrap script:
   - Downloads repository tarball
   - Verifies checksums against `SOURCE_MANIFEST.txt`
   - Asks user to verify manifest hash
   - Builds `run-remote-steps` from Go source
   - Runs the compiled installer
3. Go installer (`run-remote-steps.go`):
   - Prompts before each step
   - Runs installation steps in sequence
   - Uses shared State struct for configuration

### State Management

All installation state is managed through a shared `State` struct (in `framework-dual/install/state.go`):

- No bash variable passing between scripts
- Type-safe, centralized configuration
- Passed as pointer to each step

### Verification Strategy

1. `SOURCE_MANIFEST.txt` contains SHA256 checksums of all source files
2. Bootstrap verifies downloaded files match manifest
3. User manually verifies manifest hash matches documentation
4. Go modules provide additional verification (go.mod/go.sum)

## Bash Shebang Paths

**CRITICAL:** All scripts that run on Guix ISO MUST use this shebang:

```bash
#!/run/current-system/profile/bin/bash
```

**DO NOT use `#!/usr/bin/env bash` or `#!/bin/bash` for critical scripts.**

This applies to:
- All scripts in the root directory that run on ISO (bootstrap-installer.sh, clean-install.sh, verify-guix-install.sh, recovery-complete-install.sh)
- All scripts in `**/install/*.sh` directories
- Any script that needs to execute on the Guix ISO

**Why `/run/current-system/profile/bin/bash` is required:**
- The Guix ISO has bash at this specific path
- `#!/usr/bin/env bash` may not work reliably on the ISO
- `#!/bin/bash` does not exist (Guix doesn't use FHS)

**For postinstall scripts** (run after booting into installed Guix):
```bash
#!/run/current-system/profile/bin/bash
```
Use the same path for consistency and reliability.

**Examples:**
- bootstrap-installer.sh: `#!/run/current-system/profile/bin/bash`
- clean-install.sh: `#!/run/current-system/profile/bin/bash`
- verify-guix-install.sh: `#!/run/current-system/profile/bin/bash`
- recovery-complete-install.sh: `#!/run/current-system/profile/bin/bash`
- All postinstall/customize scripts: `#!/run/current-system/profile/bin/bash`

## Common Pitfalls

1. **Don't skip pre-deployment validation** - Always run `lib/validate-before-deploy.sh --verbose` before committing
2. **Don't use Unicode in ISO scripts** - See above
3. **Don't read from os.Stdin directly** - Use /dev/tty
4. **Don't use `done < file.txt`** - Use process substitution
5. **Don't add timestamps to manifest** - Makes hash unstable
6. **Don't use `exec` in bootstrap** - Breaks stdin for Go installer
7. **Don't commit without running tests** - Always run `./run-tests.sh` first
8. **Don't commit without updating manifest** - See below
9. **Don't add new code without tests** - All new functions need corresponding tests
10. **Don't refactor without integration tests** - Verify functionality after moving code
11. **Don't use wrong bash shebang** - See "Bash Shebang Paths" above

## Kernel Tracking System

### Hypothesis ID Policy

**CRITICAL:** Hypothesis IDs (letters) must be consistent across all platforms. The same letter always means the same hypothesis strategy, regardless of platform.

**Current Hypothesis Assignments:**
- **G**: Standard Build Path (both platforms)
- **M**: Network Diagnostics (both platforms)
- **H**: Build Kernel Package (both platforms)
- **K**: Deep System Generation Search (both platforms)
- **N**: Store-Wide Kernel Search (both platforms)
- **E**: Error Recovery (both platforms)

**When Adding New Hypotheses:**
- Use the next available letter alphabetically
- It's OK to skip letters if a hypothesis is platform-specific (e.g., if cloudzy needs something framework-dual doesn't)
- Document any platform-specific hypotheses clearly
- Ensure the same letter is never reused for different purposes across platforms

**See also:** Full documentation in `docs/KERNEL_TRACKING.md` and `docs/TROUBLESHOOTING.md`

### Channel Pinning Policy

**CRITICAL:** Framework-dual installations MUST use wingolog-era pinned channels (Feb 2024 commits) to ensure initrd generation works correctly.

**DO NOT:**
- Change framework-dual to use unpinned channels
- Remove the platform check in `SetupNonguixChannel()`
- Update commits without testing initrd generation

**See:** `docs/CHANNEL_PINNING_POLICY.md` for complete policy and `docs/WINGOLOG_CHANNEL_ANALYSIS.md` for technical analysis.

### NVMe Module Filtering Policy

**CRITICAL:** Framework-dual and framework installations MUST filter `nvme` from `initrd-modules` because:

1. **NVMe is built-in to kernel 6.6.16**: In wingolog-era pinned channels (Feb 2024), kernel 6.6.16 has NVMe support compiled directly into the kernel, not as a loadable module
2. **ISO Guix version compatibility**: The ISO's Guix version may be newer than wingolog-era, causing module expectation mismatches (similar to the glibc issue in commit `36e1674`)
3. **Prevents build failures**: Including `nvme` in `initrd-modules` causes "kernel module not found" errors during `guix time-machine system build`

**Implementation:**
- Both `framework-dual/install/03-config-dual-boot.go` and `framework/install/03-config.go` use:
  ```scheme
  (remove (lambda (module) (string=? module "nvme")) %base-initrd-modules)
  ```
- This filters `nvme` even if it appears in `%base-initrd-modules`

**DO NOT:**
- ❌ Add `nvme` back to `initrd-modules` - it's built-in, not a loadable module
- ❌ Remove the filter - this prevents compatibility issues across ISO versions
- ❌ Assume `nvme` will work as a module - verify kernel configuration first

**See:** `docs/NVME_MODULE_FIX.md` for complete technical documentation and `docs/CHANNEL_PINNING_POLICY.md` for related channel pinning requirements.

## Development Workflow

### Pre-Deployment Validation

**CRITICAL:** Before committing changes that will be deployed to remote machines, you **MUST** run the validation script:

```bash
lib/validate-before-deploy.sh --verbose
```

**Why this is required:**

Remote Guix installations are expensive in time and money. A single syntax error or missing parameter can waste 30+ minutes of remote debugging. The validation script catches common issues locally:

**What it checks:**

1. **Guix command syntax** - Detects unquoted URLs, wrong argument order
2. **Store path validation** - Ensures paths are validated before use
3. **Error handling** - Checks exec.Command calls have error handling
4. **Hypothesis logging** - Verifies platform/buildType fields are present
5. **Hypothesis ID consistency** - Ensures same letter = same hypothesis across platforms (see Hypothesis ID Policy in docs/KERNEL_TRACKING.md)
6. **Unicode in ISO scripts** - Catches characters that break on Guix terminal
7. **Function signatures** - Validates callers match function definitions
8. **Compilation** - Ensures code compiles successfully
9. **Unit tests** - Runs all tests to verify functionality
10. **Manifest consistency** - Checks if manifest needs updating
11. **Anti-patterns** - Detects os.Stdin usage, stdin consumption issues

**When to run:**

- After modifying `lib/common.go` or any install steps
- Before committing changes to guix build commands
- After adding new functions with parameters
- When adding or changing kernel tracking logs
- Before pushing commits that will be deployed

**Exit codes:**

- `0` = All validations passed (safe to deploy)
- `1` = Validation failed (DO NOT DEPLOY - fix issues first)

**Example workflow:**

```bash
# 1. Make code changes
vim lib/common.go

# 2. Run validation (catches issues early)
lib/validate-before-deploy.sh --verbose

# 3. If validation fails, fix issues and re-validate
# DO NOT proceed until validation passes

# 4. Once validation passes, run tests
./run-tests.sh

# 5. Update manifest and commit
./update-manifest.sh
git add -A
git commit -m "..."
```

**Cost savings:**

Running validation locally saves 2-3 hours per deployment cycle by catching:
- Command syntax errors (~30 min per iteration on remote)
- Missing parameters (~20 min debugging)
- Unicode issues (~45 min, hard to debug remotely)
- Compilation errors (~10 min)
- Test failures (~20 min remote testing)

### Testing After Code Changes

**CRITICAL:** After refactoring or modifying code, you **MUST** run tests and fix any issues before committing:

```bash
# Run all tests to verify changes work correctly
./run-tests.sh

# If tests fail, fix the code and re-run tests
# Only proceed to commit after all tests pass
```

**Test Coverage Requirements:**

- **New functions** must have corresponding unit tests in `lib/common_test.go`
- **Refactored code** must have integration tests to verify functionality
- **String operations** (like partition path generation) must be tested
- **Error handling** must be tested with invalid inputs
- **Function signatures** must be verified after refactoring

**Test Files:**

- `lib/common_test.go` - Unit tests for shared functions
- `framework-dual/install/*_test.go` - Integration tests for install steps
- `run-tests.sh` - Automated test runner

**Why Testing is Required:**

- Ensures refactored code works correctly
- Prevents regressions when moving functions to common library
- Verifies function signatures and accessibility
- Validates string operations and error handling
- Maintains code quality and reliability

### Before Committing Changes

If you modified any scripts that run on the Guix ISO, you **MUST** follow this workflow:

```bash
# 1. Run pre-deployment validation (REQUIRED for all changes)
lib/validate-before-deploy.sh --verbose

# 2. If validation passes, run tests
./run-tests.sh

# 3. Update manifest with new file checksums
./update-manifest.sh

# 4. Commit all changes including tests and manifest
git add .
git commit -m "Your commit message"
```

**IMPORTANT:** Do NOT skip step 1. Validation catches expensive issues before they reach remote machines.

**Files that require manifest update:**

- `bootstrap-installer.sh`
- `run-remote-steps.go`
- Any file in `**/install/*.go`
- Any file in `**/install/*.sh`
- Any new test files (`*_test.go`)

**Why:** The manifest contains SHA256 checksums of all source files. Users verify this manifest hash to ensure GitHub's CDN has the latest version. If you don't update it, users will get checksum mismatches when they try to install.

**The update-manifest.sh script:**

- Generates checksums for all Go source files and bootstrap script
- Writes them to `SOURCE_MANIFEST.txt`
- Displays the manifest hash for documentation/verification
- Only changes when actual source files change (no timestamp)

## Repository Conventions

### Factor Common Code into lib/common.go

When adding features that are needed in multiple installers (cloudzy, framework, framework-dual, raspberry-pi), extract reusable logic into `lib/common.go` rather than duplicating it. Examples:

- Label checks: use `VerifyLabelsExist(...)`
- Mount-by-label: use `MountByLabel(label, mountPoint)`
- Free-space checks: use `GetMountFreeSpaceGiB(path)`
- Command logging: enable once per step with `EnableCommandLogging("/tmp/guix-install.log")`

This keeps installers small, readable, and consistent.

### Add Tests for New Code

**MANDATORY:** When adding new functions or features, you **MUST** add corresponding tests:

**For new functions in `lib/common.go`:**

- Add unit tests in `lib/common_test.go`
- Test with various input combinations
- Test error conditions and edge cases
- Test string operations (partition paths, device detection, etc.)

**For new install steps:**

- Add integration tests in `{platform}/install/*_test.go`
- Test function signatures and accessibility
- Test state management and persistence
- Test error handling and recovery

**Test Naming Convention:**

- Unit tests: `TestFunctionName`
- Integration tests: `TestStepName_Integration`
- Error tests: `TestFunctionName_ErrorHandling`

**Example test structure:**

```go
func TestMakePartitionPath(t *testing.T) {
    testCases := []struct {
        device   string
        partNum  string
        expected string
    }{
        {"/dev/nvme0n1", "1", "/dev/nvme0n1p1"},
        {"/dev/sda", "2", "/dev/sda2"},
        // ... more test cases
    }
    
    for _, tc := range testCases {
        t.Run(tc.device+"_"+tc.partNum, func(t *testing.T) {
            result := MakePartitionPath(tc.device, tc.partNum)
            if result != tc.expected {
                t.Errorf("got %s, want %s", result, tc.expected)
            }
        })
    }
}
```

**Why tests are required:**

- Ensures new code works correctly
- Prevents regressions during refactoring
- Documents expected behavior
- Enables safe code changes
- Maintains code quality standards

### Update Documentation After Adding Features

After implementing user-visible behavior or flow changes, update the docs in the same commit:

- `CHECKLIST.md` statuses and guidance
- `README.md` and `QUICKSTART.md` usage notes
- Platform readmes under `framework*/README.md` or `raspberry-pi/**` where applicable

Docs should reflect new safety checks, logging, and any changed commands or defaults.

### Checklist-Driven Workflow

We work from `CHECKLIST.md` and track progress explicitly:

- Before starting a feature, review the relevant checklist items
- Implement changes, then immediately update `CHECKLIST.md` statuses
- Keep commits aligned with checklist items so changes are auditable
- If items spawn new sub-tasks, add them to the checklist and complete them before closing the parent

### Resolve Design Conflicts Early

If you encounter a potential conflict between existing code, documentation, or stated design goals:

- Do not guess; add a brief note in the commit or PR description
- Alert the user promptly to discuss the trade-offs and pick a direction
- Once resolved, update code and docs together to reflect the agreed design

## Reviewer Personas (Offer These at Milestones)

After a developer completes a milestone, offer to review changes by adopting one of these personas:

1. **Mid‑Level Unix User New to Guix**
   - Focus: onboarding clarity, data‑loss warnings, Secure Boot notes, first‑boot expectations, networking quick path
   - Goal: ensure the Quickstart and README answer "what do I do next?" with minimal Guix background

2. **Seasoned Guix User**
   - Focus: channel pinning, substitute trust, receipts/provenance, services (NetworkManager, TLP, time sync, fstrim), storage options (LUKS/btrfs), diagnostics
   - Goal: robustness, reproducibility, and clean Guix idioms

3. **Dual‑Boot Laptop Owner**
   - Focus: GRUB visibility, chainloading Pop!_OS, keeping existing partitions safe, clear rollback paths
   - Goal: confidence that dual‑boot stays usable and recoverable

How to offer: “Do you want a review as a [Mid‑Level Unix User], [Seasoned Guix User], or [Dual‑Boot Laptop Owner]?”
