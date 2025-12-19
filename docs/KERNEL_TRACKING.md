# Kernel Tracking System

## Overview

The installer uses comprehensive kernel tracking instrumentation to log every step of the kernel discovery and installation process. This structured logging system helps debug kernel-related issues across different platforms and installation methods.

**Status**: ✅ **Complete** - Kernel tracking is now fully implemented for both cloudzy and framework-dual platforms with complete logging parity. All `logDebug` calls include `platform` and `buildType` fields for complete traceability.

## Log Location

All kernel tracking logs are written to `/tmp/kernel_tracking.log` in NDJSON format (one JSON object per line).

## Hypothesis System

The installer uses a hypothesis-driven system where each hypothesis represents a different strategy for locating kernel files. Each hypothesis logs structured JSON data with:

- `hypothesisId`: Which strategy (G, M, H, K, N, E)
- `platform`: Installation target (cloudzy, framework, framework-dual, raspberry-pi)
- `buildType`: Software freedom level (libre, non-libre)
- `step`: Specific operation within hypothesis
- `timestamp`: When the operation occurred
- Additional context-specific fields (paths, sizes, errors, etc.)

### Hypothesis ID Policy

**Consistency Rule:** Hypothesis IDs (letters) must be consistent across all platforms. The same letter always means the same hypothesis strategy, regardless of platform.

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

### Hypothesis G: Standard Build Path

**Purpose:** Use `guix system build` to create system generation

**Steps:**
1. Run `guix system build /mnt/etc/config.scm` (or `guix time-machine ... system build` for framework-dual)
2. Check if system generation was created
3. List system generation contents
4. Search for kernel in standard locations
5. Copy kernel/initrd to `/boot`
6. Run `guix system init` to install bootloader

**Tracking Fields:**
- `hypothesisId`: "G"
- `step`: `before_guix_system_build`, `after_guix_system_build`, `system_path_found`, `list_system_contents`, `kernel_search_complete`, `kernel_copy_succeeded`, `initrd_copy_succeeded`, `created_symlink`, `before_guix_system_init`, `guix_system_init_succeeded`, `guix_system_init_failed`, `guix_system_init_failed_attempt`, `daemon_check_after_failure`, `daemon_restart_attempted`

**Common Outcomes:**
- **Success:** System generation contains kernel → proceed to copy
- **Partial Success:** System generation created but no kernel → try fallback strategies (K, N, H)
- **Failure:** Build fails → error and retry

**Note:** Both cloudzy and framework-dual can experience the kernel bug where system generation only contains `["gnu","gnu.go","guix"]` with no kernel/initrd files.

### Hypothesis M: Network Diagnostics

**Purpose:** Check if network/DNS is working before attempting network-based builds

**Steps:**
1. Test DNS resolution for `ci.guix.gnu.org`
2. Return success/failure to guide next steps

**Tracking Fields:**
- `hypothesisId`: "M"
- `platform`: Installation target (cloudzy, framework, framework-dual)
- `buildType`: Software freedom level (libre, non-libre)
- `step`: `check_network_start`, `network_ok`, `dns_failed`

**Common Outcomes:**
- **Network OK:** Proceed with Hypothesis H (build kernel package)
- **DNS Failed:** Try offline approaches (Hypotheses K, N)

### Hypothesis H: Build Kernel Package

**Purpose:** Build kernel package separately when system generation doesn't contain kernel

**Steps:**
1. Check network connectivity (Hypothesis M)
2. Run `guix build linux-libre` (or `linux` for nonguix)
3. Find kernel binary in package output
4. Extract kernel from package

**Tracking Fields:**
- `hypothesisId`: "H"
- `step`: `before_guix_build_kernel`, `guix_build_kernel_failed`, `kernel_package_path_found`, `kernel_found_in_package`, `kernel_not_found_in_package`

**Common Outcomes:**
- **Success:** Kernel package built → extract kernel
- **Failure:** Build fails → try Hypotheses K, N

**Note:** Currently only implemented for cloudzy (free-software builds). Framework-dual uses time-machine which typically includes kernel in system generation.

### Hypothesis K: Deep System Generation Search

**Purpose:** Search subdirectories and config files in system generation for kernel paths

**Steps:**
1. Explore subdirectories (`gnu/`, `guix/`, `bin/`, `boot/`)
2. Check for symlinks that might point to kernel
3. Parse `parameters` files for kernel paths
4. Search recursively through store paths

**Tracking Fields:**
- `hypothesisId`: "K"
- `step`: `deep_search_start`, `explore_subdir`, `kernel_from_parameters`, `initrd_from_parameters`, `kernel_found_via_symlink`

**Common Outcomes:**
- **Success:** Kernel found in subdirectory → use it
- **Failure:** No kernel found → try Hypothesis N

### Hypothesis N: Store-Wide Kernel Search

**Purpose:** Search entire `/gnu/store` for kernel packages

**Steps:**
1. List all packages in store
2. Filter for kernel-related packages
3. Check each package for kernel binaries
4. Return first valid kernel found

**Tracking Fields:**
- `hypothesisId`: "N"
- `step`: `store_search_start`, `packages_found`, `kernel_found_in_store`

**Common Outcomes:**
- **Success:** Kernel found in store → use it
- **Failure:** No kernel found → installation fails

### Hypothesis E: Error Recovery

**Purpose:** Recover kernel/initrd files after `guix system init` completes

**Steps:**
1. Check if kernel/initrd exist in `/mnt/boot/`
2. Verify file sizes (kernel > 5MB, initrd > 10MB)
3. If missing or too small, copy from system generation
4. Retry up to 3 times with delays

**Tracking Fields:**
- `hypothesisId`: "E"
- `step`: `check_system_link`, `system_path_found`, `check_kernel_src`, `copy_kernel`, `kernel_copy_succeeded`, `check_initrd_src`, `initrd_found_deep_search`, `initrd_found_store_search`, `initrd_size_invalid`, `initrd_copy_succeeded`, `recovery_succeeded`, `recovery_failed_retry`, `recovery_failed_final`

**Common Outcomes:**
- **Success:** Files recovered → continue installation
- **Failure:** Recovery failed after retries → manual intervention needed

**Note:** This is used by `VerifyAndRecoverKernelFiles()` which runs after system init completes. Includes fallback strategies:
- If initrd not found in system generation, tries Hypothesis K (deep search)
- If Hypothesis K fails, tries Hypothesis N (store-wide search)
- Validates initrd size (>10MB) and filters out invalid placeholders like `raw-initrd`

## Platform-Specific Implementation

### Cloudzy (Free Software Only)

**Function:** `RunGuixSystemInitFreeSoftware(platform string)`

**Build Type:** `libre`

**Hypotheses Used:** G, H, M, K, N

**Key Characteristics:**
- Uses standard `guix system build` (no time-machine)
- Kernel files in system generation are **symlinks** (must use `cp -L`)
- If kernel not found in system generation, tries Hypothesis H (build `linux-libre` package)
- Comprehensive fallback chain: G → M → H → K → N
- All logging includes `platform` and `buildType` fields for complete traceability

### Framework-Dual (Nonguix)

**Function:** `RunGuixSystemInit(platform string)`

**Build Type:** `non-libre`

**Hypotheses Used:** G, K, N, H, E

**Key Characteristics:**
- Uses `guix time-machine` with nonguix channel
- **CRITICAL DISCOVERY (2025-01-XX):** System generation may only contain `["gnu","gnu.go","guix"]` - no kernel/initrd files
- Same kernel bug affects framework-dual as cloudzy - kernel not always in system generation
- Fallback chain: G → K → N → H (if network available)
- If kernel missing after init, Hypothesis E (recovery) handles it with fallback strategies (K, N)
- Hypothesis H builds `linux` package (not `linux-libre`) using nonguix substitutes
- All logging includes `platform` and `buildType` fields for complete traceability
- `SearchStoreForKernel()` is platform-aware: searches for `linux` (non-libre) or `linux-libre` (libre) based on buildType
- Initrd recovery includes size validation (>10MB) and filters out invalid `raw-initrd` placeholders

## Using Kernel Tracking Logs

### Check Network Issues
```bash
grep '"hypothesisId":"M"' /tmp/kernel_tracking.log | jq .
```
Look for `dns_failed` or `network_ok`

### Check Kernel Build
```bash
grep '"hypothesisId":"H"' /tmp/kernel_tracking.log | jq .
```
Look for `guix_build_kernel_failed`, `kernel_build_output_invalid`, or `kernel_not_found_in_package`

### Check Deep Search
```bash
grep '"hypothesisId":"K"' /tmp/kernel_tracking.log | jq .
```
Check which `subdirPath` was explored and if `parameters_found`

### Check Store Search
```bash
grep '"hypothesisId":"N"' /tmp/kernel_tracking.log | jq .
```
See `packageCount` and which `packages` were found

### Check Main Build Path
```bash
grep '"hypothesisId":"G"' /tmp/kernel_tracking.log | jq .
```
See all steps of the main installation path

### Check Recovery Attempts
```bash
grep '"hypothesisId":"E"' /tmp/kernel_tracking.log | jq .
```
See recovery attempts and outcomes

### Filter by Platform
```bash
grep '"platform":"framework-dual"' /tmp/kernel_tracking.log | jq .
```

### Filter by Build Type
```bash
grep '"buildType":"non-libre"' /tmp/kernel_tracking.log | jq .
```

## Implementation History

### Initial Implementation (Cloudzy)
- ✅ Comprehensive kernel tracking added to `RunGuixSystemInitFreeSoftware()`
- ✅ Hypothesis system (G, H, M, K, N) fully implemented
- ✅ Platform and buildType tracking in all logs

### Parity Implementation (Framework-Dual)
- ✅ Kernel tracking added to `RunGuixSystemInit()` (2025-01-XX)
- ✅ Platform parameter added to function signature
- ✅ All major operations now logged with Hypothesis G
- ✅ Recovery tracking via Hypothesis E already existed
- ✅ All call sites updated to pass platform parameter
- ✅ Fallback strategies (K, N, H) added (2025-01-XX) - same bug affects framework-dual
- ✅ All `logDebug` calls in `RunGuixSystemInitFreeSoftware()` now include `platform` and `buildType` fields (2025-01-XX)
- ✅ Complete logging parity achieved - both platforms log identical fields for all hypotheses
- ✅ Hypothesis ID consistency policy established (2025-01-XX): Same letter = same hypothesis across platforms
- ✅ Fixed inconsistency: Changed Hypothesis "D" (daemon retry) to "G" in cloudzy to match framework-dual (2025-01-XX)

### Critical Bug Fixes Discovered via Kernel Tracking (2025-01-XX)

**Kernel tracking logs (7th-16th) revealed three critical bugs:**

#### Bug 1: Using Wrong System Generation Path
**Discovery:** Logs showed build output contained correct system path, but code was searching and finding wrong (old cached) path.

**Evidence from logs:**
- Build output: `/gnu/store/v4fisq22npmk12aqjbsw4la4679ld9f2-system` (NEW, complete)
- Code found: `/gnu/store/0wqwbqgvw60bvc1jhry5x6axbspkz3f1-guix-system` (OLD, incomplete)
- Result: System generation only had `["gnu","gnu.go","guix"]` entries

**Fix (commit c4e91aa):**
- Extract `systemPathFromOutput` from build output
- Use path from build output if available
- Only fall back to searching if build output didn't contain path
- Log warning if paths don't match

**Impact:** This was THE critical bug preventing framework-dual installation from working.

#### Bug 2: Kernel Symlink Points to Directory
**Discovery:** Kernel copy failed because kernel symlink points to profile directory, not a file.

**Evidence from logs:**
- `kernelInfo.symlinkTarget: "/gnu/store/...-profile"` (directory)
- `kernel_copy_failed: exit status 1`
- `cp -L` tried to copy directory → failed

**Fix (commit 7642cc2):**
- Detect if kernel symlink points to directory
- Resolve symlink and search for `bzImage` inside profile
- Try multiple paths: `bzImage`, `vmlinuz`, `Image`, `boot/bzImage`, `boot/vmlinuz`
- Use actual kernel binary path for copy

**Impact:** Kernel copy now succeeds by finding actual binary file.

#### Bug 3: Initrd Packages Are Directories
**Discovery:** Initrd search found packages (directories) but code expected files directly.

**Evidence from logs:**
- Found: `/gnu/store/...-microcode-initrd` (directory)
- Code rejected it because it was a directory
- Actual initrd file: `/gnu/store/...-microcode-initrd/initrd.cpio` (inside directory)

**Fix (commit 54e61eb):**
- Look inside initrd package directories for actual initrd files
- Try common paths: `initrd.cpio.gz`, `initrd.img`, `initrd.cpio`, `initrd`
- Use recursive `find` as fallback
- Log directory contents if initrd not found

**Impact:** Initrd recovery now works correctly.

#### How Kernel Tracking Helped
1. **Systematic logging:** Every step logged with hypothesis ID, step, and context
2. **Before/after comparison:** Logs showed what existed before build vs. after
3. **Path tracking:** Logged exact paths being used vs. what build output said
4. **Size validation:** Logged file sizes to detect symlinks vs. actual files
5. **Error details:** Logged exact error messages and exit codes

**Key insight:** Without kernel tracking logs, these bugs would have been nearly impossible to diagnose. The structured logging made it clear:
- Which system generation was being used
- Why kernel copy was failing
- Where initrd files actually were

**Lessons learned:**
- Always use system path from build output, never search
- Kernel symlinks point to directories, not files
- Initrd packages are directories containing the actual files
- Kernel tracking is essential for debugging complex Guix installation issues

## Code Locations

- **Main logging function:** `lib/common.go:logDebug()` (line ~3410)
- **Cloudzy implementation:** `lib/common.go:RunGuixSystemInitFreeSoftware()` (line ~1904)
- **Framework-dual implementation:** `lib/common.go:RunGuixSystemInit()` (line ~1168)
- **Recovery function:** `lib/common.go:VerifyAndRecoverKernelFiles()` (line ~1618)
- **Helper functions:**
  - `CheckNetworkConnectivity()` - Hypothesis M (line ~3439)
  - `SearchSystemGenerationDeep()` - Hypothesis K (line ~3472)
  - `SearchStoreForKernel()` - Hypothesis N

## Related Documentation

- [TROUBLESHOOTING.md](../docs/TROUBLESHOOTING.md) - Detailed hypothesis system documentation
- [INSTALLATION_KNOWLEDGE.md](./INSTALLATION_KNOWLEDGE.md) - Kernel file journey and workarounds
- [CHECKLIST.md](../CHECKLIST.md) - Implementation status and testing
