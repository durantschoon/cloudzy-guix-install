# Framework-Dual Installation Milestone (2025-01-XX)

## 🎉 Status: SOLVED

Framework-dual installation is now **fully working** after rediscovering and fixing critical bugs through systematic kernel tracking.

## The Journey: From Broken to Working

### Initial Problem
Framework-dual installations were failing silently:
- `guix time-machine system build` appeared to succeed
- System generation was created
- But kernel/initrd files were missing from `/boot/`
- System wouldn't boot

### The Rediscovery Process

This issue was solved before but had to be rediscovered. **Kernel tracking logs (7th-16th)** provided the systematic evidence needed to identify and fix three critical bugs.

## Critical Bugs Discovered and Fixed

### Bug 1: Using Wrong System Generation Path ⚠️ CRITICAL

**Root Cause:** Code was searching for newest system generation instead of using the one from build output.

**Evidence from Kernel Tracking Logs:**
- **14th log:** Build output showed: `/gnu/store/v4fisq22npmk12aqjbsw4la4679ld9f2-system` (NEW, complete)
- Code searched and found: `/gnu/store/0wqwbqgvw60bvc1jhry5x6axbspkz3f1-guix-system` (OLD, incomplete)
- Result: System generation only had `["gnu","gnu.go","guix"]` entries (3 entries instead of 8+)

**Why This Happened:**
- Previous failed builds left incomplete system generations in `/gnu/store`
- `ls -td /gnu/store/*-system | head -1` finds the most recently modified, not the newest created
- Cached incomplete system generations from previous runs

**The Fix (commit c4e91aa):**
```go
// Extract system path from build output
systemPathFromOutput := extractPathFromBuildOutput(buildStdout)

// Use path from build output if available
if systemPathFromOutput != "" {
    systemPath = systemPathFromOutput
} else {
    // Only fall back to searching if build output didn't contain path
    systemPath = findNewestSystemGeneration()
}
```

**Impact:** This was THE critical bug preventing framework-dual installation. After this fix, system generation correctly contains 8+ entries including kernel and initrd.

### Bug 2: Kernel Symlink Points to Directory

**Root Cause:** Kernel symlink points to a profile directory, not a file. `cp -L` fails when trying to copy a directory.

**Evidence from Kernel Tracking Logs:**
- **15th log:** `kernelInfo.symlinkTarget: "/gnu/store/...-profile"` (directory)
- `kernel_copy_failed: exit status 1`
- Kernel symlink: 51 bytes (symlink), points to directory

**The Fix (commit 7642cc2):**
```go
// Detect if kernel symlink points to directory
if linkInfo.Mode()&os.ModeSymlink != 0 {
    resolvedPath := os.Readlink(kernelSrc)
    if info.IsDir() {
        // Search for kernel binary inside profile
        kernelBinaryPaths := []string{
            "bzImage", "vmlinuz", "Image",
            "boot/bzImage", "boot/vmlinuz",
        }
        // Find actual kernel binary and use that path
    }
}
```

**Impact:** Kernel copy now succeeds by finding `bzImage` inside the profile directory.

### Bug 3: Initrd Packages Are Directories

**Root Cause:** Initrd search found packages (directories like `microcode-initrd`) but code expected files directly.

**Evidence from Kernel Tracking Logs:**
- Found: `/gnu/store/...-microcode-initrd` (directory)
- Code rejected it because it was a directory
- Actual initrd file: `/gnu/store/...-microcode-initrd/initrd.cpio` (inside directory)

**The Fix (commit 54e61eb):**
```go
if info.IsDir() {
    // Look inside directory for actual initrd files
    initrdFiles := []string{
        "initrd.cpio.gz", "initrd.img",
        "initrd.cpio", "initrd",
    }
    // Try each path, use recursive find as fallback
}
```

**Impact:** Initrd recovery now works correctly by looking inside package directories.

## How Kernel Tracking Helped

### Systematic Evidence Collection

Kernel tracking logs provided structured evidence at every step:

1. **Before/After Comparison:**
   - Logged existing system paths BEFORE build
   - Logged system path FROM build output
   - Logged system path BEING USED
   - Made it clear when paths didn't match

2. **File Structure Analysis:**
   - Logged system generation contents (entry count, names)
   - Logged kernel/initrd info (isSymlink, symlinkTarget, size)
   - Made it clear when system generation was incomplete

3. **Error Details:**
   - Logged exact error messages and exit codes
   - Logged file sizes to detect symlinks vs. actual files
   - Logged directory contents when files not found

### Key Log Entries That Revealed Bugs

**14th Log - System Path Mismatch:**
```json
{"systemPathFromOutput": "/gnu/store/v4fisq22npmk12aqjbsw4la4679ld9f2-system"}
{"systemPath": "/gnu/store/0wqwbqgvw60bvc1jhry5x6axbspkz3f1-guix-system"}
{"usingPathFromOutput": false}  // ← BUG REVEALED!
```

**15th Log - Kernel Symlink Structure:**
```json
{"kernelInfo": {
  "isSymlink": true,
  "symlinkTarget": "/gnu/store/...-profile"  // ← Directory, not file!
}}
{"kernel_copy_failed": "exit status 1"}
```

**16th Log - Success After Fixes:**
```json
{"usingPathFromOutput": true}  // ← Using correct path!
{"hasKernel": true, "hasInitrd": true}
{"entries": ["activate","boot","etc","initrd","kernel","locale","parameters","profile"]}  // ← 8 entries!
{"kernel_copy_succeeded": true}
{"initrd_copy_succeeded": true}
```

## The Complete Solution

### Implementation Checklist

- [x] Extract system path from build output (commit c4e91aa)
- [x] Use system path from build output instead of searching (commit c4e91aa)
- [x] Detect kernel symlink pointing to directory (commit 7642cc2)
- [x] Find kernel binary (`bzImage`) inside profile directory (commit 7642cc2)
- [x] Handle initrd packages as directories (commit 54e61eb)
- [x] Look inside initrd packages for actual files (commit 54e61eb)
- [x] Force rebuild with `--no-substitutes --no-grafts` (commit 3c548d8)
- [x] Add comprehensive build output analysis (commit 3c548d8)
- [x] Verify files exist after copy (commit 81efe00)
- [x] Verify files exist after `guix system init` (commit 81efe00)
- [x] Fix password setting to find passwd in Guix store (commit 01e0636)
- [x] Pass username as environment variable (commit f15f7e9)

### Code Locations

**Main fixes in `lib/common.go`:**
- System path extraction: Lines 1407-1490
- Kernel symlink resolution: Lines 2004-2084
- Initrd directory handling: Lines 4417-4451 (Hypothesis N)
- Build output analysis: Lines 1318-1405
- File verification: Lines 2348-2390

## Prevention: How to Never Lose This Again

### Documentation Updates

1. **INSTALLATION_KNOWLEDGE.md:**
   - Updated "Step 2: Finding the System Generation" with critical bug fix
   - Updated "Step 3: Copying Kernel Files" with kernel symlink resolution
   - Added reboot/unmount instructions

2. **KERNEL_TRACKING.md:**
   - Added "Critical Bug Fixes Discovered via Kernel Tracking" section
   - Documented all three bugs with evidence from logs
   - Explained how kernel tracking helped discover each bug

3. **This document (FRAMEWORK_DUAL_MILESTONE.md):**
   - Complete record of the rediscovery process
   - All bugs documented with evidence
   - Prevention strategies

### Code Comments

All critical fixes include detailed comments explaining:
- Why the fix is needed
- What the bug was
- How kernel tracking logs revealed it

### Testing

**Verification checklist:**
- [ ] System path from build output matches path being used
- [ ] System generation has 8+ entries (not just 3)
- [ ] Kernel copy succeeds (finds `bzImage` in profile)
- [ ] Initrd copy succeeds (finds file inside package directory)
- [ ] Files exist after copy
- [ ] Files exist after `guix system init`
- [ ] Password setting works (finds passwd in store)

## Reboot Instructions

**After installation completes:**

1. **Sync filesystems:**
   ```bash
   sync
   ```

2. **Unmount all filesystems:**
   ```bash
   umount -R /mnt
   ```
   The `-R` flag recursively unmounts all mounts under `/mnt`.

3. **Reboot:**
   ```bash
   reboot
   ```
   Remove the ISO/USB and boot from the installed system.

**⚠️ CRITICAL:** Always sync and unmount before rebooting! Failure to unmount can cause filesystem corruption.

## Success Metrics

**16th Kernel Tracking Log showed:**
- ✅ Using correct system generation from build output
- ✅ System generation complete (8 entries)
- ✅ Kernel found and copied successfully (12.3 MB)
- ✅ Initrd found and copied successfully (30.2 MB)
- ✅ System init succeeded
- ✅ All verification passed

**Framework-dual installation is now fully working!**

## Related Documentation

- [INSTALLATION_KNOWLEDGE.md](./INSTALLATION_KNOWLEDGE.md) - Complete installation guide
- [KERNEL_TRACKING.md](./KERNEL_TRACKING.md) - Kernel tracking system documentation
- [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) - Hypothesis system and debugging guide
