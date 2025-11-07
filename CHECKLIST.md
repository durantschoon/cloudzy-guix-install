# Guix Installer Implementation Checklist

This checklist tracks remaining work for the cloudzy-guix-install project. Completed items have been removed to keep this file focused on what's left to do.

For implementation history and completed features, see git commit history.

## 🔄 Currently Working On

**Status:** 🚨 CRITICAL - Missing vmlinuz*/initrd* Files Prevent Boot

**Most Recent Additions (2025-11):**
- ✅ **Recovery script** - Automatic recovery script generation for all installers
- ✅ **Installation verification** - Post-install checks for kernel/initrd/GRUB
- ✅ **User password setup** - Pre-reboot password setting via chroot
- ✅ **Nonguix integration** - Time-machine based nonguix channel support
- ✅ **Boot mode detection fix** - Cloudzy VPS now correctly detects BIOS vs UEFI
- ✅ **Initrd fix** - Added `(initrd microcode-initrd)` to framework configs
- ✅ **Clean install script** - Removes all artifacts for fresh reinstall
- ✅ **Bash shebang documentation** - Fixed contradictory documentation
- ✅ **Nonguix prompt fix** - Debug logging revealed user was accidentally declining
- ✅ **Hybrid progress monitoring** - Detects hangs, shows log growth, warns after 15min
- ✅ **GRUB verification fix** - Corrected false warning about grub.cfg location

**🚨 CRITICAL ISSUE #1 - BLOCKS ALL INSTALLATIONS:**

### Missing vmlinuz* and initrd* Files After guix system init

**Problem:** After `guix system init` appears to complete, `/mnt/boot/` is missing kernel and initrd files. System cannot boot without these files.

**What's present:**
- ✅ `/mnt/boot/grub/grub.cfg` exists
- ✅ `/mnt/boot/efi/EFI/guix/grubx64.efi` exists
- ❌ `/mnt/boot/vmlinuz-*` MISSING
- ❌ `/mnt/boot/initrd-*` MISSING

**What we know:**
- Config.scm has correct nonguix imports: `(nongnu packages linux)` and `(nongnu system linux-initrd)`
- Config.scm has `(initrd microcode-initrd)` field
- User confirmed seeing nonguix trust prompt and accepting
- `guix system init` appears to run but may hang silently or fail during build

**Root causes to investigate:**
1. **Silent build failure** - `guix system init` may be failing to build kernel but not showing error
2. **Process hangs** - May hang during download/build phase (spinner kept going overnight)
3. **Incomplete init** - Build succeeds but bootloader installation phase fails silently
4. **Substitute failures** - Can't download kernel package and build from source fails

**New tools to help diagnose:**
- ✅ Enhanced progress monitoring (shows log growth every 60s)
- ✅ Hang detection (warns after 15min of no output)
- ✅ Log file tracking at `/tmp/guix-install.log`

**Research findings - Most likely causes:**

1. **Disk space exhaustion** (MOST LIKELY)
   - ISO tmpfs fills up during kernel compilation
   - Build fails with "no space left on device"
   - Need to verify TMPDIR is set early enough

2. **cow-store not redirecting properly**
   - Store writes filling up ISO RAM instead of /mnt
   - Need to verify cow-store is actually working

3. **Silent build failure**
   - Kernel build errors but guix system init continues
   - Never generates vmlinuz/initrd but appears to succeed

4. **Nonguix substitute unavailable + build hangs**
   - substitutes.nonguix.org lacks pre-built kernel
   - Local compilation gets stuck indefinitely

**Next debugging steps:**

1. **Before starting install:**
   ```bash
   df -h          # Check all filesystem space
   df -h /mnt     # Check target partition space
   df -h /tmp     # Check tmpfs space
   ```

2. **Check if kernel was built:**
   ```bash
   ls /gnu/store/*linux-6*
   find /gnu/store -name "vmlinuz*"
   find /gnu/store -name "initrd*"
   ```

3. **Check log for actual errors:**
   ```bash
   tail -200 /tmp/guix-install.log | grep -i "error\|fail\|space\|denied"
   ```

4. **Verify build process:**
   ```bash
   ps aux | grep guix      # Check if guix is running
   ls -lah /tmp/guix*      # Check temp files
   ```

5. **Re-run with new monitoring** - Enhanced progress monitoring will show:
   - If log is growing (build working but slow)
   - If log stopped (hung or failed)
   - Warnings after 15min of silence

**Current Focus:**
1. 🚨 **#1 PRIORITY: Fix missing vmlinuz*/initrd* files** - System cannot boot without these
2. Test with enhanced monitoring to identify why guix system init isn't generating kernel files
3. Test Cloudzy installer on fresh VPS with BIOS boot fix
4. Test and validate complete framework-dual installation flow

---

## 📋 Remaining Work

### 🟡 Medium Priority

#### 1. Dual-Boot GRUB UX Improvements
**Status:** ❌ Not implemented

Ensure readable GRUB theme and visible timeout; add explicit chainloader entry for Pop!_OS in EFI if auto-detection fails.

**Current state:**
- ✅ Timeout set to 5 seconds
- ✅ os-prober configured in recovery script
- ❌ Need to test chainloader detection
- ❌ GRUB theme not customized

**Impact:** ⭐⭐ Medium - Smoother dual-boot selection

---

#### 2. Bootloader Timeout Configuration
**Status:** ⚠️ Partially implemented

**Current:**
```scheme
(bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5))  ; Already set in framework-dual
```

**Need to verify:**
- Framework single-boot installer also has timeout
- Cloudzy installer has appropriate timeout
- Timeout is documented in generated configs

**Impact:** ⭐⭐ Medium - Affects dual-boot usability

---

#### 3. Storage Options Documentation
**Status:** ❌ Not documented

Provide documented flows for:
- LUKS + ext4 root
- btrfs with subvolumes and periodic scrub hooks
- Flag to reserve N GiB unallocated and/or create separate `/home`

**Impact:** ⭐⭐ Medium - Security/flexibility options for advanced users

---

#### 4. Safer Retries and Diagnostics
**Status:** ❌ Not implemented

Toggle verbose vs quiet logging; capture `guix describe` and `guix weather` summaries into the log and receipt.

**Impact:** ⭐⭐ Medium - Easier troubleshooting

---

#### 5. Post-Install Customization Profiles
**Status:** ❌ Not implemented

Split `/etc/config.scm` into base OS vs hardware profile; provide a "first reconfigure" profile that adds firmware, NetworkManager, SSH, time sync, and trim in one step.

**Impact:** ⭐⭐ Medium - Faster, cleaner onboarding

---

#### 6. Troubleshooting/Rescue Basics Documentation
**Status:** ❌ Not documented

Document where logs/receipts live and add a short chroot/repair/rerun guide when post-install verification fails.

**Current state:**
- ✅ Recovery script exists
- ✅ Installation receipt written
- ❌ Need comprehensive troubleshooting guide
- ❌ Need rescue/repair documentation

**Impact:** ⭐⭐ Medium - Speeds recovery

---

### 🟢 Low Priority (Nice to Have)

#### 7. Color-Coded Step Distinction in Go Installers
**Status:** ❌ Not implemented

**Goal:** Add visual distinction between installation steps (01-partition, 02-mount, 03-config, 04-system-init) using subtle background colors.

**Inspiration:** The verify-guix-install.sh script uses colors effectively for status output.

**Implementation approach:**
- Add ANSI color codes to Go installer steps
- Use alternating subtle light background colors when transitioning between steps
- Example progression:
  - Step 01: Light blue background
  - Step 02: Light green background
  - Step 03: Light yellow background
  - Step 04: Light cyan background
- Keep foreground text readable (dark text on light backgrounds)
- Add color reset codes at end of each step
- Consider adding colored [OK], [WARN], [ERROR] markers similar to shell scripts

**Benefits:**
- Easier to visually scan installation output
- Quickly identify which step is running
- Better distinguish step transitions in logs
- More polished user experience

**Impact:** ⭐ Low - Nice visual polish, improves readability

---

#### 8. Label Verification Output
**Status:** ❌ Not shown to user

Should display:
```bash
# Show labels after formatting
echo "Verifying partition labels..."
e2label /dev/nvme0n1p2        # Should show: GUIX_ROOT
fatlabel /dev/nvme0n1p1       # Should show: EFI
parted /dev/nvme0n1 print     # Should show GPT names
```

**Impact:** ⭐ Low - Nice for debugging

---

#### 8. Stronger Installation Receipts
**Status:** ⚠️ Partially implemented

**Current:**
- ✅ Basic receipt written
- ✅ Channel commits included (via recovery script)
- ❌ Need `/run/current-system` derivation
- ❌ Need complete substitute server list
- ❌ Need authorization keys list

**Impact:** ⭐ Low - Better provenance tracking

---

#### 9. Raspberry Pi Track Enhancements
**Status:** ❌ Not implemented

Add optional image build recipe and Pi-specific initrd modules/services (chrony, headless SSH with key drop).

**Impact:** ⭐ Low - Broader hardware support

---

#### 10. Labels vs Device Paths Explanation
**Status:** ❌ Not documented

Add a one-sentence explanation and simple diagram where labels first appear in documentation.

**Impact:** ⭐ Low - Easier mental model for new users

---

#### 11. Optional Channel Pinning Toggle Documentation
**Status:** ❌ Not documented

Provide a short on/off toggle doc section; default remains safe/unpinned.

**Impact:** ⭐ Low - Simpler onboarding choice

---

#### 12. Swap Partition Support
**Status:** ⚠️ Only swapfile support

**Current:** Only supports creating swapfile in step 4

**Could add:** Detection and use of existing swap partition

**Impact:** ⭐ Low - Swapfile works fine for most users

---

#### 13. Reserved Disk Space Option
**Status:** ❌ Not implemented

**Could add:**
- Allow leaving 10-20GB unallocated
- User configurable via env var

**Impact:** ⭐ Low - Most users don't need this

---

#### 14. Script Directory Reorganization
**Status:** ❌ Not implemented

**Proposal:**
- Move critical scripts that run on Guix ISO/system into `lib/` subdirectory:
  - `verify-guix-install.sh` → `lib/verify-guix-install.sh`
  - `recovery-complete-install.sh` → `lib/recovery-complete-install.sh`
  - `bootstrap-installer.sh` → `lib/bootstrap-installer.sh`
  - `lib/postinstall.sh` (already in lib/)
- Keep development/repo scripts at top level:
  - `update-manifest.sh`
  - `run-tests.sh`

**Benefits:**
- Clear separation between Guix runtime scripts and development scripts
- Consistent with `lib/common.go` and `lib/postinstall.sh` patterns
- Easier to understand repository structure

**Breaking changes:**
- Bootstrap script path references need updating
- Documentation URLs need updating
- Manifest paths need updating
- GitHub download URLs change

**Impact:** ⭐ Low - Better organization, but significant refactoring

---

## 🎯 Core Design Principles

These principles guide all implementation work:

### 1. Super-Minimal Initial config.scm
- Keep only: host-name, locale, timezone, bootloader, file-systems, users
- No desktop environment, SSH, or optional services in initial install
- Goal: Reliably install a bootable Guix system shell

### 2. Verify Before Reboot
- Check kernel and initrd exist in `/mnt/boot/`
- Verify GRUB EFI files exist
- Refuse to reboot if critical files missing

### 3. Pre-Set User Password
- After `guix system init` but before reboot
- Use `chroot` and `passwd` command
- Avoids storing secrets in version control

### 4. Hardware-Aware Defaults
- Framework-specific: include AMD GPU, NVMe, USB modules in initrd
- Include linux-firmware via nonguix for real-world hardware
- Set stable kernel arguments

---

## 📊 Implementation Phases

| Phase | Goal | Status |
|-------|------|--------|
| **Phase 1: Core Installer** | Reliable single-boot installation | ✅ Complete |
| **Phase 2: Dual-Boot Support** | Framework-dual installer working | ✅ Complete |
| **Phase 3: Recovery & Safety** | Recovery script and verification | ✅ Complete |
| **Phase 4: Documentation** | First-boot guides and customization | ✅ Complete |
| **Phase 5: Advanced Options** | LUKS, btrfs, profiles | ⏳ In Progress |
| **Phase 6: Raspberry Pi** | ARM support and image building | ❌ Not Started |

---

## 📝 Notes

- All critical safety features are implemented
- Focus is now on advanced user options and polish
- Recovery script handles most installation failures
- Framework 13 is primary target, other platforms secondary

For detailed implementation history, see:
- Git commit log
- INSTALLATION_KNOWLEDGE.md
- Individual platform README files
