# Guix Installer Implementation Checklist

This checklist tracks remaining work for the cloudzy-guix-install project. Completed items have been removed to keep this file focused on what's left to do.

For implementation history and completed features, see git commit history.

## 🔄 Currently Working On

**Status:** ✅ FIXED - Solution implemented and tested successfully on Framework 13

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

**🎯 ROOT CAUSE IDENTIFIED (2025-11-07):**

**Broken System Generation** - `guix system init` creates broken symlink at `/mnt/run/current-system`

**Diagnostics from Framework 13 testing:**
- ✅ No disk space issues (73GB available on /mnt)
- ✅ No errors in installation log
- ✅ Config.scm has correct nonguix imports and `(kernel linux)` specification
- ✅ Nonguix channel properly configured in `/tmp/channels.scm`
- ✅ GRUB installed successfully
- ❌ `/mnt/run/current-system` is a **broken symlink** (points to non-existent directory)
- ❌ No kernel package in `/gnu/store` (only test data found)
- ❌ No vmlinuz*/initrd* files anywhere

**What's happening:**
`guix time-machine` with nonguix channel runs `system init`, which:
1. Successfully installs GRUB bootloader
2. **Fails to resolve or build the `linux` kernel package from nonguix**
3. Creates a broken system generation symlink
4. **Exits with success status (no error reported)**

**Why it fails silently:**
- Guix doesn't treat missing/unresolvable kernel package as a fatal error
- System generation is created without a working kernel
- GRUB config references non-existent kernel files
- Installation appears to succeed but system is unbootable

**Previous hypotheses ruled out:**
- ~~cow-store not redirecting~~ - Not the issue (73GB free space on target)
- ~~Disk space exhaustion~~ - Not the issue (plenty of space)
- ~~Build hangs~~ - Build doesn't even start (package not found)
- ~~Substitute server down~~ - Channel is configured correctly

**✅ SOLUTION IMPLEMENTED (2025-11-08):**

Modified `lib/common.go:RunGuixSystemInit()` to use 3-step workaround:

1. **Build system first:** `guix time-machine -C /tmp/channels.scm -- system build /mnt/etc/config.scm`
   - Creates complete system in `/gnu/store/*-system` with kernel and initrd

2. **Manually copy kernel files:**
   - Copy `/gnu/store/*-system/kernel` → `/mnt/boot/vmlinuz`
   - Copy `/gnu/store/*-system/initrd` → `/mnt/boot/initrd`
   - Create symlink `/mnt/run/current-system` → `/gnu/store/*-system`

3. **Install bootloader:** `guix time-machine -C /tmp/channels.scm -- system init /mnt/etc/config.scm /mnt`
   - System already built, this just installs GRUB

**Testing:**
- ✅ Successfully tested on Framework 13 AMD
- ✅ System boots correctly with kernel/initrd in place
- ✅ All installers updated to use this approach
- ✅ Issue permanently resolved

**Files modified:**
- [lib/common.go:824-960](lib/common.go#L824-L960) - Updated `RunGuixSystemInit()` with 3-step workaround
- All platform installers inherit the fix automatically

**Next debugging steps (if issues persist):**

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

**ALTERNATIVE WORKAROUND (Not needed - kept for reference):**

~~Pre-build kernel on Mac with Docker~~ - We found a better solution (3-step build process), but this approach remains documented in [PREBUILD_KERNEL.md](PREBUILD_KERNEL.md) in case it's useful for other scenarios.

**Historical context**: Initially considered pre-building kernel on Mac to bypass suspected cow-store or disk space issues. After debugging, discovered the real issue was `guix system init` not copying kernel files to /boot. The 3-step approach (build → copy → init) permanently resolves this without needing Docker pre-builds.

---

## 🧪 Framework 13 First Boot Assessment (Before Customization)

**Date:** 2025-11-10
**Platform:** Framework 13 AMD (mid-2025)
**Guix Version:** 1.4.0 ISO
**Config:** Minimal dual-boot config from installer

### What Works Out-of-Box:

✅ **System boots successfully** - Kernel and initrd present in /boot
✅ **User login** - User account created, password set
✅ **Basic services running** - nscd, pam, udev, transient
✅ **Hardware detected**:
  - Wired ethernet (eth0)
  - WiFi card (wlp192s0 - MediaTek MT7922)
  - Battery (88% detected)
  - Sound devices (/dev/snd/ populated)

✅ **Manual networking works** - `dhclient` gets IP on wired connection
✅ **Guix functional** - `guix describe` works, on master branch

### What's Missing (Needs Customization):

❌ **No persistent networking** - No NetworkManager, only manual dhclient
❌ **No WiFi configured** - Can't connect to wireless
❌ **No basic CLI tools** - No git, vim, emacs, htop, curl, wget
❌ **No sudo installed** - Can't elevate privileges easily
❌ **No desktop environment** - No X, no $DISPLAY, command-line only
❌ **No Bluetooth stack** - bluetoothctl not found
❌ **Firmware warnings** - amdgpu errors in dmesg (need linux-firmware)
❌ **Pop!_OS not in GRUB** - Dual-boot chainloading not configured
❌ **No SSH server** - Can't remote in
❌ **No battery optimization** - TLP not installed
❌ **No automatic SSD TRIM** - fstrim.timer not enabled
❌ **Empty /etc/resolv.conf** - No DNS until dhclient runs

### Critical Gap Found:

**NetworkManager is NOT in the customize script!** The script adds:
- ✅ Framework hardware support (firmware)
- ✅ Desktop environments
- ✅ Common packages
- ❌ **Missing: NetworkManager service**

This needs to be added to the customize script as a high-priority option.

### Recommended First-Boot Improvements:

1. **Add NetworkManager to customize script** (option 0 or automatic)
2. **Add TLP for battery management** (Framework-specific)
3. **Add fstrim.timer for SSD health**
4. **Auto-detect and configure dual-boot GRUB**
5. **Pre-install curl/wget in minimal config** (for downloading customize tools)

### Current Post-Install Setup Approach (2025-11-10):

**Goal:** Get Framework 13 from minimal install to fully functional system

**Steps in progress:**

1. ✅ **Manual networking** - Used dhclient to get temporary IP
2. 🔄 **Channel setup** - Creating `~/.config/guix/channels.scm` without introductions for old Guix
3. 🔄 **First guix pull** - Upgrading to newer Guix (in progress)
4. ⏳ **Second guix pull with nonguix** - Will add nonguix channel after first pull
5. ⏳ **Edit /etc/config.scm** - Add NetworkManager service
6. ⏳ **Reconfigure system** - Apply NetworkManager changes
7. ⏳ **Reboot and test WiFi** - Use nmcli to connect wirelessly
8. ⏳ **Run customize script** - Add firmware, packages, desktop

**Challenges encountered:**
- Old Guix (1.4.0 from ISO) doesn't support channel introductions
- Solution: Two-step pull (default channels first, then add nonguix)
- NetworkManager missing from customize script (need to add manually first)

**Next session:**
- Complete Framework 13 setup
- Test end-to-end installation on fresh Framework
- Try cloudzy installer with all latest improvements

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
**Status:** ✅ Complete

Document where logs/receipts live and add a short chroot/repair/rerun guide when post-install verification fails.

**Completed:**
- ✅ Recovery script exists
- ✅ Installation receipt written
- ✅ Comprehensive troubleshooting guide ([TROUBLESHOOTING.md](TROUBLESHOOTING.md))
- ✅ Rescue/repair documentation with chroot procedures
- ✅ Common failure scenarios and solutions
- ✅ Log locations and diagnostic commands
- ✅ Quick reference for common tasks

**Impact:** ⭐⭐ Medium - Speeds recovery

---

### 🟢 Low Priority (Nice to Have)

#### 7. Color-Coded Step Distinction in Go Installers
**Status:** ✅ Complete - Tested successfully on Framework 13

**Completed:**
- ✅ Created `lib/colors.go` with color utilities
- ✅ Added `PrintStepHeader()` function with background colors
- ✅ Added `PrintSectionHeader()` with cycling colors for all subsections
- ✅ Added `PrintProgress()` for consistent cyan progress messages
- ✅ Added rainbow spinner animation (8 colors)
- ✅ Updated all installers (framework, framework-dual, cloudzy)
- ✅ Step color scheme:
  - Step 01: Light blue background
  - Step 02: Light green background
  - Step 03: Light yellow background
  - Step 04: Light cyan background
- ✅ Section headers cycle through: blue → green → yellow → cyan
- ✅ Spinner cycles through: red → orange → yellow → green → cyan → blue → purple → magenta
- ✅ Added utility functions: PrintSuccess, PrintWarning, PrintError, PrintInfo, PrintProgress
- ✅ Tested on Framework 13 Guix ISO - all colors display correctly

**Implementation:**
- [lib/colors.go](lib/colors.go) - ANSI color codes and helper functions
- All `*/install/0*-*.go` files updated to use colored output
- [lib/common.go](lib/common.go) - Progress monitoring uses colored messages
- Colors automatically reset after each message

**Impact:** ⭐ Low - Nice visual polish, significantly improves readability during long operations

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
