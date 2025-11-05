# Guix Installer Implementation Checklist

This checklist tracks remaining work for the cloudzy-guix-install project. Completed items have been removed to keep this file focused on what's left to do.

For implementation history and completed features, see git commit history.

## 🔄 Currently Working On

**Status:** ⚠️ CRITICAL BUG - Framework13 Nonguix Prompt Missing

**Most Recent Additions (2025-11):**
- ✅ **Recovery script** - Automatic recovery script generation for all installers
- ✅ **Installation verification** - Post-install checks for kernel/initrd/GRUB
- ✅ **User password setup** - Pre-reboot password setting via chroot
- ✅ **Nonguix integration** - Time-machine based nonguix channel support
- ✅ **Boot mode detection fix** - Cloudzy VPS now correctly detects BIOS vs UEFI
- ✅ **Initrd fix** - Added `(initrd microcode-initrd)` to framework configs
- ✅ **Clean install script** - Removes all artifacts for fresh reinstall
- ✅ **Bash shebang documentation** - Fixed contradictory documentation

**CRITICAL ISSUE TO INVESTIGATE:**

### Framework13 Nonguix Prompt Not Appearing

**Problem:** After running clean-install.sh and downloading latest bootstrap-installer.sh, the framework installer never prompts the user about trusting nonguix. The generated config.scm is missing nonguix imports.

**Expected behavior:**
- User should see nonguix trust prompt during Step 3 (config generation)
- Config should include `(nongnu packages linux)` and `(nongnu system linux-initrd)`
- Config should have `(initrd microcode-initrd)` field

**Actual behavior:**
- No nonguix prompt appears
- Config missing nonguix use-modules
- Generates "free software only" config

**Investigation needed:**
- Why is `lib.SetupNonguixChannel()` not prompting?
- Is /tmp/channels.scm somehow persisting?
- Is the function being called at all?
- Check if stdin/tty redirection is suppressing the prompt

**Current Focus:**
1. ⚠️ DEBUG: Why nonguix prompt not appearing on framework13
2. Test Cloudzy installer on fresh VPS with BIOS boot fix
3. Test and validate complete framework-dual installation flow
4. Improve dual-boot GRUB experience
5. Document storage options (LUKS/btrfs)

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

#### 7. Label Verification Output
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
