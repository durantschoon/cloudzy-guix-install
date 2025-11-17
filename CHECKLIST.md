# Guix Installer Implementation Checklist

This checklist tracks remaining work for the cloudzy-guix-install project.

## 📋 How to Update This Checklist

**When completing an item:**
1. Move the completed item to [archive/CHECKLIST_COMPLETED.md](archive/CHECKLIST_COMPLETED.md) (newest at top)
2. Remove it from the active checklist sections below
3. Update the "Latest Completed Items" section below with the 3 most recent completions
4. Keep the active checklist focused on **remaining work only**

**Format for archive:**
- Use date headers (YYYY-MM-DD) for grouping related completions
- List items with ✅ checkmarks
- Include context/notes when helpful
- Keep newest items at the top

**For implementation history and completed features, see:**
- [archive/CHECKLIST_COMPLETED.md](archive/CHECKLIST_COMPLETED.md) - All completed items (newest first)
- Git commit history
- [docs/INSTALLATION_KNOWLEDGE.md](docs/INSTALLATION_KNOWLEDGE.md) - Hard-won lessons and fixes
- [docs/TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) - Debugging guides
- Individual platform README files

---

## ✅ Latest Completed Items

**Most Recent:**
1. ✅ GNOME keyboard layout auto-configuration (gsettings/Wayland-compatible) → [See archive](archive/CHECKLIST_COMPLETED.md#gnome-keyboard-layout--password-fixes-2025-11-16)
2. ✅ Ctrl-C signal handling with recovery instructions → [See archive](archive/CHECKLIST_COMPLETED.md#gnome-keyboard-layout--password-fixes-2025-11-16)
3. ✅ Keyboard layout warning during password setup → [See archive](archive/CHECKLIST_COMPLETED.md#gnome-keyboard-layout--password-fixes-2025-11-16)

**See [archive/CHECKLIST_COMPLETED.md](archive/CHECKLIST_COMPLETED.md) for full history.**

---

## 🔄 Currently Working On

**Guile Conversion Project (IN PROGRESS):**

See [docs/GUILE_CONVERSION.md](docs/GUILE_CONVERSION.md) for comprehensive plan.

- ✅ Phase 1: Library infrastructure complete → [See archive](archive/CHECKLIST_COMPLETED.md#guile-conversion-project---phase-1-2025-11-15)
- ✅ Phase 2: Update postinstall scripts to use Guile helper → [See archive](archive/CHECKLIST_COMPLETED.md#guile-conversion-project---phase-2-2025-11-15)

- ⏳ Phase 3: Convert critical lib/*.sh scripts to Guile
  - **Before converting**: Add structured comments to originals (see [docs/BATCH_CONVERSION_BEST_PRACTICES.md](docs/BATCH_CONVERSION_BEST_PRACTICES.md))
  - Priority: lib/postinstall.sh (simplest, 31 lines)
  - Next: lib/clean-install.sh, lib/verify-guix-install.sh
  - Later: bootstrap-installer.sh, recovery-complete-install.sh
  - **Workflow**: See [tools/README.md](tools/README.md) for batch conversion process

- ✅ Batch Conversion Tools (COMPLETED) → [See archive](archive/CHECKLIST_COMPLETED.md#batch-conversion-tools-improvements-2025-11-15)

**Testing Strategy:**

- ✅ **Guile (.scm) scripts**: Fully tested in Docker + run-tests.sh → [See archive](archive/CHECKLIST_COMPLETED.md#testing-infrastructure-2025-11-15)
- ⏸️ **Shell (.sh) scripts**: Not actively testing, will migrate to Guile

**Framework-dual postinstall (IN TESTING):**

- ✅ All fixes complete → [See archive](archive/CHECKLIST_COMPLETED.md#framework-dual-postinstall-improvements-2025-11-15)
- ✅ Bootstrap script fixes → [See archive](archive/CHECKLIST_COMPLETED.md#recent-bootstrap--path-resolution-fixes-2025-11-15)
- ✅ GNOME launches successfully - display manager working (but login issues remain - see current issues below)
- ⚠️ **CURRENT ISSUES** (framework-dual testing):
  1. **Password login failure at GDM**: Password works at text console but fails at GNOME login screen
     - Password is lowercase letters only (no Caps Lock/Ctrl keys)
     - Password works via `su` at console, but GDM rejects it
     - Even after resetting password multiple times, still fails
     - New user created also has same issue
  2. **Keyboard layout mismatch**: Password set with `ctrl:swapcaps` but GDM uses default layout
     - ✅ **FIX IMPLEMENTED**: Warning added when setting password during installation → [See archive](archive/CHECKLIST_COMPLETED.md#gnome-keyboard-layout--password-fixes-2025-11-16)
     - ✅ **FIX IMPLEMENTED**: Automatic `gsettings` autostart script creation (Wayland-compatible) → [See archive](archive/CHECKLIST_COMPLETED.md#gnome-keyboard-layout--password-fixes-2025-11-16)
  3. **User shell path issue**: New users created with `/bin/bash` which doesn't exist in Guix
     - Should use `/run/current-system/profile/bin/bash`
     - Fixed manually via `chsh` or editing `/etc/passwd`
  4. **Next step**: Try Xfce instead of GNOME to isolate if issue is GNOME/GDM-specific
- 🧪 **NEXT**: Test Xfce login, verify if issue is GNOME-specific or affects all desktop environments

**Bootstrap Command for Testing:**

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-postinstall.scm | guile
cd ~/guix-customize
./customize
# Select option 2 (Add desktop), then option 1 (GNOME)
```

**What's Ready:**

- GNOME installation uses Guile S-expression parser (no more sed!)
- NetworkManager, SSH, and desktop services all use guile_add_service()
- Full checksum verification via SOURCE_MANIFEST.txt
- Platform auto-detection (framework-dual)
- All Guile tests passing in Docker
- Bootstrap script fixed (syntax errors, path resolution, Go detection)
- Hash-to-words conversion requires Go (fatal error if missing)
- Customize scripts properly resolve paths (symlink support, INSTALL_ROOT)
- postinstall/lib.sh functions correctly use INSTALL_ROOT
- Batch conversion tools ready for production use

*(For detailed completion history, see [archive/CHECKLIST_COMPLETED.md](archive/CHECKLIST_COMPLETED.md))*

**Note:** Framework-dual postinstall testing should focus on GNOME configuration workflow. See [docs/POSTINSTALL_DEV.md](docs/POSTINSTALL_DEV.md) for testing and development instructions.

---

## 🔀 Parallel Projects

### Batch Conversion System

**Goal**: Automated bash-to-Guile conversion using Anthropic Batch API with comprehensive validation.

**Status Summary:**

| Component | Status | Details |
|-----------|--------|---------|
| **Tools** | ✅ Complete | All batch conversion tools built and tested |
| **Conversions** | ⏳ In Progress | 20 scripts converted, 7 critical scripts remain |
| **Deployment** | ⏸️ Not Started | Converted scripts not yet integrated into main codebase |

**Conversion Status:**

**✅ Converted (20 scripts in `tools/converted-scripts/`):**
- Postinstall recipes (development, fonts, spacemacs, doom-emacs, vanilla-emacs)
- Some lib scripts (bootstrap-installer, clean-install, postinstall, recovery-complete-install, verify-guix-install, verify-postinstall, channel-utils)
- Note: These are converted but not yet deployed/reviewed

**⏳ Remaining Critical Scripts (7 in `lib/`):**
1. `lib/bootstrap-installer.sh` (267 lines) - Bootstrap logic
2. `lib/channel-utils.sh` (235 lines) - Channel/mirror selection
3. `lib/clean-install.sh` (134 lines) - Installation wrapper
4. `lib/postinstall.sh` (31 lines) - Channel utilities (priority: simplest)
5. `lib/recovery-complete-install.sh` (458 lines) - Recovery flows
6. `lib/verify-guix-install.sh` (305 lines) - Verification checks
7. `lib/verify-postinstall.sh` - Post-install verification

**Next Steps:**
1. ⏳ Review and test converted scripts in `tools/converted-scripts/`
2. ⏳ Convert remaining 7 critical scripts (priority: `lib/postinstall.sh`)
3. ⏳ Deploy converted scripts (replace `.sh` references with `.scm`)
4. ⏳ Add structured comments to remaining scripts before conversion

**Documentation:**
- **Getting Started**: [tools/README.md](tools/README.md) - Complete workflow and usage guide
- **Detailed Plan**: [tools/BATCH_CONVERSION_PLAN.md](tools/BATCH_CONVERSION_PLAN.md) - Roadmap and enhancement plan
- **Best Practices**: [docs/BATCH_CONVERSION_BEST_PRACTICES.md](docs/BATCH_CONVERSION_BEST_PRACTICES.md) - Pre-conversion preparation guide
- **Deployment Guide**: [tools/DEPLOYMENT_CHECKLIST.md](tools/DEPLOYMENT_CHECKLIST.md) - Steps to deploy converted scripts

**Why Parallel**: Can be developed independently while framework-dual testing proceeds. Low risk, high value for future script migrations.

**Cost**: ~$0.12 for 3 scripts (50% savings vs interactive conversion)

---

**Testing cloudzy installer with latest improvements:**

- ✅ 3-step kernel/initrd fix applied and tested
- ✅ Color-coded output with cycling headers
- ✅ Enhanced manifest verification with Quick checksum view
- ✅ Improved swap creation error messages
- ✅ Daemon startup timeout increased to 2 minutes
- ✅ Graceful validation skip if daemon not responsive
- ✅ Robust daemon startup: functional approach that ensures daemon is actually ready (restarts until responsive, not just retries)
- ✅ Post-install steps made resilient: password setting always attempted, verification non-fatal
- ✅ Better error handling: clear messages when post-install steps incomplete, suggests recovery script
- ✅ Comprehensive verification at end: runs full verify-guix-install.sh script, ensures EFI mounted, prevents reboot if verification fails
- ✅ Framework-dual kernel fixes applied to cloudzy: checks broken symlink, automatic fallback copy of kernel/initrd if missing
- ✅ Verification after guix system init: checks for kernel/initrd files and broken symlink, retries with manual copy if needed

**Framework 13 Post-Install Process (2025-11-10):**

Learned the complete workflow for getting Framework 13 fully operational after minimal install:

1. **First Boot State:**
   - Wired ethernet works (dhclient running)
   - WiFi/Bluetooth NOT working (missing firmware)
   - No NetworkManager (can't easily switch to WiFi)
   - Guix 1.4.0 from ISO (old, doesn't support channel introductions)

2. **Post-Install Steps Required:**

   ```bash
   # Step 1: First guix pull (upgrade Guix to support channel introductions)
   guix pull
   # Takes 10-30 min, upgrades to latest Guix from master

   # Step 2: Create channels.scm with nonguix
   mkdir -p ~/.config/guix
   cat > ~/.config/guix/channels.scm <<'EOF'
   (cons* (channel
           (name 'nonguix)
           (url "https://gitlab.com/nonguix/nonguix")
           (branch "master")
           (introduction
            (make-channel-introduction
             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
             (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
          %default-channels)
   EOF

   # Step 3: Second guix pull (add nonguix channel)
   guix pull
   # Takes 10-30 min, fetches nonguix

   # Step 4: Fix PATH to use pulled Guix
   export PATH="$HOME/.config/guix/current/bin:$PATH"
   # Add to ~/.bashrc for persistence

   # Step 5: Verify nonguix is available
   guix describe  # Should show both guix and nonguix
   guix show linux  # Should find non-free kernel
   guix show linux-firmware  # Should find proprietary firmware

   # Step 6: Add NetworkManager to /etc/config.scm
   sudo nano /etc/config.scm
   # Add (service network-manager-service-type) to services

   # Step 7: Reconfigure system
   sudo guix system reconfigure /etc/config.scm
   # Takes 5-15 min, installs NetworkManager

   # Step 8: Connect to WiFi
   nmcli device wifi list
   nmcli device wifi connect "SSID" --ask

   # Step 9: Run customize script
   ~/guix-customize/customize
   # Add desktop, packages, etc.
   ```

3. **Common Pitfalls:**
   - **PATH issue:** `guix describe` shows old Guix if PATH not updated
   - **Generation mismatch:** Pulled Guix is generation 2, system uses generation 1
   - **Channel introduction required:** Old Guix 1.4.0 can't authenticate nonguix without upgrade
   - **Two-step pull required:** Can't add nonguix until after first pull upgrades Guix

4. **Automation Opportunities:**
   - Post-install script could automate the two-pull process
   - Could pre-populate ~/.bashrc with correct PATH
   - Could check for and fix PATH issues automatically
   - Customize script should detect missing NetworkManager and offer to add it

---

## 📋 Remaining Work

### 🟡 Medium Priority

#### 1. Add NetworkManager to Framework Customize Script

**Status:** ❌ Missing from customize script

**Current gap:** Framework 13 first boot has no persistent networking. User must manually add NetworkManager service to config.scm before running customize script.

**Proposed solution:**

- Add NetworkManager as high-priority option (option 0 or automatic)
- Include in Framework-specific hardware setup
- Document in first-boot instructions

**Impact:** ⭐⭐⭐ High - Critical for laptop usability

---

#### 2. Dual-Boot GRUB UX Improvements

**Status:** ❌ Not implemented

Ensure readable GRUB theme and visible timeout; add explicit chainloader entry for Pop!_OS in EFI if auto-detection fails.

**Current state:**

- ✅ Timeout set to 5 seconds
- ✅ os-prober configured in recovery script
- ❌ Need to test chainloader detection
- ❌ GRUB theme not customized

**Impact:** ⭐⭐ Medium - Smoother dual-boot selection

---

#### 3. Bootloader Timeout Configuration

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

#### 4. Storage Options Documentation

**Status:** ❌ Not documented

Provide documented flows for:

- LUKS + ext4 root
- btrfs with subvolumes and periodic scrub hooks
- Flag to reserve N GiB unallocated and/or create separate `/home`

**Impact:** ⭐⭐ Medium - Security/flexibility options for advanced users

---

#### 5. Safer Retries and Diagnostics

**Status:** ❌ Not implemented

Toggle verbose vs quiet logging; capture `guix describe` and `guix weather` summaries into the log and receipt.

**Impact:** ⭐⭐ Medium - Easier troubleshooting

---

#### 6. Post-Install Customization Profiles

**Status:** ❌ Not implemented

Split `/etc/config.scm` into base OS vs hardware profile; provide a "first reconfigure" profile that adds firmware, NetworkManager, SSH, time sync, and trim in one step.

**Impact:** ⭐⭐ Medium - Faster, cleaner onboarding

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

**Status:** ✅ Complete (v1.1.0)

**Completed:**

- ✅ Moved critical scripts to `lib/` subdirectory:
  - `lib/verify-guix-install.sh`
  - `lib/recovery-complete-install.sh`
  - `lib/bootstrap-installer.sh`
  - `lib/postinstall.sh` (already in lib/)
- ✅ Kept development/repo scripts at top level:
  - `update-manifest.sh`
  - `run-tests.sh`
- ✅ Updated bootstrap script internal paths
- ✅ Updated SOURCE_MANIFEST.txt with new paths
- ✅ Updated documentation references:
  - README.md
  - QUICKSTART.md
  - docs/INSTALLATION_KNOWLEDGE.md
  - postinstall/CHANNEL_MANAGEMENT.md
- ✅ All tests pass after reorganization

**Breaking changes (v1.1.0):**

- GitHub download URLs changed to use `lib/bootstrap-installer.sh`
- Users should update their bookmarks/documentation

**Benefits achieved:**

- Clear separation between Guix runtime scripts and development scripts
- Consistent with `lib/common.go` pattern
- Easier to understand repository structure

**Impact:** ⭐ Low - Better organization with minimal disruption

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
- docs/INSTALLATION_KNOWLEDGE.md
- Individual platform README files
