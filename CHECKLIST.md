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
1. ✅ **Framework 13 AMD GDM login fix**: Root cause identified (AMD GPU firmware issue) and Wingo-era channel pinning implemented → See "Framework-dual postinstall" section below
2. ✅ Cloudzy initrd fix: Removed explicit base-initrd specification (fixes "Invalid keyword" error) → See "Testing cloudzy installer" section below
3. ✅ VERBOSE=1 instructions added for verify script everywhere → Helps debug grub.cfg and file detection issues

**See [archive/CHECKLIST_COMPLETED.md](archive/CHECKLIST_COMPLETED.md) for full history.**

---

## 🔄 Currently Working On

**Two Active Fronts:**

### Front 1: Framework-dual (Testing & Development)
**Focus**: Real-world installation testing, GNOME configuration, troubleshooting

**Status**: See "Framework-dual postinstall (IN TESTING)" section below

### Front 2: Cloudzy (Guile Conversion & Testing)
**Focus**: Complete conversion to `.scm` scripts and comprehensive testing

**Guile Conversion Project (IN PROGRESS):**

See [docs/GUILE_CONVERSION.md](docs/GUILE_CONVERSION.md) for comprehensive plan.

- ✅ Phase 1: Library infrastructure complete → [See archive](archive/CHECKLIST_COMPLETED.md#guile-conversion-project---phase-1-2025-11-15)
- ✅ Phase 2: Update postinstall scripts to use Guile helper → [See archive](archive/CHECKLIST_COMPLETED.md#guile-conversion-project---phase-2-2025-11-15)
- ✅ Phase 3: All scripts converted (20 total) → See "Batch Conversion System" section below
- ⏳ **Phase 4: Cloudzy Deployment** (CURRENT FOCUS)
  - Review all converted scripts in `tools/converted-scripts/`
  - Test converted scripts (verify functionality matches bash versions)
  - Deploy converted scripts (replace `.sh` references with `.scm` in cloudzy codebase)
  - Remove original `.sh` files after successful deployment
  - **Goal**: Complete Guile conversion for cloudzy platform

- ✅ Batch Conversion Tools (COMPLETED) → [See archive](archive/CHECKLIST_COMPLETED.md#batch-conversion-tools-improvements-2025-11-15)

**Testing Strategy:**

- ✅ **Guile (.scm) scripts**: Fully tested in Docker + run-tests.sh → [See archive](archive/CHECKLIST_COMPLETED.md#testing-infrastructure-2025-11-15)
- ⏸️ **Shell (.sh) scripts**: Not actively testing, will migrate to Guile

**Framework-dual postinstall (IN TESTING):**

- ✅ All fixes complete → [See archive](archive/CHECKLIST_COMPLETED.md#framework-dual-postinstall-improvements-2025-11-15)
- ✅ Bootstrap script fixes → [See archive](archive/CHECKLIST_COMPLETED.md#recent-bootstrap--path-resolution-fixes-2025-11-15)
- ✅ GNOME launches successfully - display manager working
- ✅ **ROOT CAUSE IDENTIFIED**: GDM login loop is AMD GPU firmware issue, not authentication problem
  - TTY login works perfectly
  - GDM accepts password but drops back to login because GNOME session fails to start
  - `dmesg` shows: `Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2`
  - Issue: Current guix/nonguix master commits don't provide working AMD firmware for Framework 13 AMD
- ✅ **FIX IMPLEMENTED**: Wingo-era channel pinning
  - Created `wingolog-channels.scm` files for framework-dual and framework platforms
  - Pins guix and nonguix to known-good commits from 2024-02-16 (Wingo's blog post)
  - Guix commit: `91d80460296e2d5a01704d0f34fb966a45a165ae`
  - NonGuix commit: `10318ef7dd53c946bae9ed63f7e0e8bb8941b6b1`
  - Updated READMEs with usage instructions
  - Updated docs/GNOME_LOGIN_TROUBLESHOOTING.md with complete root cause analysis
- 🧪 **IN PROGRESS**: Testing Wingo channel pinning on Framework 13 AMD
  - Running: `sudo guix time-machine -C ~/wingolog-channels.scm -- system reconfigure /etc/config.scm`
  - Expected: amdgpu firmware loads correctly, GDM/GNOME login works
  - **Issue encountered**: D-Bus activation failure during system reconfigure
    - **Root cause**: `/var/run/dbus` was a symlink → `/run/dbus` instead of actual directory
    - **Error**: `file name component is not a directory "/var/run/dbus"` in `mkdir-p/perms`
    - **Explanation**: Guix activation script expects to create/manage `/var/run/dbus` as real directory
    - **Solution**: `sudo rm /var/run/dbus` then re-run reconfigure (Guix will create proper directory)
    - **Follow-up**: After successful reconfigure, restart D-Bus: `sudo herd restart dbus` or reboot
  - **Status**: D-Bus issue identified and solution in progress, awaiting reconfigure completion

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
| **Conversions** | ✅ Complete | All 20 scripts converted (7 lib scripts + 13 postinstall recipes) |
| **Review** | ⏸️ Not Started | Converted scripts not yet reviewed or tested |
| **Deployment** | ⏸️ Not Started | Converted scripts not yet integrated into main codebase |

**Conversion Status:**

**✅ Converted Scripts (20 total in `tools/converted-scripts/`):**

**Lib Scripts (7):**
1. `lib_bootstrap-installer.scm` (from `lib/bootstrap-installer.sh` - 267 lines)
2. `lib_channel-utils.scm` (from `lib/channel-utils.sh` - 235 lines)
3. `lib_clean-install.scm` (from `lib/clean-install.sh` - 134 lines)
4. `lib_postinstall.scm` (from `lib/postinstall.sh` - 31 lines)
5. `lib_recovery-complete-install.scm` (from `lib/recovery-complete-install.sh` - 458 lines)
6. `lib_verify-guix-install.scm` (from `lib/verify-guix-install.sh` - 305 lines)
7. `lib_verify-postinstall.scm` (from `lib/verify-postinstall.sh`)

**Postinstall Recipes (13):**
- `postinstall/recipes/add-development.scm`
- `postinstall/recipes/add-fonts.scm`
- `postinstall/recipes/add-spacemacs.scm`
- `postinstall/recipes/add-doom-emacs.scm`
- `postinstall/recipes/add-vanilla-emacs.scm`
- Plus test files and templates

**⚠️ Important:** All scripts are converted but **not yet reviewed, tested, or deployed**. Original `.sh` versions still in use.

**Next Steps (Cloudzy Focus):**
1. ⏳ **Review** converted scripts in `tools/converted-scripts/` for correctness
2. ⏳ **Test** converted scripts (run tests, verify functionality matches bash versions)
3. ⏳ **Deploy** converted scripts (replace `.sh` references with `.scm` in cloudzy codebase)
4. ⏳ **Remove** original `.sh` files after successful deployment
5. ⏳ **Comprehensive testing** of cloudzy installer with all `.scm` scripts

**Documentation:**
- **Getting Started**: [tools/README.md](tools/README.md) - Complete workflow and usage guide
- **Detailed Plan**: [tools/BATCH_CONVERSION_PLAN.md](tools/BATCH_CONVERSION_PLAN.md) - Roadmap and enhancement plan
- **Best Practices**: [docs/BATCH_CONVERSION_BEST_PRACTICES.md](docs/BATCH_CONVERSION_BEST_PRACTICES.md) - Pre-conversion preparation guide
- **Deployment Guide**: [tools/DEPLOYMENT_CHECKLIST.md](tools/DEPLOYMENT_CHECKLIST.md) - Steps to deploy converted scripts

**Why Parallel**: Can be developed independently while framework-dual testing proceeds. Low risk, high value for future script migrations.

**Cost**: ~$0.12 for 3 scripts (50% savings vs interactive conversion)

---

**Testing cloudzy installer with latest improvements:**

- ✅ **Initrd configuration fix (2025-01-XX)**: Removed explicit `base-initrd` specification for cloudzy
  - **Issue**: `base-initrd` doesn't accept `#:linux` and `#:linux-modules` keyword arguments that Guix passes when `(kernel linux-libre)` is specified
  - **Error**: `Invalid keyword: (#:linux ...)` during config validation
  - **Fix**: Omit initrd specification entirely for free software installations - Guix uses default initrd generation which automatically handles kernel and modules
  - **Documentation**: Updated `INSTALLATION_KNOWLEDGE.md` to clarify when to use explicit initrd vs defaults
  - **Status**: Fixed in `cloudzy/install/03-config.go`, ready for testing
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
- ✅ VERBOSE=1 instructions added everywhere verify script is mentioned (helps debug file detection issues)

**Oracle Cloud Free Tier Support (Future Work):**

- ⏳ **Goal**: Adapt cloudzy installer to work on Oracle Cloud Free Tier VPS instances
- **Why**: Oracle Cloud Free Tier offers ARM64 and x86_64 instances with generous free tier limits, expanding platform support
- **Top 5 Things Needed to Update Cloudzy Scripts:**

  1. **Device Detection Updates** (`cloudzy/install/01-partition.go`):
     - Oracle Cloud may use different device naming (e.g., `/dev/sda` vs `/dev/vda`)
     - May need to detect device type (NVMe, SCSI, VirtIO) and handle accordingly
     - Oracle Cloud Free Tier ARM64 instances might use different storage controllers

  2. **Boot Mode Detection** (`cloudzy/install/01-partition.go`, `lib/common.go`):
     - Oracle Cloud Free Tier typically uses UEFI, but detection might differ
     - May need to handle Oracle Cloud's specific EFI partition requirements
     - Verify EFI partition detection works correctly in Oracle Cloud environment

  3. **Network Configuration** (`cloudzy/install/03-config.go`, `postinstall/customize`):
     - Oracle Cloud uses different network interface naming (may be `ens3` instead of `eth0`)
     - May need Oracle Cloud-specific network service configuration
     - Consider Oracle Cloud's cloud-init integration (if applicable)

  4. **Console/Serial Access** (`lib/bootstrap-installer.sh`):
     - Oracle Cloud uses web-based console access (different from Cloudzy's VNC/KVM)
     - May need to handle serial console differently
     - Font selection and display might need adjustments for Oracle Cloud console

  5. **Storage and Partitioning** (`cloudzy/install/01-partition.go`):
     - Oracle Cloud Free Tier has specific storage limits and configurations
     - May need to handle Oracle Cloud's block volume attachments differently
     - Consider Oracle Cloud's boot volume vs block volume distinction
     - Verify partitioning works with Oracle Cloud's storage backend

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
