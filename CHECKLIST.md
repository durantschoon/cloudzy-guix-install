# Guix Installer Implementation Checklist

This checklist tracks remaining work for the cloudzy-guix-install project.

For implementation history and completed features, see:

- Git commit history
- [docs/INSTALLATION_KNOWLEDGE.md](docs/INSTALLATION_KNOWLEDGE.md) - Hard-won lessons and fixes
- [docs/TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) - Debugging guides
- Individual platform README files

---

## 🔄 Currently Working On

**Pre-Installation UX Improvements (COMPLETED):**

- ✅ Added USEBIGFONT environment variable for larger console fonts during installation
  - Supports boolean values ("1", "yes", "true", "t") → defaults to solar24x32
  - Supports custom font names (e.g., "ter-v32b", "solar24x32")
  - Falls back to solar24x32 if specified font not found
  - Sets font before installer runs for better readability on high-DPI displays
  - Updated bootstrap-installer.sh, README.md, and QUICKSTART.md
- ✅ KEYBOARD_LAYOUT environment variable already supported for Caps Lock ↔ Ctrl swap
  - Applied at first login via generated config.scm
  - Can be set before bootstrap to avoid prompts

**Guile Conversion Project (IN PROGRESS):**

See [docs/GUILE_CONVERSION.md](docs/GUILE_CONVERSION.md) for comprehensive plan.

- ✅ Phase 1: Library infrastructure complete
  - Created lib/guile-config-helper.scm for S-expression manipulation
  - Integrated Guile testing in Docker and run-tests.sh
  - All tests passing

- ✅ Phase 2: Update postinstall scripts to use Guile helper
  - ✅ Updated framework-dual/postinstall/customize to use Guile helper
  - ✅ Fixed GNOME configuration sed permission errors
  - ✅ Replaced fragile sed patterns with proper S-expression parsing
  - ✅ All service additions now use guile_add_service()
  - ✅ Consolidated shared postinstall code into postinstall/lib.sh
    - Created shared library with common functions (add_ssh, add_desktop, add_packages, etc.)
    - Updated all platform customize scripts (cloudzy, framework, framework-dual) to source shared lib
    - Moved test infrastructure to postinstall/tests/
    - All platforms now benefit from improved error handling and guile_add_service()
  - ⏳ Ready for real-world testing on framework-dual

- ⏳ Phase 3: Convert critical lib/*.sh scripts to Guile
  - Priority: lib/postinstall.sh (simplest, 31 lines)
  - Next: lib/clean-install.sh, lib/verify-guix-install.sh
  - Later: bootstrap-installer.sh, recovery-complete-install.sh

- ✅ Batch Conversion Tools (COMPLETED):
  - ✅ Fixed generate-batch-conversion.sh to use temp files (avoids command-line arg limits)
  - ✅ Fixed submit-batch.sh to convert JSONL to proper API format ({"requests": [...]})
  - ✅ Added JSONL validation before submission
  - ✅ Successfully submitted first batch: msgbatch_01UhbzJfuzqGwymdd9i8BKMD
  - ✅ Fixed check-batch-status.sh to handle missing 'total' field and stdin consumption
  - ✅ Fixed retrieve-batch.sh to handle missing fields and error responses gracefully
  - ✅ Batch processing completed successfully (5/5 requests succeeded)
  - ✅ Created generate-customize-batch.sh for converting customize scripts (4 scripts)
  - ✅ Fixed all tool scripts to work from repo root and find .env there
  - ✅ Fixed view-jsonl.sh to find files in tools directory when run from root
  - ✅ Fixed submit-batch.sh Python script (variable expansion, stderr handling)
  - ✅ Fixed custom_id format to match API requirements (^[a-zA-Z0-9_-]{1,64}$)
  - ⏳ Ready for review: Results available at tools/batch-results.jsonl

**Testing Strategy:**

- ✅ **Guile (.scm) scripts**: Fully tested in Docker + run-tests.sh
- ⏸️ **Shell (.sh) scripts**: Not actively testing, will migrate to Guile
- 🎯 **Focus**: Guile-based postinstall workflow from GNOME config onward
- 📝 **Developer docs**: See docs/POSTINSTALL_DEV.md for workflow

**Framework-dual postinstall (READY FOR TESTING):**

- ✅ Fixed sed permission errors (chown USER:USER → chown USER)
- ✅ Created guile_add_service() helper using lib/guile-config-helper.scm
- ✅ Updated add_networkmanager(), add_ssh(), add_desktop()
- ✅ Added to SOURCE_MANIFEST.txt for integrity verification
- ✅ Created docs/POSTINSTALL_DEV.md with developer workflow
- ✅ Created lib/bootstrap-postinstall.scm (pure Guile bootstrap)
- ✅ Enhanced docs/GUILE_KNOWLEDGE.md with community best practices
- ✅ Fixed bootstrap-postinstall.scm syntax errors and path resolution
- ✅ Fixed Go detection in bootstrap script (uses bash to run 'command -v go')
- ✅ Made hash-to-words conversion failure fatal (requires Go)
- ✅ Fixed customize script path resolution for symlinks
- ✅ Fixed postinstall/lib.sh to use INSTALL_ROOT correctly (exported from customize scripts)
- ✅ Fixed guile-config-helper.scm path resolution in guile_add_service()
- 🧪 **NEXT**: Test full GNOME installation workflow on real hardware

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

**Note:** Framework-dual postinstall testing should focus on GNOME configuration workflow. See [docs/POSTINSTALL_DEV.md](docs/POSTINSTALL_DEV.md) for testing and development instructions.

---

## 🔀 Parallel Projects

### Batch Conversion System (Phase 2 - COMPLETED)

**Goal**: Automated bash-to-Guile conversion using Anthropic Batch API with comprehensive validation.

**Current Status**: All tooling complete ✅ - Ready for production use

**Plan**: See [tools/BATCH_CONVERSION_PLAN.md](tools/BATCH_CONVERSION_PLAN.md) for detailed roadmap.

**Quick Summary:**

- ✅ Phase 1: Core batch tools complete (generate, submit, check, retrieve)
- ✅ Enhanced features complete:
  - ✅ Test generation (automatic test-*.scm files)
  - ✅ Comment structure preservation (structured vs unstructured handling)
  - ✅ All tool scripts work from repo root directory
  - ✅ .env file support (checks repo root first, then tools/)
  - ✅ Fixed submit-batch.sh Python script (variable expansion, stderr handling)
  - ✅ Fixed custom_id format to match API requirements (^[a-zA-Z0-9_-]{1,64}$)
  - ✅ Fixed view-jsonl.sh to find files in tools directory from root
  - ✅ Created generate-customize-batch.sh for converting customize scripts
  - ✅ Test extraction and integration into test runner
  - ✅ .env file support for API key
  - ✅ Verification script (verify-setup.sh)
  - ✅ Magit integration for diff viewing
  - ✅ Validation script (validate-comment-structure.sh)
- ⏳ Phase 2: Add validation (syntax checks, diff-based comparison, test account)
- 📅 Phase 3: Execute first batch conversion (3 recipe scripts)
- 📋 Future: Generalize system for any language pair (see [tools/GENERALIZATION_PLAN.md](tools/GENERALIZATION_PLAN.md))
  - System is ~85% ready for generalization
  - ~15 hardcoded values need parameterization (2-3 hours work)
  - Architecture is well-separated and clean

**Why Parallel**: Can be developed independently while framework-dual testing proceeds. Low risk, high value for future script migrations.

**Timeline**: 2-3 weeks to complete enhancements, then ready for production use.

**Cost**: ~$0.12 for 3 scripts (50% savings vs interactive conversion)

---

**Testing cloudzy installer with latest improvements:**

- ✅ 3-step kernel/initrd fix applied and tested
- ✅ Color-coded output with cycling headers
- ✅ Enhanced manifest verification with Quick checksum view
- ✅ Improved swap creation error messages
- ✅ Daemon startup timeout increased to 2 minutes
- ✅ Graceful validation skip if daemon not responsive

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
