# Comprehensive Best Practices Checklist Status

Analysis of what we have vs. comprehensive best practices list.

## 🧱 Partitioning & Filesystem Layout

| Practice | Status | Notes |
|----------|--------|-------|
| Use GPT instead of MBR | ✅ Done | All scripts use `mklabel gpt` |
| Create explicit ESP (FAT32, 512MB-1GB, flagged boot,esp) | ✅ Done | 512MB ESP, properly flagged |
| Use labels (UPPERCASE) or UUIDs, never bare /dev/sdXN | ✅ Done | Just implemented! Labels: EFI, GUIX_ROOT |
| Keep root FS simple (ext4) | ✅ Done | All use ext4 for root |
| Mount /home or /data separately | ⚠️ Partial | Framework-dual detects HOME partition, others don't |
| Avoid overlap with other OS partitions | ✅ Done | Framework-dual keeps Pop!_OS untouched |
| Reserve unallocated space for flexibility | ❌ Missing | We use 100% of available space |

**Recommendations:**
- Consider option to leave 10-20GB unallocated (low priority)
- Document /home partition strategy better

---

## ⚙️ Mounting & Environment Prep

| Practice | Status | Notes |
|----------|--------|-------|
| Always mount by label or UUID | ✅ Done | Just implemented mount-by-label |
| Check mounts before install (df -h, df -T) | ❌ Missing | No mount verification before init |
| Ensure ESP is empty and cleanly formatted | ❌ Missing | No pre-check for ESP contents |
| Enable swap early | ⚠️ Partial | Cloudzy: no swap. Framework: needs swap partition. Only swapfile in step 4 |
| Never bind-mount /gnu or /var/guix | ✅ Done | Removed, now use cow-store |
| Use herd start cow-store /mnt | ✅ Done | Just added! |
| Add sanity checks for missing mounts/labels | ❌ Missing | No verification that labels exist |

**Critical gaps:**
1. **No ESP verification** - Should check `df -T /mnt/boot/efi | grep vfat`
2. **No label existence check** - Should verify `/dev/disk/by-label/EFI` exists before mount
3. **No swap partition support** - Only swapfile (works but not ideal)

---

## 🧰 Bootloader & EFI

| Practice | Status | Notes |
|----------|--------|-------|
| Verify ESP before bootloader install | ❌ Missing | No vfat/flag verification |
| Use uppercase FAT32 labels | ✅ Done | Label: EFI (uppercase) |
| Keep GRUB and systemd-boot peacefully | ✅ Done | Framework-dual handles both |
| Confirm post-install files exist | ❌ Missing | No verification of vmlinuz/initrd/grubx64.efi |
| Add (timeout 5) to bootloader-configuration | ✅ Done | Just added! |
| Optional: grub-mkconfig for OS detection | ❌ Not implemented | Could add os-prober |

**Critical gap:**
- **Post-install file verification** - Should check boot files exist before unmounting

---

## 🧩 Guix-Specific Best Practices

| Practice | Status | Notes |
|----------|--------|-------|
| Use substitute servers | ✅ Done | nonguix.org, ci.guix.gnu.org, bordeaux |
| Include (kernel linux) in config.scm | ❌ Missing | Config uses base packages, no explicit kernel |
| Valid (bootloader …) stanza | ✅ Done | All configs have proper bootloader |
| Start with minimal config | ✅ Done | Minimal base config, customize after |
| Don't reboot after failed init | ✅ Documented | Retry logic in place |
| Add --fallback flag | ✅ Done | All use --fallback |
| Use guix system reconfigure after boot | ✅ Documented | Mentioned in README/scripts |

**Minor gap:**
- Could explicitly add `(kernel linux-libre)` to config (though base includes it)

---

## 🧩 Automation & Script Hygiene

| Practice | Status | Notes |
|----------|--------|-------|
| Pre-label partitions and verify | ⚠️ Partial | We label, but don't verify before use |
| Log everything (tee install.log) | ❌ Missing | No logging to file |
| Exit on error (set -euo pipefail) | ⚠️ Partial | Go scripts handle errors, but no bash equivalent |
| Check disk space before install | ⚠️ Partial | Framework-dual checks free space, others don't |
| Sync and unmount cleanly | ✅ Done | All scripts sync + unmount -R /mnt |

**Improvements needed:**
1. Add label verification before mounting
2. Add logging option (tee to /tmp/guix-install.log)
3. Verify free space on /mnt and /tmp before starting

---

## 🧠 System Reliability After Install

| Practice | Status | Notes |
|----------|--------|-------|
| Keep /etc/config.scm in version control | ✅ Documented | Mentioned in CUSTOMIZATION.md |
| Record exact commit of Guix (guix describe) | ❌ Missing | No post-install logging |
| Set up guix pull && upgrade job | ❌ Missing | Not automated |
| Update bootloaders periodically in dual-boot | ❌ Missing | Manual process |

**Low priority** - These are post-install user responsibilities, not installer tasks

---

## 💡 Meta Best Practices

| Practice | Status | Notes |
|----------|--------|-------|
| Label everything consistently | ✅ Done | UPPERCASE labels: EFI, GUIX_ROOT |
| Name consistently across systems | ✅ Done | Same labels on all platforms |
| Comment generously | ✅ Done | Scripts have detailed comments |
| Test for presence, not absence | ⚠️ Partial | Some checks, not comprehensive |
| Treat install as idempotent | ✅ Done | Safe to re-run steps |
| Document the why | ✅ Done | CLAUDE.md, INSTALLATION_KNOWLEDGE.md |

---

## 📊 Summary: Critical Gaps to Address

### High Priority (Prevents Install Failures)

1. **ESP Verification** ⭐⭐⭐
   ```bash
   df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: ESP not FAT32"; exit 1; }
   ```

2. **Label Existence Check** ⭐⭐⭐
   ```bash
   [ -e /dev/disk/by-label/EFI ] || { echo "ERROR: No EFI label"; exit 1; }
   [ -e /dev/disk/by-label/GUIX_ROOT ] || { echo "ERROR: No GUIX_ROOT label"; exit 1; }
   ```

3. **Post-Install File Verification** ⭐⭐⭐
   ```bash
   ls /mnt/boot/vmlinuz-* || echo "WARNING: No kernel installed"
   ls /mnt/boot/efi/EFI/guix/grubx64.efi || echo "WARNING: No GRUB EFI"
   ```

### Medium Priority (Improves Reliability)

4. **Free Space Check** ⭐⭐
   - Add to cloudzy and framework (already in framework-dual)
   - Check both /mnt and /tmp

5. **Installation Logging** ⭐⭐
   ```bash
   exec > >(tee /tmp/guix-install.log) 2>&1
   ```

6. **Swap Partition Support** ⭐
   - Currently only swapfile
   - Could detect and use existing swap partition

### Low Priority (Nice to Have)

7. **Reserved Space Option** ⭐
   - Allow leaving 10-20GB unallocated
   - User configurable via env var

8. **Post-Install Logging**
   - Save guix describe output
   - Create install receipt with timestamps

---

## ✅ What We've Already Implemented (Recent Wins!)

1. ✅ **cow-store usage** - Prevents ISO space issues
2. ✅ **Uppercase labels** - EFI, GUIX_ROOT
3. ✅ **Mount by label** - Platform independent
4. ✅ **file-system-label in config** - Stable boot config
5. ✅ **Bootloader timeout** - 5-second GRUB menu
6. ✅ **Retry logic** - 3 attempts with fallback
7. ✅ **Multiple substitute servers** - Redundancy

---

## 🎯 Recommended Next Steps

**Phase 1: Critical Safety Checks** (30 min)
1. Add ESP vfat verification before init
2. Add label existence checks before mount
3. Add post-install file verification

**Phase 2: Better Error Prevention** (1 hour)
4. Add free space checks to all installers
5. Add installation logging option
6. Improve error messages with actionable steps

**Phase 3: Nice-to-Haves** (optional)
7. Swap partition detection/support
8. Reserved space option
9. Post-install receipt generation

---

## 📈 Current Implementation Score

- **Partitioning:** 6/7 (86%)
- **Mounting:** 3/7 (43%) ⚠️ Needs verification checks
- **Bootloader:** 4/6 (67%) ⚠️ Needs post-install checks
- **Guix-Specific:** 6/7 (86%)
- **Automation:** 3/5 (60%) ⚠️ Needs logging and checks
- **Meta Practices:** 6/6 (100%) ✅

**Overall:** ~70% complete

**Biggest impact improvements:**
1. ESP verification (prevents most common error)
2. Post-install checks (prevents boot failures)
3. Label verification (prevents mount errors)
