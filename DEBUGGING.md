# Debugging Session: Framework 13 Dual-Boot Installation Issues

**Date:** 2025-10-18
**Platform:** Framework 13 AMD with Pop!_OS dual-boot
**Issue:** Guix OS installation hangs during boot, user account not created

---

## Summary of Issues Found

1. **Missing kernel and initrd files** - `/mnt/boot/vmlinuz-*` and `/mnt/boot/initrd-*` do not exist
2. **Guix system init did not complete successfully** - Installation appears incomplete
3. **No user account created** - Only `guest` user exists in installed system
4. **Boot hangs** - System hangs at "checking free cluster summary" on `/dev/nvme0n1p1` (EFI partition)
5. **Dirty bit repeatedly set** - Filesystem never cleanly unmounts because boot never completes

---

## Debugging Steps Performed

### 1. Filesystem Checks (Initially Suspected Root Cause)

**Problem:** "Dirty bit is set" error on boot

**Actions taken:**
```bash
# From Guix ISO
fsck.ext4 -y -f /dev/nvme0n1p4        # Guix root partition - passed
tune2fs -C 0 /dev/nvme0n1p4           # Reset mount count
tune2fs -l /dev/nvme0n1p4 | grep state  # Verified "clean"
badblocks -sv /dev/nvme0n1p4          # No bad blocks found

# EFI partition check
fsck.vfat -a -y /dev/nvme0n1p1        # Fixed and verified clean
fsck.vfat -n /dev/nvme0n1p1           # No errors reported
```

**Result:** Filesystems are healthy. Dirty bit returns after each boot attempt because system crashes before clean unmount.

---

### 2. Kernel Parameter Investigation

**Problem:** Suspected aggressive kernel parameters preventing boot

**Initial config had:**
```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "acpi=off" "noapic" "nolapic"))
```

**Actions taken:**
- Checked `/mnt/etc/config.scm` - found only `"quiet"` and `"loglevel=3"` (not the aggressive params)
- Checked `/mnt/boot/grub/grub.cfg` - confirmed only minimal params
- **Fixed install scripts** to remove aggressive parameters from future installs:
  - [framework-dual/install/03-config-dual-boot.go](framework-dual/install/03-config-dual-boot.go#L202)
  - [framework/install/03-config.go](framework/install/03-config.go#L185)

**Result:** Kernel parameters are acceptable. Not the root cause.

---

### 3. Boot Process Analysis

**Observed behavior:**
1. GRUB loads successfully
2. Boot starts, shows: "dirty bit is set, fs not properly unmounted"
3. Shows: "automatically removing dirty bit, checking free cluster summary"
4. Message: `/dev/nvme0n1p1 21 files ...`
5. **System hangs indefinitely** - Ctrl+C has no effect (hard hang)

**Testing:**
- Waited 5+ minutes - no progress
- Ctrl+C / Ctrl+Alt+F2 - no response
- Only Ctrl+Alt+Del (reboot) works

**Analysis:** Hard hang during kernel/driver initialization, not a user-space hang.

---

### 4. User Account Investigation

**Problem:** Cannot log in - user does not exist

**Actions taken:**
```bash
# From booted system (actually still in ISO - see below)
cat /etc/passwd | grep "/home"  # Only found "guest" user
ls /home/                       # Empty except guest
```

**Initial confusion:** Thought we were in installed system, but `/` was mounted as `none` (overlay filesystem)

**Result:** Never successfully booted into installed Guix system - always in ISO.

---

### 5. Critical Discovery: Missing Boot Files

**Problem:** Installation incomplete

**Actions taken:**
```bash
# From Pop!_OS
sudo mount /dev/nvme0n1p4 /mnt
sudo mount /dev/nvme0n1p1 /mnt/boot/efi

sudo ls -la /mnt/boot/
# Result: Only 'efi' and 'grub' directories - NO vmlinuz* or initrd* files

sudo ls -la /mnt/boot/initrd* /mnt/boot/vmlinuz*
# Result: Files not found

# Check installation completeness
sudo ls /mnt/gnu/store/ | wc -l
# Result: 2137 packages (some packages were installed)

# Check GRUB
sudo ls -lh /mnt/boot/grub/grub.cfg
# Result: 1.3K (exists but has no kernel to boot)

sudo du -sh /mnt/boot/
# Result: 118M

sudo du -sh /mnt/boot/efi/
# Result: 107M
```

**Verification scripts exist but were bypassed or ignored:**
- `lib/common.go` has `VerifyInstallation()` function
- Checks for `/mnt/boot/vmlinuz-*` and `/mnt/boot/initrd-*`
- Warns if missing (but doesn't block/fail)
- Called from all `04-system-init.go` scripts

---

## Root Cause Analysis

### Primary Issue: Missing `initrd` Field in config.scm

**CRITICAL DISCOVERY (2025-11-04):** The framework and framework-dual configs are missing the explicit `initrd` field in the `operating-system` definition.

When using the nonguix channel with `(nongnu system linux-initrd)`, you **MUST** explicitly specify:

```scheme
(use-modules (gnu)
             (gnu packages linux)
             (gnu system nss)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(operating-system
  ;; ... other fields ...
  (kernel linux)
  (initrd microcode-initrd)  ; <-- THIS LINE WAS MISSING!
  (firmware (list linux-firmware))
  ;; ... rest of config ...
)
```

**Why this matters:**
- Without `(initrd microcode-initrd)`, Guix System doesn't generate the initrd file properly
- The initrd file is critical for loading kernel modules during boot
- `guix system init` may appear to complete but won't create `/mnt/boot/initrd-*`
- Without initrd, the system cannot boot (kernel loads but can't mount root filesystem)

**Files affected:**
- `framework-dual/install/03-config-dual-boot.go` - needs `(initrd microcode-initrd)` added
- `framework/install/03-config.go` - needs `(initrd microcode-initrd)` added

**Note:** The cloudzy installer uses `linux-libre` (not nonguix) and doesn't need this because it uses Guix's default initrd handling.

**Defensive measure (optional):** After fixing the config to include `(initrd microcode-initrd)`, you can also add an activation service to ensure kernel/initrd symlinks are always present in `/boot/`:

```scheme
;; Add to services section
(services
  (append (list
    (simple-service 'copy-kernel-to-boot activation-service-type
      #~(begin
          (use-modules (guix build utils))
          (let* ((boot   "/boot")
                 (vk     (string-append boot "/vmlinuz-guix"))
                 (ir     (string-append boot "/initrd-guix"))
                 (kernel "/run/current-system/kernel")
                 (initrd "/run/current-system/initrd"))
            (mkdir-p boot)
            (false-if-exception (delete-file vk))
            (false-if-exception (delete-file ir))
            (symlink kernel vk)
            (symlink initrd ir)
            (chmod vk #o644)
            (chmod ir #o644)))))
    %base-services))
```

This activation service runs after each `guix system reconfigure` and ensures the kernel/initrd symlinks are always up-to-date. However, this is a **post-boot safety measure** - the primary fix is adding `(initrd microcode-initrd)` so the files are created during installation.

### Secondary Issue: Incomplete `guix system init`

The installation process did not complete successfully:
1. ✅ Partitions created and formatted
2. ✅ Config file generated at `/mnt/etc/config.scm`
3. ✅ Some packages installed to `/mnt/gnu/store/` (2137 items)
4. ✅ GRUB installed to EFI partition
5. ❌ **Kernel and initrd NOT installed to `/mnt/boot/`** ← Root cause: missing initrd field
6. ❌ **User account NOT created**
7. ❌ **User password NOT set**

### Why Boot Hangs

GRUB starts the boot process, but:
1. No kernel exists at expected path
2. System hangs trying to load non-existent kernel
3. Filesystem never gets mounted properly
4. Clean unmount never happens → dirty bit set on next boot

### Verification Gap

While `VerifyInstallation()` exists and checks for these files:
- It only **warns** when files are missing
- It doesn't **fail/abort** the installation
- User may have missed warnings in output
- No clear "INSTALLATION FAILED - DO NOT REBOOT" message

---

## Questions Remaining

1. **Did `guix system init` error during original install?**
   - User doesn't remember seeing errors
   - Error may have been buried in output

2. **Could files have been removed accidentally?**
   - Unlikely - no commands were run that would delete them
   - More likely they were never created

3. **Why did `guix system init` partially complete?**
   - Store populated with packages
   - GRUB installed
   - But kernel/initrd not copied to `/boot/`
   - Suggests failure during late stage of init

---

## Next Steps to Resolve

### Immediate Fix (Re-run Installation with Verification)

From Guix ISO:

```bash
# 1. Mount existing partitions
mount /dev/nvme0n1p4 /mnt
mount /dev/nvme0n1p1 /mnt/boot/efi

# 2. Verify config exists and is correct
cat /mnt/etc/config.scm
# Check kernel-arguments line - should be: (kernel-arguments '("quiet"))

# 3. RUN VERIFICATION SCRIPT (should fail initially)
./verify-guix-install.sh
# Expected: FAIL with missing kernel/initrd errors

# 4. Start cow-store
herd start cow-store /mnt

# 5. Re-run guix system init (will use existing config)
guix system init /mnt/etc/config.scm /mnt \
  --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org'

# 6. RUN VERIFICATION SCRIPT AGAIN (CRITICAL - must pass!)
./verify-guix-install.sh
# Expected: ALL CHECKS PASSED
# If verification fails, DO NOT REBOOT - review errors and retry step 5

# 7. Set user password (only if verification passed)
guix system chroot /mnt
passwd YOUR_USERNAME  # Use username from config.scm
exit

# 8. RUN VERIFICATION SCRIPT ONE MORE TIME (optional but recommended)
./verify-guix-install.sh
# Confirms nothing broke during password setting

# 9. Clean unmount
umount /mnt/boot/efi
umount /mnt

# 10. Reboot (only if all verifications passed)
reboot
```

**Key Points:**
- Run `verify-guix-install.sh` **before** `guix system init` (expect failure - baseline)
- Run `verify-guix-install.sh` **after** `guix system init` (must pass - DO NOT REBOOT if fails)
- Optionally run again after setting password (safety check)
- The script will be automatically installed to `/usr/local/bin/verify-guix-install` during init

---

## Verification Script

A standalone verification script (`verify-guix-install.sh`) has been created to help diagnose installation issues:

**Features:**
- Works from both Guix ISO (checks `/mnt`) and installed system (checks `/`)
- Checks all critical boot files (kernel, initrd, GRUB)
- Verifies user accounts exist
- Shows file sizes to confirm integrity
- Color-coded output (errors, warnings, OK)
- Exit codes: 0=success, 1=failure

**Usage:**

```bash
# From Guix ISO (before rebooting):
./verify-guix-install.sh

# From installed Guix system (after boot):
sudo verify-guix-install

# The script is automatically installed to /usr/local/bin/ during system init
```

**When to use:**
1. **Before leaving Guix ISO** - Must pass before rebooting
2. **After boot** - If system has issues, run to diagnose
3. **During troubleshooting** - Check state at any point
4. **After fixes** - Verify problems are resolved

---

## Current Status

**System State:**
- Pop!_OS: ✅ Working, restored boot entries
- Guix OS: ❌ Incomplete installation, cannot boot
- Filesystems: ✅ All healthy, no corruption
- Config file: ✅ Generated correctly at `/mnt/etc/config.scm`
- GNU Store: ⚠️ Partially populated (2137 packages)
- Boot files: ❌ Missing kernel and initrd
- User account: ❌ Not created

**Next Action:**
Re-run `guix system init` from Guix ISO following steps above.
