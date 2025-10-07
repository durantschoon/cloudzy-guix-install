# Implementation Gaps Analysis

Comparing our current installation scripts against best practices from real-world experience.

## ✅ What We're Doing Correctly

### 1. cow-store Usage
- ✅ **Added `herd start cow-store /mnt` before `guix system init`** (recent fix)
- ✅ Never using `mount --bind /mnt/gnu /gnu`

### 2. Partition Labeling
- ✅ **Using uppercase labels: `GUIX_ROOT` and `EFI`** (recent standardization)
- ✅ Setting GPT partition names via `parted name`
- ✅ Setting filesystem labels via `mkfs.ext4 -L` and `mkfs.vfat -n`

### 3. Installation Command
- ✅ Using `--fallback` flag for local builds
- ✅ Using multiple substitute URLs for redundancy
- ✅ Retry logic (3 attempts with 10s delay)

### 4. Mount Order
- ✅ Correct sequence: root first, then EFI at `/mnt/boot/efi`
- ✅ Creating mount points before mounting

### 5. Storage Management
- ✅ Setting `TMPDIR=/mnt/var/tmp` to use target disk
- ✅ Setting `XDG_CACHE_HOME=/mnt/var/cache`
- ✅ Clearing substitute cache

### 6. Safety Features
- ✅ Checking for Guix live ISO environment
- ✅ Idempotency checks (skip if already formatted)
- ✅ User confirmation prompts for destructive operations

## ❌ What We're Missing

### 1. Mount by Label (CRITICAL)
**Status:** ❌ **Not implemented**

**Current approach:**
```go
// We mount by device path
state.EFI = "/dev/nvme0n1p1"
state.Root = "/dev/nvme0n1p2"
```

**Should be:**
```go
// Mount by label for reliability
state.EFI = "/dev/disk/by-label/EFI"
state.Root = "/dev/disk/by-label/GUIX_ROOT"
```

**Why it matters:**
- Device paths can change between boots (especially with multiple disks)
- Labels are stable and don't depend on device enumeration order
- This is especially important for dual-boot scenarios

**Impact:** Medium-High - Can cause boot failures if device order changes

---

### 2. EFI Partition Verification (CRITICAL)
**Status:** ❌ **Not implemented**

**Missing checks:**
```bash
# Verify ESP is FAT32
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: EFI not FAT32"; exit 1; }

# Verify mount is correct
mount | grep "/mnt/boot/efi"
```

**Why it matters:**
- Most common installation failure: "doesn't look like an EFI partition"
- Early detection prevents wasted installation time
- Provides clear error messages

**Impact:** High - Prevents cryptic errors during `guix system init`

---

### 3. Bootloader Timeout Configuration
**Status:** ❌ **Not set**

**Current:**
```scheme
(bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))
```

**Should include:**
```scheme
(bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5)  ; Show menu for 5 seconds
  (keyboard-layout (keyboard-layout "us")))
```

**Why it matters:**
- Default timeout is 0 (hidden menu) - users can't select OS in dual-boot
- Framework-dual needs visible menu to access Pop!_OS
- F12 firmware menu is a workaround, not a solution

**Impact:** Medium - Affects dual-boot usability

---

### 4. Post-Installation Verification (IMPORTANT)
**Status:** ❌ **Not implemented**

**Missing checks after `guix system init`:**
```bash
# Verify kernel and initrd
ls /mnt/boot/vmlinuz-* || echo "ERROR: No kernel"
ls /mnt/boot/initrd-* || echo "ERROR: No initrd"

# Verify GRUB installation
ls /mnt/boot/grub/grub.cfg || echo "ERROR: No GRUB config"
ls /mnt/boot/efi/EFI/guix/grubx64.efi || echo "ERROR: No GRUB EFI"
ls /mnt/boot/efi/EFI/guix/grub.cfg || echo "ERROR: No GRUB EFI config"
```

**Why it matters:**
- Detects incomplete installations before reboot
- Prevents "no bootable device" scenarios
- Allows immediate retry while still in live environment

**Impact:** High - Prevents failed boot experiences

---

### 5. Label Verification Commands (NICE TO HAVE)
**Status:** ❌ **Not shown to user**

**Should display:**
```bash
# Show labels after formatting
echo "Verifying partition labels..."
e2label /dev/nvme0n1p2        # Should show: GUIX_ROOT
fatlabel /dev/nvme0n1p1       # Should show: EFI
parted /dev/nvme0n1 print     # Should show GPT names
```

**Why it matters:**
- Confirms labels are correctly set
- Helps users debug if something goes wrong
- Educational for users learning Guix

**Impact:** Low - Nice for debugging

---

### 6. Config.scm Uses Device Path (MEDIUM)
**Status:** ⚠️ **Partially problematic**

**Current:**
```scheme
(file-system
  (mount-point "/boot/efi")
  (device "/dev/nvme0n1p1")  ; Device path, not label!
  (type "vfat"))
```

**Should be:**
```scheme
(file-system
  (mount-point "/boot/efi")
  (device (file-system-label "EFI"))
  (type "vfat"))
```

**Why it matters:**
- Root already uses UUID (good!)
- EFI should use label for consistency
- Prevents boot issues if device enumeration changes

**Impact:** Medium - Can cause boot failures in some scenarios

---

### 7. Free Space Verification (NICE TO HAVE)
**Status:** ⚠️ **Framework-dual only**

**Missing for cloudzy and framework:**
```bash
# Check free space before installation
available=$(df -BG /mnt | tail -1 | awk '{print $4}' | sed 's/G//')
[[ $available -lt 40 ]] && echo "WARNING: Less than 40GB free"
```

**Why it matters:**
- Prevents running out of space during installation
- Early warning for users

**Impact:** Low - Installation will fail anyway, but later

---

## 📋 Priority Action Items

### High Priority (Do First)

1. **Add EFI partition verification before `guix system init`**
   - Check `df -T /mnt/boot/efi | grep vfat`
   - Abort with clear error if not FAT32

2. **Add post-installation verification**
   - Check for kernel, initrd, and GRUB files
   - Warn if incomplete before reboot

3. **Use mount-by-label in config.scm for EFI**
   - Change from device path to `(file-system-label "EFI")`

### Medium Priority

4. **Add bootloader timeout to framework-dual config**
   - Set `(timeout 5)` for dual-boot scenarios

5. **Switch to mount-by-label in scripts**
   - Use `/dev/disk/by-label/` paths where possible
   - May require additional logic to handle missing labels

### Low Priority

6. **Add label verification output**
   - Show `e2label` and `fatlabel` results to user
   - Helps with debugging

7. **Add free space check to all installers**
   - Currently only in framework-dual
   - Warn if <40GB available

---

## 🔧 Recommended Implementation Order

```bash
# Phase 1: Critical safety checks (do now)
1. Add pre-init EFI verification
2. Add post-init file verification
3. Use file-system-label for EFI in config

# Phase 2: Configuration improvements (next)
4. Add bootloader timeout for dual-boot
5. Add label verification output

# Phase 3: Nice-to-haves (when time permits)
6. Switch to mount-by-label in mount scripts
7. Add free space checks to all installers
```

---

## 📝 Notes

- **cow-store fix** was critical and already implemented ✅
- **Uppercase labels** standardization was important and done ✅
- **EFI verification** is the most impactful remaining gap
- **Post-install checks** would prevent frustrating boot failures
- **Mount-by-label** is best practice but requires more refactoring

The current implementation is solid for single-boot VPS scenarios. The gaps mostly affect edge cases and dual-boot reliability.
