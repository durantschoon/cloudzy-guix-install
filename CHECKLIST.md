# Guix Installer Design Goals and Implementation Checklist

The goal of this project is to create a **reliable, repeatable, and hardware-aware Guix OS installation flow** tailored to the Framework 13 laptop. The installer should maximize success on first boot while remaining portable and adaptable for other systems.

---

## 🧭 Core Design Principles

### 1. Start From a Super-Minimal `config.scm`

We want the initial install configuration to be as small and deterministic as possible. That means:

* **Remove everything non-essential** from the base `config.scm`: no keyboard layout, no user comment field, no desktop environment, no CUPS, etc.
* Keep only:
  * `host-name`
  * `locale` and `timezone`
  * `bootloader` section
  * Essential `file-systems`
  * `users` block (with password set before reboot — see below)

**Goal:** This stage should do *one thing only* — reliably install a bootable Guix system shell.

Later stages (like adding services, desktop environments, etc.) will happen via a `guix system reconfigure` using a richer configuration file.

---

### 2. Verify Kernel and Initrd Before Reboot

One of the most common failure modes is rebooting into a system without a kernel or initrd present. To prevent this:

* Add a **post-`system init` verification step** to the scripts.
* The script should check that the following files exist before offering to reboot:

```bash
ls /mnt/boot/vmlinuz* /mnt/boot/initrd* >/dev/null
ls /mnt/boot/efi/EFI/guix/grubx64.efi >/dev/null
```

* If any of these are missing, the script should print a clear error and refuse to reboot until the issue is fixed.
* This verification step is simple but essential for catching broken installs early.

**Status:** ✅ Implemented

---

### 3. Pre-Set the User Password Before First Boot

To avoid the frustrating "can't log in" situation:

* After `guix system init` succeeds but **before reboot**, `chroot` into the installed system and run `passwd` for the primary user.
* This will guarantee that the first boot lands on a login screen you can actually enter.

This approach is preferred over embedding a password hash directly in the config, because it avoids storing secrets in version control.

**Status:** ✅ Implemented

---

### 4. Eliminate "Nomodeset" Workarounds With Hardware-Aware Defaults

Currently, first boots sometimes require pressing `e` in GRUB and adding `nomodeset` to work around AMD GPU issues. This is fragile and confusing for users. To solve this:

* In the **Framework-specific scripts/config**, include the absolute minimum required to boot cleanly without manual GRUB editing:
  * Add the `amdgpu`, `nvme`, `xhci_pci`, `usbhid`, and `i2c_piix4` modules to `initrd`.
  * Include `(kernel linux)` and `(firmware (list linux-firmware))` (requires `nonguix` channel).
  * Set kernel arguments to something stable like `("quiet" "loglevel=3")`.

This makes the Framework variant slightly more opinionated but much more user-friendly — the system should "just boot" into a login prompt.

**Status:** ✅ Implemented (kernel from nonguix, linux-firmware, required initrd modules, and kernel args added in `framework/install/03-config.go` and `framework-dual/install/03-config-dual-boot.go`)

---

## ✅ What We're Doing Correctly

### 1. cow-store Usage

* ✅ **Added `herd start cow-store /mnt` before `guix system init`**

* ✅ Never using `mount --bind /mnt/gnu /gnu`

### 2. Partition Labeling

* ✅ **Using uppercase labels: `GUIX_ROOT` and `EFI`**

* ✅ Setting GPT partition names via `parted name`
* ✅ Setting filesystem labels via `mkfs.ext4 -L` and `mkfs.vfat -n`

### 3. Installation Command

* ✅ Using `--fallback` flag for local builds

* ✅ Using multiple substitute URLs for redundancy
* ✅ Retry logic (3 attempts with 10s delay)

### 4. Mount Order

* ✅ Correct sequence: root first, then EFI at `/mnt/boot/efi`

* ✅ Creating mount points before mounting

### 5. Storage Management

* ✅ Setting `TMPDIR=/mnt/var/tmp` to use target disk

* ✅ Setting `XDG_CACHE_HOME=/mnt/var/cache`
* ✅ Clearing substitute cache

### 6. Safety Features

* ✅ Checking for Guix live ISO environment

* ✅ Idempotency checks (skip if already formatted)
* ✅ User confirmation prompts for destructive operations

### 7. Documentation & Meta

* ✅ Label everything consistently (UPPERCASE: EFI, GUIX_ROOT)

* ✅ Name consistently across systems
* ✅ Comment generously
* ✅ Treat install as idempotent
* ✅ Document the why (CLAUDE.md, README files)

---

## 📋 Implementation Gaps and Action Items

### 🔴 High Priority (Critical - Do First)

#### 1. EFI Partition Verification

**Status:** ✅ Implemented

**Missing checks:**

```bash
# Verify ESP is FAT32
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: EFI not FAT32"; exit 1; }

# Verify mount is correct
mount | grep "/mnt/boot/efi"
```

**Why it matters:**

* Most common installation failure: "doesn't look like an EFI partition"
* Early detection prevents wasted installation time
* Provides clear error messages

**Impact:** ⭐⭐⭐ High - Prevents cryptic errors during `guix system init`

---

#### 2. Post-Installation File Verification

**Status:** ✅ Implemented

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

* Detects incomplete installations before reboot
* Prevents "no bootable device" scenarios
* Allows immediate retry while still in live environment

**Impact:** ⭐⭐⭐ High - Prevents failed boot experiences

---

#### 3. Label Existence Check Before Mount

**Status:** ✅ Implemented

**Missing checks:**

```bash
[ -e /dev/disk/by-label/EFI ] || { echo "ERROR: No EFI label"; exit 1; }
[ -e /dev/disk/by-label/GUIX_ROOT ] || { echo "ERROR: No GUIX_ROOT label"; exit 1; }
```

**Why it matters:**

* Verifies labels exist before attempting mount
* Provides clear error if labeling step failed
* Prevents confusing mount errors

**Impact:** ⭐⭐⭐ High - Prevents mount failures

---

#### 4. Use file-system-label for EFI in config.scm

**Status:** ✅ Implemented

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

* Root already uses UUID (good!)
* EFI should use label for consistency
* Prevents boot issues if device enumeration changes

**Impact:** ⭐⭐⭐ Medium-High - Can cause boot failures in some scenarios

---

### 🟡 Medium Priority

#### 5. Mount by Label in Scripts

**Status:** ✅ Implemented

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

* Device paths can change between boots (especially with multiple disks)
* Labels are stable and don't depend on device enumeration order
* This is especially important for dual-boot scenarios

**Impact:** ⭐⭐ Medium-High - Can cause boot failures if device order changes

---

#### 6. Bootloader Timeout Configuration

**Status:** ❌ Not set

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

* Default timeout is 0 (hidden menu) - users can't select OS in dual-boot
* Framework-dual needs visible menu to access Pop!_OS
* F12 firmware menu is a workaround, not a solution

**Impact:** ⭐⭐ Medium - Affects dual-boot usability

---

#### 7. Free Space Check (All Installers)

**Status:** ✅ Implemented (warns if < 40GiB after mount)

**Missing for cloudzy and framework:**

```bash
# Check free space before installation
available=$(df -BG /mnt | tail -1 | awk '{print $4}' | sed 's/G//')
[[ $available -lt 40 ]] && echo "WARNING: Less than 40GB free"
```

**Why it matters:**

* Prevents running out of space during installation
* Early warning for users

**Impact:** ⭐⭐ Medium - Installation will fail anyway, but later

---

#### 8. Installation Logging

**Status:** ✅ Implemented (tee-style to /tmp/guix-install.log)

**Should add:**

```bash
exec > >(tee /tmp/guix-install.log) 2>&1
```

**Why it matters:**

* Helps with debugging failures
* Provides installation receipt
* Users can review what happened

**Impact:** ⭐⭐ Medium - Improves troubleshooting

---

### 🟢 Low Priority (Nice to Have)

#### 9. Label Verification Output

**Status:** ❌ Not shown to user

**Should display:**

```bash
# Show labels after formatting
echo "Verifying partition labels..."
e2label /dev/nvme0n1p2        # Should show: GUIX_ROOT
fatlabel /dev/nvme0n1p1       # Should show: EFI
parted /dev/nvme0n1 print     # Should show GPT names
```

**Why it matters:**

* Confirms labels are correctly set
* Helps users debug if something goes wrong
* Educational for users learning Guix

**Impact:** ⭐ Low - Nice for debugging

---

#### 10. Swap Partition Support

**Status:** ⚠️ Only swapfile support

**Current:** Only supports creating swapfile in step 4

**Could add:** Detection and use of existing swap partition

**Why it matters:**

* Some users prefer swap partitions
* More traditional setup

**Impact:** ⭐ Low - Swapfile works fine

---

#### 11. Reserved Disk Space Option

**Status:** ❌ Not implemented

**Could add:**

* Allow leaving 10-20GB unallocated
* User configurable via env var

**Why it matters:**

* Flexibility for future partitions
* Some users prefer reserved space

**Impact:** ⭐ Low - Most users don't need this

---

## 🔧 Recommended Implementation Order

### Phase 1: Critical Safety Checks (30 min - Do Now)

1. ✅ Add pre-init EFI verification (vfat check)
2. ✅ Add label existence checks before mount
3. ✅ Add post-init file verification (kernel, initrd, GRUB)
4. ✅ Use file-system-label for EFI in config

### Phase 2: Password & Hardware Defaults (1 hour - Next)

5. ✅ Set user password via chroot before reboot
6. ✅ Add Framework-specific initrd modules
7. ✅ Add bootloader timeout for dual-boot

### Phase 3: Robustness Improvements (1-2 hours - When Time Permits)

8. Add label verification output

### Phase 4: Nice-to-Haves (Optional)

12. Swap partition detection/support
13. Reserved space option
14. Post-install receipt generation
15. Optional: Generate fallback GRUB entry with `nomodeset`

---

## 📊 Summary of Installation Stages

| Phase                                | Goal                                                  | Result                   |
| ------------------------------------ | ----------------------------------------------------- | ------------------------ |
| **Stage 1: Minimal Install**         | Tiny config, password pre-set, kernel/initrd verified | Always boots to a shell  |
| **Stage 2: Framework Profile**       | Add GPU firmware, essential modules, network, DE      | Boots without GRUB hacks |
| **Stage 3: Full System Reconfigure** | Add `/data` mounts, services, desktop, dotfiles, etc. | Complete environment     |

---

## 📈 Current Implementation Status

### By Category

* **Partitioning:** 6/7 (86%) ✅
* **Mounting:** 3/7 (43%) ⚠️ Needs verification checks
* **Bootloader:** 4/6 (67%) ⚠️ Needs post-install checks
* **Guix-Specific:** 6/7 (86%) ✅
* **Automation:** 3/5 (60%) ⚠️ Needs logging and checks
* **Meta Practices:** 6/6 (100%) ✅

**Overall:** ~70% complete

### Biggest Impact Improvements

1. **ESP verification** (prevents most common error)
2. **Post-install checks** (prevents boot failures)
3. **Label verification** (prevents mount errors)
4. **User password setup** (prevents login failures)

---

## 🎯 Summary

**In short:** we want a *bulletproof base install*, automatic sanity checks, and hardware-aware defaults that make first boot smooth and predictable — no typing `nomodeset`, no guessing at passwords, and no surprises when the reboot happens.

The current implementation is solid for single-boot VPS scenarios. The gaps mostly affect edge cases, dual-boot reliability, and first-boot user experience on Framework laptops.
