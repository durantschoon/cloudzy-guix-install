# Guix OS Installation: Hard-Won Knowledge

This document captures critical lessons learned from real-world Guix OS installations, particularly focusing on dual-boot scenarios and common pitfalls.

## 🎯 Free Software vs Nonguix: Platform-Specific Design Decisions

**Design Philosophy:** Different platforms have different hardware requirements and use cases.

### Cloudzy VPS Installer - Free Software Only

**Why no nonguix:**
- VPS hardware is fully virtualized (KVM/QEMU) with free software drivers
- No physical WiFi cards, Bluetooth, or proprietary GPU firmware needed
- Server workloads typically don't require proprietary software
- Keeps system fully free software compliant (important for many server deployments)
- `linux-libre` kernel works perfectly with virtualized hardware

**Code:** Uses `RunGuixSystemInitFreeSoftware()` - standard `guix system init` without time-machine or nonguix channel

### Framework 13 Installer - Requires Nonguix

**Why nonguix is mandatory:**
- **WiFi:** MediaTek MT7922 requires proprietary firmware blobs (`linux-firmware` package)
- **GPU:** AMD graphics need proprietary firmware for full functionality
- **Bluetooth:** Modern Bluetooth chips require proprietary firmware
- **Practical necessity:** Without nonguix, the laptop has no wireless connectivity
- Laptop users expect "it just works" - nonguix delivers that

**Code:** Uses `RunGuixSystemInit()` with time-machine + nonguix channel for `(kernel linux)` and `(firmware (list linux-firmware))`

### When to Use Which Approach

**Use Free Software Only (like Cloudzy):**
- Virtual machines and VPS instances
- Servers with Intel/Realtek NICs that have free drivers
- Systems where hardware is known to work with `linux-libre`
- Deployments where free software compliance is required

**Use Nonguix (like Framework 13):**
- Modern laptops (almost all need WiFi firmware)
- Gaming systems (proprietary GPU drivers)
- Workstations with recent AMD/Nvidia GPUs
- Any system where wireless connectivity is essential

**This is a feature, not a bug:** Having separate installers lets users make informed choices based on their hardware and values.

## 🧠 The Golden Rule: cow-store

**Always run `herd start cow-store /mnt` before `guix system init`.**

- This redirects Guix store writes to the target disk while keeping the live installer's `/gnu/store` usable
- **Never use `mount --bind /mnt/gnu /gnu`** — this shadows the live system's store and breaks the `guix` command
- Without cow-store, the ISO's limited tmpfs fills up and the installation fails

## 🐚 Bash Paths in Guix

**CRITICAL: All scripts MUST use `/run/current-system/profile/bin/bash` shebang.**

Guix does not follow the Filesystem Hierarchy Standard (FHS). There is NO `/bin/bash` on Guix systems.

**For all scripts (ISO and installed system):**
```bash
# CORRECT - Required for all scripts
#!/run/current-system/profile/bin/bash

# WRONG - Does not exist in Guix
#!/bin/bash

# WRONG - Not reliable on Guix ISO (DO NOT USE)
#!/usr/bin/env bash
```

**For chroot commands:**
```bash
# CORRECT
chroot /mnt /run/current-system/profile/bin/bash

# WRONG
chroot /mnt /bin/bash
```

**Why `/run/current-system/profile/bin/bash` is mandatory:**
- This is where bash exists on both Guix ISO and installed systems
- `#!/usr/bin/env bash` is NOT reliable on Guix ISO
- `/bin/bash` does not exist at all
- All critical scripts must use the explicit path

## 🧩 Partitioning & Filesystems

### Partition Table Requirements

- Use GPT partition table
- EFI System Partition (ESP): FAT32, 512MB-1GB, flags: `boot,esp`
- Root partition: ext4, labeled for reliability

### Label Convention

Use **UPPERCASE with underscores** for all partition labels:

- `EFI` → FAT32 ESP → mounted at `/boot/efi`
- `GUIX_ROOT` → ext4 → mounted at `/`
- `DATA` (optional) → ext4/btrfs → mounted at `/data` (not `/home`)

Verify labels:

```bash
e2label /dev/nvme0n1pX         # ext4 labels
fatlabel /dev/nvme0n1pY        # FAT labels
parted /dev/nvme0n1 print      # GPT partition names
```

Mount by label for reliability:

```bash
/dev/disk/by-label/GUIX_ROOT
/dev/disk/by-label/EFI
```

**Critical: Use file-system-label instead of UUID in config.scm**

Always use `(file-system-label "LABEL_NAME")` instead of `(uuid "xxxx-xxxx" 'ext4)` in your config.scm:

```scheme
;; CORRECT - Use file-system-label
(file-system
  (mount-point "/")
  (device (file-system-label "GUIX_ROOT"))
  (type "ext4"))

;; INCORRECT - Don't use UUID
(file-system
  (mount-point "/")
  (device (uuid "xxxx-xxxx" 'ext4))
  (type "ext4"))
```

**Why file-system-label is better:**

- **More reliable** - Labels don't change between boots
- **More readable** - Clear partition purpose
- **More portable** - Works across different systems
- **Easier debugging** - Can see labels with `lsblk -f`

### DATA Partition Usage

**Important**: The `DATA` partition is mounted at `/data`, not `/home`:

- **`/data`** - Shared data partition for files, documents, media
- **`/home`** - User home directories remain on the root filesystem
- **Benefits**: Cleaner separation, easier backup, shared access across systems
- **Options**: `defaults,noatime` for better performance on data storage

### BIOS vs UEFI Boot Modes

**Critical**: The partition layout must match the boot mode.

**UEFI Mode (most modern systems):**
```bash
# Partition layout:
# 1. EFI System Partition (ESP) - 512MB, FAT32, flags: esp
# 2. Root partition - remaining space, ext4

parted /dev/sda --script \
  mklabel gpt \
  mkpart EFI fat32 1MiB 513MiB \
  set 1 esp on \
  mkpart root ext4 513MiB 100%
```

**BIOS Mode (legacy systems, some VPS):**
```bash
# Partition layout:
# 1. BIOS Boot partition - 1MB, no filesystem, flags: bios_grub
# 2. EFI System Partition - 512MB, FAT32, flags: esp (for future UEFI boot)
# 3. Root partition - remaining space, ext4

parted /dev/sda --script \
  mklabel gpt \
  mkpart BIOSBOOT 1MiB 2MiB \
  set 1 bios_grub on \
  mkpart EFI fat32 2MiB 514MiB \
  set 2 esp on \
  mkpart root ext4 514MiB 100%
```

**Why BIOS needs a special partition:**
- GPT + BIOS boot requires a BIOS Boot Partition for GRUB's core.img
- Without it: `grub-install` fails with "GPT partition label has no BIOS boot partition"
- The partition must have the `bios_grub` flag set
- Size: 1MB is sufficient
- Filesystem: none (GRUB writes directly to it)

**How to detect boot mode:**
```bash
# Check if /sys/firmware/efi exists
if [ -d /sys/firmware/efi ]; then
    echo "UEFI mode"
else
    echo "BIOS mode"
fi
```

**Common VPS boot modes:**
- Cloudzy: BIOS mode (requires BIOS boot partition)
- Most cloud providers: UEFI mode
- Physical systems: Check BIOS/UEFI settings

## 🔧 Nonguix Channel Setup

**Critical**: Framework installers require the nonguix channel for proprietary firmware and kernel support.

### Automatic Setup (Recommended)

The framework installers automatically setup the nonguix channel:

```bash
# Creates /tmp/channels.scm with nonguix channel
# Authorizes nonguix substitutes
# Pulls Guix with nonguix channel
# Uses guix time-machine for system init
```

### Manual Setup (If Needed)

```bash
# 1. Create channels.scm
cat > /tmp/channels.scm <<'EOF'
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
       %default-channels)
EOF

# 2. Authorize nonguix substitutes (faster builds)
wget -qO- https://substitutes.nonguix.org/signing-key.pub | guix archive --authorize

# 3. Pull with nonguix channel
guix pull -C /tmp/channels.scm

# 4. Use time-machine for system init
guix time-machine -C /tmp/channels.scm -- \
  system init /mnt/etc/config.scm /mnt --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Why Nonguix Channel is Required

- **Proprietary firmware** - `linux-firmware` package for hardware support
- **Non-free kernel** - `linux` kernel (not `linux-libre`) for better hardware compatibility
- **Hardware-specific modules** - AMD GPU, NVMe, USB drivers for Framework 13

### Common Errors Without Nonguix

```text
no code for module (nongnu packages linux)
unbound variable: linux
unbound variable: linux-firmware
```

**Solution**: Ensure nonguix channel is available before config generation.

### Troubleshooting Nonguix Channel Issues

**Problem**: `/tmp/channels.scm` missing during installer re-runs

**Symptoms:**
- Config exists but channels.scm doesn't
- "no code for module (nongnu packages linux)" error during system init
- Installer skips nonguix setup prompt

**Cause**: Installer idempotency check skips config generation (including channel setup) when config.scm already exists from a previous partial run.

**Solution**: The installer now checks for both config.scm AND channels.scm. If config exists but channels.scm is missing, it will:
1. Prompt for nonguix consent
2. Create /tmp/channels.scm
3. Skip config file writing (already exists)
4. Continue to system init with time-machine

**Manual fix if needed:**
```bash
# Remove config to trigger full re-generation
rm /mnt/etc/config.scm /tmp/channels.scm

# Re-run installer - will prompt for nonguix and create both files
```

## 🚀 Installer Best Practices

### Always Specify Platform

**Critical**: Always specify the platform either as an argument or environment variable

```bash
# RECOMMENDED - Pass as argument (most explicit)
bash bootstrap.sh framework-dual
bash bootstrap.sh cloudzy

# ALTERNATIVE - Set environment variable before running
export GUIX_PLATFORM="framework-dual"
bash bootstrap.sh

# WRONG - No platform specified (empty GUIX_PLATFORM, behavior undefined)
bash bootstrap.sh   # Without GUIX_PLATFORM set
```

**Why this matters:**
- Different platforms have different partition layouts (BIOS vs UEFI)
- Different platforms need different channels (nonguix for framework, not for cloudzy)
- Different platforms use different installer code paths
- Without platform, installer behavior is undefined (may use wrong code path)

### Setting Environment Variables

Set user variables BEFORE running the installer:

```bash
export USER_NAME="yourname"
export FULL_NAME="Your Full Name"
export TIMEZONE="America/New_York"
export HOST_NAME="my-guix-system"

# Then run installer
bash bootstrap.sh framework-dual
```

**Why set variables first:**
- Installer reads variables during config generation (step 3)
- If not set, uses defaults (USER_NAME=guix, FULL_NAME="Guix User")
- Cannot change after config is generated without deleting /mnt/etc/config.scm

### Handling Installer Interruptions

If the installer is interrupted or fails:

1. **Check what completed:**
   ```bash
   lsblk -f                    # Check partitions and labels
   ls /mnt/etc/config.scm      # Check if config exists
   ls /tmp/channels.scm        # Check if channels exist
   ```

2. **Re-run is safe (idempotent):**
   - Installer skips completed steps
   - Won't repartition formatted disks
   - Won't regenerate existing configs

3. **To force re-run of specific step:**
   ```bash
   # Force config regeneration
   rm /mnt/etc/config.scm /tmp/channels.scm

   # Force partition recreation (DESTROYS DATA!)
   wipefs -a /dev/vda
   ```

### Daemon Responsiveness Issues (VPS Systems)

**Problem**: "Connection refused" errors when validating config, especially on VPS systems

**Symptoms:**
- Config validation fails before system init
- Error: "guix system: error: failed to connect to '/var/guix/daemon-socket/socket': Connection refused"
- Daemon check says "[OK] Daemon is responsive" but then immediately fails
- System init never starts
- More common on Cloudzy VPS than bare metal (Framework 13)

**Root Cause**: guix-daemon is slow to become responsive on VPS systems, race condition between check and actual use

**Implemented Solutions** (as of 2025-11-10):

1. **Increased wait time** (commit 44be9d8):
   - Wait up to 2 minutes (was 60 seconds)
   - 40 iterations × 3 seconds each
   - Tests responsiveness with `guix build --version`
   - Shows progress: "Waiting... (5/40)"

2. **Graceful validation skip** (commit 059f1dd):
   - If daemon not responsive, skip validation with warning
   - Don't block installation - system init will validate anyway
   - Allows installation to proceed to system build step

**Still investigating** (2025-11-10):
- Why VPS daemon slower than bare metal
- Why daemon check passes but then fails immediately
- May need socket file check: `test -S /var/guix/daemon-socket/socket`
- May need to check daemon process directly: `pgrep -x guix-daemon`
- Consider manual daemon start if herd fails: `guix-daemon --build-users-group=guixbuild &`

**Manual fix if needed:**
```bash
# Check daemon status
herd status guix-daemon
ps aux | grep guix-daemon
ls -la /var/guix/daemon-socket/socket

# Restart daemon
herd stop guix-daemon
sleep 5
herd start guix-daemon

# Wait longer on VPS
sleep 30

# Test multiple times
for i in {1..5}; do
  echo "Test $i:"
  guix build --version && break
  sleep 10
done

# If that works, continue manually
guix system init /mnt/etc/config.scm /mnt
```

**Workaround for persistent issues:**
- Answer 'y' to continue when validation warns about daemon
- System build step will start daemon if needed
- Real validation happens during system build anyway

## 🐧 Kernel Configuration

### Always Specify the Kernel Explicitly

**Critical**: Always specify the kernel in your `config.scm` file. Do not rely on defaults.

```scheme
(use-modules (gnu)
             (gnu packages linux)  ; Required for kernel packages
             (gnu system nss))

(operating-system
  ;; ... other fields ...
  (kernel linux-libre)  ; Explicitly specify kernel
  ;; ... rest of config ...
)
```

### Common Kernel Issues

- **"linux is unbound"**: Need to import `(gnu packages linux)` module
- **"linux-libre is unbound"**: The package name is correct, ensure module import
- **Default kernel**: Never omit the kernel field - always specify explicitly

### Why Explicit Kernel Specification Matters

- Guix defaults can change between versions
- Explicit specification ensures reproducible builds
- Prevents "unbound variable" errors during `guix system init`
- Makes the configuration self-documenting

### Initrd Configuration

**Critical**: Always specify the initrd explicitly in your `config.scm` file.

```scheme
(operating-system
  ;; ... other fields ...
  (kernel linux-libre)
  (initrd (lambda (fs . rest)
            (base-initrd fs rest)))
  ;; ... rest of config ...
)
```

**Why Explicit Initrd Specification Matters**:

- Ensures proper initrd generation for your specific filesystem setup
- Prevents boot failures due to missing initrd files
- Makes the configuration explicit and reproducible

### Critical Bug: Missing Kernel/Initrd Files (FIXED 2025-11-08)

**Issue**: `guix system init` had a bug where it would create a system generation but fail to copy kernel and initrd files to `/boot/`, leaving the system unbootable.

**Symptoms**:
- Installation appears to succeed without errors
- `/mnt/boot/grub/grub.cfg` exists
- `/mnt/boot/vmlinuz*` and `/mnt/boot/initrd*` are MISSING
- `/mnt/run/current-system` is a broken symlink

**Root cause**: `guix system init` creates the system profile in `/gnu/store` but doesn't copy kernel/initrd to `/boot` as expected.

**✅ Solution (implemented in installer commit b956baf)**:

The installer now uses a 3-step process instead of calling `system init` directly:

1. **Build system first**:
   ```bash
   guix time-machine -C /tmp/channels.scm -- system build /mnt/etc/config.scm
   ```
   This creates complete system in `/gnu/store/*-system/` with kernel and initrd.

2. **Manually copy kernel files to /boot**:
   ```bash
   cp /gnu/store/*-system/kernel /mnt/boot/vmlinuz
   cp /gnu/store/*-system/initrd /mnt/boot/initrd
   ln -s /gnu/store/*-system /mnt/run/current-system
   ```

3. **Install bootloader**:
   ```bash
   guix time-machine -C /tmp/channels.scm -- system init /mnt/etc/config.scm /mnt
   ```
   System already built, this just installs GRUB.

**Why this works**:
- `system build` creates a complete, valid system generation with all files
- Manual copy ensures kernel/initrd are in `/boot` where GRUB expects them
- `system init` then succeeds because system is already built

**For manual installations**: Always use the 3-step approach above. Don't rely on `guix system init` alone to copy kernel files.
- Required for proper kernel initialization

### GRUB EFI Bootloader Configuration

**Critical**: Always specify `grub-efi-bootloader` explicitly in your `config.scm` file.

```scheme
(bootloader
  (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (timeout 5)))
```

**Critical Bootloader Configuration Notes**:

- **DO NOT include `keyboard-layout`** in bootloader-configuration - this causes boot issues
- **DO NOT include `keyboard-layout`** in the minimal config - add it later if needed
- **Keep bootloader config minimal** - only essential fields for reliable boot

**Critical Targets Syntax Convention**:

- **Correct**: `(targets '("/boot/efi"))` - Note the single quote `'` before the parentheses
- **Incorrect**: `(targets "/boot/efi")` - Missing the quote mark
- **Incorrect**: `(targets ("/boot/efi"))` - Missing the quote mark

The single quote `'` is required in Scheme to properly quote the list. This applies to both UEFI (`'("/boot/efi")`) and BIOS (`'("/dev/sda")`) configurations.

**Why Explicit GRUB EFI Specification Matters**:

- Ensures consistent bootloader installation across different systems
- Prevents confusion between GRUB legacy and GRUB EFI
- Makes the configuration explicit and reproducible
- Required for proper EFI boot setup

### GRUB EFI Bootloader Issues

**Common Problem**: `guix system init` fails because it can't find GRUB EFI files (`grubx64.efi`, `grub.cfg`) or kernel files (`vmlinuz*`, `initrd*`) that don't exist yet.

**Root Cause**: This is a chicken-and-egg problem - `guix system init` is supposed to *create* these files, but it's looking for them before they exist.

**Solution**: Ensure proper directory structure exists before running `guix system init`:

```bash
# Create EFI directory structure
mkdir -p /mnt/boot/efi/Guix

# Clean up any existing files from previous failed attempts
rm -f /mnt/boot/efi/Guix/grubx64.efi
rm -f /mnt/boot/efi/Guix/grub.cfg
```

**Why This Happens**:

- `guix system init` needs to create GRUB EFI files in `/boot/efi/Guix/`
- It also needs to create kernel files (`vmlinuz*`, `initrd*`) in `/boot/`
- If the directory structure doesn't exist, the installation fails
- Previous failed attempts may leave partial files that cause conflicts

## ⚙️ Mount Order & Verification

### Correct Mount Sequence

```bash
# 1. Mount root
mount /dev/nvme0n1p4 /mnt

# 2. Create EFI mount point
mkdir -p /mnt/boot/efi

# 3. Mount ESP
mount /dev/nvme0n1p1 /mnt/boot/efi

# 4. Enable swap (optional)
swapon /dev/nvme0n1p2
```

### Pre-Installation Verification

Always verify ESP is correctly mounted:

```bash
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: EFI not FAT32"; exit 1; }
mount | grep "/mnt/boot/efi"
```

The ESP **must** show `Type: vfat` via `df -T /mnt/boot/efi`.

## 🚀 Running the Installation

### Command Pattern

```bash
# 1. Start cow-store FIRST
herd start cow-store /mnt

# 2. Run system init with fallback and multiple mirrors
guix system init /mnt/etc/config.scm /mnt \
  --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Key Flags

- `--fallback`: Build locally if substitutes fail (essential for reliability)
- `-v6`: Verbose output for debugging (shows what's happening)
- Multiple `--substitute-urls`: Redundancy when mirrors are flaky

### Resume After Failure

- **Do NOT reboot** if installation fails — the store is in RAM and will be lost
- Re-run the exact same `guix system init` command to resume
- It will reuse cached downloads and continue from where it failed

## 🧱 EFI & Bootloader Verification

### Most Common Error

```
"/mnt/boot/efi doesn't look like an EFI partition"
```

**Fix checklist:**

1. Ensure FAT32 format: `mkfs.vfat -F32 /dev/nvme0n1p1`
2. Verify correct flags: `parted /dev/nvme0n1 set 1 esp on`
3. Ensure empty mount point before mounting
4. Remount freshly before `guix system init`

### Post-Installation Verification

After successful installation, `/mnt/boot` should contain:

```
/mnt/boot/vmlinuz-*                    # Linux kernel
/mnt/boot/initrd-*                     # Initial RAM disk
/mnt/boot/grub/grub.cfg                # GRUB config (main config file)
/mnt/boot/efi/EFI/guix/grubx64.efi     # GRUB EFI bootloader binary
```

**Warning signs:**

- Only `grubx64.efi` present (no `grub.cfg` or kernel) → Installation incomplete, re-run
- Empty `/boot/efi` before init → ✅ Normal
- Empty `/boot/efi` after init → ❌ Installation failed

## 🖥️ Framework 13 AMD GPU Boot Issues

### Critical Kernel Parameters

**Framework 13 with AMD GPU requires specific kernel parameters to prevent boot hangs:**

```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "acpi=off" "noapic" "nolapic"))
```

### What These Parameters Do

- **`nomodeset`**: Disables kernel mode setting (fixes AMD GPU display issues)
- **`acpi=off`**: Disables ACPI (prevents power management conflicts)
- **`noapic`**: Disables APIC (prevents interrupt controller issues)
- **`nolapic`**: Disables Local APIC (prevents local interrupt issues)

### Boot Hang Symptoms

- System hangs at "Loading kernel modules..."
- Repeating "time with localhost and MARK" messages every 20 minutes
- Never reaches login prompt
- Ctrl+C doesn't work

### Manual Recovery

If you encounter boot hangs:

1. **At GRUB menu, press 'e' to edit**
2. **Find the kernel line and add parameters:**

   ```
   linux /boot/vmlinuz-... quiet splash nomodeset acpi=off noapic nolapic 3
   ```

3. **Press Ctrl+X or F10 to boot**

### Framework 13 Specific Initrd Modules

```scheme
(initrd-modules
 (append '("amdgpu"      ; AMD GPU driver (critical for display)
           "nvme"        ; NVMe SSD driver
           "xhci_pci"    ; USB 3.0 host controller
           "usbhid"      ; USB keyboard/mouse
           "i2c_piix4")  ; SMBus/I2C for sensors
         %base-initrd-modules))
```

## 🧰 Dual-Boot with Pop!_OS

### Bootloader Coexistence

- Pop!_OS uses `systemd-boot`, Guix uses `GRUB` — they coexist peacefully in `/boot/efi/EFI/`
- Directory structure:

  ```
  /boot/efi/EFI/
  ├── Boot/           # Fallback bootloader
  ├── systemd/        # Pop!_OS systemd-boot
  └── guix/           # Guix GRUB
  ```

### Boot Order Behavior

- Whichever OS installs last controls the default boot order
- Guix's GRUB can detect Pop!_OS (via os-prober)
- Pop!_OS systemd-boot cannot detect Guix

### Dual-Boot GRUB Configuration

**Critical**: The framework-dual installer must properly configure GRUB to detect Pop!_OS:

```scheme
(bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5)))
```

**Common Issue**: If Pop!_OS disappears from boot menu after Guix installation:

1. **Boot from Pop!_OS live ISO**
2. **Reinstall Pop!_OS GRUB:**

   ```bash
   sudo mount /dev/nvme0n1p3 /mnt  # Adjust partition number
   sudo mount /dev/nvme0n1p1 /mnt/boot/efi
   sudo grub-install --target=x86_64-efi --efi-directory=/mnt/boot/efi --bootloader-id=Pop_OS
   sudo grub-mkconfig -o /mnt/boot/grub/grub.cfg
   sudo umount /mnt/boot/efi
   sudo umount /mnt
   sudo reboot
   ```

3. **Or from Guix system:**

   ```bash
   sudo os-prober
   sudo grub-mkconfig -o /boot/grub/grub.cfg
   sudo reboot
   ```

### Accessing Pop!_OS from Guix

**Method 1:** Firmware boot menu

```bash
# During boot, press F12 → select "Pop!_OS" entry
```

**Method 2:** One-time boot from Guix

```bash
sudo efibootmgr -n <PopEntry>  # Boot Pop!_OS next time only
sudo reboot
```

### Making GRUB Menu Visible

Add to `bootloader-configuration` in `/etc/config.scm`:

```scheme
(bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5))  ; Show menu for 5 seconds
```

## 🧩 Troubleshooting Checklist

### Installation Issues

| Symptom | Diagnosis | Solution |
|---------|-----------|----------|
| Empty `/boot/efi` before init | ✅ Normal pre-install state | Continue normally |
| Empty `/boot/efi` after init | ❌ Installation failed | Re-run `guix system init` |
| Missing `vmlinuz` or `initrd` | Kernel missing from config | Add `linux-libre` to packages or check kernel config |
| Slow/failing install | Network flakes or small tmpfs | Re-run with `--fallback` and extra mirrors |
| Install appears frozen | Slow download or compilation | Ctrl+Alt+F3 to check; if Caps Lock toggles, wait longer |

### Verification Commands

```bash
# Verify mounts
mount | grep /mnt
df -h /mnt

# Verify EFI partition
df -T /mnt/boot/efi
parted /dev/nvme0n1 print

# Verify labels
lsblk -o NAME,LABEL,FSTYPE,MOUNTPOINT

# Check GRUB installation
ls -la /mnt/boot/efi/EFI/guix/
ls -la /mnt/boot/
```

## 🧾 Best Practices for Automation

### Safety Checks Before Installation

```bash
#!/run/current-system/profile/bin/bash

# 1. Verify correct mount points
mount | grep -q "/mnt " || { echo "ERROR: /mnt not mounted"; exit 1; }
mount | grep -q "/mnt/boot/efi" || { echo "ERROR: ESP not mounted"; exit 1; }

# 2. Verify FAT32 ESP
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: ESP not FAT32"; exit 1; }

# 3. Verify partition labels
blkid /dev/nvme0n1p4 | grep -q GUIX_ROOT || echo "WARNING: Root not labeled GUIX_ROOT"

# 4. Check free space
available=$(df -BG /mnt | tail -1 | awk '{print $4}' | sed 's/G//')
[[ $available -lt 40 ]] && { echo "WARNING: Less than 40GB free"; }

# 5. Start cow-store
herd start cow-store /mnt || { echo "ERROR: cow-store failed"; exit 1; }

# 6. Run installation
guix system init /mnt/etc/config.scm /mnt \
  --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Configuration Strategy

- Keep initial `/etc/config.scm` minimal for first boot
- Add services, packages, and customization after successful boot via `guix system reconfigure`
- Test configuration syntax before init: `guix system build /mnt/etc/config.scm`

## 💡 Miscellaneous Tips

### General

- **Rebooting is the only way to switch OSes** — there's no live toggle
- **F12 opens firmware boot menu** — useful when GRUB menu is hidden
- **Uppercase labels avoid UEFI quirks** — some firmware is case-sensitive
- **Never bind-mount or reformat the store mid-install** — guaranteed breakage

### Storage Management

- Set `TMPDIR=/mnt/var/tmp` to use target disk space instead of ISO tmpfs
- Set `XDG_CACHE_HOME=/mnt/var/cache` to avoid filling ISO memory
- Clear substitute cache if low on space: `rm -rf /var/guix/substitute-cache/`

### Network Issues

For slow/unreliable connections, set Git environment variables:

```bash
export GIT_HTTP_MAX_REQUESTS=2
export GIT_HTTP_LOW_SPEED_LIMIT=1000
export GIT_HTTP_LOW_SPEED_TIME=60
```

### Retry Logic

Installation can fail due to temporary network issues. Always implement retry logic:

```bash
for attempt in {1..3}; do
  if guix system init /mnt/etc/config.scm /mnt --fallback; then
    break
  fi
  echo "Attempt $attempt failed, retrying..."
  sleep 10
done
```

## 📚 Reference Commands

### Partition Management

```bash
# Create GPT table
parted /dev/nvme0n1 mklabel gpt

# Create ESP
parted /dev/nvme0n1 mkpart ESP fat32 1MiB 513MiB
parted /dev/nvme0n1 set 1 esp on

# Create root partition
parted /dev/nvme0n1 mkpart GUIX_ROOT ext4 513MiB 100%

# Format partitions
mkfs.vfat -F32 -n EFI /dev/nvme0n1p1
mkfs.ext4 -L GUIX_ROOT /dev/nvme0n1p2
```

### Boot Management

```bash
# List EFI boot entries
efibootmgr -v

# Set next boot (one-time)
efibootmgr -n <num>

# Change boot order
efibootmgr -o <num>,<num>,<num>
```

### Debugging

```bash
# Check what's using space in tmpfs
du -sh /tmp/* /var/tmp/* 2>/dev/null | sort -h

# Monitor download progress
watch -n 1 'du -sh /var/guix/substitute-cache'

# Check GRUB installation
grub-install --version
ls -la /boot/efi/EFI/*/

# Check Guix daemon status
herd status guix-daemon
guix build --version  # Test daemon connectivity
```

## 🔧 Common Issues and Fixes

### Daemon Connection Issues

**Problem:** "cannot connect to daemon-socket" error during config validation

**Root Cause:** The `ValidateGuixConfig()` function was being called before `EnsureGuixDaemonRunning()`, causing validation to fail when the daemon wasn't ready.

**Fix Applied:** Moved daemon startup before config validation in `RunGuixSystemInit()`:

1. `EnsureGuixDaemonRunning()` starts and verifies the daemon
2. `ValidateGuixConfig()` can successfully connect to the daemon
3. Installation proceeds normally

**Manual Recovery:** If you encounter this issue:

```bash
# Start the daemon manually
herd start guix-daemon

# Wait for it to be ready (test connectivity)
guix build --version

# Then continue with system init
guix system init /mnt/etc/config.scm /mnt
```

**Daemon Restart Loop:** The installer includes a robust daemon restart mechanism:

- 5 retry attempts with proper daemon stopping/starting
- Uses `herd` service manager with fallback to direct `guix-daemon` startup
- Tests daemon responsiveness with `guix build --version`
- 8-second wait periods for daemon initialization

## 🆘 Recovery Script

**New in 2024:** All installers now automatically generate a comprehensive recovery script at `/root/recovery-complete-install.sh` before running `guix system init`.

### What the Recovery Script Does

The recovery script provides a safe, idempotent way to complete or retry installation after failures:

1. **Verifies current installation state** - Checks for kernel, initrd, GRUB files
2. **Detects platform** - Automatically uses time-machine for framework installers, plain init for cloudzy
3. **Re-runs system init if needed** - Only if kernel/initrd are missing
4. **Sets user password** - Prompts if not already set
5. **Downloads customization tools** - To `/home/username/guix-customize/`
6. **Configures dual-boot GRUB** - For framework-dual, detects Pop!_OS
7. **Writes installation receipt** - Documents installation details
8. **Final verification** - Prevents reboot if critical files are missing

### When to Use the Recovery Script

Use the recovery script if:

- `guix time-machine` or `guix system init` completed but you're not sure what happened next
- The installer script was interrupted (Ctrl+C, network failure, etc.)
- You manually ran `guix time-machine` from the console
- You see "Installation incomplete" warnings
- Kernel or initrd files are missing from `/mnt/boot/`

### How to Use the Recovery Script

**From the Guix ISO (after a failed/incomplete installation):**

```bash
# The script is already on the ISO at /root/recovery-complete-install.sh
# (written before system init runs)

# Run it
/root/recovery-complete-install.sh
```

**Or download it fresh:**

```bash
cd /root
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/recovery-complete-install.sh
chmod +x recovery-complete-install.sh
./recovery-complete-install.sh
```

### Recovery Script Features

**Intelligent Detection:**
- Automatically detects if `/tmp/channels.scm` exists (time-machine needed)
- Reads username from existing `/mnt/etc/config.scm`
- Skips steps that are already complete (idempotent)

**Safety Checks:**
- Verifies mounts before proceeding
- Checks daemon responsiveness
- Verifies ESP is vfat
- Won't reboot if kernel/initrd are missing

**User-Friendly:**
- Clear progress messages with [OK]/[MISSING]/[ERROR] indicators
- Prompts before destructive actions
- Provides manual recovery commands if needed
- Writes installation receipt for reference

### Common Recovery Scenarios

**Scenario 1: time-machine completed but no kernel files**

```bash
# This is your situation - run the recovery script
/root/recovery-complete-install.sh

# It will detect missing kernel/initrd and re-run system init
# Then complete all post-install steps
```

**Scenario 2: Everything installed but password not set**

```bash
# Recovery script will detect this and only prompt for password
/root/recovery-complete-install.sh

# Or set password manually:
chroot /mnt /run/current-system/profile/bin/passwd username
```

**Scenario 3: Installation complete but customization tools missing**

```bash
# Recovery script detects installed system and skips to final steps
/root/recovery-complete-install.sh

# Or download manually after first boot
```

### What Makes It Safe

The recovery script is **completely idempotent** - you can run it multiple times safely:

- ✅ Won't repartition or reformat existing partitions
- ✅ Won't overwrite `/mnt/etc/config.scm` if it exists
- ✅ Skips system init if kernel/initrd already exist
- ✅ Asks before changing already-set passwords
- ✅ Skips downloads if files already present
- ✅ Won't unmount/reboot without confirmation

### For Developers

The recovery script is generated by `lib.WriteRecoveryScript()` and injected with the platform name at generation time. It's written before `guix system init` runs, so it's always available even if the installer crashes.

## 🔄 Idempotency Techniques

The installer implements comprehensive idempotency to allow safe reruns and recovery from failures.

### Partition Step Idempotency

**Device Unmounting:**

- Automatically detects if target device is mounted
- Unmounts all mount points in reverse order (deepest first)
- Uses `findmnt` with fallback to `mount` command parsing
- Employs lazy unmount (`umount -l`) for stubborn mounts

**Partition Existence Check:**

- Detects if partitions already exist using `lsblk`
- Skips partitioning if partitions are already present and formatted
- Verifies partition labels before proceeding

**Format Check:**

- Uses `blkid` to check if partitions are already formatted
- Supports both ext4 (root) and vfat (EFI) filesystems
- Skips formatting if partitions are already properly formatted

**Space Validation:**

- Checks device size before partitioning
- Warns if device is smaller than 40 GiB
- Displays actual device size for user awareness

### Mount Step Idempotency

**Mount Status Check:**

- Verifies if `/mnt` is already mounted using `lib.IsMounted()`
- Skips mount operations if already mounted

**Store Population Check:**

- Checks if `/mnt/gnu/store` has contents (more than 10 entries)
- Skips store copy if already populated
- Prevents unnecessary rsync/cp operations

**Label Verification:**

- Verifies required labels (`GUIX_ROOT`, `EFI`) exist before mounting
- Uses `lib.VerifyLabelsExist()` for consistent checking

### Config Step Idempotency

**Config File Check:**

- Skips config generation if `/mnt/etc/config.scm` already exists
- Provides clear message about skipping and how to regenerate
- Allows safe reruns without overwriting existing config

### System Init Step Idempotency

**Daemon Management:**

- Ensures daemon is running before config validation
- Includes robust restart mechanism with multiple fallback methods
- Tests daemon responsiveness before proceeding

**Installation Verification:**

- Verifies critical files were installed correctly
- Provides clear error messages if verification fails

### Safe Rerun Strategy

**Variable Persistence:**

- All environment variables persist between steps
- Skipping steps doesn't break downstream operations
- State information flows through all installation phases

**Graceful Degradation:**

- Each step can be run independently
- Failed steps can be rerun without affecting completed work
- Clear messaging about what's being skipped and why

**Recovery Instructions:**

- Each step provides clear instructions for manual recovery
- Error messages include specific commands to run
- Troubleshooting guidance for common failure scenarios

## 💻 Code Structure and Development

### Platform-Specific vs Shared Code

- **Platform-specific code**: Goes in `{platform}/install/*.go` files
  - Example: `framework-dual/install/01-partition.go`
  - Contains logic unique to that platform (partition layout, hardware detection, etc.)

- **Shared code for all platforms**: Goes in `lib/common.go`
  - Example: `DownloadCustomizationTools()`, `CreateSwapFile()`, `RunGuixSystemInit()`
  - Used by multiple platforms
  - Should accept parameters for platform-specific values (usernames, paths, etc.)

### When to Factor Code into common.go

Factor code into `lib/common.go` when:

1. The code is identical across 2+ platforms
2. The code performs a common operation (mounting, downloading, verification)
3. The code can be parameterized for platform differences

Keep code platform-specific when:

1. It's only used by one platform
2. The logic is fundamentally different per platform
3. Factoring would make the code harder to understand

### Adding New Scripts to the Repository

When you create a new shell script, determine if it's a **critical script** that should be:
1. Tracked in the manifest (`SOURCE_MANIFEST.txt`)
2. Copied to `/root/` by the bootstrap installer
3. Verified during installation

**Critical scripts are:**
- Scripts that users run directly from the Guix ISO
- Recovery/diagnostic tools needed when installation fails
- Scripts that modify system state or prepare for installation

**Examples of critical scripts:**
- `bootstrap-installer.sh` - Entry point for installation
- `clean-install.sh` - Prepares system for clean reinstall
- `verify-guix-install.sh` - Diagnostic tool for checking installation
- `recovery-complete-install.sh` - Recovery tool for completing partial installations

**Steps to add a new critical script:**

1. **Create the script** in the repository root (or appropriate subdirectory)
   ```bash
   vim my-new-script.sh
   chmod +x my-new-script.sh
   ```

   **CRITICAL**: Use the correct bash shebang for Guix ISO:
   ```bash
   #!/run/current-system/profile/bin/bash
   ```

   **Never use** `#!/usr/bin/env bash` or `#!/bin/bash` for critical scripts that run on the Guix ISO. The Guix ISO has bash at `/run/current-system/profile/bin/bash` and this path must be used for scripts to execute correctly.

2. **Add to `update-manifest.sh`** in the "Critical Shell Scripts" section:
   ```bash
   # My new script (description)
   hash=$(shasum -a 256 my-new-script.sh | awk '{print $1}')
   echo "$hash  my-new-script.sh" >> "$MANIFEST_FILE"
   ```

3. **Add to `bootstrap-installer.sh`** CRITICAL_SCRIPTS array:
   ```bash
   CRITICAL_SCRIPTS=(
       "clean-install.sh"
       "verify-guix-install.sh"
       "recovery-complete-install.sh"
       "my-new-script.sh"  # <-- Add your script here
   )
   ```

4. **Update the manifest**:
   ```bash
   ./update-manifest.sh
   ```

5. **Test the changes**:
   - Verify the script is in SOURCE_MANIFEST.txt
   - Check that bootstrap-installer.sh copies it to /root/
   - Confirm the manifest hash changed

6. **Commit everything together**:
   ```bash
   git add my-new-script.sh update-manifest.sh bootstrap-installer.sh SOURCE_MANIFEST.txt
   git commit -m "Add my-new-script.sh as critical script"
   ```

**Non-critical scripts** (like `update-manifest.sh` or `run-tests.sh`) are development tools that don't need to be in the manifest or copied to `/root/`. They stay in the repository for developers but aren't distributed to end users during installation.

---

**Remember:** The Guix installation is more forgiving than it seems. Most failures can be recovered by re-running the same command without rebooting.
