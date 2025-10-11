# Guix OS Installation: Hard-Won Knowledge

This document captures critical lessons learned from real-world Guix OS installations, particularly focusing on dual-boot scenarios and common pitfalls.

## 🧠 The Golden Rule: cow-store

**Always run `herd start cow-store /mnt` before `guix system init`.**

- This redirects Guix store writes to the target disk while keeping the live installer's `/gnu/store` usable
- **Never use `mount --bind /mnt/gnu /gnu`** — this shadows the live system's store and breaks the `guix` command
- Without cow-store, the ISO's limited tmpfs fills up and the installation fails

## 🧩 Partitioning & Filesystems

### Partition Table Requirements

- Use GPT partition table
- EFI System Partition (ESP): FAT32, 512MB-1GB, flags: `boot,esp`
- Root partition: ext4, labeled for reliability

### Label Convention

Use **UPPERCASE with underscores** for all partition labels:

- `EFI` → FAT32 ESP → mounted at `/boot/efi`
- `GUIX_ROOT` → ext4 → mounted at `/`
- `DATA` (optional) → ext4/btrfs → mounted at `/home` or `/data`

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
/mnt/boot/vmlinuz-*              # Kernel
/mnt/boot/initrd-*               # Initial RAM disk
/mnt/boot/grub/grub.cfg          # GRUB config
/mnt/boot/efi/EFI/guix/grubx64.efi
/mnt/boot/efi/EFI/guix/grub.cfg
```

**Warning signs:**

- Only `grubx64.efi` present (no `grub.cfg` or kernel) → Installation incomplete, re-run
- Empty `/boot/efi` before init → ✅ Normal
- Empty `/boot/efi` after init → ❌ Installation failed

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
#!/usr/bin/env bash

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
```

---

**Remember:** The Guix installation is more forgiving than it seems. Most failures can be recovered by re-running the same command without rebooting.
