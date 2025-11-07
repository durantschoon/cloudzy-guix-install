# Troubleshooting Guide

This guide covers common installation problems, how to diagnose them, and recovery procedures.

## Table of Contents

- [Installation Logs and Receipts](#installation-logs-and-receipts)
- [Common Installation Failures](#common-installation-failures)
- [Recovery and Repair Procedures](#recovery-and-repair-procedures)
- [Chroot into Installed System](#chroot-into-installed-system)
- [Missing Kernel/Initrd Files](#missing-kernelinitrd-files)
- [GRUB Bootloader Issues](#grub-bootloader-issues)
- [Nonguix Channel Problems](#nonguix-channel-problems)
- [Network Connectivity Issues](#network-connectivity-issues)
- [Verification Failures](#verification-failures)

---

## Installation Logs and Receipts

### Where to Find Logs

**During installation (on Guix ISO):**
```bash
# Main installation log
/tmp/guix-install.log

# Guix daemon log
journalctl -u guix-daemon

# Recent system messages
dmesg | tail -100
```

**After installation (on installed system):**
```bash
# Installation receipt (created during install)
/root/guix-install-receipt.txt

# System logs
/var/log/messages
/var/log/guix/

# Boot logs
journalctl -b
```

### Installation Receipt Contents

The installer writes `/root/guix-install-receipt.txt` containing:
- Installation timestamp
- Platform (framework, framework-dual, cloudzy, etc.)
- Device and partition information
- Guix version and channel commits
- Configuration file snapshot
- User account information

This file is essential for troubleshooting and reproducing installations.

---

## Common Installation Failures

### 1. Bootstrap Checksum Mismatch

**Symptom:**
```
[ERROR] Checksum mismatch for file: lib/common.go
Expected: abc123...
Got:      def456...
```

**Cause:** GitHub CDN served stale content

**Solution:**
```bash
# Wait 5 minutes for CDN to update, then retry
# Or manually verify manifest hash matches documentation
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt | shasum -a 256
```

### 2. Guix System Init Hangs

**Symptom:** Spinner runs for hours with no progress

**Diagnosis:**
```bash
# Check if log is growing (build progressing)
watch -n 5 'ls -lh /tmp/guix-install.log'

# Check what guix is doing
ps aux | grep guix

# Check for errors in log
tail -100 /tmp/guix-install.log | grep -i "error\|fail"

# Check disk space
df -h
df -h /mnt
```

**Solutions:**
- If log is growing: Wait (build takes 30-120 minutes)
- If log stopped: Check for errors, may need to retry
- If disk full: Use [pre-build workaround](PREBUILD_KERNEL.md)

### 3. Missing vmlinuz/initrd Files

**Symptom:** Verification warns files missing from `/mnt/boot/`

**See:** [Missing Kernel/Initrd Files](#missing-kernelinitrd-files) section below

### 4. Partition Not Found

**Symptom:**
```
[ERROR] Could not find EFI partition
[ERROR] Could not find GUIX_ROOT partition
```

**Diagnosis:**
```bash
# Check partition labels
lsblk -f

# Check partition types
fdisk -l

# Verify partitions exist
ls -l /dev/nvme0n1*  # or /dev/sda* etc.
```

**Solution:**
```bash
# Re-label partitions if needed
e2label /dev/nvme0n1p2 GUIX_ROOT
fatlabel /dev/nvme0n1p1 EFI

# Or set environment variables manually
export ROOT=/dev/nvme0n1p2
export EFI=/dev/nvme0n1p1
```

---

## Recovery and Repair Procedures

### Using the Recovery Script

The installer automatically creates `/root/recovery-complete-install.sh`:

```bash
# On Guix ISO, if system init succeeded but post-install failed:
/root/recovery-complete-install.sh
```

This script:
1. Re-mounts partitions
2. Reruns verification
3. Sets user password
4. Downloads customization tools
5. Creates installation receipt

### Manual Recovery Steps

If the recovery script doesn't exist or fails:

```bash
# 1. Mount partitions
mount LABEL=GUIX_ROOT /mnt
mkdir -p /mnt/boot/efi
mount LABEL=EFI /mnt/boot/efi

# 2. Verify installation
bash verify-guix-install.sh

# 3. Set user password (replace 'username' with your user)
chroot /mnt /run/current-system/profile/bin/bash
passwd username
exit

# 4. Write receipt
cat > /mnt/root/guix-install-receipt.txt <<EOF
Installation Date: $(date)
Platform: framework-dual
User: username
Config: /etc/config.scm
EOF

# 5. Unmount and reboot
sync
umount -R /mnt
reboot
```

---

## Chroot into Installed System

### From Guix ISO

```bash
# Mount the installed system
mount LABEL=GUIX_ROOT /mnt
mount LABEL=EFI /mnt/boot/efi

# Chroot into it
chroot /mnt /run/current-system/profile/bin/bash

# Now you're inside the installed system
# You can run commands, fix config, etc.

# Exit chroot
exit
```

### Common Chroot Tasks

**1. Reset user password:**
```bash
chroot /mnt /run/current-system/profile/bin/bash
passwd username
exit
```

**2. Edit configuration:**
```bash
chroot /mnt /run/current-system/profile/bin/bash
nano /etc/config.scm
exit
```

**3. Reinstall bootloader:**
```bash
chroot /mnt /run/current-system/profile/bin/bash
guix system reconfigure /etc/config.scm
exit
```

**4. Check what's installed:**
```bash
chroot /mnt /run/current-system/profile/bin/bash
guix package -I
ls -l /run/current-system
exit
```

---

## Missing Kernel/Initrd Files

**Critical Issue:** System won't boot without kernel and initrd in `/boot/`

### Diagnosis

```bash
# Check if files exist
ls -lh /mnt/boot/vmlinuz*
ls -lh /mnt/boot/initrd*

# Should see something like:
# /mnt/boot/vmlinuz-6.1.82-gnu
# /mnt/boot/initrd-6.1.82-gnu

# Check if kernel is in store
find /gnu/store -name "vmlinuz*"
find /gnu/store -name "initrd*"
```

### Possible Causes

1. **cow-store not working** - Store writes went to ISO tmpfs instead of /mnt
2. **Disk space exhaustion** - Build failed due to no space
3. **Build failure** - Kernel compilation errors
4. **Substitute unavailable** - Can't download pre-built kernel

### Solutions

#### Option 1: Use Pre-built Kernel (Recommended)

See [PREBUILD_KERNEL.md](PREBUILD_KERNEL.md) for complete instructions.

```bash
# Pre-build on Mac with Docker, transfer to ISO, then:
guix archive --import < /root/linux-kernel.nar

# Retry system init
/root/guix-init-time-machine.sh
```

#### Option 2: Retry with More Space

```bash
# Free up space
rm -rf /tmp/*
rm -rf /var/guix/substitute-cache/*

# Ensure TMPDIR points to target disk
export TMPDIR=/mnt/var/tmp
export XDG_CACHE_HOME=/mnt/var/cache

# Verify cow-store is running
herd status cow-store

# If not running, start it
herd start cow-store /mnt

# Retry system init
/root/guix-init-time-machine.sh
```

#### Option 3: Manual Kernel Installation

If kernel was built but not copied to /boot:

```bash
# Find kernel in store
KERNEL=$(find /gnu/store -name "vmlinuz*" | head -1)
INITRD=$(find /gnu/store -name "initrd*" | grep -v "\.drv$" | head -1)

# Copy to /boot
cp $KERNEL /mnt/boot/
cp $INITRD /mnt/boot/

# Update GRUB
chroot /mnt /run/current-system/profile/bin/bash
grub-mkconfig -o /boot/grub/grub.cfg
exit
```

---

## GRUB Bootloader Issues

### GRUB Not Installed

**Symptom:** No EFI binary at `/mnt/boot/efi/EFI/guix/grubx64.efi`

**Solution:**
```bash
# Reinstall GRUB
chroot /mnt /run/current-system/profile/bin/bash
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=guix
grub-mkconfig -o /boot/grub/grub.cfg
exit
```

### GRUB Config Missing

**Symptom:** No `/mnt/boot/grub/grub.cfg`

**Solution:**
```bash
chroot /mnt /run/current-system/profile/bin/bash
grub-mkconfig -o /boot/grub/grub.cfg
exit
```

### Dual-Boot Not Detecting Other OS

**Symptom:** Only see Guix in boot menu, Pop!_OS missing

**Solution:**
```bash
chroot /mnt /run/current-system/profile/bin/bash

# Enable os-prober
echo 'GRUB_DISABLE_OS_PROBER=false' >> /etc/default/grub

# Regenerate config
grub-mkconfig -o /boot/grub/grub.cfg

# Check output for "Found Pop!_OS..."
exit
```

### Wrong Boot Mode (UEFI vs BIOS)

**Symptom:** GRUB installed for wrong mode

**Diagnosis:**
```bash
# Check current boot mode
[ -d /sys/firmware/efi ] && echo "UEFI" || echo "BIOS"

# Check what GRUB expects in config
grep bootloader /mnt/etc/config.scm
```

**Solution:** Edit `/mnt/etc/config.scm` to match boot mode:

```scheme
;; For UEFI:
(bootloader grub-efi-bootloader)
(targets '("/boot/efi"))

;; For BIOS:
(bootloader grub-bootloader)
(targets '("/dev/nvme0n1"))  ;; Use device, not partition
```

Then reconfigure:
```bash
chroot /mnt /run/current-system/profile/bin/bash
guix system reconfigure /etc/config.scm
exit
```

---

## Nonguix Channel Problems

### Nonguix Trust Prompt Not Appearing

**Symptom:** Installation proceeds but nonguix packages missing

**Diagnosis:**
```bash
# Check if channels.scm includes nonguix
cat /tmp/channels.scm

# Should contain nonguix channel with introduction
```

**Solution:**
```bash
# Manually setup nonguix
guix describe  # Note current commit

# Pull with nonguix
guix pull --url=https://git.savannah.gnu.org/git/guix.git

# Retry installation
```

### Nonguix Substitute Server Down

**Symptom:** Build hangs downloading from substitutes.nonguix.org

**Diagnosis:**
```bash
# Test connectivity
wget -q --spider https://substitutes.nonguix.org && echo "UP" || echo "DOWN"

# Check substitute availability
guix weather linux
```

**Solution:** Use pre-build workaround (see [PREBUILD_KERNEL.md](PREBUILD_KERNEL.md))

---

## Network Connectivity Issues

### No Network After Boot

**Symptom:** Can't run `guix pull` or access internet

**Diagnosis:**
```bash
# Check network interfaces
ip link show

# Check if NetworkManager running
herd status

# Try to ping
ping -c 3 8.8.8.8
```

**Solution:**

Add NetworkManager to config and reconfigure:

```scheme
;; In /etc/config.scm
(use-modules (gnu)
             (gnu services networking))

(operating-system
  ;; ... other fields ...
  (services
   (cons* (service network-manager-service-type)
          %base-services)))
```

```bash
sudo guix system reconfigure /etc/config.scm
sudo herd start networking
```

### DNS Not Working

**Symptom:** Can ping IPs but not domains

**Solution:**
```bash
# Check resolv.conf
cat /etc/resolv.conf

# Add nameserver manually
echo "nameserver 8.8.8.8" | sudo tee /etc/resolv.conf

# Or use NetworkManager's DNS
sudo systemctl restart NetworkManager
```

---

## Verification Failures

### What Verification Checks

The `verify-guix-install.sh` script checks:
1. ✅ Kernel files exist (`/boot/vmlinuz*`)
2. ✅ Initrd files exist (`/boot/initrd*`)
3. ✅ GRUB EFI binary exists (`/boot/efi/EFI/guix/grubx64.efi`)
4. ✅ GRUB config exists (`/boot/grub/grub.cfg`)
5. ✅ Config file exists (`/etc/config.scm`)

### Failed Verification: Missing Files

**If ANY files missing:**

1. DO NOT REBOOT - System won't be bootable
2. Check logs for errors: `tail -100 /tmp/guix-install.log`
3. Follow relevant section above to fix missing files
4. Re-run verification: `bash verify-guix-install.sh`
5. Only reboot when verification passes

### Manual Verification

```bash
# Mount system
mount LABEL=GUIX_ROOT /mnt
mount LABEL=EFI /mnt/boot/efi

# Check critical files
ls -lh /mnt/boot/vmlinuz* || echo "MISSING KERNEL"
ls -lh /mnt/boot/initrd* || echo "MISSING INITRD"
ls -lh /mnt/boot/efi/EFI/guix/grubx64.efi || echo "MISSING GRUB EFI"
ls -lh /mnt/boot/grub/grub.cfg || echo "MISSING GRUB CONFIG"
ls -lh /mnt/etc/config.scm || echo "MISSING CONFIG"

# All files must exist for successful boot
```

---

## Quick Reference: Common Commands

### Mounting

```bash
# Mount by label (preferred)
mount LABEL=GUIX_ROOT /mnt
mount LABEL=EFI /mnt/boot/efi

# Mount by device
mount /dev/nvme0n1p2 /mnt
mount /dev/nvme0n1p1 /mnt/boot/efi

# Unmount
umount -R /mnt
```

### Chroot

```bash
# Enter installed system
chroot /mnt /run/current-system/profile/bin/bash

# Common chroot tasks
passwd username              # Reset password
nano /etc/config.scm         # Edit config
guix system reconfigure /etc/config.scm  # Apply changes
grub-mkconfig -o /boot/grub/grub.cfg    # Rebuild GRUB
exit                         # Leave chroot
```

### Guix Commands

```bash
# Check Guix version
guix describe

# List installed packages
guix package -I

# Update Guix
guix pull
guix package -u

# Reconfigure system
sudo guix system reconfigure /etc/config.scm

# Roll back system
sudo guix system roll-back

# Check system generations
sudo guix system list-generations
```

### Logs

```bash
# Installation log
tail -f /tmp/guix-install.log

# System logs
journalctl -xe
journalctl -u guix-daemon
dmesg | tail -100

# Boot logs
journalctl -b
```

---

## Getting Help

If you've tried the above and still have issues:

1. **Check logs** - Look for specific error messages
2. **Search issues** - https://github.com/durantschoon/cloudzy-guix-install/issues
3. **Guix help** - https://guix.gnu.org/help/
4. **File issue** - Include:
   - Platform (framework, framework-dual, etc.)
   - Installation log excerpt
   - Output of `guix describe`
   - Verification results

---

## Related Documentation

- [CHECKLIST.md](CHECKLIST.md) - Current development status and known issues
- [PREBUILD_KERNEL.md](PREBUILD_KERNEL.md) - Pre-building kernel workaround
- [INSTALLATION_KNOWLEDGE.md](INSTALLATION_KNOWLEDGE.md) - Detailed installation reference
- [README.md](README.md) - Project overview
