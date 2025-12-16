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
# For maximum verbosity: VERBOSE=1 bash verify-guix-install.sh

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

**🎯 ROOT CAUSE IDENTIFIED (2025-11-07):** `guix system init` creates broken system generation when it cannot resolve the nonguix `linux` kernel package. The installation appears to succeed but creates a broken symlink at `/mnt/run/current-system` and no kernel files.

### Diagnosis

```bash
# Check if system generation was created properly
ls -la /mnt/run/current-system        # Should be valid symlink
readlink /mnt/run/current-system      # Should point to existing directory

# Check if files exist in /boot
ls -lh /mnt/boot/vmlinuz*
ls -lh /mnt/boot/initrd*

# Check if kernel package is in store
find /gnu/store -name "vmlinuz*" -o -name "initrd*" | grep -v "test"

# Check disk space (should have plenty)
df -h /mnt

# Check for errors in log
tail -200 /tmp/guix-install.log | grep -i "error\|fail"
```

### Root Cause

**Broken System Generation** - Guix silently fails when nonguix kernel package cannot be resolved:

1. `guix time-machine` runs with nonguix channel configured
2. `system init` attempts to build system with `(kernel linux)` from nonguix
3. **Kernel package resolution fails** (substitute unavailable or channel issue)
4. Guix creates system generation anyway, but without kernel
5. `/mnt/run/current-system` becomes a **broken symlink**
6. GRUB installed successfully but references non-existent kernel
7. **No error reported** - installation appears to succeed

**Key diagnostic:** If `/mnt/run/current-system` is a broken symlink, the system generation was not built correctly.

### Previously Suspected Causes (Ruled Out)

1. ~~**cow-store not working**~~ - Not the issue (73GB free on /mnt confirms cow-store working)
2. ~~**Disk space exhaustion**~~ - Not the issue (plenty of space available)
3. ~~**Build failure with errors**~~ - Not the issue (no errors in log)
4. ~~**Substitute server down**~~ - Channel configured correctly

### Solution

**✅ FIXED (2025-11-08):** This issue is permanently resolved in the latest installer.

**For users with installer versions after commit `b956baf`:**
- The installer automatically uses the correct 3-step approach
- No manual intervention needed
- Kernel and initrd will be properly installed to /boot

**How the fix works:**

The installer now does this automatically:

1. **Build system:** `guix time-machine -C /tmp/channels.scm -- system build /mnt/etc/config.scm`
   - Creates complete system in `/gnu/store/*-system` with kernel and initrd

2. **Copy kernel files to /boot:**
   ```bash
   cp /gnu/store/*-system/kernel /mnt/boot/vmlinuz
   cp /gnu/store/*-system/initrd /mnt/boot/initrd
   ln -s /gnu/store/*-system /mnt/run/current-system
   ```

3. **Install bootloader:** `guix time-machine -C /tmp/channels.scm -- system init /mnt/etc/config.scm /mnt`
   - System already built, this just installs GRUB

**For users with older installer versions:**

If you're stuck with missing kernel files, use this manual workaround:

```bash
# Step 1: Build the system
cd /root
guix time-machine -C /tmp/channels.scm -- system build /mnt/etc/config.scm

# Step 2: Find the built system and copy kernel files
SYSTEM=$(ls -td /gnu/store/*-system 2>/dev/null | head -1)
cp $SYSTEM/kernel /mnt/boot/vmlinuz
cp $SYSTEM/initrd /mnt/boot/initrd
ln -s $SYSTEM /mnt/run/current-system

# Step 3: Verify and reboot
bash verify-guix-install.sh
# For maximum verbosity: VERBOSE=1 bash verify-guix-install.sh
reboot
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
sudo herd start network-manager
```

**Note:** NetworkManager takes over all network interfaces, including wired ethernet. If you were using `dhclient` manually, NetworkManager will replace it.

### NetworkManager Not Starting

**Symptom:** NetworkManager service fails to start after reconfigure

**Diagnosis:**
```bash
# Check service status
sudo herd status network-manager

# Check service logs
sudo herd log network-manager

# Try starting manually
sudo herd start network-manager

# Check for errors in system journal
journalctl -u network-manager
```

**Common Issues:**
- Service may need a reboot after first installation
- Check config.scm syntax: `guix system build /etc/config.scm`
- Verify `(gnu services networking)` is in use-modules

### WiFi Devices Not Detected

**Symptom:** `nmcli device wifi list` shows no WiFi devices

**Diagnosis:**
```bash
# Check if firmware is loaded
dmesg | grep -i firmware

# Check if WiFi device exists
ip link show

# Should see something like wlp1s0 or similar
# If no wlan/wlp interface, firmware may be missing
```

**Solution:**
- Ensure `linux-firmware` is in your config.scm:
  ```scheme
  (firmware (list linux-firmware))
  ```
- Reconfigure and reboot
- On Framework 13, firmware requires nonguix channel

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
   - For maximum verbosity: `VERBOSE=1 bash verify-guix-install.sh`
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

## Guix Daemon Issues

### Daemon Not Responding

**Symptom:** "Connection refused" or "cannot connect to daemon socket" errors

**Diagnosis:**
```bash
# Check if daemon is running
herd status guix-daemon

# Check socket exists
ls -l /var/guix/daemon-socket/socket
```

**Solution:**
```bash
# Restart daemon
herd restart guix-daemon

# Or manually start
guix-daemon --build-users-group=guixbuild &

# Verify it's working
guix describe
```

### Daemon Connection Timeout

**Symptom:** Commands hang waiting for daemon

**Solution:**
```bash
# Kill hung daemon
kill all guix-daemon

# Clear any stale locks
rm -f /var/guix/daemon-socket/socket.lock

# Restart fresh
herd start guix-daemon
```

### Cloudzy: Daemon Not Started After Installation (Before Reboot)

**Symptom:** Installation completes but `guix-daemon` is not running, causing issues after reboot

**When this happens:** After Step 4 (System Init) completes on Cloudzy VPS installations

**Root Cause:** The daemon is stopped during Step 2 (Mount) to copy the store, and may not restart properly before the installer completes and reboots.

**Solution:**
```bash
# Before rebooting, exit the installer (Ctrl+C or let it finish)
# Then manually start the daemon:

herd start guix-daemon

# Verify it's running
herd status guix-daemon
guix build --version

# Now safe to reboot
reboot
```

**Why this matters:** The installed system needs the daemon running to function properly. If the daemon isn't started before reboot, you may encounter issues after first boot.

**Note:** This is specific to Cloudzy VPS installations. Framework installers handle daemon startup differently.

---

## Corrupted Derivation Files

### Symptom: "error parsing derivation ... expected string 'Derive(['"

**What happened:** A derivation file in `/gnu/store` is corrupted, causing `guix system reconfigure` to fail.

**⚠️ BEFORE DELETING ALL GENERATIONS** - Try these less destructive options first:

#### Step 1: Find What's Referencing the Corrupted Derivation

```bash
# Find what's keeping the derivation "alive"
sudo guix gc --referrers /gnu/store/...-corrupted-file.drv

# This shows what's referencing it (system generation, user profile, etc.)
```

#### Step 2: Try to Repair the Corruption

```bash
# Attempt to repair corrupted store items
sudo guix gc --verify=contents,repair

# This may fix the corruption without needing to rebuild
```

#### Step 3: Check User Profiles (Not Just System Generations)

```bash
# List user profile generations that might reference it
guix package --list-generations

# Delete old user profile generations
guix package --delete-generations=1m  # Delete older than 1 month

# Then try garbage collection
sudo guix gc
```

#### Step 4: Restart Daemon (Clears Cache)

```bash
# Daemon might have corrupted derivation cached
herd restart guix-daemon

# Wait a moment, then try reconfigure again
sudo guix system reconfigure /etc/config.scm
```

#### Step 5: Force Rebuild with --fallback

```bash
# Force rebuild even if substitutes fail (may bypass corrupted derivation)
sudo guix system reconfigure /etc/config.scm --fallback

# Or use --no-grafts to avoid graft derivations
sudo guix system reconfigure /etc/config.scm --no-grafts
```

#### Step 6: Remove Only Old System Generations (Not Current)

```bash
# List all system generations
sudo guix system list-generations

# Delete only old ones (keep current)
sudo guix system delete-generations 1d  # Delete older than 1 day

# Or delete specific old generation
sudo guix system delete-generations <generation-number>

# Then try deleting the corrupted derivation
sudo guix gc --delete /gnu/store/...-corrupted-file.drv
```

#### Step 7: Manual Fix (Advanced - Only if Derivation File is Trivially Corrupted)

```bash
# If it's just a parsing error, you might be able to manually fix the .drv file
# WARNING: This is risky and may cause inconsistencies

# First, make a backup
sudo cp /gnu/store/...-corrupted-file.drv /gnu/store/...-corrupted-file.drv.backup

# Check what's wrong with it
cat /gnu/store/...-corrupted-file.drv | head -20

# If it's just missing a closing bracket or similar, you might fix it
# But this is NOT recommended - better to rebuild
```

#### Step 8: Last Resort - Delete All Generations

**Only if all above steps fail:**

```bash
# Delete all system generations (removes rollback capability)
sudo guix system delete-generations

# Delete all user profile generations
guix package --delete-generations

# Run garbage collection
sudo guix gc

# Now try to delete corrupted derivation
sudo guix gc --delete /gnu/store/...-corrupted-file.drv

# Rebuild system (will download/build everything again)
sudo guix system reconfigure /etc/config.scm
```

**Why deleting all generations worked:** The corrupted derivation was referenced by the current system generation (which you can't delete). By deleting all generations, you removed all references, allowing the corrupted derivation to be garbage collected. However, this forces a complete rebuild.

**Better approach:** If `guix gc --referrers` shows it's referenced by the current generation, you can:
1. Make a small change to `config.scm` (add a comment, change a service option)
2. Run `guix system reconfigure` - this creates a NEW generation
3. The old generation (with corrupted derivation) can then be deleted
4. The corrupted derivation will be garbage collected

**Prevention:**
- Run `guix gc --verify=contents` periodically to catch corruption early
- Keep multiple generations for rollback (don't delete all at once)
- Use `guix gc --verify=repair` if corruption is detected

---

## Cow-Store Issues

### Cow-Store Not Redirecting

**Symptom:** "No space left on device" during `guix system init`

**Diagnosis:**
```bash
# Check cow-store status
herd status cow-store

# Check where writes are going
df -h /gnu/store
df -h /mnt
```

**Solution:**
```bash
# Ensure /mnt is mounted
mount | grep /mnt

# Start cow-store pointing to target
herd start cow-store /mnt

# Verify it started
herd status cow-store

# Check overlay is active
mount | grep overlay
```

### Store Writes Filling ISO

**Symptom:** ISO tmpfs full, but target disk has space

**Root cause:** cow-store not working, writes going to ISO RAM

**Solution:** Use [pre-build kernel workaround](PREBUILD_KERNEL.md) or:

```bash
# Stop any guix processes
killall guix-daemon
herd stop guix-daemon

# Ensure clean mount state
umount -R /mnt || true
mount /dev/nvme0n1p2 /mnt
mount /dev/nvme0n1p1 /mnt/boot/efi

# Restart daemon
herd start guix-daemon

# Start cow-store fresh
herd start cow-store /mnt

# Retry system init
guix time-machine -C /tmp/channels.scm -- system init /mnt/etc/config.scm /mnt
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
