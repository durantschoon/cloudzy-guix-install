# cloudzy-guix-install

Status: this set of scripts was not completed (ran out of time, so I'm choosing a different path)

## ⚠️ CRITICAL WARNING

**THIS SCRIPT WILL DESTROY ALL DATA ON THE TARGET DEVICE!**

This installation script is designed for **fresh VPS instances** where you want to completely replace the existing system with Guix. It will:

- **Wipe the entire disk** and create new partitions
- **Destroy all existing data** on the target device
- **Replace the operating system** with Guix

### ✅ **Safe to Use When:**

- You have a **fresh VPS instance** with no important data
- You're running from a **Guix live ISO**
- You want to **completely replace** the existing system
- You're **100% certain** you're targeting the right device

### ❌ **DO NOT USE When:**

- You have **important data** on the target device
- You're running on a **production server** with existing data
- You're **unsure** which device will be targeted
- You want to **preserve** any existing data or system

### 🛡️ **Safety Features:**

- Script detects if you're not on a Guix live ISO
- Script warns if multiple filesystems are mounted (existing system)
- Script shows which device will be targeted before proceeding
- **10-second delay** when warnings are triggered (time to abort)

**If you're not absolutely certain you're in the right situation, STOP NOW!**

---

## Script Overview

This installation process consists of several scripts that work together to set up a complete Guix system:

- **`01-partition.sh`**: **DESTRUCTIVE OPERATION** - Automatically detects the primary storage device (supports /dev/sda, /dev/vda, /dev/xvda, /dev/nvme0n1, /dev/nvme1n1, /dev/sdb, /dev/vdb) or uses user-specified DEVICE environment variable. Includes safety warnings for wrong environments. Creates a GPT partition table with EFI boot partition (512MB) and root partition (remaining space). Validates device exists before proceeding and formats partitions.

- **`02-mount-bind.sh`**: Mounts the root partition, copies the Guix store from the ISO to the target system, and sets up bind mounts to redirect `/gnu` and `/var/guix` to the target filesystem. Validates that required device variables are set by previous scripts.

- **`03-config-write.sh`**: Generates a complete Guix system configuration file (`/mnt/etc/config.scm`) with user-provided variables (hostname, timezone, user account, etc.) and replaces placeholders with actual values. Automatically detects BIOS/UEFI boot mode and configures the appropriate bootloader. Supports configurable desktop environments (GNOME, Xfce, MATE, LXQt, or none for server mode). Validates all required environment variables are set before proceeding. Includes SSH service and essential packages.

- **`04-system-init.sh`**: Sets up configurable swap space (default 4G), configures Git for slow connections, pulls the specified Guix version, and initializes the new system. Validates that the configuration file exists before proceeding. This is the main installation step that creates the bootable Guix system.

- **`05-postinstall-console.sh`**: Post-installation script for console access. Sets the root password and starts the SSH daemon with clear instructions on how to connect remotely. Provides IP address discovery commands and SSH connection examples.

- **`06-postinstall-own-terminal.sh`**: Post-installation script for remote terminal access. Configures additional Guix channels (including nonguix for proprietary software) and performs a system reconfigure. Provides guidance on installing additional packages and examples of nonguix packages.

## Preparation

### On local machine

Where repo is cloned, get the shasum256 of the main `run-remote-steps.sh`

**IMPORTANT**: Always get the checksum from the same source you'll download from!

- **For testing/development**: Use `main` branch
- **For stable releases**: Use a specific tag (e.g., `v0.1.3`)

Substitute your command for pbcopy if not on a mac

To check and record on the iso instance:

```bash
# For testing latest changes (main branch)
shasum -a 256 run-remote-steps.sh | pbcopy

# For stable version (specific tag)
git checkout v0.1.3
shasum -a 256 run-remote-steps.sh | pbcopy
```

or instead prepare to manually check

```bash
shasum -a 256 run-remote-steps.sh
```

## Environment Variables

### DEVICE (auto-detected)

The `DEVICE` environment variable specifies the target storage device for installation. If not set, the script automatically detects from common VPS device names in order of preference:

- `/dev/sda` - Standard SATA/SCSI devices (most common)
- `/dev/vda` - Virtio block devices (KVM/QEMU)
- `/dev/xvda` - Xen virtual block devices
- `/dev/nvme0n1` - NVMe SSD devices (first drive)
- `/dev/nvme1n1` - NVMe SSD devices (second drive)
- `/dev/sdb` - Secondary SATA/SCSI devices
- `/dev/vdb` - Secondary Virtio block devices

You can override with: `DEVICE="/dev/sdb"`

### BOOT_MODE (auto-detected)

The `BOOT_MODE` environment variable specifies the boot mode for the system. If not set, the script automatically detects whether the system uses BIOS or UEFI boot:

- **UEFI**: Uses `grub-efi-bootloader` with `/boot/efi` target
- **BIOS**: Uses `grub-bootloader` with device target

You can override with: `BOOT_MODE="bios"` or `BOOT_MODE="uefi"`

### DESKTOP_ENV (default: gnome)

The `DESKTOP_ENV` environment variable specifies the desktop environment to install. If not set, defaults to GNOME:

- **`gnome`** (default) - Full-featured, modern desktop environment
- **`xfce`** - Lightweight, fast, traditional desktop feel
- **`mate`** - Classic GNOME 2 experience, traditional layout
- **`lxqt`** - Very lightweight, minimal resource usage
- **`none`** - Server mode, no desktop environment

You can override with: `DESKTOP_ENV="xfce"` or `DESKTOP_ENV="none"`

### SWAP_SIZE (default: 4G)

The `SWAP_SIZE` environment variable controls the size of the swap file created during installation. The default of 4G is chosen because:

- **VPS Compatibility**: Covers most VPS memory configurations (1-8GB RAM)
- **Guix Requirements**: Guix package compilation and system updates are memory-intensive
- **Modern Best Practices**: 2-4GB is recommended for systems with 1-8GB RAM
- **Safety Margin**: Provides headroom for memory spikes and system stability
- **Hibernation Support**: Allows for suspend-to-disk if needed

You can customize the swap size using formats like:

- `SWAP_SIZE="2G"` - 2 gigabytes
- `SWAP_SIZE="512M"` - 512 megabytes  
- `SWAP_SIZE="8192K"` - 8192 kilobytes

## Quick Start

**⚠️ READ THE WARNING ABOVE FIRST! This script will destroy all data on the target device.**

### 1. Prepare Guix Live ISO Environment

First, install required packages in the Guix live ISO (perl is needed for shasum)

```bash
guix install curl
guix install perl
```

### 2. Set Environment Variables

First, configure your installation variables (the second group contains optional variables (with defaults)):

```bash
# Required variables
USER_NAME="YOUR_USER_NAME"
FULL_NAME="YOUR_FULL_NAME"
TIMEZONE="America/New_York"
HOST_NAME="guix-vps"

DESKTOP_ENV="gnome"
BOOT_MODE="uefi"
SWAP_SIZE="4G"
```

#### Options

```bash
DESKTOP_ENV="gnome"     # Options: gnome, xfce, mate, lxqt, none
BOOT_MODE="uefi"        # Options: uefi, bios (auto-detected)
SWAP_SIZE="4G"          # Options: 2G, 4G, 8G, etc.
```

### 3. Download and Verify Script

**Choose the right URL based on what you're testing:**

```bash
# For testing latest changes (main branch)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh

# For stable version (specific tag)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/v0.1.3/run-remote-steps.sh -o run-remote-steps.sh
```

**Pro tip: Use environment variable for easy switching:**

```bash
# For debugging/development
GUIX_INSTALL_REF=main bash ./run-remote-steps.sh

# For stable version
GUIX_INSTALL_REF=v0.1.3 bash ./run-remote-steps.sh
```

### 4. Verify with Checksum

**Option A: Automated verification (recommended):**

```bash
echo " PASTE-YOUR-SHASUM-HERE-WITH-NO-SPACES-INSIDE-THESE-QUOTES-SINGLE-NEWLINE-IS-OK " | head -1 > rrs-checksum.txt
cat rrs-checksum.txt
shasum -a 256 -c rrs-checksum.txt

chmod +x run-remote-steps.sh
# GUIX_INSTALL_REF=main bash ./run-remote-steps.sh
# GUIX_INSTALL_REF=vX.Y.Z bash ./run-remote-steps.sh
```

**Option B: Manual verification:**

```bash
shasum -a 256 run-remote-steps.sh
# Compare with your expected checksum

chmod +x run-remote-steps.sh
# GUIX_INSTALL_REF=main bash ./run-remote-steps.sh
# GUIX_INSTALL_REF=vX.Y.Z bash ./run-remote-steps.sh
```

### Troubleshooting: CDN Cache Issues

If you're getting a different checksum than expected (especially across different regions), use:

```bash
# Add timestamp to bypass cache (most reliable method)
curl -fsSL "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh?$(date +%s)" -o run-remote-steps.sh
```

### Development Tips: Working with 'main' branch

When developing or debugging, you'll often want to use the latest code from the `main` branch. Here are some helpful aliases and workflows:

```bash
# Set up aliases for easier development
alias run='GUIX_INSTALL_REF=main bash ./run-remote-steps.sh'
alias recurl='curl -fsSL "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh?$(date +%s)" -o run-remote-steps.sh'

# Quick workflow for testing changes:
# 1. Make changes to scripts locally
# 2. Update SHA256 checksums: ./update-sha256.sh
# 3. Commit and push changes
# 4. Test with: recurl && run
```

**Pro tip:** Keep a local file with the current SHA256 checksums for the `main` branch so you can quickly update them when testing changes.

**Why the timestamp trick works:**

- GitHub's CDN caches content by URL
- Adding `?$(date +%s)` creates a unique URL each time
- This forces the CDN to fetch fresh content instead of serving cached version

**If it still doesn't work:**

- CDN propagation can take 2-3 minutes across different regions
- Keep retrying every minute or so
- The timestamp method will eventually work once the CDN updates

## Version Anticipation Workflow

**IMPORTANT: Only tag versions that are tested and working!**

When making changes that affect the scripts:

1. **Make your changes** on the `main` branch
2. **Test thoroughly** in the target environment (Guix ISO)
3. **Update SHA256 checksums** in `run-remote-steps.sh`:

   ```bash
   shasum -a 256 *.sh
   ```

4. **Only when you have a working version:**
   - Anticipate the next version in `run-remote-steps.sh`:
     - Change `REF="v0.1.3"` to `REF="v0.1.4"` (or next version)
   - Commit the changes:

     ```bash
     git add .
     git commit -m "Description of changes"
     ```

   - Create the anticipated tag:

     ```bash
     git tag -a v0.1.4 -m "Description of changes"
     ```

   - Push everything:

     ```bash
     git push origin main
     git push origin v0.1.4
     ```

**For development/testing:**

- Use `REF="main"` during development
- Only switch to a specific tag when you have a stable, tested version

### Why This Works

- **No chicken-and-egg problem**: The script points to a tag that includes the script itself
- **Stable releases**: Tags are immutable, so users get consistent behavior
- **Clean development**: Continue working on `main` without affecting stable versions
- **Self-referential**: Each tag contains the script pointing to itself

### Current Stable Version

- **Tag**: `v0.1.3`
- **Features**: Safety flags (`set -euo pipefail`) on all scripts
- **Status**: Production ready

### Latest Development Version (main branch)

- **Checksum**: `c5abb8ca797bdd6dcefa9acf03062d47a6e18555215f5eebe259e9581616a8c3`
- **Features**: Latest fixes including race condition improvements, variable sharing between scripts, user variable setup
- **Status**: Testing/development
