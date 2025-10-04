# Guix Installation Scripts

Modular scripts for installing minimal Guix OS on different platforms:

- **`cloudzy/`** - VPS fresh install (Cloudzy and similar providers)
- **`framework/`** - Framework 13 single-boot (Guix only)
- **`framework-dual/`** - Framework 13 dual-boot with Pop!_OS
- **`raspberry-pi/`** - Raspberry Pi 4 (aarch64, experimental)

## Quick Navigation

- ⚡ **Quick Start Guide**: See [`QUICKSTART.md`](QUICKSTART.md) - Complete workflow from ISO to customized system
- 🎨 **Customization Guide**: See [`CUSTOMIZATION.md`](CUSTOMIZATION.md) - Add features after minimal install
- 🌍 **Mirror Configuration**: See [`lib/mirrors.md`](lib/mirrors.md) - Optimize download speeds globally
- 🚀 **VPS Installation**: See [`cloudzy/README.md`](cloudzy/README.md)
- 💻 **Framework 13 Single-Boot**: See [`framework/README.md`](framework/README.md)
- 💻 **Framework 13 Dual-Boot**: See [`framework-dual/README.md`](framework-dual/README.md)
- 🥧 **Raspberry Pi 4** (Experimental): See [`raspberry-pi/README.md`](raspberry-pi/README.md)

---

## Installation Philosophy

**Minimal First, Customize Later**

All installation scripts create a **truly minimal** bootable Guix system:
- ✅ Base system packages only
- ✅ User account with sudo access
- ❌ No desktop environment
- ❌ No SSH server
- ❌ No extra packages

**Why?** Get a working system fast (~10 min), boot into it (free of ISO!), then customize for your exact needs.

**Workflow:**
1. Install minimal Guix from ISO (scripts 01-04)
2. Boot into installed system
3. Customize using `guix-customize` tool or manual config edits

See [`QUICKSTART.md`](QUICKSTART.md) for complete workflow.

---

## Installation Types

### 1. VPS Fresh Install (Cloudzy)

**⚠️ THIS WILL DESTROY ALL DATA ON THE TARGET DEVICE!**

Designed for **fresh VPS instances** where you want to completely replace the existing system with Guix. It will:

- **Wipe the entire disk** and create new partitions
- **Destroy all existing data** on the target device
- **Replace the operating system** with minimal Guix
- **No SSH/desktop** until you customize after boot

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

**Installation:** See [`cloudzy/README.md`](cloudzy/README.md)
**After Boot:** See [`CUSTOMIZATION.md`](CUSTOMIZATION.md) to add SSH/packages

---

### 2. Framework 13 Single-Boot

**⚠️ THIS WILL DESTROY ALL DATA ON THE TARGET DEVICE!**

Designed for **Framework 13 laptops** where you want Guix as your only OS:

- **Wipes the entire disk** and creates new partitions
- **Destroys all existing data** on the laptop
- **Replaces any existing OS** with minimal Guix
- **WiFi firmware** needed after boot (add via customize tool)

**Prerequisites:**

- Fresh Framework 13 or willingness to wipe everything
- Guix live ISO
- Backup of all important data

**Installation:** See [`framework/README.md`](framework/README.md)
**After Boot:** See [`CUSTOMIZATION.md`](CUSTOMIZATION.md) to add WiFi/desktop

---

### 3. Framework 13 Dual-Boot

**Safe dual-boot installation alongside existing Pop!_OS.**

Designed for Framework 13 laptops with existing Pop!_OS installations:

- **Preserves** existing Pop!_OS installation
- **Reuses** existing EFI System Partition
- **Creates** new Guix partition in free space
- **Shares** bootloader with Pop!_OS

**Prerequisites:**

- Existing Pop!_OS installation
- At least 40-60GB free space (unallocated)
- Backup of important data

**Installation:** See [`framework-dual/README.md`](framework-dual/README.md)
**After Boot:** See [`CUSTOMIZATION.md`](CUSTOMIZATION.md) to add desktop/WiFi

---

### 4. Raspberry Pi 4 (Experimental)

⚠️ **EXPERIMENTAL** - Requires manual steps, not fully automated like other platforms.

Designed for **Raspberry Pi 4** single-board computer:

- **Build image** on aarch64 machine (or Qemu VM)
- **Manual firmware** addition required (cannot be automated)
- **Image-based** installation (not interactive installer)
- **Works!** Many users run Guix successfully on Pi 4

**Prerequisites:**

- Raspberry Pi 4 (4GB+ RAM recommended)
- MicroSD card (32GB+, Class 10)
- aarch64 build environment (or substitute server access)
- Manual firmware download from Raspberry Pi repo

**Installation:** See [`raspberry-pi/README.md`](raspberry-pi/README.md)
**After Boot:** Use customize tool + shared recipes

**Why experimental?**
- Cannot automate firmware addition (licensing)
- Requires off-device image build (aarch64)
- Smaller community, less testing
- But proven to work for home servers, learning, IoT

---

## Global Mirror Optimization

**Faster downloads worldwide** - The installation scripts automatically optimize download speeds by selecting the best mirrors for your region.

### Automatic Region Detection

Scripts detect your location and configure:
- **Git repository mirrors** (for `guix pull`)
- **Substitute servers** (for binary downloads)
- **Channel configurations** (post-boot)

### Supported Regions

- 🇨🇳 **Asia/China**: SJTU, Tsinghua mirrors (10x+ faster in China)
- 🇪🇺 **Europe**: Bordeaux build farm, Savannah
- 🇺🇸 **Americas**: CI server, Bordeaux fallback
- 🌍 **Global**: Official mirrors (default)

### Manual Override

```bash
# Set region before installation
export GUIX_REGION="asia"
./run-remote-steps.sh

# Or use custom mirrors
export GUIX_GIT_URL="https://your-mirror.example.com/guix.git"
```

**Full documentation:** [`lib/mirrors.md`](lib/mirrors.md)

---

## Architecture

Both installation types share a common architecture:

### Script Structure

Each installation step has two parts:

- **`*-warnings.sh`**: Pre-flight checks, variable validation, user confirmation
- **`*-clean.sh`**: Actual implementation (partition, mount, configure, install)

### Shared Components

Scripts 04-06 are identical across platforms (symlinked in `framework-dual/`):

- `04-system-init`: Swap setup, Guix pull (with mirror optimization), system initialization
- `05-postinstall-console`: Root password, SSH setup
- `06-postinstall-own-terminal`: Channel configuration (nonguix, etc.)

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
