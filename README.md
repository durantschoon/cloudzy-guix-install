# Guix Installation Scripts

Modular scripts for installing minimal Guix OS on different platforms with automated hardware detection, safety checks, and guided workflows.

buymeacoffee.com/durantschoon (🙏 help support my LLM habit)

## ⚠️ Choose Your Platform

**Pick the right installer for your use case:**

| Platform | Use Case | Data Safety | Requirements |
|----------|----------|-------------|--------------|
| **[`cloudzy`](cloudzy/)** | Fresh VPS (Cloudzy, etc.) | ⚠️ **WIPES ENTIRE DISK** | VPS with Guix ISO access |
| **[`framework`](framework/)** | Framework 13 single-boot | ⚠️ **WIPES ENTIRE DISK** | Framework 13, no existing OS needed |
| **[`framework-dual`](framework-dual/)** | Framework 13 dual-boot with Pop!_OS | ✅ **PRESERVES existing OS** | Framework 13 with Pop!_OS already installed |
| **[`raspberry-pi`](raspberry-pi/)** | Raspberry Pi 3/4/5 | N/A (Image-based) | Apple Silicon Mac for building |

### Hardware Requirements

- **Disk Space**: 40GB minimum (60GB+ recommended)
- **RAM**: 2GB minimum (4GB+ recommended)
- **Architecture**: x86_64 (Framework/VPS) or aarch64 (Raspberry Pi)
- **Secure Boot**: Not supported - disable in BIOS before installation

---

## Quick Install

**One command from Guix ISO:**

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh | bash -s -- <platform>
```

Replace `<platform>` with: `cloudzy`, `framework`, or `framework-dual`

**⚠️ WARNING:** `cloudzy` and `framework` will **WIPE YOUR ENTIRE DISK**. Only use on fresh systems.

See [`QUICKSTART.md`](QUICKSTART.md) for complete instructions.

## Quick Navigation

- ⚡ **Quick Start Guide**: See [`QUICKSTART.md`](QUICKSTART.md) - Complete workflow from ISO to customized system
- 🎨 **Customization Guide**: See [`CUSTOMIZATION.md`](CUSTOMIZATION.md) - Add features after minimal install
- 🌍 **Mirror Configuration**: See [`lib/mirrors.md`](lib/mirrors.md) - Optimize download speeds globally
- 🚀 **VPS Installation**: See [`cloudzy/README.md`](cloudzy/README.md)
- 💻 **Framework 13 Single-Boot**: See [`framework/README.md`](framework/README.md)
- 💻 **Framework 13 Dual-Boot**: See [`framework-dual/README.md`](framework-dual/README.md)
- 🥧 **Raspberry Pi 3/4/5** (Experimental): See [`raspberry-pi/README.md`](raspberry-pi/README.md)

---

## Installation Philosophy

### Minimal First, Customize Later

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

### 4. Raspberry Pi (Apple Silicon Required)

⚠️ **REQUIRES APPLE SILICON MAC** - Build Raspberry Pi images on M1/M2/M3/M4 Macs.

Designed for building **Raspberry Pi 3/4/5** images using Apple Silicon:

- **Build on Mac** with Apple Silicon (M1/M2/M3/M4)
- **Native ARM64** compilation (no cross-compilation needed)
- **Single image** works on Pi 3, 4, and 5 (all aarch64)
- **Manual firmware** addition (licensing restrictions)
- **Flash to SD card** and boot on any compatible Raspberry Pi

**Prerequisites:**

- **Apple Silicon Mac** (M1, M2, M3, or M4) - Intel Macs won't work
- **Raspberry Pi** (3, 4, or 5)
  - Pi 5: 4GB+ recommended (best performance)
  - Pi 4: 2GB+ works well
  - Pi 3: 1GB (headless/server only)
- MicroSD card (32GB+, Class 10)
- Guix installed on your Mac
- Balena Etcher or `dd` for SD card flashing

**Installation:** See [`raspberry-pi/README.md`](raspberry-pi/README.md)
**After Boot:** Use customize tool + shared recipes

**Why Apple Silicon?**

- Native ARM64 architecture (same as all Raspberry Pi models)
- Fast builds without emulation
- Build once, works on Pi 3, 4, and 5
- Proven workflow on M-series Macs
- Simplified process compared to cross-compilation

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

### New Safety Checks and Logging

- Mounts use filesystem labels (`/dev/disk/by-label/GUIX_ROOT` and `EFI`) for reliability
- Installers verify labels exist before mounting and warn if free space on `/mnt` is < 40GiB
- Command output is tee-logged to `/tmp/guix-install.log` during system init steps
- If an install fails, review `/tmp/guix-install.log` for details

### Go-Based Installer (framework-dual)

The `framework-dual` platform uses a **Go-based installer** for reliability and type safety:

**Benefits:**

- ✅ No bash variable passing issues
- ✅ Type-safe error handling
- ✅ Built-in verification via Git and Go modules
- ✅ Easier to debug and maintain

**Prerequisites (install on Guix ISO first):**

```bash
guix install go gcc-toolchain curl perl rsync
source ~/.guix-profile/etc/profile
```

**Usage from Guix ISO:**

Step 1: Verify the manifest checksum (get this from your local machine after running `./update-manifest.sh`):

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt | shasum -a 256
# Should output: 3834036f9b6bd4ecc2503ea74b729901942f1ed0df00a0e74182e797af86ec31
```

Step 2: Download and run the bootstrap:

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh -o bootstrap.sh
bash bootstrap.sh
```

**For specific version:**

```bash
export GUIX_INSTALL_REF=v0.1.6  # Use a specific tag
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/${GUIX_INSTALL_REF}/bootstrap-installer.sh -o bootstrap.sh
bash bootstrap.sh
```

**Verification strategy:**

1. Git verifies commit/tag integrity when cloning
2. Go verifies go.mod (and go.sum if external deps exist)
3. Source compiled locally before execution

**Interactive installation flow:**

The installer prompts before each step:

- Answer **"Y"** (yes) to run the step - idempotent design skips already-completed work
- Answer **"n"** (no) to skip and exit - useful for pausing installation

**Idempotency for safe reruns:**

- Step 1: Skips formatting if partition already has ext4 filesystem
- Step 2: Skips store sync if /mnt is mounted and populated
- Step 3: Skips config generation if /mnt/etc/config.scm exists

This means you can safely rerun after interruptions - answer "yes" to all steps and only incomplete work will be performed.

**Manual build (for advanced users):**

```bash
git clone --depth 1 --branch main https://github.com/durantschoon/cloudzy-guix-install.git
cd cloudzy-guix-install
go build -o run-remote-steps .
./run-remote-steps
```

---

### Bash-Based Installer (cloudzy, framework)

Other platforms still use bash scripts with SHA256 verification:

**Script Structure:**

Each installation step has two parts:

- **`*-warnings.sh`**: Pre-flight checks, variable validation, user confirmation
- **`*-clean.sh`**: Actual implementation (partition, mount, configure, install)

**Shared Components:**

Scripts 04-06 are identical across platforms:

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

### GUIX_PLATFORM (default: cloudzy)

The `GUIX_PLATFORM` environment variable specifies which platform's installation scripts to use. If not set, defaults to `cloudzy`:

- **`cloudzy`** (default) - VPS fresh install (Cloudzy and similar providers)
- **`framework`** - Framework 13 single-boot (Guix only)
- **`framework-dual`** - Framework 13 dual-boot with Pop!_OS

You can override with: `GUIX_PLATFORM="framework"` or `GUIX_PLATFORM="framework-dual"`

**Platform-specific script sequences:**

- **cloudzy**: `01-partition` → `02-mount-bind` → `03-config-write` → `04-system-init`
- **framework**: `01-partition` → `02-mount-bind` → `03-config-write` → `04-system-init`
- **framework-dual**: `01-partition-check` → `02-mount-existing` → `03-config-dual-boot` → `04-system-init`

**Note**: Raspberry Pi installation uses a different approach (image-based) and is not supported by this script-based installer.

## Quick Start (Go Installer)

**⚠️ READ THE WARNING ABOVE FIRST! This script will destroy all data on the target device.**

### 1. Prepare Guix Live ISO Environment

```bash
guix install go gcc-toolchain glibc curl bc
GUIX_PROFILE="$HOME/.guix-profile"
source "$GUIX_PROFILE/etc/profile"
```

### 2. Download and Build Installer

```bash
# Download source (use timestamp to bypass CDN cache)
curl -fsSL "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.go?$(date +%s)" -o run-remote-steps.go

# Verify checksum (get expected checksum from your local machine first)
shasum -a 256 run-remote-steps.go
# Compare with expected value to ensure you have the latest version

# Build binary
go build run-remote-steps.go
```

**To get the expected checksum on your local machine:**

```bash
# On your Mac/local machine where repo is cloned
cd /path/to/cloudzy-guix-install
shasum -a 256 run-remote-steps.go
# Copy this value and compare with the output on Guix ISO
```

### 3. Set Environment Variables

```bash
# Required
export USER_NAME="YOUR_USER_NAME"
export FULL_NAME="YOUR_FULL_NAME"
export TIMEZONE="America/New_York"
export HOST_NAME="guix-vps"

# Optional (with defaults)
export BOOT_MODE="uefi"         # uefi or bios (auto-detected)
export SWAP_SIZE="4G"           # 2G, 4G, 8G, etc.
export GUIX_PLATFORM="cloudzy"  # cloudzy, framework, or framework-dual
```

### 4. Run Installer

```bash
# Use latest code from main branch
GUIX_INSTALL_REF=main ./run-remote-steps

# Or use a specific release tag
GUIX_INSTALL_REF=v0.2.0 ./run-remote-steps
```

**Done!** The installer handles all downloads, checksums, and script execution automatically.

**Note for development/testing:** If you're testing changes and re-running the installer, delete the `guix-install-scripts/` directory first to ensure fresh downloads:

```bash
rm -rf guix-install-scripts/
GUIX_INSTALL_REF=main ./run-remote-steps
```

---

## Development

### Variable Passing Between Steps

The installer automatically passes variables between steps using the `INCOMING_VARS` pattern:

- Each script outputs variables via `###GUIX_INSTALL_VARS###` marker
- Variables include both disk-related (DEVICE, EFI, ROOT) and user config (USER_NAME, FULL_NAME, etc.)
- **All variables are passed forward**, even if a step doesn't use them
- This allows **skipping steps** without breaking downstream steps

When you skip a step, the variables from the previous step are automatically passed to the next step.

### Updating Checksums

After modifying any installation scripts, update the checksums:

```bash
./update-sha256.sh
```

This will regenerate SHA256 checksums and update `run-remote-steps.go` automatically.
