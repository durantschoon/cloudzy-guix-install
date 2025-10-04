# Cloudzy VPS Installation Scripts

Scripts for installing Guix OS on a fresh VPS instance (originally designed for Cloudzy).

## ⚠️ CRITICAL WARNING

**THIS SCRIPT WILL DESTROY ALL DATA ON THE TARGET DEVICE!**

These installation scripts are designed for **fresh VPS instances** where you want to completely replace the existing system with Guix. They will:

- **Wipe the entire disk** and create new partitions
- **Destroy all existing data** on the target device
- **Replace the operating system** with Guix

### ✅ Safe to Use When:

- You have a **fresh VPS instance** with no important data
- You're running from a **Guix live ISO**
- You want to **completely replace** the existing system
- You're **100% certain** you're targeting the right device

### ❌ DO NOT USE When:

- You have **important data** on the target device
- You're running on a **production server** with existing data
- You're **unsure** which device will be targeted
- You want to **preserve** any existing data or system
- You want to **dual-boot** (use `framework-dual/` scripts instead)

### 🛡️ Safety Features:

- Script detects if you're not on a Guix live ISO
- Script warns if multiple filesystems are mounted (existing system)
- Script shows which device will be targeted before proceeding
- **10-second delay** when warnings are triggered (time to abort)

## Directory Structure

```
cloudzy/
├── install/          # ISO phase: Minimal Guix installation
│   ├── 01-partition-{warnings,clean}.sh
│   ├── 02-mount-bind-{warnings,clean}.sh
│   ├── 03-config-write-{warnings,clean}.sh
│   └── 04-system-init-{warnings,clean}.sh
│
└── postinstall/      # Post-boot phase: VPS-specific customization
    ├── customize     # Interactive tool for adding SSH, packages, etc.
    ├── recipes/      # Modular scripts (to be added)
    └── templates/    # Pre-configured setups (to be added)
```

## Installation Scripts (ISO Phase)

Located in `install/` - Run from Guix live ISO to create minimal bootable system.

1. **`01-partition-{warnings,clean}.sh`**: Device detection and partitioning
   - Auto-detects primary storage device or uses `DEVICE` env var
   - Creates GPT partition table with EFI (512MB) and root (remaining)
   - Validates device exists before proceeding

2. **`02-mount-bind-{warnings,clean}.sh`**: Mount setup and store copy
   - Mounts root partition
   - Copies Guix store from ISO to target
   - Sets up bind mounts for `/gnu` and `/var/guix`

3. **`03-config-write-{warnings,clean}.sh`**: System configuration
   - Generates **minimal** `/mnt/etc/config.scm` (no desktop, no SSH)
   - Auto-detects BIOS/UEFI boot mode
   - Uses `lib/common.sh` for config generation

4. **`04-system-init-{warnings,clean}.sh`**: System installation
   - Sets up swap space (default 4G)
   - Pulls specified Guix version
   - Initializes the bootable system

These scripts are called automatically by `run-remote-steps.sh`.

## Customization Tools (Post-Boot Phase)

Located in `postinstall/` - Run after booting into installed minimal Guix system.

**`customize`** - Interactive VPS customization tool:
- Add SSH service (critical for headless VPS)
- Add common packages (git, vim, emacs, etc.)
- View/edit config manually
- Apply changes with system reconfigure

**Usage:**
```bash
# After booting into minimal Guix system
curl https://raw.githubusercontent.com/YOUR_USERNAME/cloudzy-guix-install/main/cloudzy/postinstall/customize -o ~/customize
chmod +x ~/customize
./customize
```

See `../../CUSTOMIZATION.md` for detailed post-boot customization workflows.

## Environment Variables

- `DEVICE`: Target storage device (auto-detected if not set)
- `BOOT_MODE`: "uefi" or "bios" (auto-detected if not set)
- `DESKTOP_ENV`: "gnome", "xfce", "mate", "lxqt", or "none" (default: gnome)
- `SWAP_SIZE`: Swap file size (default: 4G)
- `USER_NAME`: Login username (required)
- `FULL_NAME`: Full name for user account (required)
- `TIMEZONE`: System timezone (required, e.g., "America/New_York")
- `HOST_NAME`: System hostname (required)
