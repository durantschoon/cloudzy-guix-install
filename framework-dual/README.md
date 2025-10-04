# Framework 13 Dual-Boot Installation Scripts

Scripts for installing minimal Guix OS on Framework 13 alongside an existing Pop!_OS installation.

## ⚠️ IMPORTANT: Dual-Boot Specific Warnings

**These scripts are designed for dual-boot installations ONLY.**

### Prerequisites

- **Existing Pop!_OS installation** with EFI System Partition (ESP)
- **At least 40-60GB of unallocated free space** on your disk
- **Backup of all important data** before proceeding
- Running from a **Guix live ISO**

### What These Scripts Do

1. ✅ **Keep** your existing Pop!_OS installation (untouched)
2. ✅ **Reuse** your existing EFI System Partition (ESP)
3. ✅ **Create** a new partition for Guix in free space
4. ✅ **Install** Guix bootloader to ESP (will chain to Pop!_OS)

### What These Scripts DON'T Do

❌ Shrink or resize existing partitions (do this manually first if needed)
❌ Modify Pop!_OS partitions
❌ Create a new ESP (uses existing one)

## Directory Structure

```
framework-dual/
├── install/          # ISO phase: Minimal Guix dual-boot installation
│   ├── 01-partition-check-{warnings,clean}.sh
│   ├── 02-mount-existing-{warnings,clean}.sh
│   ├── 03-config-dual-boot-{warnings,clean}.sh
│   └── 04-system-init-{warnings,clean}.sh → ../../cloudzy/install/04-*
│
└── postinstall/      # Post-boot phase: Laptop-specific customization
    ├── customize     # Interactive tool for WiFi firmware, desktop, etc.
    ├── recipes/      # Modular scripts (to be added)
    └── templates/    # Pre-configured setups (to be added)
```

## Installation Scripts (ISO Phase)

Located in `install/` - Run from Guix live ISO to create minimal bootable dual-boot system.

1. **`01-partition-check-{warnings,clean}.sh`**: Dual-boot partition setup
   - Detects existing ESP (required for dual-boot)
   - Checks for Pop!_OS installation
   - Verifies sufficient free space (warns if < 40GB)
   - Creates new Guix root partition in free space
   - Requires explicit "YES" confirmation

2. **`02-mount-existing-{warnings,clean}.sh`**: Mount existing ESP
   - Mounts new Guix root partition
   - Mounts existing ESP (shared with Pop!_OS)
   - Copies Guix store from ISO
   - Sets up bind mounts

3. **`03-config-dual-boot-{warnings,clean}.sh`**: Dual-boot configuration
   - Forces UEFI mode (required for Framework 13)
   - Generates **minimal** `/mnt/etc/config.scm` (no desktop, no SSH)
   - Configures bootloader to share ESP with Pop!_OS
   - Uses `lib/common.sh` for config generation

4. **`04-system-init-{warnings,clean}.sh`**: System installation
   - Sets up swap space (default 4G)
   - Pulls specified Guix version
   - Initializes the bootable dual-boot system

These scripts are called automatically by the main installer runner.

## Customization Tools (Post-Boot Phase)

Located in `postinstall/` - Run after booting into installed minimal Guix system.

**`customize`** - Interactive Framework 13 customization tool:
- Add WiFi/Bluetooth firmware (critical for Framework 13)
- Add desktop environment (GNOME/Xfce/MATE/LXQt)
- Add common packages (git, vim, emacs, etc.)
- View/edit config manually
- Apply changes with system reconfigure

**Usage:**
```bash
# After booting into minimal Guix system
# Copy customize tool to system (via USB or another method)
chmod +x ~/customize
./customize
```

See `../../CUSTOMIZATION.md` for detailed post-boot customization workflows.

## Usage

These scripts are called by the main installer runner from the repository root.

### Required Environment Variables

```bash
# User configuration (required)
export USER_NAME="yourname"
export FULL_NAME="Your Full Name"
export TIMEZONE="America/New_York"
export HOST_NAME="framework-guix"

# Optional configuration
export DESKTOP_ENV="gnome"  # Options: gnome, xfce, mate, lxqt, none
export SWAP_SIZE="4G"       # Default: 4G
```

### Optional: Manual Partition Resizing

If you don't have free space, you'll need to shrink Pop!_OS first:

```bash
# Boot into Pop!_OS live ISO (not Guix ISO)
# Use GParted to shrink your Pop!_OS partition
# Leave at least 40-60GB of unallocated space
# Then boot into Guix ISO and run these scripts
```

## Key Differences from VPS (Cloudzy) Scripts

| Feature | Cloudzy | Framework Dual-Boot |
|---------|---------|---------------------|
| **Partitioning** | Wipes entire disk | Creates partition in free space |
| **ESP** | Creates new ESP | Reuses existing ESP |
| **Boot Mode** | Auto-detects BIOS/UEFI | Forces UEFI only |
| **Safety Checks** | Warns about existing data | Requires existing Pop!_OS |
| **Confirmation** | Optional | Explicit "YES" required |
| **Target** | Fresh VPS | Existing laptop dual-boot |

## Post-Installation

After installation, your GRUB menu should show:
- Guix System (default)
- Pop!_OS (available via submenu or chainloading)

If Pop!_OS doesn't appear in GRUB, you can manually add it or use `update-grub` after first boot.

## Troubleshooting

### "No EFI System Partition found"
- You need to install Pop!_OS first
- Pop!_OS must be installed in UEFI mode (not legacy BIOS)

### "Less than 40GB of free space available"
- Use GParted (from Pop!_OS live ISO) to shrink Pop!_OS partition
- Create at least 40-60GB of unallocated space

### GRUB doesn't show Pop!_OS
- Boot into Guix
- Run: `sudo os-prober` (may need to install)
- Run: `sudo grub-mkconfig -o /boot/grub/grub.cfg`

## Framework 13 Hardware Notes

- Uses NVMe SSD (typically `/dev/nvme0n1`)
- Requires UEFI boot (not legacy BIOS)
- WiFi/Bluetooth firmware needed (add via `postinstall/customize` tool after first boot)
