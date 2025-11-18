# Framework 13 Dual-Boot Installation Scripts

Scripts for installing minimal Guix OS on Framework 13 alongside an existing Pop!_OS installation.

## ⚠️ IMPORTANT: Dual-Boot Specific Warnings

**These scripts are designed for dual-boot installations ONLY.**

### Prerequisites

- **Existing Pop!_OS installation** with EFI System Partition (ESP)
- **Secure Boot DISABLED** in BIOS
  - Guix does not support Secure Boot
  - Press F2 at boot → Security tab → Disable Secure Boot
  - Framework 13: Boot menu is F12, BIOS is F2
- **Labeled partitions** (filesystem labels in UPPERCASE, set during/after partition creation):
  - ESP labeled as `EFI` (set with: `fatlabel /dev/nvme0n1p1 EFI`)
  - Pop!_OS root optionally labeled as `POPOS_ROOT` (set with: `e2label /dev/nvme0n1pX POPOS_ROOT`)
- **Pre-created partition for Guix** (40-60GB minimum)
  - Use GParted or `parted` to create the partition
  - Leave it unformatted (script will format as ext4)
  - **Label it `GUIX_ROOT` after formatting** (script does this automatically)
- **Optional: Separate /home or /data partition**
  - If you have a separate partition to share between Pop!_OS and Guix
  - **Label the partition `DATA`** (set with: `e2label /dev/nvme0n1p5 DATA`)
  - Script will auto-detect and mount at /data
- **Backup of all important data** before proceeding
- **Running from a Guix live ISO** (not Pop!_OS)

### What These Scripts Do

1. ✅ **Keep** your existing Pop!_OS installation (untouched)
2. ✅ **Reuse** your existing EFI System Partition (ESP)
3. ✅ **Create** a new partition for Guix in free space
4. ✅ **Install** Guix bootloader to ESP (will chain to Pop!_OS)

### What These Scripts DON'T Do

- ❌ Shrink or resize existing partitions (do this manually first if needed)
- ❌ Modify Pop!_OS partitions
- ❌ Create a new ESP (uses existing one)

## Directory Structure

```text
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

See `../../postinstall/CUSTOMIZATION.md` for detailed post-boot customization workflows.

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
export SWAP_SIZE="4G"       # Default: 4G
```

## Partition Planning

### Recommended Partition Layout

This is a suggested partition layout for dual-booting Guix with Pop!_OS on Framework 13:

**Note:** All partitions should be primary partitions on a GPT partition table (GPT is standard for UEFI systems).

| Partition | Size | Filesystem | Label | Set Label Command | Purpose |
|-----------|------|------------|-------|-------------------|---------|
| p1 **EFI System Partition (ESP)** | 1 GB | FAT32 | `EFI` | `fatlabel /dev/nvme0n1p1 EFI` | Shared UEFI bootloader (Pop!\_OS creates, Guix adds entries, room for Windows) |
| p2 **Swap** *(optional)* | 8 – 32 GB | swap | *(none)* | N/A | Suspend-to-disk / extra memory (optional) |
| p3 **Pop!\_OS Root (`/`)** | 60 – 100 GB | ext4 | `POPOS_ROOT` | `e2label /dev/nvme0n1p3 POPOS_ROOT` | Pop!\_OS system files |
| p4 **Guix Root (`/`)** | 40 – 60 GB | ext4 | `GUIX_ROOT` | `e2label /dev/nvme0n1p4 GUIX_ROOT` | Guix system files (leave unformatted until Guix install) |
| p5 **Home / Data** *(optional)* | Remainder | ext4 *(or exFAT)* | `DATA` | `e2label /dev/nvme0n1p5 DATA` | Shared personal files (optional) |

**Verify labels:**

```bash
lsblk -o NAME,SIZE,FSTYPE,LABEL
```

### Manual Partition Resizing

If you don't have free space, shrink Pop!_OS first:

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

## Skipping Installation Steps

You can safely skip steps during installation (e.g., if you've already
created partitions):

- **Variables automatically pass through** - Disk info (DEVICE, EFI, ROOT)
  and user config (USER_NAME, FULL_NAME) carry forward
- **Skipping is safe** - Downstream steps receive all necessary variables
  even when earlier steps are skipped
- **Example**: Skip step 01 if partition already formatted, variables from
  user environment still flow to step 03

The installer uses an `INCOMING_VARS` pattern where each step passes all
variables forward, not just the ones it uses.

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
- **AMD GPU boot issues**: Scripts automatically include `nomodeset acpi=off noapic nolapic` kernel parameters to prevent boot hangs

## Framework 13 AMD: Known-Good Channel Pinning

**Problem:** Framework 13 AMD laptops may experience GDM login loops or AMD GPU firmware failures with current guix/nonguix master commits. Symptoms include:

- TTY login works, but GDM immediately drops you back to login after entering password
- `dmesg` shows: `Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2`
- GNOME sessions never register and are immediately removed

**Solution:** Use the included `wingolog-channels.scm` to pin to known-good channel commits from [Wingo's Framework 13 AMD guide](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd) (2024-02-16).

### Using Wingo-Era Channel Pinning

After completing the basic installation, if you experience the AMD GPU issues described above:

```bash
# After booting into your installed Guix system (via TTY login)
# Copy wingolog-channels.scm to your system if not already present
# (It's included in this repository at framework-dual/wingolog-channels.scm)

# Reconfigure using the pinned channels
sudo guix time-machine -C ~/wingolog-channels.scm -- \
  system reconfigure /etc/config.scm

# Reboot
sudo reboot
```

The `wingolog-channels.scm` file pins both guix and nonguix channels to commits from February 2024 that are known to work with Framework 13 AMD hardware.

**What this fixes:**
- AMD GPU firmware loading (amdgpu driver)
- GDM/GNOME desktop login
- Graphics acceleration via DRI3

**After the fix works**, you can optionally experiment with newer channel commits by editing the commit hashes in `wingolog-channels.scm` and re-running the `guix time-machine` command.

See [docs/GNOME_LOGIN_TROUBLESHOOTING.md](../docs/GNOME_LOGIN_TROUBLESHOOTING.md) for detailed troubleshooting steps and background.
