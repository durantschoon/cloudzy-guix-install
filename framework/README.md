# Framework 13 Single-Boot Installation Scripts

Scripts for installing minimal Guix OS on Framework 13 as the **only** operating system (single-boot).

## ⚠️ CRITICAL WARNING

**THIS SCRIPT WILL DESTROY ALL DATA ON THE TARGET DEVICE!**

These installation scripts are designed for **fresh Framework 13 installations** where you want Guix as your only OS. They will:

- **Wipe the entire disk** and create new partitions
- **Destroy all existing data** on the target device
- **Replace any existing operating system** with Guix

### ✅ Safe to Use When:

- You have a **fresh Framework 13** with no important data
- You're running from a **Guix live ISO**
- You want **Guix as your only OS** (single-boot)
- You're **100% certain** you're targeting the right device

### ❌ DO NOT USE When:

- You have **important data** on the Framework 13
- You want to **dual-boot** with another OS (use `framework-dual/` instead)
- You're **unsure** which device will be targeted
- You want to **preserve** any existing data or system

### 🛡️ Safety Features:

- Script detects if you're not on a Guix live ISO
- Script warns if multiple filesystems are mounted
- Script shows which device will be targeted before proceeding
- **10-second delay** when warnings are triggered

---

## Directory Structure

```text
framework/
├── install/          # ISO phase: Minimal Guix installation
│   ├── 01-partition-{warnings,clean}.sh → ../../cloudzy/install/01-*
│   ├── 02-mount-bind-{warnings,clean}.sh → ../../cloudzy/install/02-*
│   ├── 03-config-write-{warnings,clean}.sh → ../../cloudzy/install/03-*
│   └── 04-system-init-{warnings,clean}.sh → ../../cloudzy/install/04-*
│
└── postinstall/      # Post-boot phase: Laptop-specific customization
    ├── customize     # Interactive tool for WiFi firmware, desktop, etc.
    ├── recipes/      # Modular scripts (to be added)
    └── templates/    # Pre-configured setups (to be added)
```

## Installation Scripts (ISO Phase)

Located in `install/` - All scripts are **symlinked to `cloudzy/install/`** since the single-boot process is identical to VPS installation.

Run from Guix live ISO to create minimal bootable system:

1. **`01-partition-{warnings,clean}.sh`**: Device detection and partitioning
   - Auto-detects Framework 13 NVMe drive (typically `/dev/nvme0n1`)
   - Creates GPT partition table with EFI (512MB) and root (remaining)
   - Formats partitions with labels: `EFI` (FAT32) and `GUIX_ROOT` (ext4)
   - **DESTRUCTIVE**: Wipes entire disk

2. **`02-mount-bind-{warnings,clean}.sh`**: Mount setup and store copy
   - Mounts partitions by label: `/dev/disk/by-label/GUIX_ROOT` and `/dev/disk/by-label/EFI`
   - Copies Guix store from ISO to target
   - Sets up bind mounts for `/gnu` and `/var/guix`

3. **`03-config-write-{warnings,clean}.sh`**: System configuration
   - Generates **minimal** `/mnt/etc/config.scm` (no desktop, no SSH)
   - Detects UEFI boot mode (Framework 13 is UEFI-only)
   - Uses `lib/common.sh` for config generation

4. **`04-system-init-{warnings,clean}.sh`**: System installation
   - Sets up swap space (default 4G)
   - Pulls specified Guix version (with mirror optimization)
   - Initializes the bootable system

These scripts are called automatically by the main installer runner.

## Customization Tools (Post-Boot Phase)

Located in `postinstall/` - Run after booting into installed minimal Guix system.

**`customize`** - Interactive Framework 13 customization tool:

- **Add WiFi/Bluetooth firmware** (critical for Framework 13!)
- Add desktop environment (GNOME/Xfce/MATE/LXQt)
- Add SSH service (optional for laptop)
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

---

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
export GUIX_REGION="auto"   # Mirror region: auto, asia, europe, americas
```

---

## Framework 13 Hardware Notes

- **Storage**: NVMe SSD (typically `/dev/nvme0n1`)
- **Boot Mode**: UEFI only (not legacy BIOS)
- **WiFi/Bluetooth**: Requires firmware (add via `postinstall/customize` after first boot)
- **Display**: High-DPI 2256x1504 screen (may need scaling adjustments)
- **Power**: Good battery life with TLP or auto-cpufreq

### Post-Installation Recommendations

1. **WiFi Firmware** (critical!)
   - Run `./customize` immediately after first boot
   - Select option 4 to add Framework 13 hardware support
   - Or manually add `linux-firmware` package

2. **Desktop Environment**
   - GNOME works well with high-DPI display
   - Xfce is lighter but may need manual scaling

3. **Power Management**
   - Install TLP: `guix install tlp`
   - Or auto-cpufreq for automatic CPU frequency scaling

4. **Nonguix Channel**
   - Needed for proprietary WiFi firmware
   - Run `./customize` option 5 for setup instructions

---

## Key Differences from Other Platforms

| Feature | framework/ | framework-dual/ | cloudzy/ |
|---------|-----------|-----------------|----------|
| **Boot Setup** | Single-boot (Guix only) | Dual-boot (shares ESP) | Single-boot (VPS) |
| **Partitioning** | Wipes entire disk | Preserves existing OS | Wipes entire disk |
| **Target Device** | `/dev/nvme0n1` | `/dev/nvme0n1` | Auto-detect VPS disk |
| **Boot Mode** | UEFI only | UEFI only | BIOS or UEFI |
| **Post-Install Focus** | WiFi, desktop, power | WiFi, desktop, dual-boot | SSH, server tools |

---

## Troubleshooting

### "No WiFi after installation"
- WiFi firmware is not included in minimal install
- Boot into system, then run `./customize` to add firmware
- Or use USB Ethernet adapter temporarily

### "High-DPI display looks tiny"
- After adding desktop, adjust scaling in display settings
- GNOME: Settings → Displays → Scale (try 200%)
- Xfce: Settings → Appearance → Fonts → DPI (try 192)

### "Device not found: /dev/nvme0n1"
- Framework 13 uses NVMe SSD, should be auto-detected
- Check with `lsblk` to verify device name
- If different, set: `export DEVICE=/dev/your-device`

### Framework 13 AMD: GDM Login Loop (GPU Firmware Issue)

**Problem:** On Framework 13 AMD, you may experience GDM login loops or AMD GPU firmware failures with current guix/nonguix master commits. Symptoms include:

- TTY login works, but GDM immediately drops you back to login after entering password
- `dmesg` shows: `Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2`
- GNOME sessions never register and are immediately removed

**Solution:** Use the included `wingolog-channels.scm` to pin to known-good channel commits from [Wingo's Framework 13 AMD guide](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd) (2024-02-16).

```bash
# After booting into your installed Guix system (via TTY login)
# Copy wingolog-channels.scm to your system if not already present
# (It's included in this repository at framework/wingolog-channels.scm)

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

See [docs/GNOME_LOGIN_TROUBLESHOOTING.md](../docs/GNOME_LOGIN_TROUBLESHOOTING.md) for detailed troubleshooting steps and background.

---

## See Also

- **For dual-boot with Pop!_OS**: Use [`framework-dual/`](../framework-dual/README.md)
- **For VPS installation**: Use [`cloudzy/`](../cloudzy/README.md)
- **Customization guide**: See [`postinstall/CUSTOMIZATION.md`](../postinstall/CUSTOMIZATION.md)
- **Quick start guide**: See [`QUICKSTART.md`](../QUICKSTART.md)
