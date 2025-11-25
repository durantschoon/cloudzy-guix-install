# Dual-Boot Installation Guide

**Installing Guix OS alongside Pop!_OS on Framework 13**

This guide covers dual-boot installation, preserving your existing Pop!_OS installation while adding Guix to the same system.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Partition Planning](#partition-planning)
3. [Installation Process](#installation-process)
4. [GRUB Configuration](#grub-configuration)
5. [Post-Installation](#post-installation)
6. [Troubleshooting](#troubleshooting)
7. [Framework 13 Specific Notes](#framework-13-specific-notes)

---

## Prerequisites

### Required Before Starting

- **Existing Pop!_OS installation** with EFI System Partition (ESP)
- **Secure Boot DISABLED** in BIOS
  - Guix does not support Secure Boot
  - Press F2 at boot → Security tab → Disable Secure Boot
  - Framework 13: Boot menu is F12, BIOS is F2
- **Labeled partitions** (filesystem labels in UPPERCASE):
  - ESP labeled as `EFI` (set with: `fatlabel /dev/nvme0n1p1 EFI`)
  - Pop!_OS root optionally labeled as `POPOS_ROOT` (set with: `e2label /dev/nvme0n1pX POPOS_ROOT`)
- **Pre-created partition for Guix** (40-60GB minimum)
  - Use GParted or `parted` to create the partition
  - Leave it unformatted (script will format as ext4)
  - **Label it `GUIX_ROOT` after formatting** (script does this automatically)
- **Backup of all important data** before proceeding
- **Running from a Guix live ISO** (not Pop!_OS)

### What the Installer Does

✅ **Keeps** your existing Pop!_OS installation (untouched)  
✅ **Reuses** your existing EFI System Partition (ESP)  
✅ **Creates** a new partition for Guix in free space  
✅ **Installs** Guix bootloader to ESP (will chain to Pop!_OS)

### What the Installer Doesn't Do

❌ Shrink or resize existing partitions (do this manually first if needed)  
❌ Modify Pop!_OS partitions  
❌ Create a new ESP (uses existing one)

---

## Partition Planning

### Recommended Partition Layout

This is a suggested partition layout for dual-booting Guix with Pop!_OS on Framework 13:

**Note:** All partitions should be primary partitions on a GPT partition table (GPT is standard for UEFI systems).

| Partition | Size | Filesystem | Label | Set Label Command | Purpose |
|-----------|------|------------|-------|-------------------|---------|
| p1 **EFI System Partition (ESP)** | 1 GB | FAT32 | `EFI` | `fatlabel /dev/nvme0n1p1 EFI` | Shared UEFI bootloader (Pop!_OS creates, Guix adds entries, room for Windows) |
| p2 **Swap** *(optional)* | 8 – 32 GB | swap | *(none)* | N/A | Suspend-to-disk / extra memory (optional) |
| p3 **Pop!_OS Root (`/`)** | 60 – 100 GB | ext4 | `POPOS_ROOT` | `e2label /dev/nvme0n1p3 POPOS_ROOT` | Pop!_OS system files |
| p4 **Guix Root (`/`)** | 40 – 60 GB | ext4 | `GUIX_ROOT` | `e2label /dev/nvme0n1p4 GUIX_ROOT` | Guix system files (leave unformatted until Guix install) |
| p5 **Home / Data** *(optional)* | Remainder | ext4 *(or exFAT)* | `DATA` | `e2label /dev/nvme0n1p5 DATA` | Shared personal files (optional) |

### Verifying Labels

**Check current partition labels:**

```bash
lsblk -o NAME,SIZE,FSTYPE,LABEL
```

**Set labels if missing:**

```bash
# ESP label
sudo fatlabel /dev/nvme0n1p1 EFI

# Pop!_OS root label (optional but recommended)
sudo e2label /dev/nvme0n1p3 POPOS_ROOT

# Guix root label (will be set automatically by installer)
# But you can set it manually after formatting:
sudo e2label /dev/nvme0n1p4 GUIX_ROOT

# Data partition label (if you have one)
sudo e2label /dev/nvme0n1p5 DATA
```

### Manual Partition Resizing

**If you don't have free space, shrink Pop!_OS first:**

1. **Boot into Pop!_OS live ISO** (not Guix ISO)
2. **Use GParted** to shrink your Pop!_OS partition
3. **Leave at least 40-60GB** of unallocated space
4. **Then boot into Guix ISO** and run the installer

**GParted steps:**

1. Right-click Pop!_OS root partition → Resize/Move
2. Shrink by 40-60GB (or more if you have space)
3. Apply changes
4. Reboot into Guix ISO

---

## Installation Process

### Step 1: Boot Guix Live ISO

1. Download Guix Live ISO from <https://guix.gnu.org/>
2. Create bootable USB (use `dd` or Etcher)
3. Boot Framework 13 from USB (F12 for boot menu)
4. Select Guix Live ISO

### Step 2: Set Installation Variables

```bash
# Required
export USER_NAME="yourname"
export FULL_NAME="Your Full Name"
export TIMEZONE="America/New_York"
export HOST_NAME="framework-guix"

# Optional
export SWAP_SIZE="4G"       # Default: 4G
```

### Step 3: Run Dual-Boot Installer

**One command to install:**

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash -s -- framework-dual
```

**What happens:**

1. **Partition check** - Verifies ESP exists, checks for Pop!_OS, verifies free space
2. **Mount existing** - Mounts Guix root partition and existing ESP
3. **Config generation** - Creates minimal `/mnt/etc/config.scm`
4. **System init** - Installs Guix bootloader to ESP (shares with Pop!_OS)

**Time estimate:** 10-25 minutes (depends on network speed and substitute availability)

### Step 4: Reboot

```bash
# Remove ISO
# Reboot into installed system
sudo reboot
```

**At GRUB menu, you should see:**

- Guix System (default)
- Pop!_OS (available via submenu or chainloading)

---

## GRUB Configuration

### Bootloader Coexistence

**How Pop!_OS and Guix coexist:**

- Pop!_OS uses `systemd-boot`, Guix uses `GRUB`
- They coexist peacefully in `/boot/efi/EFI/`
- Directory structure:

```
/boot/efi/EFI/
├── Boot/           # Fallback bootloader
├── systemd/        # Pop!_OS systemd-boot
└── guix/           # Guix GRUB
```

### Boot Order Behavior

- **Whichever OS installs last controls the default boot order**
- Guix's GRUB can detect Pop!_OS (via os-prober)
- Pop!_OS systemd-boot cannot detect Guix

### Dual-Boot GRUB Configuration

**The framework-dual installer automatically configures GRUB:**

```scheme
(bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5)))
```

**This configuration:**
- Installs GRUB to existing ESP (shared with Pop!_OS)
- Sets 5-second timeout for boot menu
- Auto-detects Pop!_OS via os-prober

### Manual GRUB Updates

**If Pop!_OS doesn't appear in GRUB:**

```bash
# Boot into Guix
sudo os-prober                    # Detect other OSes
sudo grub-mkconfig -o /boot/grub/grub.cfg  # Update GRUB config
sudo reboot
```

**If Pop!_OS disappears after Guix installation:**

1. **Boot from Pop!_OS live ISO**
2. **Reinstall Pop!_OS GRUB:**

   ```bash
   sudo mount /dev/nvme0n1p3 /mnt      # Adjust partition number
   sudo mount /dev/nvme0n1p1 /mnt/boot/efi
   sudo grub-install --target=x86_64-efi --efi-directory=/mnt/boot/efi --bootloader-id=Pop_OS
   sudo grub-mkconfig -o /mnt/boot/grub/grub.cfg
   ```

3. **Reboot** - Both OSes should appear in boot menu

---

## Post-Installation

### First Boot Checklist

**After rebooting into Guix:**

1. ✅ **Login at console** (password set during installation)
2. ✅ **Set root password** (optional but recommended):
   ```bash
   sudo passwd root
   ```
3. ✅ **Get network working** (see below)
4. ✅ **Add WiFi firmware** (Framework 13 - critical!)
5. ✅ **Add desktop environment** (if desired)

### Network Setup

**Check if network is already working:**

```bash
ping -c 3 guix.gnu.org
```

**If network doesn't work:**

#### Option A: DHCP (Most Wired Connections)

```bash
# Check interface name
ip link show

# Bring up interface and get DHCP
sudo ip link set eth0 up      # or your interface name
sudo dhclient eth0
```

#### Option B: WiFi Setup (After Adding Firmware)

**Note:** WiFi won't work until you add `linux-firmware` package. You'll need:

- Wired ethernet connection, or
- USB tethering from phone, or
- USB WiFi adapter with free drivers

**Once you have network:**

1. **Add WiFi firmware** using customize tool:
   ```bash
   cd ~/guix-customize
   ./customize
   # Select option to add WiFi firmware
   # Reconfigure and reboot
   ```

2. **After reboot with firmware:**
   ```bash
   # Scan for networks
   sudo iwlist wlan0 scan | grep ESSID
   
   # Connect to WPA network
   wpa_passphrase "YourSSID" "YourPassword" | sudo tee /etc/wpa_supplicant.conf
   sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant.conf
   sudo dhclient wlan0
   ```

**Better WiFi management:** Add NetworkManager service (see [QUICKSTART.md](../QUICKSTART.md) Phase 2).

### Customization Tool

**The customize tool is pre-installed:**

```bash
cd ~/guix-customize
./customize
```

**Available options:**

- Add WiFi/Bluetooth firmware (critical for Framework 13)
- Add desktop environment (GNOME/Xfce/MATE/LXQt)
- Add common packages (git, vim, emacs, etc.)
- View/edit config manually
- Apply changes with system reconfigure

See [postinstall/CUSTOMIZATION.md](../postinstall/CUSTOMIZATION.md) for detailed customization workflows.

---

## Troubleshooting

### "No EFI System Partition found"

**Problem:** Installer can't find ESP.

**Solutions:**

- You need to install Pop!_OS first
- Pop!_OS must be installed in UEFI mode (not legacy BIOS)
- Verify ESP exists: `lsblk -o NAME,FSTYPE,LABEL | grep EFI`
- Set ESP label: `sudo fatlabel /dev/nvme0n1p1 EFI`

### "Less than 40GB of free space available"

**Problem:** Not enough space for Guix installation.

**Solutions:**

- Use GParted (from Pop!_OS live ISO) to shrink Pop!_OS partition
- Create at least 40-60GB of unallocated space
- Verify free space: `lsblk -o NAME,SIZE`

### GRUB doesn't show Pop!_OS

**Problem:** Pop!_OS missing from boot menu.

**Solutions:**

```bash
# Boot into Guix
sudo os-prober                    # Detect other OSes
sudo grub-mkconfig -o /boot/grub/grub.cfg  # Update GRUB config
sudo reboot
```

**If os-prober not installed:**

```bash
sudo guix install os-prober
sudo os-prober
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

### Pop!_OS Boots Instead of GRUB Menu

**Problem:** System boots directly to Pop!_OS, skipping GRUB.

**Solutions:**

1. **Change boot order in BIOS:**
   - Press F2 at boot → Boot tab
   - Set "Guix" or "grubx64.efi" as first boot option

2. **Or use Pop!_OS boot menu:**
   - Pop!_OS systemd-boot should show Guix option
   - If not, reinstall Pop!_OS GRUB (see GRUB Configuration section)

### Can't Login After Install

**Problem:** No password set during installation.

**Solutions:**

1. Switch to a text console: Press `Alt+F3` or `Alt+F4`
2. Login as `root` (no password)
3. Set user password: `passwd yourname`
4. Return to graphical login: Press `Alt+F7`

### WiFi Not Working (Framework 13)

**Problem:** Missing firmware.

**Solutions:**

1. **Get network via ethernet/USB first**
2. **Add WiFi firmware:**
   ```bash
   cd ~/guix-customize
   ./customize
   # Select option to add WiFi firmware
   # Reconfigure and reboot
   ```
3. **After reboot, connect to WiFi** (see Network Setup section)

See [GNOME_LOGIN_TROUBLESHOOTING.md](GNOME_LOGIN_TROUBLESHOOTING.md) for desktop-specific issues.

---

## Framework 13 Specific Notes

### Hardware Requirements

- **NVMe SSD** (typically `/dev/nvme0n1`)
- **UEFI boot** (not legacy BIOS)
- **WiFi/Bluetooth firmware** needed (add via customize tool after first boot)
- **AMD GPU boot issues** - Scripts automatically include kernel parameters to prevent boot hangs

### AMD GPU Boot Issues

**Problem:** Framework 13 AMD laptops may experience boot hangs.

**Solution:** Installer automatically includes kernel parameters:

```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "acpi=off" "noapic" "nolapic"))
```

**What these parameters do:**

- **`nomodeset`**: Disables kernel mode setting (fixes AMD GPU display issues)
- **`acpi=off`**: Disables ACPI (prevents power management conflicts)
- **`noapic`**: Disables APIC (prevents interrupt controller issues)
- **`nolapic`**: Disables Local APIC (prevents local interrupt issues)

**Boot hang symptoms:**

- System hangs at "Loading kernel modules..."
- Repeating "time with localhost and MARK" messages every 20 minutes
- Never reaches login prompt

**Manual recovery:**

1. At GRUB menu, press 'e' to edit
2. Find the kernel line and add parameters:
   ```
   linux /boot/vmlinuz-... quiet splash nomodeset acpi=off noapic nolapic 3
   ```
3. Press Ctrl+X or F10 to boot

### Framework 13 Specific Initrd Modules

**Required modules for Framework 13:**

```scheme
(initrd-modules
 (append '("amdgpu"      ; AMD GPU driver (critical for display)
           "nvme"        ; NVMe SSD driver
           "xhci_pci"    ; USB 3.0 host controller
           "usbhid"      ; USB keyboard/mouse
           "i2c_piix4")  ; SMBus/I2C for sensors
         %base-initrd-modules))
```

**These are automatically included** by the installer.

### Known-Good Channel Pinning (AMD GPU Issues)

**Problem:** Framework 13 AMD laptops may experience GDM login loops with current guix/nonguix master commits.

**Solution:** Use the included `wingolog-channels.scm` to pin to known-good channel commits.

**After completing basic installation:**

```bash
# Copy wingolog-channels.scm to your system
# (It's included in this repository at framework-dual/wingolog-channels.scm)

# Reconfigure using the pinned channels
sudo guix time-machine -C ~/wingolog-channels.scm -- \
  system reconfigure /etc/config.scm

# Reboot
sudo reboot
```

**What this fixes:**

- AMD GPU firmware loading (amdgpu driver)
- GDM/GNOME desktop login
- Graphics acceleration via DRI3

See [framework-dual/README.md](../framework-dual/README.md) for complete details on channel pinning.

---

## Key Differences from Single-Boot Installation

| Feature | Single-Boot (framework) | Dual-Boot (framework-dual) |
|---------|------------------------|---------------------------|
| **Partitioning** | Wipes entire disk | Creates partition in free space |
| **ESP** | Creates new ESP | Reuses existing ESP |
| **Boot Mode** | Auto-detects BIOS/UEFI | Forces UEFI only |
| **Safety Checks** | Warns about existing data | Requires existing Pop!_OS |
| **Confirmation** | Optional | Explicit "YES" required |
| **Target** | Fresh laptop | Existing laptop dual-boot |

---

## Further Reading

- [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) - Deep technical details on dual-boot
- [Seasoned Guix User Guide](GUIDE_SEASONED_GUIX.md) - Advanced configuration and channel management
- [GNOME Login Troubleshooting](GNOME_LOGIN_TROUBLESHOOTING.md) - Desktop environment issues
- [Customization Guide](../postinstall/CUSTOMIZATION.md) - Post-install customization
- [framework-dual/README.md](../framework-dual/README.md) - Installer-specific documentation

---

**Next Steps:** After installation, see [QUICKSTART.md](../QUICKSTART.md) Phase 3 for customization, or [postinstall/CUSTOMIZATION.md](../postinstall/CUSTOMIZATION.md) for detailed recipes.

