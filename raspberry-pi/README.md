# Raspberry Pi 4 Guix Installation

⚠️ **REQUIRES APPLE SILICON MAC** - This installation method is designed for building Raspberry Pi images using Apple Silicon (M1/M2/M3/M4) Macs.

## Prerequisites

### Required Hardware

- **Apple Silicon Mac** (M1, M2, M3, or M4)
  - These are ARM64 (aarch64) machines that can natively build Raspberry Pi images
  - Intel Macs will NOT work with these scripts
- **Raspberry Pi 4** (4GB or 8GB RAM recommended)
- **MicroSD card** (32GB+ recommended, Class 10 or better)
- **Power supply** (official 5V 3A USB-C recommended)
- **For initial setup**: Ethernet cable, monitor, keyboard

### Software Requirements

- **macOS** with Homebrew
- **Guix** installed on your Mac (installation script provided)
- Balena Etcher or `dd` for writing SD card image

### Why Apple Silicon?

Apple Silicon Macs are ARM64 machines, the same architecture as Raspberry Pi 4. This means:

- ✅ Native compilation (no cross-compilation needed)
- ✅ Fast builds (no emulation overhead)
- ✅ Proven workflow (tested on M1/M2/M3 Macs)
- ✅ Reliable results

**What works:**

- ✅ Guix System runs on Raspberry Pi 4 (aarch64)
- ✅ Can boot through U-Boot → GRUB → Guix
- ✅ Official raspberry-pi-64.tmpl template available
- ✅ Build on Apple Silicon, flash to SD card, boot on Pi

**What's different from other platforms:**

- ⚠️ Requires Apple Silicon Mac for building
- ⚠️ Must manually add Raspberry Pi firmware files
- ⚠️ Image-based installation (not live ISO)
- ⚠️ Limited community testing compared to x86_64

---

## Installation Steps

### Step 1: Install Guix on Your Apple Silicon Mac

**1. Install Homebrew (if not already installed):**

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**2. Install Guix via the official installer:**

```bash
# Download the official Guix installer
cd /tmp
curl -O https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh

# Review the script (recommended)
less guix-install.sh

# Run the installer (requires sudo)
sudo bash guix-install.sh
```

**3. Start the Guix daemon:**

```bash
sudo -i guix-daemon --build-users-group=guixbuild &
```

**4. Verify Guix is working:**

```bash
guix --version
guix describe
```

---

### Step 2: Build Raspberry Pi Image on Your Mac

**1. Find and copy the Raspberry Pi template:**

```bash
# On your Apple Silicon Mac, verify architecture
uname -m  # Should show: arm64

# Update Guix to latest version
guix pull

# Find the Raspberry Pi template
guix system search raspberry-pi-64

# Or locate it directly
find /gnu/store -name '*raspberry-pi-64.tmpl'

# Build the image (this will take 30-60 minutes on M1/M2)
# Note: No need for --system=aarch64-linux since we're on ARM64 already!
guix system image /gnu/store/...-raspberry-pi-64.tmpl

# The output will show the path to the generated image, like:
# /gnu/store/xxxxx-disk-image
```

**2. Download and prepare Raspberry Pi firmware:**

```bash
# Download official Raspberry Pi firmware
cd ~/Downloads
git clone --depth=1 https://github.com/raspberrypi/firmware.git

# Note the firmware path for next step
FIRMWARE_DIR=~/Downloads/firmware
```

---

### Step 3: Add Firmware to Image

**1. Mount the generated image:**

```bash
# Identify the image path from previous step
IMAGE_PATH="/gnu/store/xxxxx-disk-image"

# Attach image as a device (macOS)
hdiutil attach "$IMAGE_PATH"

# This will mount two partitions:
# - /Volumes/boot (FAT32 - this is what we need)
# - /Volumes/Guix-on-RPi (ext4 - system partition)
```

**2. Copy firmware files to boot partition:**

```bash
# Copy firmware files
sudo cp ~/Downloads/firmware/boot/*.dat /Volumes/boot/
sudo cp ~/Downloads/firmware/boot/*.elf /Volumes/boot/
sudo cp ~/Downloads/firmware/boot/bootcode.bin /Volumes/boot/

# Create config.txt for Raspberry Pi 4
sudo tee /Volumes/boot/config.txt <<EOF
# Raspberry Pi 4 boot configuration for Guix
enable_uart=1
arm_64bit=1
kernel=u-boot.bin

# GPU memory (adjust as needed)
gpu_mem=128

# Optional: HDMI settings
hdmi_force_hotplug=1
EOF

# Verify files are present
ls -lh /Volumes/boot/
```

**3. Unmount the image:**

```bash
hdiutil detach /Volumes/boot
```

---

### Step 4: Flash Image to SD Card

**1. Insert SD card and identify device:**

```bash
# Before inserting SD card
diskutil list

# Insert SD card, then run again to see new device
diskutil list

# Identify the SD card (usually /dev/disk2 or /dev/disk4)
# CAUTION: Double-check this is your SD card, not your Mac's drive!
SD_CARD=/dev/disk2
```

**2. Flash the image:**

```bash
# Unmount the SD card (but don't eject)
diskutil unmountDisk "$SD_CARD"

# Flash the image (this will take 5-10 minutes)
sudo dd if="$IMAGE_PATH" of="$SD_CARD" bs=4m status=progress

# Flush writes
sync

# Eject the SD card
diskutil eject "$SD_CARD"
```

**3. Alternative: Use Balena Etcher (easier, GUI):**

- Download from: https://www.balena.io/etcher/
- Select your image file
- Select your SD card
- Click "Flash!"

---

## Configuration Example

Based on Guix's `raspberry-pi-64.tmpl`, create a minimal config:

```scheme
;; raspberry-pi-minimal.scm
(use-modules (gnu)
             (gnu bootloader u-boot)
             (gnu system nss))

(operating-system
  (host-name "raspberry-pi")
  (timezone "America/New_York")
  (locale "en_US.utf8")

  ;; U-Boot bootloader for Raspberry Pi 4
  (bootloader (bootloader-configuration
                (bootloader u-boot-raspberry-pi-arm64-bootloader)
                (targets '("/dev/mmcblk0"))))

  ;; File systems
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (file-system-label "Guix-on-RPi"))
                         (type "ext4"))
                       %base-file-systems))

  ;; User account
  (users (cons* (user-account
                  (name "pi")
                  (comment "Raspberry Pi User")
                  (group "users")
                  (home-directory "/home/pi")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Minimal packages
  (packages %base-packages)

  ;; Minimal services
  (services %base-services))
```

---

## Post-Installation Customization

After successfully booting Guix on your Raspberry Pi:

### 1. Network Setup

```bash
# Wired (Ethernet) should work out of the box
# WiFi requires additional firmware

# Check network status
ip addr show
```

### 2. Add WiFi Support (if needed)

WiFi on Raspberry Pi 4 may require proprietary firmware (from nonguix):

```bash
# Add nonguix channel
# Install firmware packages
# Reconfigure system
```

### 3. Use Shared Recipes

```bash
# Copy customize tool to Raspberry Pi
chmod +x customize
./customize

# Use shared recipes:
# s - Install Spacemacs
# d - Install development tools
# f - Install fonts
```

---

## Hardware-Specific Notes

### Raspberry Pi 4 Specifications

- **CPU**: Broadcom BCM2711 (quad-core Cortex-A72, ARM v8, 64-bit)
- **Architecture**: aarch64
- **Storage**: MicroSD card (primary)
- **USB**: Can boot from USB 3.0 (requires bootloader update)
- **Network**: Gigabit Ethernet, WiFi 5 (802.11ac), Bluetooth 5.0
- **Video**: Dual 4K HDMI output

### Known Issues

- **Firmware dependency**: Requires manual firmware addition
- **WiFi**: May need additional firmware (consider using Ethernet initially)
- **Heat**: Consider heatsink or fan for prolonged use
- **SD card**: Use high-quality cards (Samsung EVO, SanDisk Extreme)

### Performance Expectations

- **Boot time**: 30-60 seconds (slower than x86_64)
- **Package builds**: Slow on device (use substitutes when possible)
- **Desktop**: LXDE/Xfce recommended over GNOME (lighter weight)

---

## Troubleshooting

### Pi doesn't boot / blank screen

**Check:**

- Firmware files present in boot partition (start4.elf, fixup4.dat, etc.)
- config.txt exists and has correct settings
- U-Boot binary is named u-boot.bin
- SD card is properly formatted (FAT32 boot partition, ext4 root)

**Serial console access:**
```bash
# Connect USB-UART adapter to GPIO pins
# Use screen, minicom, or similar
screen /dev/ttyUSB0 115200
```

### Can't build image

**Solution**: Use substitute servers or find an aarch64 machine
```bash
# Enable substitutes
guix-daemon --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org'
```

### Image built for wrong architecture

**Solution**: Explicitly specify --system
```bash
guix system image --system=aarch64-linux config.scm
```

---

## Resources

### Official Guix Documentation

- Guix Manual: https://guix.gnu.org/manual/
- Guix on ARM: System examples (raspberry-pi-64.tmpl)

### Community Resources

- Guix mailing lists: help-guix@gnu.org
- Blog: "Booting Guix on a Raspberry Pi 4" - superkamiguru.org
- IRC: #guix on libera.chat

### Raspberry Pi Resources

- Firmware repository: https://github.com/raspberrypi/firmware
- Documentation: https://www.raspberrypi.com/documentation/
- Boot configuration: config.txt reference

---

## Why This Platform is Experimental

1. **Manual firmware requirement** - Can't be fully automated
2. **aarch64 build dependency** - Not everyone has ARM build machine
3. **Limited testing** - Smaller community than x86_64
4. **SD card limitations** - Slower than SSD, wear issues
5. **No official Guix images** - Must build yourself

**But it works!** Many users successfully run Guix on Raspberry Pi 4 for:

- Home automation
- Network services (Pi-hole, VPN)
- Learning/education
- Low-power servers

---

## Future Improvements

**Planned (contributions welcome):**

- [ ] Pre-built aarch64 images (with firmware)
- [ ] Automated firmware download script
- [ ] USB boot support
- [ ] Raspberry Pi-specific recipes (GPIO, camera)
- [ ] Performance tuning guide

---

## Contributing

If you successfully install Guix on Raspberry Pi 4:

- Share your config.scm
- Document any issues/workarounds
- Test and report performance
- Contribute improvements to this guide

---

## See Also

- **For desktop/laptop**: Use `framework/` or `framework-dual/`
- **For VPS**: Use `cloudzy/`
- **Shared recipes**: `postinstall/recipes/` (work on RPi too!)
- **Main guide**: `../README.md`
