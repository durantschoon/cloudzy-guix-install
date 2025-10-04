# Raspberry Pi Guix Installation

⚠️ **REQUIRES APPLE SILICON MAC** - This installation method is designed for building Raspberry Pi images using Apple Silicon (M1/M2/M3/M4) Macs.

## Supported Models

This guide works for:

- **Raspberry Pi 5** (recommended) - BCM2712, Cortex-A76 @ 2.4GHz, 4GB/8GB RAM
- **Raspberry Pi 4** - BCM2711, Cortex-A72 @ 1.8GHz, 2GB/4GB/8GB RAM
- **Raspberry Pi 3** (slower) - BCM2837, Cortex-A53 @ 1.2GHz, 1GB RAM

All models are ARM64 (aarch64) and use the same installation process.

## Prerequisites

### Required Hardware

- **Apple Silicon Mac** (M1, M2, M3, or M4)
  - These are ARM64 (aarch64) machines that can natively build Raspberry Pi images
  - Intel Macs will NOT work with these scripts
- **Raspberry Pi** (3, 4, or 5)
  - Pi 5: 4GB+ RAM recommended for desktop use
  - Pi 4: 2GB+ RAM (4GB/8GB for desktop)
  - Pi 3: 1GB RAM (headless/lightweight only)
- **MicroSD card** (32GB+ recommended, Class 10 or better)
- **Power supply**
  - Pi 5: 5V 5A USB-C (27W recommended)
  - Pi 4: 5V 3A USB-C
  - Pi 3: 5V 2.5A Micro USB
- **For initial setup**: Ethernet cable, monitor, keyboard

### Software Requirements

- **macOS** with Homebrew
- **Guix** installed on your Mac (installation script provided)
- Balena Etcher or `dd` for writing SD card image

### Why Apple Silicon?

Apple Silicon Macs are ARM64 machines, the same architecture as all Raspberry Pi 3/4/5 models. This means:

- ✅ Native compilation (no cross-compilation needed)
- ✅ Fast builds (no emulation overhead)
- ✅ Proven workflow (tested on M1/M2/M3 Macs)
- ✅ Same binary works on Pi 3, 4, and 5 (all aarch64)

**What works:**

- ✅ Guix System runs on Raspberry Pi 3, 4, and 5 (aarch64)
- ✅ Can boot through U-Boot → GRUB → Guix
- ✅ Official raspberry-pi-64.tmpl template (works for all models)
- ✅ Build once on Apple Silicon, flash to any Pi model

**What's different from other platforms:**

- ⚠️ Requires Apple Silicon Mac for building
- ⚠️ Must manually add Raspberry Pi firmware files (model-specific)
- ⚠️ Image-based installation (not live ISO)
- ⚠️ Limited community testing compared to x86_64

**Performance expectations:**

- **Pi 5**: 2-3x faster than Pi 4, can run GNOME smoothly
- **Pi 4**: Good for Xfce/MATE, usable for light desktop work
- **Pi 3**: Best for headless/server use, very slow for desktop

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
# Copy firmware files (same for all Pi models)
sudo cp ~/Downloads/firmware/boot/*.dat /Volumes/boot/
sudo cp ~/Downloads/firmware/boot/*.elf /Volumes/boot/
sudo cp ~/Downloads/firmware/boot/bootcode.bin /Volumes/boot/

# Create config.txt - CHOOSE YOUR MODEL:

# Option 1: Use pre-made templates (recommended)
# Copy the appropriate template for your Pi model:
# For Pi 3: cp ~/Downloads/cloudzy-guix-install/raspberry-pi/postinstall/templates/config-pi3.txt /Volumes/boot/config.txt
# For Pi 4: cp ~/Downloads/cloudzy-guix-install/raspberry-pi/postinstall/templates/config-pi4.txt /Volumes/boot/config.txt  
# For Pi 5: cp ~/Downloads/cloudzy-guix-install/raspberry-pi/postinstall/templates/config-pi5.txt /Volumes/boot/config.txt

# Option 2: Create manually (choose your model):

# For Raspberry Pi 5:
sudo tee /Volumes/boot/config.txt <<EOF
# Raspberry Pi 5 boot configuration for Guix
enable_uart=1
arm_64bit=1
kernel=u-boot.bin
gpu_mem=128
dtparam=pciex1
dtparam=pciex1_gen=2
EOF

# For Raspberry Pi 4:
sudo tee /Volumes/boot/config.txt <<EOF
# Raspberry Pi 4 boot configuration for Guix
enable_uart=1
arm_64bit=1
kernel=u-boot.bin
gpu_mem=128
hdmi_force_hotplug=1
EOF

# For Raspberry Pi 3:
sudo tee /Volumes/boot/config.txt <<EOF
# Raspberry Pi 3 boot configuration for Guix
enable_uart=1
arm_64bit=1
kernel=u-boot.bin
gpu_mem=64
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

- Download from: <https://www.balena.io/etcher/>
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

### 4. Update Boot Configuration (if needed)

If you need to change your Pi model's boot configuration:

```bash
# Copy the config setup script to your Pi
# (from the templates directory in this repository)

# Auto-detect your Pi model and setup config
./setup-config.sh

# Or specify your model manually
./setup-config.sh pi4    # For Raspberry Pi 4
./setup-config.sh pi5    # For Raspberry Pi 5
./setup-config.sh pi3    # For Raspberry Pi 3
```

---

## Hardware-Specific Notes

### Raspberry Pi 5 Specifications

- **CPU**: Broadcom BCM2712 (quad-core Cortex-A76 @ 2.4GHz, ARM v8, 64-bit)
- **Architecture**: aarch64
- **RAM**: 4GB or 8GB LPDDR4X
- **Storage**: MicroSD card or NVMe via PCIe 2.0
- **Network**: Gigabit Ethernet, WiFi 5 (802.11ac), Bluetooth 5.0
- **Video**: Dual 4K @ 60fps HDMI output
- **Power**: 5V 5A USB-C (27W recommended)

### Raspberry Pi 4 Specifications

- **CPU**: Broadcom BCM2711 (quad-core Cortex-A72 @ 1.8GHz, ARM v8, 64-bit)
- **Architecture**: aarch64
- **RAM**: 2GB, 4GB, or 8GB LPDDR4
- **Storage**: MicroSD card (can boot from USB 3.0)
- **Network**: Gigabit Ethernet, WiFi 5 (802.11ac), Bluetooth 5.0
- **Video**: Dual 4K HDMI output
- **Power**: 5V 3A USB-C

### Raspberry Pi 3 Specifications

- **CPU**: Broadcom BCM2837 (quad-core Cortex-A53 @ 1.2GHz, ARM v8, 64-bit)
- **Architecture**: aarch64 (same as Pi 4/5!)
- **RAM**: 1GB LPDDR2
- **Storage**: MicroSD card only
- **Network**: 100Mbps Ethernet, WiFi 4 (802.11n), Bluetooth 4.2
- **Video**: Single HDMI output
- **Power**: 5V 2.5A Micro USB

### GPIO and Expansion (All Models)

- **GPIO**: 40-pin header (Pi 3/4/5), 26-pin header (older Pi 3)
- **I2C/SPI**: Available on all models (enable in config.txt)
- **Camera**: CSI connector for Raspberry Pi Camera Module
- **Display**: DSI connector for official touchscreen displays
- **Pi 5**: PCIe 2.0 x1 connector for NVMe, USB 3.0, or other expansion

### Known Issues (All Models)

- **Firmware dependency**: Requires manual firmware addition
- **WiFi**: May need additional firmware (consider using Ethernet initially)
- **Heat**: Pi 5 runs hot - heatsink/fan recommended; Pi 4 needs passive cooling
- **SD card**: Use high-quality cards (Samsung EVO, SanDisk Extreme)
- **Pi 3**: Very limited RAM (1GB) - headless use recommended

### Performance Expectations

**Raspberry Pi 5:**

- **Boot time**: 20-30 seconds
- **Desktop**: Can run GNOME smoothly, excellent for Xfce/MATE
- **Package builds**: Moderate speed, substitutes still recommended
- **Use cases**: Desktop replacement, development, media center

**Raspberry Pi 4:**

- **Boot time**: 30-45 seconds
- **Desktop**: Xfce/MATE recommended, GNOME usable but slow
- **Package builds**: Slow, always use substitutes
- **Use cases**: Light desktop, server, home automation

**Raspberry Pi 3:**

- **Boot time**: 45-60 seconds
- **Desktop**: Only lightweight WMs (i3, openbox), no full DE
- **Package builds**: Very slow, substitutes required
- **Use cases**: Headless server, learning, IoT only

---

## Troubleshooting

### Pi doesn't boot / blank screen

**Check:**

- Firmware files present in boot partition (start4.elf, fixup4.dat, etc.)
- config.txt exists and has correct settings for your Pi model
- U-Boot binary is named u-boot.bin
- SD card is properly formatted (FAT32 boot partition, ext4 root)

**Model-specific checks:**

- **Pi 3**: Ensure `gpu_mem=64` in config.txt
- **Pi 4**: Ensure `hdmi_force_hotplug=1` in config.txt
- **Pi 5**: Ensure `dtparam=pciex1` and `dtparam=pciex1_gen=2` in config.txt

**Serial console access:**

```bash
# Connect USB-UART adapter to GPIO pins
# Use screen, minicom, or similar
screen /dev/ttyUSB0 115200
```

### Model-specific issues

#### Raspberry Pi 3 Issues

**Problem**: Very slow boot or system unresponsive

- **Cause**: Limited RAM (1GB) and slower CPU
- **Solution**: Use headless configuration, avoid desktop environments

**Problem**: WiFi not working

- **Cause**: Pi 3 has older WiFi chip
- **Solution**: Use Ethernet or add nonguix channel for firmware

#### Raspberry Pi 4 Issues

**Problem**: No HDMI output

- **Cause**: Missing `hdmi_force_hotplug=1` in config.txt
- **Solution**: Add the setting and reboot

**Problem**: System overheats and throttles

- **Cause**: Pi 4 runs hot under load
- **Solution**: Add heatsink/fan, ensure adequate power supply

**Problem**: USB devices not working

- **Cause**: Insufficient power or USB issues
- **Solution**: Use powered USB hub, check power supply (5V 3A)

#### Raspberry Pi 5 Issues

**Problem**: PCIe devices not detected

- **Cause**: Missing PCIe configuration in config.txt
- **Solution**: Add `dtparam=pciex1` and `dtparam=pciex1_gen=2`

**Problem**: System unstable or crashes

- **Cause**: Insufficient power (Pi 5 needs 5V 5A)
- **Solution**: Use official 27W power supply or equivalent

**Problem**: NVMe not working

- **Cause**: Missing NVMe configuration
- **Solution**: Add `dtparam=nvme` to config.txt

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

### Config.txt issues

**Problem**: Wrong config.txt for your Pi model

- **Solution**: Use the setup script to auto-detect and configure

```bash
./setup-config.sh
```

**Problem**: Config changes not taking effect

- **Solution**: Ensure config.txt is in the boot partition and reboot

---

## Resources

### Official Guix Documentation

- Guix Manual: <https://guix.gnu.org/manual/>
- Guix on ARM: System examples (raspberry-pi-64.tmpl)

### Community Resources

- Guix mailing lists: <help-guix@gnu.org>
- Blog: "Booting Guix on a Raspberry Pi 4" - superkamiguru.org
- IRC: #guix on libera.chat

### Raspberry Pi Resources

- Firmware repository: <https://github.com/raspberrypi/firmware>
- Documentation: <https://www.raspberrypi.com/documentation/>
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
