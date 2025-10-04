# Raspberry Pi 4 Guix Installation

⚠️ **EXPERIMENTAL PLATFORM** - Raspberry Pi support in Guix is functional but requires manual steps and workarounds.

## Status: Work in Progress

**What works:**
- ✅ Guix System runs on Raspberry Pi 4 (aarch64)
- ✅ Can boot through U-Boot → GRUB → Guix
- ✅ Official raspberry-pi-64.tmpl template available

**What requires manual work:**
- ⚠️ Must build image on aarch64 machine (or cross-compile)
- ⚠️ Must manually add Raspberry Pi firmware files
- ⚠️ Boot configuration requires careful setup
- ⚠️ Limited community testing compared to x86_64

---

## Prerequisites

### Hardware Requirements
- **Raspberry Pi 4** (4GB or 8GB RAM recommended)
- **MicroSD card** (32GB+ recommended, Class 10 or better)
- **Power supply** (official 5V 3A USB-C recommended)
- **For initial setup**: Ethernet cable, monitor, keyboard

### Software Requirements
- **aarch64 build machine** (another ARM64 system or VM)
- **Or**: Access to Guix substitute servers with aarch64 builds
- Balena Etcher or `dd` for writing SD card image

---

## Installation Methods

### Method 1: Build Image on aarch64 Machine (Recommended)

This is the most reliable method.

**1. On an aarch64 machine with Guix:**

```bash
# Find the Raspberry Pi template
guix system search raspberry-pi-64

# Or locate it directly
find /gnu/store -name '*raspberry-pi-64.tmpl'

# Build the image (this will take a while)
guix system image --system=aarch64-linux \
  /gnu/store/.../raspberry-pi-64.tmpl

# Or use a custom config
guix system image --system=aarch64-linux \
  /path/to/your/raspberry-pi-config.scm
```

**2. Add Required Firmware:**

The generated image lacks Raspberry Pi firmware files. You must add them manually:

```bash
# Download firmware
git clone --depth=1 https://github.com/raspberrypi/firmware.git

# Mount the boot partition of your image
# (or of your SD card after flashing)

# Copy firmware files to boot partition
cp firmware/boot/*.dat /path/to/boot/partition/
cp firmware/boot/*.elf /path/to/boot/partition/
cp firmware/boot/bootcode.bin /path/to/boot/partition/

# Create config.txt
cat > /path/to/boot/partition/config.txt <<EOF
# Raspberry Pi 4 boot configuration for Guix
enable_uart=1
arm_64bit=1
kernel=u-boot.bin

# GPU memory (adjust as needed)
gpu_mem=128

# Optional: HDMI settings
hdmi_force_hotplug=1
EOF
```

**3. Flash to SD Card:**

```bash
# Using dd (Linux/macOS)
sudo dd if=/gnu/store/...-disk-image of=/dev/sdX bs=4M status=progress
sync

# Or use Balena Etcher (cross-platform, easier)
# https://www.balena.io/etcher/
```

---

### Method 2: Use Qemu aarch64 VM

If you don't have an aarch64 machine, use Qemu/UTM to run an aarch64 VM:

**On macOS (using UTM):**

1. Download Debian aarch64 image
2. Create aarch64 VM in UTM
3. Install Guix in the Debian VM
4. Follow Method 1 inside the VM

**On Linux (using Qemu):**

```bash
# Install qemu-system-aarch64
# Create Debian aarch64 VM
# Install Guix
# Build Raspberry Pi image
```

---

### Method 3: Direct Installation (Advanced)

**Not currently automated** - Would require:
- Custom partitioning scripts for SD card
- U-Boot installation
- Firmware setup
- GRUB configuration for ARM

This method is not yet implemented in this repository.

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
