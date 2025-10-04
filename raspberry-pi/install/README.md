# Raspberry Pi Installation Scripts

## Apple Silicon Required

⚠️ **This platform requires an Apple Silicon Mac (M1/M2/M3/M4) to build Raspberry Pi images.**

Unlike other platforms (cloudzy, framework), Raspberry Pi installation uses a different approach:

### Build on Apple Silicon Mac → Flash to SD Card → Boot on Pi

**Why this approach:**

- Apple Silicon Macs are ARM64 (aarch64) machines - same architecture as Raspberry Pi
- Native compilation without cross-compilation or emulation
- Fast build times (30-60 minutes on M1/M2)
- Proven workflow tested on M-series Macs

### Technical Constraints

#### 1. Manual Firmware Dependency

- Raspberry Pi requires proprietary firmware files (start4.elf, fixup4.dat, etc.)
- Cannot be distributed with Guix due to licensing
- Must be manually downloaded from Raspberry Pi firmware repository
- Firmware added to boot partition after image generation

#### 2. Image-Based Installation

- Raspberry Pi requires pre-built SD card image
- Image built on Mac, then written to SD card
- Different from VPS/laptop where we run installer from live ISO
- Boot process: GPU firmware → U-Boot → GRUB → Guix kernel

#### 3. Why Not Fully Automated?

- Cannot automate firmware download (licensing restrictions)
- Requires Apple Silicon Mac for native ARM64 builds
- Image must be built off-device, then flashed
- Manual firmware addition step cannot be avoided

---

## What We Provide

Complete documentation for building Raspberry Pi images on Apple Silicon:

### ✅ **Step-by-Step Guide**

- Install Guix on your Apple Silicon Mac
- Build Raspberry Pi image natively (no emulation)
- Add firmware files to image
- Flash to SD card using `dd` or Balena Etcher
- Boot on Raspberry Pi 4

### ✅ **Post-Installation Customization**

- `raspberry-pi/postinstall/customize` - Interactive menu
- Access to shared recipes (Spacemacs, dev tools, fonts)
- Pi-specific tips (lightweight desktops, etc.)

### ✅ **Configuration Templates**

- Based on Guix's official `raspberry-pi-64.tmpl`
- Minimal working configuration
- U-Boot bootloader setup
- Correct file system layout

### ✅ **macOS-Specific Commands**

- `hdiutil` for mounting images
- `diskutil` for SD card management
- Native `dd` for flashing
- Tested on M1, M2, and M3 Macs

---

## Installation Workflow

On your Apple Silicon Mac:

```text
1. Install Guix on Mac
   ↓
2. Build Raspberry Pi image (native ARM64 compilation)
   ↓
3. Download Raspberry Pi firmware from GitHub
   ↓
4. Mount image and add firmware to boot partition
   ↓
5. Flash image to SD card
   ↓
6. Insert SD card into Raspberry Pi and boot
   ↓
7. Run post-installation customization on Pi
```

**Steps 1-5: On your Mac**
**Steps 6-7: On the Raspberry Pi**

---

## Future Automation Possibilities

**What could be scripted:**

1. **Mac setup script**
   - Install Guix on macOS
   - Verify Apple Silicon architecture
   - Download firmware repository

2. **Image build script**
   - Automate `guix system image` command
   - Mount image automatically
   - Copy firmware files
   - Unmount image

3. **SD card flash script**
   - Detect SD card automatically
   - Safety checks before flashing
   - Progress indication

**What cannot be automated:**

- Firmware distribution (licensing)
- Requiring physical SD card insertion
- Raspberry Pi first boot

### Current approach: Well-documented manual process

- Clear step-by-step instructions
- macOS-specific commands (`hdiutil`, `diskutil`)
- Works reliably on Apple Silicon
- Tested on M1/M2/M3 Macs

---

## If You Want to Contribute

**Helpful contributions:**

1. Test builds on different Apple Silicon models (M1/M2/M3/M4)
2. Create automation scripts for Mac workflow
3. Document working configurations
4. Test different Raspberry Pi models (3B+, Zero 2 W, etc.)
5. Write Pi-specific recipes (GPIO, camera, etc.)

**What would be valuable:**

1. macOS setup automation script
2. Image build + firmware integration script
3. SD card detection and flashing script
4. More configuration templates

---

## See Installation Guide

For complete installation instructions:
👉 **`raspberry-pi/README.md`**

This README explains:

- ✅ Why automation isn't possible
- ✅ What manual steps are required
- ✅ How to work within these limitations

We believe honest documentation is better than scripts that fail mysteriously.
