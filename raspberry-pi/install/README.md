# Raspberry Pi Installation Scripts

## Why No Automated Scripts?

Unlike other platforms (cloudzy, framework), Raspberry Pi installation **cannot be fully automated** for several technical reasons:

### 1. **aarch64 Build Requirement**

- Guix images must be built on an aarch64 machine (or cross-compiled)
- Most users run x86_64 systems
- Would require Qemu emulation or remote build server
- Can't assume user has access to aarch64 build environment

### 2. **Manual Firmware Dependency**

- Raspberry Pi requires proprietary firmware files (start4.elf, fixup4.dat, etc.)
- These files are not included in Guix
- Must be manually downloaded from Raspberry Pi firmware repository
- Cannot be automatically fetched due to licensing/distribution concerns
- Firmware must be added to boot partition **after** image generation

### 3. **Boot Process Complexity**

- Raspberry Pi boot: GPU firmware → U-Boot → GRUB → Guix kernel
- config.txt must be manually configured
- U-Boot binary must be correctly named (u-boot.bin)
- Partitioning differs from standard PC (FAT32 boot + ext4 root)
- Cannot run installation scripts FROM Raspberry Pi (chicken-and-egg problem)

### 4. **Image-Based Installation**

- Raspberry Pi requires pre-built SD card image
- Image is built **off-device** then written to SD card
- Can't run interactive installer on the Pi itself during first boot
- Different from VPS/laptop where we can run installer from live ISO

### 5. **Testing Limitations**

- Limited aarch64 CI/CD infrastructure
- Smaller community testing than x86_64
- Hardware-specific issues (SD card compatibility, power, heat)
- Can't guarantee scripts work across all Pi configurations

---

## What We Provide Instead

Since full automation isn't possible, we provide:

### ✅ **Comprehensive Documentation**

- `raspberry-pi/README.md` - Complete setup guide
- Step-by-step firmware installation
- Config examples (raspberry-pi-minimal.scm)
- Troubleshooting guide

### ✅ **Post-Installation Customization**

- `raspberry-pi/postinstall/customize` - Interactive menu
- Access to shared recipes (Spacemacs, dev tools, fonts)
- Pi-specific tips (lightweight desktops, etc.)

### ✅ **Configuration Template**

Based on Guix's official `raspberry-pi-64.tmpl`:

- Minimal working configuration
- U-Boot bootloader setup
- Correct file system layout

### ✅ **Clear Instructions**

- Method 1: Build on aarch64 machine (recommended)
- Method 2: Use Qemu/UTM VM
- Firmware download and installation
- SD card flashing

---

## Installation Workflow

Instead of automated scripts:

```text
1. Build environment (aarch64 machine or VM)
   ↓
2. Build Guix image using template
   ↓
3. Download Raspberry Pi firmware
   ↓
4. Add firmware to image boot partition
   ↓
5. Flash image to SD card
   ↓
6. Boot Raspberry Pi
   ↓
7. Run post-installation customization
```

**Automation starts at step 7** (after successful boot)

---

## Could This Be Improved?

**Possible future automation:**

1. **Pre-built images** (with firmware included)
   - Legal/licensing complexity
   - Storage/bandwidth requirements
   - Need to maintain multiple Pi versions

2. **Firmware download script**
   - Could automate firmware fetch
   - Still requires manual partition mounting
   - Helpful but doesn't solve main issues

3. **Cross-compilation support**
   - Allow building aarch64 from x86_64
   - Slow and complex
   - Guix substitutes are better solution

4. **Hosted build service**
   - Users upload config, get image
   - Requires infrastructure
   - Security/trust concerns

### Current approach: Manual but well-documented

- Realistic about limitations
- Acknowledges complexity
- Provides clear guidance
- Works reliably when followed

---

## If You Want to Contribute

**Helpful contributions:**

1. Test builds on different aarch64 platforms
2. Document working configurations
3. Create more config.scm examples
4. Test different Raspberry Pi models (3B+, Zero 2 W, etc.)
5. Write Pi-specific recipes (GPIO, camera, etc.)

**Not currently feasible:**

1. Fully automated installer scripts
2. Self-contained installation process
3. x86_64 → aarch64 automated cross-compilation

---

## See Installation Guide

For complete installation instructions:
👉 **`raspberry-pi/README.md`**

This README explains:

- ✅ Why automation isn't possible
- ✅ What manual steps are required
- ✅ How to work within these limitations

We believe honest documentation is better than scripts that fail mysteriously.
