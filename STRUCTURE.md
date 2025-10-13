# Repository Structure

Clean separation between installation (ISO phase) and customization (post-boot phase), organized by platform.

## Directory Layout

```
.
├── cloudzy/                    # VPS Platform (Cloudzy, DigitalOcean, AWS, etc.)
│   ├── install/               # ISO phase: Minimal Guix installation
│   │   ├── 01-partition.go
│   │   ├── 02-mount.go
│   │   ├── 03-config.go
│   │   ├── 04-system-init.go
│   │   └── state.go
│   │
│   ├── postinstall/           # Post-boot phase: VPS-specific customization
│   │   ├── customize          # Interactive tool (SSH-first, server-focused)
│   │   ├── recipes/           # Modular scripts (add-ssh, add-monitoring, etc.)
│   │   └── templates/         # Pre-configured setups (web-server.scm, etc.)
│   │
│   └── README.md
│
├── framework/                  # Framework 13 Laptop (Single-boot, Guix only)
│   ├── install/               # ISO phase: Minimal Guix installation
│   │   ├── 01-partition.go
│   │   ├── 02-mount.go
│   │   ├── 03-config.go
│   │   ├── 04-system-init.go
│   │   └── state.go
│   │
│   ├── postinstall/           # Post-boot phase: Laptop-specific customization
│   │   ├── customize          # Interactive tool (WiFi-first, desktop-focused)
│   │   ├── recipes/           # Modular scripts (add-firmware, add-desktop, etc.)
│   │   └── templates/         # Pre-configured setups (gnome-laptop.scm, etc.)
│   │
│   └── README.md
│
├── framework-dual/             # Framework 13 Laptop (Dual-boot with Pop!_OS)
│   ├── install/               # ISO phase: Minimal Guix dual-boot installation
│   │   ├── 01-partition-check.go
│   │   ├── 02-mount-existing.go
│   │   ├── 03-config-dual-boot.go
│   │   ├── 04-system-init.go
│   │   └── state.go
│   │
│   ├── postinstall/           # Post-boot phase: Laptop-specific customization
│   │   ├── customize          # Interactive tool (WiFi-first, desktop-focused)
│   │   ├── recipes/           # Modular scripts (add-firmware, add-desktop, etc.)
│   │   └── templates/         # Pre-configured setups (gnome-laptop.scm, etc.)
│   │
│   └── README.md
│
├── lib/                        # Shared installation libraries
│   ├── common.go              # Shared Go functions (password setup, downloads, etc.)
│   ├── postinstall.sh         # Post-boot helper functions
│   ├── mirrors.sh             # Regional mirror detection and configuration
│   └── mirrors.md             # Mirror configuration documentation
│
├── bootstrap-installer.sh      # Bootstrap script (downloads, verifies, builds installer)
├── run-remote-steps.go         # Main Go installer entry point
├── update-manifest.sh          # Manifest checksum generator
│
└── Documentation
    ├── README.md               # Main entry point
    ├── QUICKSTART.md           # Complete workflow (install → boot → customize)
    ├── CUSTOMIZATION.md        # Post-boot customization recipes
    └── STRUCTURE.md            # This file
```

---

## Phase Separation

### Phase 1: Installation (ISO)
**Location:** `{platform}/install/`
**Purpose:** Create minimal bootable Guix system
**Duration:** ~10-15 minutes
**Result:**
- Bootable system
- User account with password
- Network support
- Customize tool in ~/guix-customize/
- NO desktop, NO SSH, NO extras

### Phase 2: Boot
**Action:** Reboot, remove ISO, login at console
**Status:** Free of ISO, running on installed system

### Phase 3: Customization (Post-boot)
**Location:** `{platform}/postinstall/`
**Purpose:** Add features specific to platform needs
**Tools:**
- Interactive: `./customize` menu
- Modular: `recipes/add-*.sh` scripts
- Templates: Pre-configured `templates/*.scm` files

---

## Platform Differences

### Cloudzy (VPS)
**Installation focus:**
- Wipe entire disk
- Auto-detect VPS storage (vda, sda, xvda, nvme)
- BIOS or UEFI auto-detection

**Customization focus:**
- SSH (critical for headless access)
- Server monitoring
- Web/database servers
- Container runtimes
- Security hardening

### Framework 13 Single-Boot
**Installation focus:**
- Wipe entire disk (like VPS)
- Auto-detect NVMe storage (typically /dev/nvme0n1)
- UEFI only (Framework requirement)
- Full disk installation (no dual-boot)

**Customization focus:**
- WiFi/Bluetooth firmware (critical first step)
- Desktop environments
- Power management (TLP, auto-cpufreq)
- Audio enhancements
- High-DPI display scaling

### Framework 13 Dual-Boot
**Installation focus:**
- Preserve existing Pop!_OS
- Reuse existing ESP
- Create partition in free space
- UEFI only (Framework requirement)

**Customization focus:**
- WiFi/Bluetooth firmware (critical first step)
- Desktop environments
- Power management (TLP, auto-cpufreq)
- Audio enhancements
- Dual-boot GRUB integration

---

## Shared vs Platform-Specific

### Shared (via lib/common.go)
- Password setup (SetUserPassword)
- Customization tool download (DownloadCustomizationTools)
- Swap file creation (CreateSwapFile)
- System initialization (RunGuixSystemInit)
- Installation verification (VerifyInstallation)
- UUID extraction (GetUUID, GetRootUUID)

### Platform-Specific

**cloudzy/install:**
- Full disk partitioning (`01-partition.go`)
- Standard mount with bind mounts (`02-mount.go`)
- Server-oriented defaults

**framework/install:**
- Full disk partitioning for laptop (`01-partition.go`)
- Standard mount (`02-mount.go`)
- UEFI-only (Framework 13 requirement)

**framework-dual/install:**
- Dual-boot partition check (`01-partition-check.go`)
- Existing ESP mount (`02-mount-existing.go`)
- UEFI-only config (`03-config-dual-boot.go`)

**cloudzy/postinstall:**
- SSH-first workflow
- No desktop options
- Server recipes

**framework/postinstall:**
- WiFi firmware first (critical!)
- Desktop + laptop optimizations
- Single-boot focus

**framework-dual/postinstall:**
- WiFi firmware first (critical!)
- Desktop + laptop optimizations
- Dual-boot tools

---

## Adding New Platforms

To add a new platform (e.g., `raspberry-pi/`):

1. **Create directory structure:**
   ```bash
   mkdir -p raspberry-pi/{install,postinstall/{recipes,templates}}
   ```

2. **Write platform-specific install scripts:**
   ```
   raspberry-pi/install/01-partition-*.sh    # ARM-specific partitioning
   raspberry-pi/install/02-mount-*.sh        # Special boot partition handling
   raspberry-pi/install/03-config-*.sh       # ARM kernel, u-boot config
   ```

3. **Symlink shared scripts:**
   ```bash
   cd raspberry-pi/install
   ln -s ../../cloudzy/install/04-system-init-*.sh .
   ```

4. **Create platform-specific customize tool:**
   ```bash
   raspberry-pi/postinstall/customize        # GPIO, camera, ARM-specific features
   raspberry-pi/postinstall/recipes/add-gpio.sh
   ```

5. **Update checksums and runner:**
   ```bash
   # Add to update-sha256.sh
   # Create run-raspberry-pi.sh or extend run-remote-steps.sh with platform parameter
   ```

---

## Current Architecture

**Language:** Go with bash helpers
**Entry point:** `bootstrap-installer.sh` → downloads, verifies, builds → `run-remote-steps.go`
**Structure:**
```
bootstrap-installer.sh          # Downloads tarball, verifies checksums
    ↓
run-remote-steps.go            # Builds from source, runs platform steps
    ↓
{platform}/install/*.go        # Step implementations (01-04)
    ↓
lib/common.go                  # Shared functions
```

**Benefits:**
- ✅ Type-safe installation logic
- ✅ Verified source checksums (SOURCE_MANIFEST.txt)
- ✅ Clear phase separation (ISO vs booted system)
- ✅ Platform-specific customization
- ✅ Modular, extensible design
