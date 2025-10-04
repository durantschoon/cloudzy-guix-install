# Repository Structure

Clean separation between installation (ISO phase) and customization (post-boot phase), organized by platform.

## Directory Layout

```
.
├── cloudzy/                    # VPS Platform (Cloudzy, DigitalOcean, AWS, etc.)
│   ├── install/               # ISO phase: Minimal Guix installation
│   │   ├── 01-partition-*.sh
│   │   ├── 02-mount-bind-*.sh
│   │   ├── 03-config-write-*.sh
│   │   └── 04-system-init-*.sh
│   │
│   ├── postinstall/           # Post-boot phase: VPS-specific customization
│   │   ├── customize          # Interactive tool (SSH-first, server-focused)
│   │   ├── recipes/           # Modular scripts (add-ssh, add-monitoring, etc.)
│   │   └── templates/         # Pre-configured setups (web-server.scm, etc.)
│   │
│   └── README.md
│
├── framework-dual/             # Framework 13 Laptop (Dual-boot with Pop!_OS)
│   ├── install/               # ISO phase: Minimal Guix dual-boot installation
│   │   ├── 01-partition-check-*.sh
│   │   ├── 02-mount-existing-*.sh
│   │   ├── 03-config-dual-boot-*.sh
│   │   └── 04-system-init-*.sh → ../../cloudzy/install/04-*
│   │
│   ├── postinstall/           # Post-boot phase: Laptop-specific customization
│   │   ├── customize          # Interactive tool (WiFi-first, desktop-focused)
│   │   ├── recipes/           # Modular scripts (add-firmware, add-desktop, etc.)
│   │   └── templates/         # Pre-configured setups (gnome-laptop.scm, etc.)
│   │
│   └── README.md
│
├── lib/                        # Shared installation libraries
│   ├── common.sh              # Config generation, UUID handling, desktop selector
│   └── runner-common.sh       # Runner utilities (msg, fetch_file, etc.)
│
├── deprecated/                 # Archived old scripts
│   ├── cloudzy/05-06-*.sh     # Old postinstall scripts (ISO-based, non-minimal)
│   └── framework-dual/        # Old symlinks
│
├── run-remote-steps.sh         # Main installer (cloudzy platform)
├── update-sha256.sh            # Checksum generator
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
- User account (no password set)
- Network support
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

### Framework 13 (Laptop)
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

### Shared (via lib/)
- UUID extraction
- Boot mode detection
- Config.scm generation
- Desktop environment selector
- Variable substitution

### Platform-Specific

**cloudzy/install:**
- Full disk partitioning (`01-partition-*`)
- Standard mount (`02-mount-bind-*`)
- Server-oriented defaults

**framework-dual/install:**
- Dual-boot partition check (`01-partition-check-*`)
- Existing ESP mount (`02-mount-existing-*`)
- UEFI-only config (`03-config-dual-boot-*`)

**cloudzy/postinstall:**
- SSH-first workflow
- No desktop options
- Server recipes

**framework-dual/postinstall:**
- Hardware firmware first
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

## Migration from Old Structure

**Old (flat):**
```
cloudzy/01-06-*.sh  (install + postinstall mixed)
```

**New (separated):**
```
cloudzy/install/01-04-*.sh      (installation only)
cloudzy/postinstall/customize   (post-boot customization)
deprecated/cloudzy/05-06-*.sh   (archived)
```

**Benefits:**
- ✅ Clear phase separation (ISO vs booted system)
- ✅ Platform-specific customization
- ✅ No confusion about when to run scripts
- ✅ Modular, extensible design
