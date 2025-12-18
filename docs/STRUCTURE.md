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
│   │   ├── 03-config_test.go
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
│   │   ├── 01-partition-check_test.go
│   │   ├── 02-mount-existing.go
│   │   ├── 02-mount-existing_test.go
│   │   ├── 03-config-dual-boot.go
│   │   ├── 03-config-dual-boot_test.go
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
├── raspberry-pi/               # Raspberry Pi 3/4/5 (ARM64, Apple Silicon build)
│   ├── install/               # Image-based installation (different approach)
│   │   └── README.md
│   │
│   ├── postinstall/           # Post-boot phase: Pi-specific customization
│   │   ├── customize          # Interactive tool
│   │   ├── recipes/           # Modular scripts
│   │   └── templates/        # Pi-specific config templates
│   │
│   ├── CHANGELOG.md
│   └── README.md
│
├── lib/                        # Shared installation libraries
│   ├── common.go              # Shared Go functions (password setup, downloads, etc.)
│   ├── common_test.go         # Unit tests for common.go
│   ├── colors.go              # Color output utilities for Go installer
│   ├── bootstrap-installer.sh # Bootstrap script (downloads, verifies, builds installer)
│   ├── channel-utils.sh       # Channel management utilities
│   ├── clean-install.sh       # Clean installation preparation script
│   ├── postinstall.sh         # Post-boot helper functions
│   ├── recovery-complete-install.sh # Recovery script for incomplete installations
│   ├── verify-guix-install.sh # Installation verification script
│   ├── validate-before-deploy.sh # Pre-deployment validation script (checks syntax, tests, etc.)
│   ├── mirrors.md             # Mirror configuration documentation
│   └── channel-templates/     # Pre-configured channel templates
│       ├── custom.scm
│       ├── development.scm
│       ├── gaming.scm
│       └── minimal.scm
│
├── cmd/                        # Go command-line tools
│   └── hash-to-words/         # Hash verification tool (converts hex to words)
│       ├── main.go
│       ├── main_test.go
│       └── words.json
│
├── postinstall/                # Shared post-installation documentation
│   ├── CHANNEL_MANAGEMENT.md  # Guix channel configuration guide
│   ├── CUSTOMIZATION.md       # Post-boot customization recipes
│   ├── EMACS_IMPORT_GUIDE.md  # Emacs configuration import guide
│   ├── README.md
│   └── recipes/               # Shared customization recipes
│       ├── add-development.sh
│       ├── add-doom-emacs.sh
│       ├── add-fonts.sh
│       ├── add-spacemacs.sh
│       └── add-vanilla-emacs.sh
│
├── docs/                       # Technical documentation
│   ├── CONSOLE_FONT_TIPS.md   # High-DPI console font configuration
│   ├── INSTALLATION_KNOWLEDGE.md # Hard-won installation knowledge
│   ├── STRUCTURE.md           # This file
│   ├── TESTING.md             # Testing guide and test structure
│   ├── TROUBLESHOOTING.md     # Common issues and solutions
│   └── VERIFICATION.md        # Installation verification guide
│
├── run-remote-steps.go         # Main Go installer entry point
├── run-remote-steps            # Compiled installer binary
├── run-tests.sh               # Test runner script
├── test-docker.sh             # Docker-based testing script
├── Dockerfile.test             # Docker test environment
├── docker-compose.test.yml     # Docker Compose test configuration
├── update-manifest.sh          # Manifest checksum generator
├── SOURCE_MANIFEST.txt         # Source file checksums for verification
├── go.mod                      # Go module definition
├── CHECKLIST.md                # Development checklist
├── CLAUDE.md                   # Development notes for AI assistants
├── QUICKSTART.md               # Quick start guide
└── README.md                   # Main entry point
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
- Network support (wired ethernet)
- Customize tool in ~/guix-customize/
- NO desktop, NO SSH, NO extras

### Phase 2: Boot
**Action:** Reboot, remove ISO, login at console
**Status:** Free of ISO, running on installed system

### Phase 3: Customization (Post-boot)
**Location:** `{platform}/postinstall/` and `postinstall/`
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
- Free software only (no nonguix)

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
- Requires nonguix channel for WiFi firmware

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
- Requires nonguix channel for WiFi firmware

**Customization focus:**
- WiFi/Bluetooth firmware (critical first step)
- Desktop environments
- Power management (TLP, auto-cpufreq)
- Audio enhancements
- Dual-boot GRUB integration

### Raspberry Pi 3/4/5
**Installation focus:**
- Image-based installation (different from script-based)
- Built on Apple Silicon Mac (native ARM64)
- Single image works on Pi 3, 4, and 5
- Manual firmware addition (licensing restrictions)

**Customization focus:**
- GPIO and hardware interfaces
- Camera support
- ARM-specific optimizations
- Headless server configurations

---

## Shared vs Platform-Specific

### Shared (via lib/common.go)
- Password setup (`SetUserPassword`)
- Customization tool download (`DownloadCustomizationTools`)
- Swap file creation (`CreateSwapFile`)
- System initialization (`RunGuixSystemInit`, `RunGuixSystemInitFreeSoftware`)
- Installation verification (`VerifyInstallation`)
- UUID extraction (`GetUUID`, `GetRootUUID`)
- Device detection (`DetectDevice`, `DetectDeviceFromState`)
- Partition path generation (`MakePartitionPath`)
- Environment variable handling (`GetEnv`, `GetEnvOrDefault`)
- Command availability checking (`CommandExists`)
- Live ISO detection (`IsGuixLiveISO`)

### Platform-Specific

**cloudzy/install:**
- Full disk partitioning (`01-partition.go`)
- Standard mount with bind mounts (`02-mount.go`)
- Server-oriented defaults (free software only)
- Uses `RunGuixSystemInitFreeSoftware()` (standard init, no time-machine)

**framework/install:**
- Full disk partitioning for laptop (`01-partition.go`)
- Standard mount (`02-mount.go`)
- UEFI-only (Framework 13 requirement)
- Uses `RunGuixSystemInit()` (time-machine + nonguix, 3-step kernel workaround)

**framework-dual/install:**
- Dual-boot partition check (`01-partition-check.go`)
- Existing ESP mount (`02-mount-existing.go`)
- UEFI-only config (`03-config-dual-boot.go`)
- Uses `RunGuixSystemInit()` (time-machine + nonguix, 3-step kernel workaround)
- Comprehensive test coverage (`*_test.go` files)

**raspberry-pi/install:**
- Image-based approach (not script-based)
- Different workflow (build on Mac, flash to SD card)

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

**raspberry-pi/postinstall:**
- GPIO and hardware interfaces
- Camera support
- ARM-specific features

---

## Testing Structure

**Test Files:**
- `lib/common_test.go` - Unit tests for shared functions
- `framework-dual/install/*_test.go` - Integration tests for framework-dual steps
- `framework/install/03-config_test.go` - Config generation tests
- `cmd/hash-to-words/main_test.go` - Hash verification tool tests

**Test Execution:**
- `./run-tests.sh` - Run all tests locally (requires Go 1.21+)
- `./test-docker.sh` - Run tests in Docker container (no local Go needed)

**Test Coverage:**
- Common library functions (device detection, partition paths, etc.)
- Platform-specific installation steps
- String operations and error handling
- State management

See [`docs/TESTING.md`](TESTING.md) for complete testing documentation.

---

## Adding New Platforms

To add a new platform (e.g., `new-platform/`):

1. **Create directory structure:**
   ```bash
   mkdir -p new-platform/{install,postinstall/{recipes,templates}}
   ```

2. **Write platform-specific install scripts:**
   ```go
   new-platform/install/01-partition.go    # Platform-specific partitioning
   new-platform/install/02-mount.go         # Mount logic
   new-platform/install/03-config.go        # Config generation
   new-platform/install/04-system-init.go   # System initialization
   new-platform/install/state.go            # State management
   ```

3. **Use shared functions from lib/common.go:**
   - Call `lib.SetUserPassword()`, `lib.DownloadCustomizationTools()`, etc.
   - Use `lib.RunGuixSystemInit()` or `lib.RunGuixSystemInitFreeSoftware()` as appropriate

4. **Create platform-specific customize tool:**
   ```bash
   new-platform/postinstall/customize        # Interactive customization menu
   new-platform/postinstall/recipes/add-feature.sh
   ```

5. **Update main installer:**
   - Add platform case to `run-remote-steps.go`
   - Add platform to `GUIX_PLATFORM` environment variable options

6. **Add tests:**
   ```go
   new-platform/install/01-partition_test.go
   # etc.
   ```

---

## Current Architecture

**Language:** Go with bash helpers
**Entry point:** `lib/bootstrap-installer.sh` → downloads, verifies, builds → `run-remote-steps.go`
**Structure:**
```
lib/bootstrap-installer.sh      # Downloads repo, verifies checksums
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
- ✅ Comprehensive test coverage
- ✅ Docker-based testing environment

---

## Development Status

**Completed platforms:**

- ✅ **cloudzy** - VPS installer (free software only, BIOS/UEFI auto-detect)
- ✅ **framework-dual** - Framework 13 dual-boot with Pop!_OS (WiFi working, NetworkManager integrated)
- ✅ **framework** - Framework 13 single-boot (derived from framework-dual)
- ✅ **raspberry-pi** - Raspberry Pi 3/4/5 image builder (Apple Silicon Mac required)

**Key Features:**

- ✅ Go-based installer with type safety
- ✅ Comprehensive test suite (unit + integration tests)
- ✅ Docker-based testing environment
- ✅ Recovery scripts for incomplete installations
- ✅ NetworkManager integration for WiFi
- ✅ Channel management utilities
- ✅ Hash-to-words verification tool
- ✅ Platform-specific customization tools
- ✅ Extensive documentation

**Architecture Highlights:**

- Clean separation between installation and customization phases
- Shared code in `lib/common.go` with platform-specific implementations
- Idempotent operations (safe to rerun)
- Robust error handling and recovery
- Comprehensive documentation in `docs/`
