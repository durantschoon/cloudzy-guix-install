# Completed Checklist Items

This archive contains all completed items from CHECKLIST.md, listed with newest items at the top.

**Last Updated:** 2026-01-01

---

## 2025-12-31

- ✅ **Framework-dual Initrd Fix**: Switched from `microcode-initrd` to `base-initrd` in framework-dual config to resolve missing initrd file issue. Added directory listing diagnostics to `lib/common.go` for better debugging of missing kernel files.

## 2025-12-16

- ✅ **Recovery script rewritten in Go**: Complete rewrite of recovery tool from bash to Go → Created `cmd/recovery/main.go` that shares code with main installer via `lib/common.go`, eliminates sync issues, consistent error handling and retry logic, falls back to bash script if Go build fails
- ✅ **Kernel symlink discovery and cp -L fix**: Critical discovery that kernel/initrd in system generation are symlinks on Cloudzy → Updated all `cp` commands to use `-L` flag to dereference symlinks, documented in `INSTALLATION_KNOWLEDGE.md`, explains why kernel files appeared to copy but were actually tiny symlink files
- ✅ **Network/DNS troubleshooting documentation**: Added comprehensive troubleshooting section to `INSTALLATION_KNOWLEDGE.md` → Documents `diagnose-guix-build.sh` usage, DNS resolution failures, network interface configuration, firewall issues, workarounds with fallback builds, references `lib/fix-network.scm` script (Guile)
- ✅ **Recovery script kernel/initrd verification improvements**: Added comprehensive verification for framework-dual recovery → Verifies kernel/initrd exist in system generation BEFORE copying, verifies files copied successfully, verifies before Step 3 bootloader install, better error messages for AMD GPU/nonguix issues
- ✅ **Recovery script platform auto-detection**: Added automatic platform detection (framework-dual vs cloudzy) if GUIX_PLATFORM not set → Uses same detection logic as verify script, ensures correct customize script downloaded

## Comprehensive Filesystem Recovery Script (2025-11-25)

- ✅ **Comprehensive recovery script**: Created `lib/recover-filesystem-invariants.sh`
  - Fixes filesystem layout (empties `/run`, fixes `/var/run`, `/var/lock` symlinks)
  - Removes ISO artifacts (`/etc/machine-id`, `/etc/resolv.conf`, ISO user profiles)
  - Optionally rebuilds system profile (chroot + `guix system reconfigure`)
  - Supports `--skip-rebuild` option for faster fixes
  - Auto-detects ISO vs installed system
- ✅ **Documentation**: Updated `docs/INSTALLATION_KNOWLEDGE.md` with comprehensive recovery guide
  - Added root cause explanation (why ISO artifacts cause persistent issues)
  - Added complete step-by-step recovery procedure (Phases A-E)
  - Added comparison table: `fix-iso-artifacts.sh` vs `recover-filesystem-invariants.sh`
  - Explained why system profile rebuild is necessary
- ✅ **Manifest**: Added `lib/recover-filesystem-invariants.sh` to SOURCE_MANIFEST.txt
- **Use case**: For systems with persistent PAM/dbus/service failures after installation
- **Recommendation**: Start with `fix-iso-artifacts.sh` for quick fixes, use `recover-filesystem-invariants.sh` if issues persist

---

## ISO Artifacts Cleanup Implementation (2025-11-20)

- ✅ **ISO artifacts cleanup function**: Created `CleanupISOArtifacts()` in `lib/common.go`
  - Fixes `/var/run` → `/run` symlink (CRITICAL - prevents service failures)
  - Fixes `/etc/mtab` → `/proc/self/mounts` symlink (IMPORTANT - prevents filesystem service issues)
  - Removes ISO artifacts (`/etc/machine-id`, `/etc/resolv.conf`, ISO user profiles)
  - Fixes `/var/guix` ownership
  - Idempotent and safe to run multiple times
- ✅ **Integration**: Added cleanup call to all mount steps
  - `cloudzy/install/02-mount.go`
  - `framework/install/02-mount.go`
  - `framework-dual/install/02-mount-existing.go`
- ✅ **One-time fix script**: Created `lib/fix-iso-artifacts.sh` for existing installations
  - Detects if running from ISO or installed system
  - Fixes all filesystem invariants
  - Provides clear status messages
- ✅ **Tests**: Added tests for `CleanupISOArtifacts` function
  - `TestCleanupISOArtifacts` in `lib/common_test.go`
  - `TestCleanupISOArtifactsAccessibility` in `framework-dual/install/02-mount-existing_test.go`
- ✅ **Documentation**: Updated CHECKLIST.md to reflect completion
- ✅ **Manifest**: Added `lib/fix-iso-artifacts.sh` to SOURCE_MANIFEST.txt
- **Root cause**: When copying `/var/guix` from ISO using rsync/cp, ISO's filesystem structure was copied, causing `/var/run` to be a directory instead of symlink
- **Impact**: Prevents D-Bus activation failures and service startup issues on all platforms

---

## Cloudzy Initrd Configuration Fix (2025-10-13)

- ✅ Fixed "Invalid keyword" error during config validation on cloudzy
  - **Issue**: `base-initrd` function doesn't accept `#:linux` and `#:linux-modules` keyword arguments that Guix automatically passes when `(kernel linux-libre)` is specified
  - **Error**: `Invalid keyword: (#:linux #<package linux-libre@6.0.10 ...> #:linux-modules ...)` during `guix system reconfigure --dry-run`
  - **Root Cause**: Explicit initrd specification `(initrd (lambda (fs . rest) (base-initrd fs rest)))` conflicts with Guix's automatic kernel argument passing
  - **Fix**: Removed explicit initrd specification for cloudzy - Guix uses default initrd generation which automatically handles kernel and modules correctly
  - **Files Changed**: `cloudzy/install/03-config.go`, `docs/INSTALLATION_KNOWLEDGE.md`
  - **Documentation**: Updated INSTALLATION_KNOWLEDGE.md to clarify:
    - Free software installations (cloudzy, VPS): Omit initrd specification, use Guix defaults
    - Framework 13 (nonguix): Use `(initrd microcode-initrd)` for proprietary firmware support
- ✅ VERBOSE=1 instructions added for verify script everywhere (helps debug grub.cfg and file detection issues)

---

## GNOME Keyboard Layout & Password Fixes (2025-11-16)

- ✅ GNOME keyboard layout auto-configuration: Automatically creates `gsettings` autostart script when GNOME + keyboard options detected (Wayland-compatible, not setxkbmap)
- ✅ Keyboard layout warning: Added warning when setting password during installation if `ctrl:swapcaps` is configured, instructing user to type password as if swap is NOT enabled (for GDM compatibility)
- ✅ Ctrl-C signal handling: Installer now catches SIGINT/SIGTERM and provides helpful recovery instructions (run verify-guix-install.sh and recovery-complete-install.sh)
- ✅ Updated autostart script to use `gsettings` instead of `setxkbmap` for Wayland/GNOME compatibility
- ✅ Added troubleshooting documentation for GDM password issues and keyboard layout configuration

---

## Recent Bootstrap & Path Resolution Fixes (2025-11-15)

- ✅ Fixed bootstrap-postinstall.scm syntax errors and path resolution
- ✅ Fixed Go detection in bootstrap script (uses bash to run 'command -v go')
- ✅ Made hash-to-words conversion failure fatal (requires Go)
- ✅ Fixed customize script path resolution for symlinks
- ✅ Fixed postinstall/lib.sh to use INSTALL_ROOT correctly (exported from customize scripts)
- ✅ Fixed guile-config-helper.scm path resolution in guile_add_service()

## Batch Conversion Tools Improvements (2025-11-15)

- ✅ Created generate-customize-batch.sh for converting customize scripts (4 scripts)
- ✅ Fixed all tool scripts to work from repo root and find .env there
- ✅ Fixed view-jsonl.sh to find files in tools directory when run from root
- ✅ Fixed submit-batch.sh Python script (variable expansion, stderr handling)
- ✅ Fixed custom_id format to match API requirements (^[a-zA-Z0-9_-]{1,64}$)

## Batch Conversion System (2025-11-15)

- ✅ Batch Conversion Tools (COMPLETED):
  - ✅ Fixed generate-batch-conversion.sh to use temp files (avoids command-line arg limits)
  - ✅ Fixed submit-batch.sh to convert JSONL to proper API format ({"requests": [...]})
  - ✅ Added JSONL validation before submission
  - ✅ Successfully submitted first batch: msgbatch_01UhbzJfuzqGwymdd9i8BKMD
  - ✅ Fixed check-batch-status.sh to handle missing 'total' field and stdin consumption
  - ✅ Fixed retrieve-batch.sh to handle missing fields and error responses gracefully
  - ✅ Batch processing completed successfully (5/5 requests succeeded)
- ✅ Enhanced features complete:
  - ✅ Test generation (automatic test-*.scm files)
  - ✅ Comment structure preservation (structured vs unstructured handling)
  - ✅ Test extraction and integration into test runner
  - ✅ Verification script (verify-setup.sh)
  - ✅ Magit integration for diff viewing
  - ✅ Validation script (validate-comment-structure.sh)

## Framework-dual Postinstall Improvements (2025-11-15)

- ✅ Fixed sed permission errors (chown USER:USER → chown USER)
- ✅ Created guile_add_service() helper using lib/guile-config-helper.scm
- ✅ Updated add_networkmanager(), add_ssh(), add_desktop()
- ✅ Added to SOURCE_MANIFEST.txt for integrity verification
- ✅ Created docs/POSTINSTALL_DEV.md with developer workflow
- ✅ Created lib/bootstrap-postinstall.scm (pure Guile bootstrap)
- ✅ Enhanced docs/GUILE_KNOWLEDGE.md with community best practices

## Guile Conversion Project - Phase 2 (2025-11-15)

- ✅ Phase 2: Update postinstall scripts to use Guile helper
  - ✅ Updated framework-dual/postinstall/customize to use Guile helper
  - ✅ Fixed GNOME configuration sed permission errors
  - ✅ Replaced fragile sed patterns with proper S-expression parsing
  - ✅ All service additions now use guile_add_service()
  - ✅ Consolidated shared postinstall code into postinstall/lib.sh
    - Created shared library with common functions (add_ssh, add_desktop, add_packages, etc.)
    - Updated all platform customize scripts (cloudzy, framework, framework-dual) to source shared lib
    - Moved test infrastructure to postinstall/tests/
    - All platforms now benefit from improved error handling and guile_add_service()

## Guile Conversion Project - Phase 1 (2025-11-15)

- ✅ Phase 1: Library infrastructure complete
  - Created lib/guile-config-helper.scm for S-expression manipulation
  - Integrated Guile testing in Docker and run-tests.sh
  - All tests passing

## Pre-Installation UX Improvements (2025-11-15)

- ✅ Added USEBIGFONT environment variable for larger console fonts during installation
  - Supports boolean values ("1", "yes", "true", "t") → defaults to solar24x32
  - Supports custom font names (e.g., "ter-v32b", "solar24x32")
  - Falls back to solar24x32 if specified font not found
  - Sets font before installer runs for better readability on high-DPI displays
  - Updated bootstrap-installer.sh, README.md, and QUICKSTART.md
- ✅ KEYBOARD_LAYOUT environment variable already supported for Caps Lock ↔ Ctrl swap
  - Applied at first login via generated config.scm
  - Can be set before bootstrap to avoid prompts

## Testing Infrastructure (2025-11-15)

- ✅ **Guile (.scm) scripts**: Fully tested in Docker + run-tests.sh

## Repository Reorganization (Earlier)

- ✅ Moved critical scripts to `lib/` subdirectory:
  - `lib/bootstrap-installer.sh`
  - `lib/postinstall.sh` (already in lib/)
- ✅ Kept development/repo scripts at top level:
  - `update-manifest.sh`
  - `run-tests.sh`
- ✅ Updated bootstrap script internal paths
- ✅ Updated SOURCE_MANIFEST.txt with new paths
- ✅ Updated documentation references
- ✅ All tests pass after reorganization

## Recovery & Safety Features (Earlier)

- ✅ 3-step kernel/initrd fix applied and tested
- ✅ Color-coded output with cycling headers
- ✅ Enhanced manifest verification with Quick checksum view
- ✅ Improved swap creation error messages
- ✅ Daemon startup timeout increased to 2 minutes
- ✅ Graceful validation skip if daemon not responsive
- ✅ Timeout set to 5 seconds
- ✅ os-prober configured in recovery script

## Documentation (Earlier)

- ✅ Basic receipt written
- ✅ Channel commits included (via recovery script)

## Implementation Phases (Earlier)

- ✅ Phase 1: Core Installer - Reliable single-boot installation
- ✅ Phase 2: Dual-Boot Support - Framework-dual installer working
- ✅ Phase 3: Recovery & Safety - Recovery script and verification
- ✅ Phase 4: Documentation - First-boot guides and customization
