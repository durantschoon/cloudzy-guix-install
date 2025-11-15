# Completed Checklist Items

This archive contains all completed items from CHECKLIST.md, listed with newest items at the top.

**Last Updated:** 2025-11-15

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

