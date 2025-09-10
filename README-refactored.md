# Guix Installation Scripts - Refactored Structure

## Overview

The installation scripts have been refactored to separate warning/validation code from the core functionality, making the scripts much easier to read and understand.

## New Structure

Each installation step now consists of two scripts:

### Warning Scripts (`*-warnings.sh`)

- Contain all validation, warnings, and safety checks
- Run first to ensure the environment is correct
- Exit with error if prerequisites aren't met
- Examples:
  - `01-partition-warnings.sh` - Checks for Guix ISO, device detection, etc.
  - `02-mount-bind-warnings.sh` - Validates required environment variables
  - `03-config-write-warnings.sh` - Validates user configuration variables

### Clean Scripts (`*-clean.sh`)

- Contain only the core functionality
- Much easier to read and understand
- Focus on the actual installation steps
- Examples:
  - `01-partition-clean.sh` - Pure partitioning logic
  - `02-mount-bind-clean.sh` - Mount and bind operations
  - `03-config-write-clean.sh` - System configuration generation

## Benefits

1. **Readability**: Core scripts are now clean and focused
2. **Maintainability**: Warning logic is separated and reusable
3. **Debugging**: Easier to understand what each script actually does
4. **Safety**: All warnings still run, but don't clutter the main logic

## Usage

The `run-remote-steps.sh` script automatically:

1. Fetches both warning and clean scripts for each step
2. Runs the warning script first (exits if validation fails)
3. Runs the clean script if warnings pass
4. Shows preview of the clean script (not the warning script) for user review

## Migration from Original Scripts

The original combined scripts have been replaced with the new split structure. The new approach provides better readability, maintainability, and safety while preserving all functionality.

## File Structure

```text
├── run-remote-steps.sh              # Main orchestrator (updated)
├── 01-partition-warnings.sh         # Partitioning warnings/validation
├── 01-partition-clean.sh            # Clean partitioning logic
├── 02-mount-bind-warnings.sh        # Mount warnings/validation  
├── 02-mount-bind-clean.sh           # Clean mount/bind logic
├── 03-config-write-warnings.sh      # Config warnings/validation
├── 03-config-write-clean.sh         # Clean config generation
├── 04-system-init-warnings.sh       # System init warnings/validation
├── 04-system-init-clean.sh          # Clean system initialization
├── 05-postinstall-console-warnings.sh # Console setup warnings
├── 05-postinstall-console-clean.sh  # Clean console setup
├── 06-postinstall-own-terminal-warnings.sh # Terminal setup warnings
├── 06-postinstall-own-terminal-clean.sh    # Clean terminal setup
└── update-sha256.sh                        # SHA256 checksum management
```
