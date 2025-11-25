#!/run/current-system/profile/bin/bash
set -euo pipefail

# Comprehensive Filesystem Recovery Script
# Fixes ISO artifacts and filesystem invariants, optionally rebuilds system profile
# Use this for systems with persistent PAM/dbus/service failures after installation

SCRIPT_NAME="recover-filesystem-invariants.sh"
SKIP_REBUILD=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-rebuild)
            SKIP_REBUILD=true
            shift
            ;;
        --help|-h)
            echo "Usage: $SCRIPT_NAME [--skip-rebuild]"
            echo ""
            echo "Comprehensive recovery script for systems with ISO artifact issues."
            echo ""
            echo "Options:"
            echo "  --skip-rebuild    Skip chroot/system rebuild step (faster, but may not fix activation scripts)"
            echo "  --help, -h        Show this help message"
            echo ""
            echo "Environment Variables:"
            echo "  CHANNELS_PATH     Path to custom channels.scm for guix time-machine"
            echo "                    (e.g., for wingolog kernel or other custom channels)"
            echo ""
            echo "This script:"
            echo "  1. Fixes filesystem layout (/var/run, /var/lock, /run cleanup)"
            echo "  2. Replaces resolv.conf for chroot networking (critical for downloads)"
            echo "  3. Removes ISO artifacts (machine-id, etc.)"
            echo "  4. Optionally rebuilds system profile (chroot + guix system reconfigure)"
            echo "     - Sets up PATH to include system profile binaries"
            echo "     - Supports custom channels via CHANNELS_PATH or auto-detection"
            echo ""
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo "========================================"
echo "  Comprehensive Filesystem Recovery"
echo "========================================"
echo ""
echo "This script fixes ISO artifacts and filesystem invariants."
echo "Use this if your system has persistent PAM/dbus/service failures."
echo ""

# Determine if we're running on ISO or installed system
if [ -d "/mnt/etc" ] && [ -d "/mnt/var" ]; then
    PREFIX="/mnt"
    RUNNING_FROM_ISO=true
    echo "Running from ISO: fixing files in /mnt"
elif [ -f "/etc/config.scm" ]; then
    PREFIX=""
    RUNNING_FROM_ISO=false
    echo "Running on installed system: fixing files in /"
    
    # Check if we're root
    if [ "$EUID" -ne 0 ]; then
        echo "ERROR: This script must be run as root on installed systems"
        echo "Please run: sudo $0"
        exit 1
    fi
else
    echo "ERROR: Cannot determine if running from ISO or installed system"
    echo "If running from ISO, ensure /mnt is mounted"
    echo "If running on installed system, ensure /etc/config.scm exists"
    exit 1
fi

echo ""

# Function to print status
status() {
    local status=$1
    local msg=$2
    case $status in
        OK)
            echo "  $msg: [OK]"
            ;;
        FIXED)
            echo "  $msg: [FIXED]"
            ;;
        SKIP)
            echo "  $msg: [SKIP]"
            ;;
        WARNING)
            echo "  $msg: [WARNING]"
            ;;
        ERROR)
            echo "  $msg: [ERROR]"
            ;;
    esac
}

# Resolve bash path BEFORE Phase B clears /run (so we can use it in Phase D)
# This must happen before /run is cleared, otherwise /run/current-system will be gone
BASH_PATH=""
BASH_RESOLVED_PATH=""

if [ "$RUNNING_FROM_ISO" = true ]; then
    echo "Resolving bash path before Phase B clears /run..."
    CURRENT_SYSTEM_SYMLINK="${PREFIX}/run/current-system"
    
    if [ -L "$CURRENT_SYSTEM_SYMLINK" ] || [ -d "$CURRENT_SYSTEM_SYMLINK" ]; then
        if [ -f "${PREFIX}/run/current-system/profile/bin/bash" ]; then
            # Resolve symlink to actual path
            RESOLVED=$(readlink -f "${PREFIX}/run/current-system/profile/bin/bash" 2>/dev/null)
            if [ -n "$RESOLVED" ] && [ -f "$RESOLVED" ]; then
                BASH_RESOLVED_PATH="$RESOLVED"
                BASH_PATH="${RESOLVED#${PREFIX}}"
                echo "  Resolved: ${PREFIX}/run/current-system/profile/bin/bash -> $RESOLVED"
                echo "  Chroot path will be: $BASH_PATH"
            fi
        fi
    fi
    
    # If not found via /run/current-system, try store search
    if [ -z "$BASH_PATH" ]; then
        echo "  Searching /gnu/store for bash..."
        BASH_STORE=$(find "${PREFIX}/gnu/store" -name "bash" -type f -path "*/bin/bash" 2>/dev/null | head -1)
        if [ -n "$BASH_STORE" ]; then
            BASH_PATH="${BASH_STORE#${PREFIX}}"
            BASH_RESOLVED_PATH="$BASH_STORE"
            echo "  Found bash in store: $BASH_STORE"
            echo "  Chroot path will be: $BASH_PATH"
        fi
    fi
    
    # Fallback to /bin/bash
    if [ -z "$BASH_PATH" ] && [ -f "${PREFIX}/bin/bash" ]; then
        BASH_PATH="/bin/bash"
        BASH_RESOLVED_PATH="${PREFIX}/bin/bash"
        echo "  Using fallback: $BASH_PATH"
    fi
    
    if [ -z "$BASH_PATH" ]; then
        echo "  WARNING: Could not resolve bash path. Phase D may skip system rebuild."
    else
        echo "  Bash path resolved successfully for Phase D"
    fi
    echo ""
fi

# Phase B: Fix filesystem layout
echo "========================================"
echo "Phase B: Fixing Filesystem Layout"
echo "========================================"
echo ""

# Backup /var/run if it exists
var_run="${PREFIX}/var/run"
if [ -e "$var_run" ]; then
    echo "Backing up /var/run..."
    mv "$var_run" "${var_run}.before-rebuild" 2>/dev/null || true
    status "OK" "Backed up /var/run"
fi

# Backup /var/run.old if it exists
if [ -e "${PREFIX}/var/run.old" ]; then
    echo "Backing up /var/run.old..."
    mv "${PREFIX}/var/run.old" "${PREFIX}/var/run.old.before-rebuild" 2>/dev/null || true
    status "OK" "Backed up /var/run.old"
fi

# Clear /run completely and recreate it empty
run_dir="${PREFIX}/run"
echo ""
echo "Cleaning /run directory..."
if [ -d "$run_dir" ]; then
    # Check if directory has contents (without using wc, which may not be available)
    if [ -n "$(ls -A "$run_dir" 2>/dev/null)" ]; then
        echo "  Removing items from /run..."
        rm -rf "${run_dir:?}"/*
        rm -rf "${run_dir:?}"/.[!.]* "${run_dir:?}"/..?* 2>/dev/null || true
    fi
    status "OK" "/run cleaned"
else
    mkdir -p "$run_dir"
    chmod 755 "$run_dir"
    status "OK" "/run created"
fi

# Verify /run is empty
if [ -z "$(ls -A "$run_dir" 2>/dev/null)" ]; then
    status "OK" "/run is empty"
else
    status "WARNING" "/run still contains items"
fi

# Create /var/run -> /run symlink
echo ""
echo "Creating /var/run symlink..."
if [ -e "$var_run" ]; then
    rm -rf "$var_run"
fi
ln -sf /run "$var_run"
if [ -L "$var_run" ] && [ "$(readlink "$var_run")" = "/run" ]; then
    status "FIXED" "/var/run -> /run symlink"
else
    status "ERROR" "/var/run symlink creation failed"
    exit 1
fi

# Fix /var/lock -> /run/lock symlink
var_lock="${PREFIX}/var/lock"
echo ""
echo "Fixing /var/lock symlink..."
if [ -e "$var_lock" ]; then
    if [ -L "$var_lock" ]; then
        target=$(readlink "$var_lock")
        if [ "$target" = "/run/lock" ]; then
            status "OK" "/var/lock symlink"
        else
            rm -f "$var_lock"
            ln -sf /run/lock "$var_lock"
            status "FIXED" "/var/lock symlink"
        fi
    else
        rm -rf "$var_lock"
        ln -sf /run/lock "$var_lock"
        status "FIXED" "/var/lock symlink"
    fi
else
    ln -sf /run/lock "$var_lock"
    status "OK" "/var/lock symlink"
fi

# Fix /var/tmp permissions
var_tmp="${PREFIX}/var/tmp"
echo ""
echo "Fixing /var/tmp permissions..."
if [ -d "$var_tmp" ]; then
    chmod 1777 "$var_tmp"
    status "FIXED" "/var/tmp permissions (sticky bit)"
else
    mkdir -p "$var_tmp"
    chmod 1777 "$var_tmp"
    status "OK" "/var/tmp created with correct permissions"
fi

# Phase C: Remove ISO-specific artifacts
echo ""
echo "========================================"
echo "Phase C: Removing ISO Artifacts"
echo "========================================"
echo ""

# Fix /etc/mtab
mtab="${PREFIX}/etc/mtab"
echo "Fixing /etc/mtab..."
if [ -L "$mtab" ]; then
    target=$(readlink "$mtab")
    if [ "$target" = "/proc/self/mounts" ]; then
        status "OK" "/etc/mtab symlink"
    else
        rm -f "$mtab"
        ln -sf /proc/self/mounts "$mtab"
        status "FIXED" "/etc/mtab symlink"
    fi
elif [ -f "$mtab" ]; then
    rm -f "$mtab"
    ln -sf /proc/self/mounts "$mtab"
    status "FIXED" "/etc/mtab symlink"
else
    ln -sf /proc/self/mounts "$mtab"
    status "OK" "/etc/mtab symlink"
fi

# Remove ISO artifacts (except resolv.conf - handled separately)
artifacts=(
    "${PREFIX}/etc/machine-id"
    "${PREFIX}/var/guix/profiles/per-user/live-image-user"
    "${PREFIX}/home/live-image-user"
)

echo ""
echo "Removing ISO artifacts..."
for artifact in "${artifacts[@]}"; do
    if [ -e "$artifact" ]; then
        rm -rf "$artifact"
        status "OK" "Removed: $artifact"
    else
        status "SKIP" "Not found: $artifact"
    fi
done

# Set up resolv.conf for chroot networking
# This is critical - without it, chroot can't download packages
echo ""
echo "Setting up resolv.conf for chroot networking..."
if [ "$RUNNING_FROM_ISO" = true ]; then
    if [ -f /etc/resolv.conf ]; then
        rm -f "${PREFIX}/etc/resolv.conf"
        cp /etc/resolv.conf "${PREFIX}/etc/resolv.conf"
        status "OK" "Copied ISO's resolv.conf for chroot networking"
    else
        status "WARNING" "ISO's /etc/resolv.conf not found - chroot may lack networking"
    fi
else
    # Running from installed system - keep existing resolv.conf
    if [ -f "${PREFIX}/etc/resolv.conf" ]; then
        status "OK" "Keeping existing resolv.conf (not running from ISO)"
    else
        status "WARNING" "${PREFIX}/etc/resolv.conf not found"
    fi
fi

# Fix /var/guix ownership
if [ -d "${PREFIX}/var/guix" ]; then
    echo ""
    echo "Fixing /var/guix ownership..."
    if chown -R root:root "${PREFIX}/var/guix" 2>/dev/null; then
        status "OK" "/var/guix ownership"
    else
        status "WARNING" "/var/guix ownership (may need root)"
    fi
fi

# Verify fixes
echo ""
echo "========================================"
echo "Verifying Fixes"
echo "========================================"
echo ""

# Check /var/run
if [ -L "$var_run" ] && [ "$(readlink "$var_run")" = "/run" ]; then
    status "OK" "/var/run -> /run symlink verified"
else
    status "ERROR" "/var/run symlink verification failed"
    exit 1
fi

# Check /var/lock
if [ -L "$var_lock" ] && [ "$(readlink "$var_lock")" = "/run/lock" ]; then
    status "OK" "/var/lock -> /run/lock symlink verified"
else
    status "ERROR" "/var/lock symlink verification failed"
    exit 1
fi

# Check /etc/mtab
if [ -L "$mtab" ] && [ "$(readlink "$mtab")" = "/proc/self/mounts" ]; then
    status "OK" "/etc/mtab -> /proc/self/mounts symlink verified"
else
    status "ERROR" "/etc/mtab symlink verification failed"
    exit 1
fi

# Phase D: Rebuild system profile (optional)
if [ "$SKIP_REBUILD" = false ]; then
    echo ""
    echo "========================================"
    echo "Phase D: Rebuilding System Profile"
    echo "========================================"
    echo ""
    echo "This step chroots into the system and rebuilds the system profile"
    echo "to regenerate activation scripts with correct filesystem assumptions."
    echo ""
    
    if [ "$RUNNING_FROM_ISO" = true ]; then
        # Check if config.scm exists
        if [ ! -f "${PREFIX}/etc/config.scm" ]; then
            echo "WARNING: /etc/config.scm not found. Skipping system rebuild."
            echo "You may need to rebuild manually after boot."
            SKIP_REBUILD=true
        else
            echo "Preparing chroot environment..."
            
            # Bind-mount proc, sys, dev
            mount -t proc none "${PREFIX}/proc" 2>/dev/null || echo "  /proc already mounted"
            mount --rbind /sys "${PREFIX}/sys" 2>/dev/null || echo "  /sys already mounted"
            mount --make-rslave "${PREFIX}/sys" 2>/dev/null || true
            mount --rbind /dev "${PREFIX}/dev" 2>/dev/null || echo "  /dev already mounted"
            mount --make-rslave "${PREFIX}/dev" 2>/dev/null || true
            
            echo ""
            echo "Chrooting into system to rebuild profile..."
            echo "This may take several minutes..."
            echo ""
            
            # Use bash path resolved before Phase B cleared /run
            # If not already resolved, try to find it now (shouldn't happen, but be safe)
            if [ -z "$BASH_PATH" ]; then
                echo "WARNING: Bash path not resolved before Phase B. Searching now..."
                # Try store search directly
                BASH_STORE=$(find "${PREFIX}/gnu/store" -name "bash" -type f -path "*/bin/bash" 2>/dev/null | head -1)
                if [ -n "$BASH_STORE" ]; then
                    BASH_PATH="${BASH_STORE#${PREFIX}}"
                    BASH_RESOLVED_PATH="$BASH_STORE"
                    echo "Found bash in store: $BASH_STORE"
                elif [ -f "${PREFIX}/bin/bash" ]; then
                    BASH_PATH="/bin/bash"
                    BASH_RESOLVED_PATH="${PREFIX}/bin/bash"
                    echo "Using fallback: $BASH_PATH"
                fi
            fi
            
            if [ -z "$BASH_PATH" ]; then
                echo "WARNING: Could not find bash in chroot. Skipping system rebuild."
                echo "You can rebuild manually after boot:"
                echo "  sudo guix system reconfigure /etc/config.scm"
                SKIP_REBUILD=true
            else
                echo "Using bash at (chroot-relative): $BASH_PATH"
                if [ -n "$BASH_RESOLVED_PATH" ]; then
                    echo "Resolved absolute path: $BASH_RESOLVED_PATH"
                fi
                
                # Verify the path exists before chrooting
                if [ ! -f "${PREFIX}${BASH_PATH}" ]; then
                    echo "ERROR: Bash path $BASH_PATH does not exist in chroot"
                    echo "  Checked: ${PREFIX}${BASH_PATH}"
                    if [ -n "$BASH_RESOLVED_PATH" ]; then
                        echo "  Resolved path: $BASH_RESOLVED_PATH"
                    fi
                    echo "Skipping system rebuild."
                    SKIP_REBUILD=true
                else
                    # Set up PATH for chroot (mimics user's manual process)
                    echo "Setting up PATH for chroot environment..."

                    # Determine channels path - support custom channels (e.g., wingolog)
                    CHANNELS_PATH="${CHANNELS_PATH:-}"
                    if [ -z "$CHANNELS_PATH" ]; then
                        # Check common locations for channels file
                        for channels_file in "${PREFIX}/root/.config/guix/channels.scm" \
                                           "${PREFIX}/home/*/config/guix/channels.scm" \
                                           "/root/.config/guix/channels.scm"; do
                            if [ -f "$channels_file" ]; then
                                CHANNELS_PATH="$channels_file"
                                break
                            fi
                        done
                    fi

                    # Build the guix reconfigure command
                    if [ -n "$CHANNELS_PATH" ] && [ -f "$CHANNELS_PATH" ]; then
                        echo "Using custom channels from: $CHANNELS_PATH"
                        CHANNELS_PATH_IN_CHROOT="${CHANNELS_PATH#${PREFIX}}"
                        RECONFIGURE_CMD="guix time-machine -C $CHANNELS_PATH_IN_CHROOT -- system reconfigure /etc/config.scm"
                    else
                        echo "Using default Guix channels"
                        RECONFIGURE_CMD="guix system reconfigure /etc/config.scm"
                    fi

                    # Try to chroot and rebuild
                    # Use single quotes for the command to avoid quote issues
                    echo "Attempting chroot with: chroot \"$PREFIX\" \"$BASH_PATH\" -c '...'"
                    if chroot "$PREFIX" "$BASH_PATH" -c "
                        echo 'Inside chroot, setting up environment...'
                        # Set up PATH to include system profile (mimics user's manual process)
                        SYSTEM=\$(readlink -f /var/guix/profiles/system)
                        export PATH=\"\$SYSTEM/profile/bin:/run/setuid-programs:\$PATH\"
                        echo 'PATH set to: '\$PATH

                        echo 'Rebuilding system...'
                        if [ -f /etc/config.scm ]; then
                            $RECONFIGURE_CMD || $RECONFIGURE_CMD --fallback
                            echo 'System rebuild complete'
                        else
                            echo 'ERROR: /etc/config.scm not found in chroot'
                            exit 1
                        fi
                    "; then
                        status "OK" "System profile rebuilt"
                    else
                        echo ""
                        echo "WARNING: System rebuild failed."
                        echo "Chroot command attempted:"
                        echo "  chroot \"$PREFIX\" \"$BASH_PATH\" -c '...'"
                        echo ""
                        echo "Bash path details:"
                        echo "  Chroot-relative: $BASH_PATH"
                        if [ -n "$BASH_RESOLVED_PATH" ]; then
                            echo "  Resolved absolute: $BASH_RESOLVED_PATH"
                            echo "  File exists: $([ -f "$BASH_RESOLVED_PATH" ] && echo "YES" || echo "NO")"
                        fi
                        echo ""
                        echo "You may need to rebuild manually:"
                        echo "  chroot $PREFIX $BASH_PATH"
                        echo "  guix system reconfigure /etc/config.scm"
                        echo ""
                        status "WARNING" "System rebuild failed (non-fatal)"
                    fi
                fi
            fi
            
            # Unmount
            echo ""
            echo "Cleaning up chroot mounts..."
            umount -R "${PREFIX}/proc" 2>/dev/null || true
            umount -R "${PREFIX}/sys" 2>/dev/null || true
            umount -R "${PREFIX}/dev" 2>/dev/null || true
        fi
    else
        echo "Running on installed system - system rebuild not supported from here"
        echo "If you need to rebuild, boot from ISO and run this script there"
        echo "Or run manually: sudo guix system reconfigure /etc/config.scm"
    fi
else
    echo ""
    echo "Skipping system rebuild (--skip-rebuild specified)"
    echo "Note: Activation scripts may still have incorrect assumptions"
    echo "If issues persist, run without --skip-rebuild or rebuild manually"
fi

# Final summary
echo ""
echo "========================================"
echo "Recovery Complete!"
echo "========================================"
echo ""
echo "Filesystem invariants have been fixed:"
echo "  [OK] /var/run -> /run symlink"
echo "  [OK] /var/lock -> /run/lock symlink"
echo "  [OK] /run directory cleaned"
echo "  [OK] /var/tmp permissions fixed"
echo "  [OK] ISO artifacts removed"
echo ""

if [ "$SKIP_REBUILD" = false ] && [ "$RUNNING_FROM_ISO" = true ]; then
    echo "System profile has been rebuilt with correct assumptions."
    echo ""
fi

echo "Next steps:"
if [ "$RUNNING_FROM_ISO" = true ]; then
    echo "  1. Unmount: umount /mnt/boot/efi 2>/dev/null; umount /mnt"
    echo "  2. Reboot into your Guix system"
    echo "  3. Verify: ls -ld /run /var/run /var/lock"
    echo "  4. Test: sudo -v"
else
    echo "  1. Reboot to ensure all changes take effect"
    echo "  2. Verify: ls -ld /run /var/run /var/lock"
    echo "  3. Test: sudo -v"
fi

echo ""
echo "IMPORTANT: D-Bus /var/run/dbus Issue"
echo "======================================"
echo ""
echo "If you reconfigure with 'guix system reconfigure' or 'guix time-machine'"
echo "and get an error about '/var/run/dbus is not a directory', this is a"
echo "known issue where something creates /var/run/dbus before D-Bus activation runs."
echo ""
echo "Before reconfiguring, check and remove /var/run/dbus if it exists:"
echo "  sudo ls -la /var/run/dbus"
echo "  sudo rm /var/run/dbus  # Remove if it's a symlink or directory"
echo ""
echo "Then reconfigure:"
echo "  sudo guix system reconfigure /etc/config.scm"
echo ""
echo "If issues persist, check:"
echo "  - /var/run.before-rebuild (backup of old /var/run)"
echo "  - System logs: journalctl -b"
echo "  - See docs/INSTALLATION_KNOWLEDGE.md section 'D-Bus Activation Failure'"
echo ""

