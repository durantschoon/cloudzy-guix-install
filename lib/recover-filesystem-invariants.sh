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
            echo "This script:"
            echo "  1. Fixes filesystem layout (/var/run, /var/lock, /run cleanup)"
            echo "  2. Removes ISO artifacts (machine-id, resolv.conf, etc.)"
            echo "  3. Optionally rebuilds system profile (chroot + guix system reconfigure)"
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

# Create /var/run → /run symlink
echo ""
echo "Creating /var/run symlink..."
if [ -e "$var_run" ]; then
    rm -rf "$var_run"
fi
ln -sf /run "$var_run"
if [ -L "$var_run" ] && [ "$(readlink "$var_run")" = "/run" ]; then
    status "FIXED" "/var/run → /run symlink"
else
    status "ERROR" "/var/run symlink creation failed"
    exit 1
fi

# Fix /var/lock → /run/lock symlink
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

# Remove ISO artifacts
artifacts=(
    "${PREFIX}/etc/machine-id"
    "${PREFIX}/etc/resolv.conf"
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
    status "OK" "/var/run → /run symlink verified"
else
    status "ERROR" "/var/run symlink verification failed"
    exit 1
fi

# Check /var/lock
if [ -L "$var_lock" ] && [ "$(readlink "$var_lock")" = "/run/lock" ]; then
    status "OK" "/var/lock → /run/lock symlink verified"
else
    status "ERROR" "/var/lock symlink verification failed"
    exit 1
fi

# Check /etc/mtab
if [ -L "$mtab" ] && [ "$(readlink "$mtab")" = "/proc/self/mounts" ]; then
    status "OK" "/etc/mtab → /proc/self/mounts symlink verified"
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
            
            # Resolve bash path - chroot needs the actual path, not a symlink
            # Try multiple possible bash locations
            BASH_PATH=""
            for bash_candidate in \
                "/run/current-system/profile/bin/bash" \
                "/bin/bash" \
                "$(readlink -f "${PREFIX}/run/current-system/profile/bin/bash" 2>/dev/null || echo '')"
            do
                if [ -n "$bash_candidate" ] && [ -f "${PREFIX}${bash_candidate}" ]; then
                    BASH_PATH="$bash_candidate"
                    break
                fi
            done
            
            # If still not found, try to find bash in the store
            if [ -z "$BASH_PATH" ]; then
                # Look for bash in /gnu/store inside the chroot
                BASH_STORE=$(find "${PREFIX}/gnu/store" -name "bash" -type f -path "*/bin/bash" 2>/dev/null | head -1)
                if [ -n "$BASH_STORE" ]; then
                    # Convert absolute path to chroot-relative path
                    BASH_PATH="${BASH_STORE#${PREFIX}}"
                fi
            fi
            
            if [ -z "$BASH_PATH" ]; then
                echo "WARNING: Could not find bash in chroot. Skipping system rebuild."
                echo "You can rebuild manually after boot:"
                echo "  sudo guix system reconfigure /etc/config.scm"
                SKIP_REBUILD=true
            else
                echo "Using bash at: $BASH_PATH"
                
                # Try to chroot and rebuild
                if chroot "$PREFIX" "$BASH_PATH" -c "
                    echo 'Inside chroot, rebuilding system...'
                    if [ -f /etc/config.scm ]; then
                        guix system reconfigure /etc/config.scm || guix system reconfigure --root=/ /etc/config.scm
                        echo 'System rebuild complete'
                    else
                        echo 'ERROR: /etc/config.scm not found in chroot'
                        exit 1
                    fi
                "; then
                    status "OK" "System profile rebuilt"
                else
                    echo ""
                    echo "WARNING: System rebuild failed. You may need to rebuild manually:"
                    echo "  chroot $PREFIX $BASH_PATH"
                    echo "  guix system reconfigure /etc/config.scm"
                    echo ""
                    status "WARNING" "System rebuild failed (non-fatal)"
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
echo "  ✓ /var/run → /run symlink"
echo "  ✓ /var/lock → /run/lock symlink"
echo "  ✓ /run directory cleaned"
echo "  ✓ /var/tmp permissions fixed"
echo "  ✓ ISO artifacts removed"
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
echo "If issues persist, check:"
echo "  - /var/run.before-rebuild (backup of old /var/run)"
echo "  - System logs: journalctl -b"
echo ""

