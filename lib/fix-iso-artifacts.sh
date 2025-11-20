#!/run/current-system/profile/bin/bash
set -euo pipefail

# One-time fix script for ISO artifacts and filesystem invariants
# Run this from your installed Guix system (or from ISO with /mnt mounted)
# Fixes issues caused by rsync/cp copying ISO filesystem structure

echo "========================================"
echo "  ISO Artifacts Fix Script"
echo "========================================"
echo ""
echo "This script fixes filesystem invariants that can cause boot/service issues"
echo "after copying from the Guix ISO."
echo ""

# Determine if we're running on installed system or ISO
if [ -d "/mnt/etc" ] && [ -d "/mnt/var" ]; then
    PREFIX="/mnt"
    echo "Running from ISO: fixing files in /mnt"
elif [ -f "/etc/config.scm" ]; then
    PREFIX=""
    echo "Running on installed system: fixing files in /"
else
    echo "ERROR: Cannot determine if running from ISO or installed system"
    echo "If running from ISO, ensure /mnt is mounted"
    echo "If running on installed system, ensure /etc/config.scm exists"
    exit 1
fi

echo ""
echo "Fixing filesystem invariants..."
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

# 1. Fix /var/run symlink (CRITICAL)
var_run="${PREFIX}/var/run"
if [ -L "$var_run" ]; then
    target=$(readlink "$var_run")
    if [ "$target" = "/run" ]; then
        status "OK" "/var/run symlink"
    else
        echo "  Fixing /var/run: wrong target ($target), fixing..."
        rm -f "$var_run"
        ln -sf /run "$var_run"
        status "FIXED" "/var/run symlink"
    fi
elif [ -d "$var_run" ]; then
    echo "  Fixing /var/run: removing directory, creating symlink..."
    rm -rf "$var_run"
    ln -sf /run "$var_run"
    status "FIXED" "/var/run symlink"
elif [ ! -e "$var_run" ]; then
    echo "  Creating /var/run symlink..."
    ln -sf /run "$var_run"
    status "OK" "/var/run symlink"
else
    status "WARNING" "/var/run: unexpected file type"
fi

# 2. Fix /etc/mtab symlink (IMPORTANT)
mtab="${PREFIX}/etc/mtab"
if [ -L "$mtab" ]; then
    target=$(readlink "$mtab")
    if [ "$target" = "/proc/self/mounts" ]; then
        status "OK" "/etc/mtab symlink"
    else
        echo "  Fixing /etc/mtab: wrong target ($target), fixing..."
        rm -f "$mtab"
        ln -sf /proc/self/mounts "$mtab"
        status "FIXED" "/etc/mtab symlink"
    fi
elif [ -f "$mtab" ]; then
    echo "  Fixing /etc/mtab: removing file, creating symlink..."
    rm -f "$mtab"
    ln -sf /proc/self/mounts "$mtab"
    status "FIXED" "/etc/mtab symlink"
elif [ ! -e "$mtab" ]; then
    echo "  Creating /etc/mtab symlink..."
    ln -sf /proc/self/mounts "$mtab"
    status "OK" "/etc/mtab symlink"
else
    status "WARNING" "/etc/mtab: unexpected file type"
fi

# 3. Remove ISO-specific artifacts
echo ""
echo "Removing ISO artifacts..."
artifacts=(
    "${PREFIX}/etc/machine-id"
    "${PREFIX}/etc/resolv.conf"
    "${PREFIX}/var/guix/profiles/per-user/live-image-user"
    "${PREFIX}/home/live-image-user"
)

for artifact in "${artifacts[@]}"; do
    if [ -e "$artifact" ]; then
        rm -rf "$artifact"
        status "OK" "Removed: $artifact"
    else
        status "SKIP" "Not found: $artifact"
    fi
done

# 4. Fix ownership of /var/guix
if [ -d "${PREFIX}/var/guix" ]; then
    echo ""
    echo "Fixing /var/guix ownership..."
    if chown -R root:root "${PREFIX}/var/guix" 2>/dev/null; then
        status "OK" "/var/guix ownership"
    else
        status "WARNING" "/var/guix ownership (may need root)"
    fi
fi

# 5. Verify fixes
echo ""
echo "Verifying fixes..."
echo ""

# Check /var/run
if [ -L "$var_run" ] && [ "$(readlink "$var_run")" = "/run" ]; then
    status "OK" "/var/run → /run symlink verified"
else
    status "ERROR" "/var/run symlink verification failed"
    exit 1
fi

# Check /etc/mtab
if [ -L "$mtab" ] && [ "$(readlink "$mtab")" = "/proc/self/mounts" ]; then
    status "OK" "/etc/mtab → /proc/self/mounts symlink verified"
else
    status "ERROR" "/etc/mtab symlink verification failed"
    exit 1
fi

echo ""
echo "========================================"
echo "  Fix Complete!"
echo "========================================"
echo ""
echo "All filesystem invariants have been fixed."
echo ""
if [ -n "$PREFIX" ]; then
    echo "You can now continue with your installation or reboot."
else
    echo "You may want to reboot to ensure all services start correctly."
fi
echo ""

