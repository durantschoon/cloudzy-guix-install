#!/run/current-system/profile/bin/bash
# Quick test script to verify kernel symlink fix
# Run this on an existing Cloudzy Guix system

echo "========================================"
echo "  Testing Kernel Symlink Fix"
echo "========================================"
echo ""

# Check if kernel file exists
if [ ! -f /boot/vmlinuz ]; then
    echo "[ERROR] Kernel file not found at /boot/vmlinuz"
    echo "        This test requires an installed system with kernel files"
    exit 1
fi

# Check file type (should be kernel binary, not symlink)
echo "1. Checking file type..."
file /boot/vmlinuz
echo ""

# Check file size (should be 5-15 MB, not a few bytes)
echo "2. Checking file size..."
KERNEL_SIZE=$(stat -c%s /boot/vmlinuz 2>/dev/null || stat -f%z /boot/vmlinuz 2>/dev/null)
KERNEL_SIZE_MB=$(echo "scale=2; $KERNEL_SIZE / 1024 / 1024" | bc)
echo "   Kernel size: ${KERNEL_SIZE_MB} MB"
echo ""

# Check if it's a symlink (should NOT be)
if [ -L /boot/vmlinuz ]; then
    echo "[ERROR] Kernel file is a SYMLINK (fix didn't work!)"
    echo "        Symlink target: $(readlink /boot/vmlinuz)"
    exit 1
else
    echo "[OK] Kernel file is a real file (not a symlink)"
fi

# Check size threshold (should be > 5 MB)
if [ "$KERNEL_SIZE" -lt 5242880 ]; then
    echo "[ERROR] Kernel file is too small (${KERNEL_SIZE_MB} MB)"
    echo "        Expected: > 5 MB"
    echo "        This suggests the symlink was copied instead of the actual file"
    exit 1
else
    echo "[OK] Kernel file size is reasonable (${KERNEL_SIZE_MB} MB)"
fi

# Check initrd if it exists
if [ -f /boot/initrd ]; then
    echo ""
    echo "3. Checking initrd..."
    INITRD_SIZE=$(stat -c%s /boot/initrd 2>/dev/null || stat -f%z /boot/initrd 2>/dev/null)
    INITRD_SIZE_MB=$(echo "scale=2; $INITRD_SIZE / 1024 / 1024" | bc)
    echo "   Initrd size: ${INITRD_SIZE_MB} MB"
    
    if [ -L /boot/initrd ]; then
        echo "[ERROR] Initrd file is a SYMLINK (fix didn't work!)"
        exit 1
    elif [ "$INITRD_SIZE" -lt 10485760 ]; then
        echo "[ERROR] Initrd file is too small (${INITRD_SIZE_MB} MB)"
        echo "        Expected: > 10 MB"
        exit 1
    else
        echo "[OK] Initrd file size is reasonable (${INITRD_SIZE_MB} MB)"
    fi
fi

echo ""
echo "========================================"
echo "  TEST PASSED"
echo "========================================"
echo ""
echo "The symlink fix appears to be working correctly."
echo "Kernel files are real binaries, not symlinks."

