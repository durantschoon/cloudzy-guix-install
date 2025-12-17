#!/run/current-system/profile/bin/bash
# Diagnostic script to investigate where kernel/initrd files are located
# Run this on the installed Guix system to understand what worked

echo "=== Kernel/Initrd Location Investigation ==="
echo ""

echo "1. Checking /boot/ directory:"
ls -lh /boot/ 2>/dev/null || echo "  /boot/ not found or empty"
echo ""

echo "2. Checking kernel files:"
ls -lh /boot/vmlinuz* 2>/dev/null || echo "  No vmlinuz files found"
echo ""

echo "3. Checking initrd files:"
ls -lh /boot/initrd* 2>/dev/null || echo "  No initrd files found"
echo ""

echo "4. Checking current system generation symlink:"
if [ -L /run/current-system ]; then
    SYSTEM_PATH=$(readlink -f /run/current-system)
    echo "  Symlink: /run/current-system -> $SYSTEM_PATH"
    echo ""
    echo "5. Listing files in system generation:"
    ls -lh "$SYSTEM_PATH" 2>/dev/null | head -20
    echo ""
    echo "6. Checking for kernel in system generation:"
    if [ -f "$SYSTEM_PATH/kernel" ]; then
        echo "  ✓ Found: $SYSTEM_PATH/kernel ($(du -h "$SYSTEM_PATH/kernel" | cut -f1))"
    else
        echo "  ✗ Not found: $SYSTEM_PATH/kernel"
    fi
    echo ""
    echo "7. Checking for initrd in system generation:"
    if [ -f "$SYSTEM_PATH/initrd" ]; then
        echo "  ✓ Found: $SYSTEM_PATH/initrd ($(du -h "$SYSTEM_PATH/initrd" | cut -f1))"
    else
        echo "  ✗ Not found: $SYSTEM_PATH/initrd"
    fi
    echo ""
    echo "8. Checking boot/ subdirectory in system generation:"
    if [ -d "$SYSTEM_PATH/boot" ]; then
        echo "  ✓ boot/ directory exists:"
        ls -lh "$SYSTEM_PATH/boot" 2>/dev/null | head -10
    else
        echo "  ✗ boot/ directory not found"
    fi
    echo ""
    echo "9. Checking alternative kernel locations:"
    for loc in "$SYSTEM_PATH/kernel" "$SYSTEM_PATH/boot/kernel" "$SYSTEM_PATH/boot/vmlinuz" "$SYSTEM_PATH/boot/vmlinuz-linux"; do
        if [ -f "$loc" ]; then
            echo "  ✓ Found: $loc ($(du -h "$loc" | cut -f1))"
        fi
    done
    echo ""
    echo "10. Checking alternative initrd locations:"
    for loc in "$SYSTEM_PATH/initrd" "$SYSTEM_PATH/boot/initrd" "$SYSTEM_PATH/boot/initrd.gz"; do
        if [ -f "$loc" ]; then
            echo "  ✓ Found: $loc ($(du -h "$loc" | cut -f1))"
        fi
    done
else
    echo "  ✗ /run/current-system symlink not found"
fi
echo ""

echo "11. Checking GRUB configuration:"
if [ -f /boot/grub/grub.cfg ]; then
    echo "  GRUB config exists"
    echo "  Kernel references:"
    grep -E "(vmlinuz|kernel)" /boot/grub/grub.cfg | head -5
else
    echo "  ✗ GRUB config not found"
fi
echo ""

echo "=== Investigation Complete ==="

