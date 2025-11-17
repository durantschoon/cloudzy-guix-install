#!/run/current-system/profile/bin/bash
# Diagnostic script to understand why kernel/initrd are missing after install
# Run this on the Guix ISO after a failed installation (before unmounting /mnt)

set -u

echo "=== Boot Files Diagnostic ==="
echo ""

# Check if /mnt is mounted
if ! mountpoint -q /mnt 2>/dev/null; then
    echo "ERROR: /mnt is not mounted"
    echo "This script should be run on the Guix ISO after installation"
    exit 1
fi

echo "1. Check /mnt/boot contents:"
ls -lah /mnt/boot/
echo ""

echo "2. Check /mnt/run/current-system symlink:"
if [ -L "/mnt/run/current-system" ]; then
    echo "  Symlink exists:"
    ls -la /mnt/run/current-system

    TARGET=$(readlink -f /mnt/run/current-system 2>/dev/null || readlink /mnt/run/current-system)
    echo "  Target: $TARGET"

    if [ -d "$TARGET" ]; then
        echo "  Target directory exists: YES"
        echo ""
        echo "3. Contents of system generation:"
        ls -lah "$TARGET/" | head -20
        echo ""

        echo "4. Check for kernel/initrd in system generation:"
        if [ -e "$TARGET/kernel" ]; then
            echo "  kernel: EXISTS"
            ls -lh "$TARGET/kernel"
            KERNEL_TARGET=$(readlink -f "$TARGET/kernel" 2>/dev/null || echo "$TARGET/kernel")
            echo "  kernel target: $KERNEL_TARGET"
        else
            echo "  kernel: NOT FOUND"
        fi

        if [ -e "$TARGET/initrd" ]; then
            echo "  initrd: EXISTS"
            ls -lh "$TARGET/initrd"
            INITRD_TARGET=$(readlink -f "$TARGET/initrd" 2>/dev/null || echo "$TARGET/initrd")
            echo "  initrd target: $INITRD_TARGET"
        else
            echo "  initrd: NOT FOUND"
        fi
    else
        echo "  Target directory exists: NO"
        echo "  ERROR: Symlink points to non-existent directory!"
    fi
else
    echo "  Symlink does not exist!"
    echo ""
    echo "3. Searching for system generations in /gnu/store:"
    ls -td /gnu/store/*-system 2>/dev/null | head -5
fi

echo ""
echo "5. Search for kernel files in /gnu/store:"
find /gnu/store -name "vmlinuz*" -o -name "bzImage*" 2>/dev/null | head -10

echo ""
echo "6. Search for initrd files in /gnu/store:"
find /gnu/store -name "initrd*" -o -name "initramfs*" 2>/dev/null | head -10

echo ""
echo "7. Check /mnt/boot/efi:"
if [ -d "/mnt/boot/efi" ]; then
    echo "  EFI directory exists"
    ls -lah /mnt/boot/efi/
    echo ""
    if [ -d "/mnt/boot/efi/EFI" ]; then
        ls -lah /mnt/boot/efi/EFI/
    fi
else
    echo "  EFI directory does NOT exist"
fi

echo ""
echo "8. Check GRUB config:"
if [ -f "/mnt/boot/grub/grub.cfg" ]; then
    echo "  GRUB config exists"
    echo "  Kernel entries in GRUB:"
    grep -E "linux|initrd" /mnt/boot/grub/grub.cfg | head -10
else
    echo "  GRUB config NOT FOUND"
fi

echo ""
echo "=== Diagnostic complete ==="
echo ""
echo "Next steps:"
echo "  1. Review the output above"
echo "  2. If kernel/initrd exist in system generation, we can copy them manually"
echo "  3. If they don't exist at all, the build may have failed"
echo ""
