#!/run/current-system/profile/bin/bash
# Recovery script to complete Guix installation after time-machine
# Use this when guix time-machine ran but didn't complete fully
#
# This script assumes:
# - Partitions are formatted and mounted at /mnt
# - /mnt/etc/config.scm exists
# - /tmp/channels.scm exists (for nonguix)
# - guix time-machine was run but may have failed or been interrupted

set -e  # Exit on error

echo "=== Guix Installation Recovery Script ==="
echo ""
echo "This script will:"
echo "  1. Verify current installation state"
echo "  2. Re-run guix system init if needed (with time-machine)"
echo "  3. Set user password"
echo "  4. Download customization tools"
echo "  5. Configure dual-boot GRUB (if framework-dual)"
echo "  6. Write installation receipt"
echo "  7. Prepare for reboot"
echo ""
read -p "Continue? [Y/n] " -r </dev/tty
if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "Aborted."
    exit 0
fi

# Check if we're on Guix ISO
if [ ! -f /run/current-system/profile/bin/guix ]; then
    echo "[ERROR] Not running on Guix ISO - cannot continue"
    exit 1
fi

# Verify mounts
echo ""
echo "=== Verifying Mounts ==="
if ! mountpoint -q /mnt; then
    echo "[ERROR] /mnt is not mounted!"
    echo "Please run the mount step first:"
    echo "  mount LABEL=GUIX_ROOT /mnt"
    echo "  mkdir -p /mnt/boot/efi"
    echo "  mount LABEL=EFI /mnt/boot/efi"
    exit 1
fi
echo "[OK] /mnt is mounted"

if ! mountpoint -q /mnt/boot/efi; then
    echo "[ERROR] /mnt/boot/efi is not mounted!"
    echo "Please mount EFI:"
    echo "  mkdir -p /mnt/boot/efi"
    echo "  mount LABEL=EFI /mnt/boot/efi"
    exit 1
fi
echo "[OK] /mnt/boot/efi is mounted"

# Verify config exists
if [ ! -f /mnt/etc/config.scm ]; then
    echo "[ERROR] /mnt/etc/config.scm not found!"
    echo "Please generate config first (step 3)"
    exit 1
fi
echo "[OK] Config exists: /mnt/etc/config.scm"

# Check if channels.scm exists (needed for framework installers)
if [ -f /tmp/channels.scm ]; then
    echo "[OK] Channels file exists: /tmp/channels.scm"
    USE_TIME_MACHINE=true
else
    echo "[WARN] No channels.scm - using plain guix system init"
    USE_TIME_MACHINE=false
fi

# Verify installation state
echo ""
echo "=== Checking Installation State ==="
HAS_KERNEL=false
HAS_INITRD=false
HAS_GRUB_EFI=false

if ls /mnt/boot/vmlinuz-* >/dev/null 2>&1; then
    echo "[OK] Kernel found: $(ls /mnt/boot/vmlinuz-* | head -1 | xargs basename)"
    HAS_KERNEL=true
else
    echo "[MISSING] No kernel in /mnt/boot/"
fi

if ls /mnt/boot/initrd-* >/dev/null 2>&1; then
    echo "[OK] Initrd found: $(ls /mnt/boot/initrd-* | head -1 | xargs basename)"
    HAS_INITRD=true
else
    echo "[MISSING] No initrd in /mnt/boot/"
fi

if [ -f /mnt/boot/efi/EFI/Guix/grubx64.efi ] || [ -f /mnt/boot/efi/EFI/guix/grubx64.efi ]; then
    echo "[OK] GRUB EFI bootloader found"
    HAS_GRUB_EFI=true
else
    echo "[MISSING] No GRUB EFI bootloader"
fi

# Determine if we need to run system init
NEED_SYSTEM_INIT=false
if [ "$HAS_KERNEL" = false ] || [ "$HAS_INITRD" = false ]; then
    echo ""
    echo "[!] Installation is INCOMPLETE - system init must be run"
    NEED_SYSTEM_INIT=true
fi

# Setup environment for installation
echo ""
echo "=== Setting Up Environment ==="
export TMPDIR=/mnt/var/tmp
export XDG_CACHE_HOME=/mnt/var/cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"
echo "TMPDIR=$TMPDIR"
echo "XDG_CACHE_HOME=$XDG_CACHE_HOME"

# Clear substitute cache to free space
echo "Clearing substitute cache..."
rm -rf /var/guix/substitute-cache/ || true

# Run system init if needed
if [ "$NEED_SYSTEM_INIT" = true ]; then
    echo ""
    echo "=== Running System Init ==="

    # Verify ESP before init
    echo "Verifying EFI partition..."
    if ! df -T /mnt/boot/efi | grep -q vfat; then
        echo "[ERROR] /mnt/boot/efi is not vfat filesystem!"
        df -T /mnt/boot/efi
        exit 1
    fi
    echo "[OK] EFI partition is vfat"

    # Start cow-store
    echo "Starting cow-store..."
    if ! herd status cow-store | grep -q "running"; then
        herd start cow-store /mnt
        sleep 2
    fi
    echo "[OK] cow-store is running"

    # Ensure guix-daemon is responsive
    echo "Checking guix-daemon..."
    for i in {1..10}; do
        if guix build --version >/dev/null 2>&1; then
            echo "[OK] guix-daemon is responsive"
            break
        fi
        echo "Waiting for daemon... ($i/10)"
        sleep 3
    done

    # Use 3-step approach for both time-machine and free software installs
    # This ensures kernel/initrd are always copied correctly
    
    if [ "$USE_TIME_MACHINE" = true ]; then
        # Step 1: Build system with time-machine
        echo ""
        echo "=== Step 1/3: Building System (with time-machine) ==="
        echo "This will take 5-30 minutes depending on substitutes..."
        echo ""
        
        if ! guix time-machine -C /tmp/channels.scm -- system build /mnt/etc/config.scm --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"; then
            echo "[ERROR] System build failed!"
            exit 1
        fi
        
        # Step 2: Copy kernel/initrd
        echo ""
        echo "=== Step 2/3: Copying Kernel Files ==="
        SYSTEM_PATH=$(ls -td /gnu/store/*-system 2>/dev/null | head -1)
        if [ -z "$SYSTEM_PATH" ]; then
            echo "[ERROR] No system generation found in /gnu/store"
            exit 1
        fi
        
        echo "Found system: $SYSTEM_PATH"
        
        if [ -f "$SYSTEM_PATH/kernel" ]; then
            cp "$SYSTEM_PATH/kernel" /mnt/boot/vmlinuz
            echo "[OK] Copied kernel"
        else
            echo "[ERROR] Kernel not found in system generation"
            exit 1
        fi
        
        if [ -f "$SYSTEM_PATH/initrd" ]; then
            cp "$SYSTEM_PATH/initrd" /mnt/boot/initrd
            echo "[OK] Copied initrd"
        else
            echo "[ERROR] Initrd not found in system generation"
            exit 1
        fi
        
        # Create symlink
        rm -f /mnt/run/current-system
        ln -s "$SYSTEM_PATH" /mnt/run/current-system
        echo "[OK] Created current-system symlink"
        
        # Step 3: Install bootloader
        echo ""
        echo "=== Step 3/3: Installing Bootloader ==="
        echo "System already built, this should be quick..."
        echo ""
        
        if ! guix time-machine -C /tmp/channels.scm -- system init --fallback -v6 /mnt/etc/config.scm /mnt --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"; then
            echo "[ERROR] Bootloader installation failed!"
            echo "However, kernel and initrd are already in place."
            exit 1
        fi
    else
        # Free software install: Use same 3-step approach
        echo ""
        echo "=== Step 1/3: Building System (Free Software) ==="
        echo "This will take 5-30 minutes depending on substitutes..."
        echo ""
        
        if ! guix system build /mnt/etc/config.scm --substitute-urls="https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"; then
            echo "[ERROR] System build failed!"
            exit 1
        fi
        
        # Step 2: Copy kernel/initrd
        echo ""
        echo "=== Step 2/3: Copying Kernel Files ==="
        SYSTEM_PATH=$(ls -td /gnu/store/*-system 2>/dev/null | head -1)
        if [ -z "$SYSTEM_PATH" ]; then
            echo "[ERROR] No system generation found in /gnu/store"
            exit 1
        fi
        
        echo "Found system: $SYSTEM_PATH"
        
        if [ -f "$SYSTEM_PATH/kernel" ]; then
            cp "$SYSTEM_PATH/kernel" /mnt/boot/vmlinuz
            echo "[OK] Copied kernel"
        else
            echo "[ERROR] Kernel not found in system generation"
            exit 1
        fi
        
        if [ -f "$SYSTEM_PATH/initrd" ]; then
            cp "$SYSTEM_PATH/initrd" /mnt/boot/initrd
            echo "[OK] Copied initrd"
        else
            echo "[ERROR] Initrd not found in system generation"
            exit 1
        fi
        
        # Create symlink
        rm -f /mnt/run/current-system
        ln -s "$SYSTEM_PATH" /mnt/run/current-system
        echo "[OK] Created current-system symlink"
        
        # Step 3: Install bootloader
        echo ""
        echo "=== Step 3/3: Installing Bootloader ==="
        echo "System already built, this should be quick..."
        echo ""
        
        if ! guix system init --fallback -v6 /mnt/etc/config.scm /mnt --substitute-urls="https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"; then
            echo "[ERROR] Bootloader installation failed!"
            echo "However, kernel and initrd are already in place."
            exit 1
        fi
    fi
    
    # Verify installation succeeded
    echo ""
    echo "=== Verifying Installation ==="
    
    if ! ls /mnt/boot/vmlinuz* >/dev/null 2>&1; then
        echo "[ERROR] Kernel still missing after installation!"
        exit 1
    fi
    echo "[OK] Kernel present: $(ls /mnt/boot/vmlinuz* | head -1 | xargs basename)"
    
    if ! ls /mnt/boot/initrd* >/dev/null 2>&1; then
        echo "[ERROR] Initrd still missing after installation!"
        exit 1
    fi
    echo "[OK] Initrd present: $(ls /mnt/boot/initrd* | head -1 | xargs basename)"
    
    echo "[OK] Installation verification complete"
else
    echo ""
    echo "[OK] System init already complete - skipping"
fi

# Get username from config
echo ""
echo "=== Detecting Username ==="
USERNAME=$(grep -oP '(?<=\(name ")[^"]+' /mnt/etc/config.scm | head -1)
if [ -z "$USERNAME" ]; then
    echo "[WARN] Could not detect username from config.scm"
    read -p "Enter your username: " USERNAME </dev/tty
fi
echo "Username: $USERNAME"

# Set user password
echo ""
echo "=== Setting User Password ==="
echo "You need this password to log in after first boot."
echo ""

# Check if password is already set
PASSWORD_SET=false
if chroot /mnt /run/current-system/profile/bin/bash -c "grep '^$USERNAME:' /etc/shadow | grep -v ':!:'" >/dev/null 2>&1; then
    echo "[OK] Password already set for $USERNAME"
    read -p "Do you want to change it? [y/N] " -r </dev/tty
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        PASSWORD_SET=false
    else
        PASSWORD_SET=true
    fi
fi

if [ "$PASSWORD_SET" = false ]; then
    if chroot /mnt /run/current-system/profile/bin/passwd "$USERNAME"; then
        echo ""
        echo "[OK] Password set successfully for $USERNAME"
    else
        echo ""
        echo "[ERROR] Failed to set password!"
        echo "You can set it manually after first boot."
    fi
fi

# Download customization tools
echo ""
echo "=== Downloading Customization Tools ==="
PLATFORM="${GUIX_PLATFORM:-framework-dual}"
USER_HOME="/mnt/home/$USERNAME"
CUSTOMIZE_DIR="$USER_HOME/guix-customize"

if [ -d "$CUSTOMIZE_DIR" ]; then
    echo "[OK] Customization tools already present at $CUSTOMIZE_DIR"
else
    echo "Downloading to $CUSTOMIZE_DIR..."
    mkdir -p "$CUSTOMIZE_DIR"

    # Download customize script
    REPO_BASE="https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main"
    if wget -q -O "$CUSTOMIZE_DIR/customize" "$REPO_BASE/$PLATFORM/postinstall/customize"; then
        chmod +x "$CUSTOMIZE_DIR/customize"
        echo "[OK] Downloaded customize script"
    else
        echo "[WARN] Failed to download customization tools"
        echo "      You can download manually after first boot"
    fi

    # Set ownership
    chroot /mnt /run/current-system/profile/bin/chown -R "$USERNAME:users" "/home/$USERNAME/guix-customize" 2>/dev/null || true
fi

# Configure dual-boot GRUB (if framework-dual)
if [ "$PLATFORM" = "framework-dual" ]; then
    echo ""
    echo "=== Configuring Dual-Boot GRUB ==="

    # Check if os-prober is available in the installed system
    if chroot /mnt /run/current-system/profile/bin/bash -c "command -v os-prober" >/dev/null 2>&1; then
        echo "Enabling os-prober..."
        chroot /mnt /run/current-system/profile/bin/bash -c 'echo "GRUB_DISABLE_OS_PROBER=false" >> /etc/default/grub' 2>/dev/null || true

        echo "Running os-prober to detect Pop!_OS..."
        chroot /mnt /run/current-system/profile/bin/os-prober || true

        echo "Updating GRUB configuration..."
        chroot /mnt /run/current-system/profile/bin/grub-mkconfig -o /boot/grub/grub.cfg || {
            echo "[WARN] Failed to update GRUB config"
            echo "      You can run this after first boot:"
            echo "      sudo os-prober && sudo grub-mkconfig -o /boot/grub/grub.cfg"
        }
    else
        echo "[WARN] os-prober not found in installed system"
        echo "      You'll need to manually configure dual-boot after first boot"
    fi
fi

# Write installation receipt
echo ""
echo "=== Writing Installation Receipt ==="
RECEIPT_PATH="/mnt/root/install-receipt.txt"
cat > "$RECEIPT_PATH" <<EOF
Guix System Installation Receipt
=================================
Date: $(date)
Platform: $PLATFORM
Username: $USERNAME
Hostname: $(grep 'host-name' /mnt/etc/config.scm | grep -oP '(?<=")[^"]+')

Installation completed via recovery script.

Config: /etc/config.scm
Channels: $([ -f /tmp/channels.scm ] && echo "/tmp/channels.scm (nonguix)" || echo "default")

Next steps:
1. Log in with your username and password
2. Run: ~/guix-customize/customize (to add SSH, desktop, packages)
3. For dual-boot: Access Pop!_OS via F12 boot menu or GRUB menu

Installation log: /tmp/guix-install.log (if available)
EOF
echo "[OK] Installation receipt written to $RECEIPT_PATH"

# Final verification using comprehensive verification script
echo ""
echo "=== Final Verification ==="

# Try to use verify-guix-install.sh if available for comprehensive check
VERIFY_SCRIPT=""
if [ -f /root/verify-guix-install.sh ]; then
    VERIFY_SCRIPT="/root/verify-guix-install.sh"
elif [ -f ./verify-guix-install.sh ]; then
    VERIFY_SCRIPT="./verify-guix-install.sh"
fi

if [ -n "$VERIFY_SCRIPT" ]; then
    echo "Running comprehensive verification..."
    echo ""
    if bash "$VERIFY_SCRIPT"; then
        ALL_GOOD=true
    else
        ALL_GOOD=false
    fi
else
    # Fallback to basic checks if verify script not available
    echo "Note: Using basic verification (verify-guix-install.sh not found)"
    echo ""
    ALL_GOOD=true

    if ! ls /mnt/boot/vmlinuz-* >/dev/null 2>&1; then
        echo "[ERROR] No kernel installed!"
        ALL_GOOD=false
    else
        echo "[OK] Kernel: $(ls /mnt/boot/vmlinuz-* | head -1 | xargs basename)"
    fi

    if ! ls /mnt/boot/initrd-* >/dev/null 2>&1; then
        echo "[ERROR] No initrd installed!"
        ALL_GOOD=false
    else
        echo "[OK] Initrd: $(ls /mnt/boot/initrd-* | head -1 | xargs basename)"
    fi

    if [ -f /mnt/boot/efi/EFI/Guix/grubx64.efi ] || [ -f /mnt/boot/efi/EFI/guix/grubx64.efi ]; then
        echo "[OK] GRUB EFI bootloader installed"
    else
        echo "[ERROR] No GRUB EFI bootloader!"
        ALL_GOOD=false
    fi

    if [ -f /mnt/boot/grub/grub.cfg ]; then
        echo "[OK] GRUB config exists"
    else
        echo "[ERROR] No GRUB config!"
        ALL_GOOD=false
    fi
fi

echo ""
if [ "$ALL_GOOD" = true ]; then
    echo "=== Installation Complete! ==="
    echo ""
    echo "The system is ready to boot."
    echo ""
    echo "Next steps:"
    echo "  1. Sync and unmount:"
    echo "     sync"
    echo "     umount -R /mnt"
    echo "  2. Reboot:"
    echo "     reboot"
    echo ""
    echo "After first boot:"
    echo "  - Log in with username: $USERNAME"
    echo "  - Run: ~/guix-customize/customize"
    echo "  - For Pop!_OS: Press F12 at boot or select from GRUB menu"
    echo ""

    read -p "Unmount and reboot now? [y/N] " -r </dev/tty
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        sync
        umount -R /mnt
        reboot
    else
        echo ""
        echo "Remember to unmount before rebooting:"
        echo "  sync && umount -R /mnt && reboot"
    fi
else
    echo "=== Installation INCOMPLETE ==="
    echo ""
    echo "Critical files are missing. DO NOT REBOOT."
    echo ""
    echo "Please review the errors above and:"
    echo "  1. Check /tmp/guix-install.log for errors"
    echo "  2. Try re-running the system init manually"
    echo "  3. Or re-run this recovery script"
    exit 1
fi
