#!/run/current-system/profile/bin/bash
# Guix Installation Verification Script
# Can be run from Guix ISO (checks /mnt) or from installed system (checks /)
#
# Note: Uses #!/run/current-system/profile/bin/bash which works on both:
#   - Guix ISO (bash at /run/current-system/profile/bin/bash)
#   - Installed Guix (same path: /run/current-system/profile/bin/bash)
#
# Usage:
#   From ISO:       ./verify-guix-install.sh
#   From installed: sudo /usr/local/bin/verify-guix-install.sh
#   From installed: sudo verify-guix-install  (if in PATH)

set -u

# Determine if we're checking /mnt (from ISO) or / (from installed system)
if mountpoint -q /mnt 2>/dev/null && [ -d /mnt/boot ]; then
    ROOT="/mnt"
    CONTEXT="Guix ISO (checking /mnt)"
else
    ROOT=""
    CONTEXT="Installed System (checking /)"
fi

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo
echo "=========================================="
echo "  Guix Installation Verification"
echo "=========================================="
echo
echo "Context: $CONTEXT"
echo "Checking: ${ROOT:-/}"
echo

ERRORS=0
WARNINGS=0

# Helper functions
error() {
    echo -e "${RED}[ERROR]${NC} $*"
    ((ERRORS++))
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
    ((WARNINGS++))
}

ok() {
    echo -e "${GREEN}[OK]${NC} $*"
}

info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

check_file() {
    local file="$1"
    local description="$2"
    local critical="${3:-yes}"

    if [ -f "${ROOT}${file}" ]; then
        local size=$(stat -f%z "${ROOT}${file}" 2>/dev/null || stat -c%s "${ROOT}${file}" 2>/dev/null)
        local size_mb=$(echo "scale=1; $size / 1024 / 1024" | bc 2>/dev/null || echo "?")
        ok "$description: ${file} (${size_mb} MB)"
        return 0
    else
        if [ "$critical" = "yes" ]; then
            error "$description: ${file} NOT FOUND (CRITICAL)"
        else
            warn "$description: ${file} NOT FOUND"
        fi
        return 1
    fi
}

check_pattern() {
    local pattern="$1"
    local description="$2"
    local critical="${3:-yes}"

    local files=(${ROOT}${pattern})
    if [ -e "${files[0]}" ]; then
        local file="${files[0]}"
        local basename=$(basename "$file")
        local size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null)
        local size_mb=$(echo "scale=1; $size / 1024 / 1024" | bc 2>/dev/null || echo "?")
        ok "$description: $basename (${size_mb} MB)"

        # Show all matches if multiple
        if [ ${#files[@]} -gt 1 ]; then
            info "  Found ${#files[@]} files matching pattern"
        fi
        return 0
    else
        if [ "$critical" = "yes" ]; then
            error "$description NOT FOUND: ${pattern} (CRITICAL)"
        else
            warn "$description NOT FOUND: ${pattern}"
        fi
        return 1
    fi
}

check_dir() {
    local dir="$1"
    local description="$2"

    if [ -d "${ROOT}${dir}" ]; then
        ok "$description: ${dir}"
        return 0
    else
        error "$description: ${dir} NOT FOUND"
        return 1
    fi
}

echo "=== Critical Boot Files ==="
echo

# Kernel
check_pattern "/boot/vmlinuz-*" "Kernel" yes

# Initrd
check_pattern "/boot/initrd-*" "Initrd" yes

# GRUB config
check_file "/boot/grub/grub.cfg" "GRUB config" yes

echo
echo "=== EFI Boot Files ==="
echo

# EFI partition should be mounted
if [ -d "${ROOT}/boot/efi/EFI" ]; then
    ok "EFI partition mounted at /boot/efi"

    # Check GRUB EFI binary (could be in different locations)
    if [ -f "${ROOT}/boot/efi/EFI/Guix/grubx64.efi" ]; then
        check_file "/boot/efi/EFI/Guix/grubx64.efi" "GRUB EFI binary (Guix)" yes
    elif [ -f "${ROOT}/boot/efi/EFI/BOOT/BOOTX64.EFI" ]; then
        check_file "/boot/efi/EFI/BOOT/BOOTX64.EFI" "GRUB EFI binary (fallback)" yes
    else
        error "GRUB EFI binary NOT FOUND in /boot/efi/EFI/"
    fi

    # GRUB EFI config
    check_file "/boot/efi/EFI/Guix/grub.cfg" "GRUB EFI config" no
else
    error "EFI partition not mounted at /boot/efi"
fi

echo
echo "=== System Configuration ==="
echo

# Config file
if check_file "/etc/config.scm" "System configuration" yes; then
    # Parse some basic info from config
    if grep -q "host-name" "${ROOT}/etc/config.scm" 2>/dev/null; then
        local hostname=$(grep "host-name" "${ROOT}/etc/config.scm" | sed 's/.*"\(.*\)".*/\1/' | head -1)
        info "  Hostname: $hostname"
    fi

    if grep -q "timezone" "${ROOT}/etc/config.scm" 2>/dev/null; then
        local timezone=$(grep "timezone" "${ROOT}/etc/config.scm" | sed 's/.*"\(.*\)".*/\1/' | head -1)
        info "  Timezone: $timezone"
    fi

    # Check for kernel-arguments
    if grep -q "kernel-arguments" "${ROOT}/etc/config.scm" 2>/dev/null; then
        local kargs=$(grep "kernel-arguments" "${ROOT}/etc/config.scm" | head -1)
        info "  Kernel args: $kargs"

        # Warn about problematic parameters
        if echo "$kargs" | grep -q "acpi=off\|noapic\|nolapic"; then
            warn "  Aggressive kernel parameters detected (may cause boot issues)"
        fi
    fi
fi

echo
echo "=== User Accounts ==="
echo

if [ -f "${ROOT}/etc/passwd" ]; then
    ok "Password file exists"

    # List non-system users
    local users=$(grep -E ":/home/.*:(bash|sh)" "${ROOT}/etc/passwd" 2>/dev/null | cut -d: -f1 || echo "")
    if [ -n "$users" ]; then
        for user in $users; do
            ok "  User: $user"
            # Check if home directory exists
            if [ -d "${ROOT}/home/$user" ]; then
                info "    Home: /home/$user exists"
            else
                warn "    Home: /home/$user NOT FOUND"
            fi
        done
    else
        warn "No regular user accounts found (only system accounts)"
        info "  You may need to set a password: guix system chroot /mnt && passwd USERNAME"
    fi
else
    error "Password file /etc/passwd NOT FOUND"
fi

echo
echo "=== GNU Store ==="
echo

if [ -d "${ROOT}/gnu/store" ]; then
    local store_count=$(ls -1 "${ROOT}/gnu/store" 2>/dev/null | wc -l | tr -d ' ')
    if [ "$store_count" -gt 100 ]; then
        ok "GNU store populated: $store_count items"
    elif [ "$store_count" -gt 0 ]; then
        warn "GNU store has only $store_count items (seems low)"
    else
        error "GNU store is empty"
    fi
else
    error "GNU store directory NOT FOUND"
fi

echo
echo "=== Filesystem Mounts ==="
echo

# Check if critical filesystems are properly labeled
if command -v blkid >/dev/null 2>&1; then
    if blkid -t LABEL=GUIX_ROOT >/dev/null 2>&1; then
        local guix_dev=$(blkid -t LABEL=GUIX_ROOT -o device)
        ok "GUIX_ROOT label found: $guix_dev"
    else
        warn "GUIX_ROOT label not found (may need to be set)"
    fi

    if blkid -t LABEL=EFI >/dev/null 2>&1; then
        local efi_dev=$(blkid -t LABEL=EFI -o device)
        ok "EFI label found: $efi_dev"
    else
        warn "EFI label not found (may need to be set)"
    fi
fi

echo
echo "=== Disk Space ==="
echo

if command -v df >/dev/null 2>&1; then
    local root_avail=$(df -BG "${ROOT}/" 2>/dev/null | tail -1 | awk '{print $4}' | sed 's/G//')
    if [ -n "$root_avail" ] && [ "$root_avail" -gt 5 ]; then
        ok "Available space on root: ${root_avail}G"
    elif [ -n "$root_avail" ]; then
        warn "Low disk space on root: ${root_avail}G"
    fi
fi

echo
echo "=========================================="
echo "  Verification Summary"
echo "=========================================="
echo

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ ALL CHECKS PASSED${NC}"
    echo
    echo "Installation appears complete and healthy."
    echo "System should be ready to boot."
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ PASSED WITH WARNINGS${NC}"
    echo
    echo "Errors: $ERRORS"
    echo "Warnings: $WARNINGS"
    echo
    echo "Installation is likely OK, but review warnings above."
    exit 0
else
    echo -e "${RED}✗ VERIFICATION FAILED${NC}"
    echo
    echo "Errors: $ERRORS"
    echo "Warnings: $WARNINGS"
    echo
    echo "CRITICAL ISSUES DETECTED!"
    echo "DO NOT REBOOT until errors are resolved."
    echo
    echo "Common fixes:"
    echo "  - Re-run: guix system init /mnt/etc/config.scm /mnt"
    echo "  - Set user password: guix system chroot /mnt && passwd USERNAME"
    echo "  - Check disk space: df -h /mnt"
    echo "  - Review installation logs"
    echo
    exit 1
fi
