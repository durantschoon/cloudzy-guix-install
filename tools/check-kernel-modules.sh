#!/usr/bin/env bash
# Script to check which modules are available vs built-in in a kernel package
# Usage: ./tools/check-kernel-modules.sh [kernel-package-path]
#
# If kernel-package-path is not provided, searches for linux-6.6.16 in /gnu/store

set -euo pipefail

KERNEL_PKG="${1:-}"

if [ -z "$KERNEL_PKG" ]; then
    echo "Searching for kernel 6.6.16 package in /gnu/store..."
    KERNEL_PKG=$(find /gnu/store -maxdepth 1 -type d -name "*-linux-6.6.16" 2>/dev/null | head -1)
    if [ -z "$KERNEL_PKG" ]; then
        echo "Error: Kernel 6.6.16 package not found in /gnu/store"
        echo "Build it first with: guix time-machine -C ~/channels.scm -- build linux"
        exit 1
    fi
    echo "Found: $KERNEL_PKG"
    echo ""
fi

if [ ! -d "$KERNEL_PKG" ]; then
    echo "Error: Kernel package path does not exist: $KERNEL_PKG"
    exit 1
fi

MODULES_DIR="$KERNEL_PKG/lib/modules"
if [ ! -d "$MODULES_DIR" ]; then
    echo "Warning: No modules directory found at $MODULES_DIR"
    echo "This kernel may have all modules built-in"
    exit 0
fi

# Find kernel version directory
KERNEL_VERSION_DIR=$(find "$MODULES_DIR" -maxdepth 1 -type d | grep -v "^$MODULES_DIR$" | head -1)
if [ -z "$KERNEL_VERSION_DIR" ]; then
    echo "Warning: No kernel version directory found in $MODULES_DIR"
    exit 0
fi

echo "Kernel version directory: $KERNEL_VERSION_DIR"
echo ""

# Modules we're interested in checking
MODULES_TO_CHECK=(
    "nvme"
    "xhci_pci"
    "usbhid"
    "i2c_piix4"
    "amdgpu"
    "ahci"
)

echo "Checking module availability:"
echo "=============================="
echo ""

BUILT_IN_MODULES=()
AVAILABLE_MODULES=()

for module in "${MODULES_TO_CHECK[@]}"; do
    # Try different naming conventions
    # Module files can be: module.ko, module.ko.gz, or in subdirectories
    # Module names might use underscores or dashes
    
    found=false
    
    # Try direct name match
    if find "$KERNEL_VERSION_DIR" -name "${module}.ko" -o -name "${module}.ko.gz" 2>/dev/null | grep -q .; then
        found=true
    fi
    
    # Try with dash instead of underscore
    module_dash=$(echo "$module" | tr '_' '-')
    if find "$KERNEL_VERSION_DIR" -name "${module_dash}.ko" -o -name "${module_dash}.ko.gz" 2>/dev/null | grep -q .; then
        found=true
    fi
    
    if [ "$found" = true ]; then
        echo "  ✓ $module: AVAILABLE (loadable module)"
        AVAILABLE_MODULES+=("$module")
    else
        echo "  ✗ $module: BUILT-IN (not a loadable module)"
        BUILT_IN_MODULES+=("$module")
    fi
done

echo ""
echo "Summary:"
echo "========"
echo "Available modules (can be in initrd-modules): ${AVAILABLE_MODULES[*]}"
echo "Built-in modules (must be filtered from initrd-modules): ${BUILT_IN_MODULES[*]}"
echo ""

if [ ${#BUILT_IN_MODULES[@]} -gt 0 ]; then
    echo "To update lib/check-kernel-modules.go, add these to GetBuiltInModulesForKernel66():"
    for mod in "${BUILT_IN_MODULES[@]}"; do
        echo "  \"$mod\","
    done
fi
