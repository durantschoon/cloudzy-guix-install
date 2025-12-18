# NVMe Module Removal: Why We Filter `nvme` from initrd-modules

## Problem Summary

Framework-dual installations fail during `guix time-machine system build` with the error:

```
gnu/build/linux-modules.scm:270:5: kernel module not found "nvme" "/gnu/store/...-linux-6.6.16/lib/modules"
```

This occurs even when `nvme` is explicitly listed in `initrd-modules` or included via `%base-initrd-modules`.

## Root Cause

### 1. **NVMe is Built-In to Kernel 6.6.16**

In Linux kernel 6.6.16 (from wingolog-era pinned channels, Feb 2024), NVMe support is **built into the kernel**, not available as a loadable module. When Guix's `linux-modules` builder tries to find `nvme` as a loadable module, it fails because the module doesn't exist - it's already part of the kernel binary.

### 2. **ISO Guix Version vs. Pinned Channel Version Mismatch**

There's a subtle interaction between:
- **ISO's Guix version**: May be newer than wingolog-era (Feb 2024)
- **Pinned channels**: Wingolog-era commits (Feb 2024) for both guix and nonguix
- **Kernel version**: Linux 6.6.16 from wingolog-era nonguix channel

**The Issue:**
- The ISO's newer Guix may have different expectations about kernel modules
- When `guix time-machine` builds with wingolog-era channels, it uses the pinned kernel (6.6.16) where `nvme` is built-in
- But the ISO's Guix (or the module builder) expects `nvme` to be a loadable module
- This mismatch causes the "kernel module not found" error

**Related Fix (commit `36e1674`):**
We removed `guix pull` to avoid glibc version mismatches. Similarly, we need to ensure module expectations match the actual kernel configuration.

## Solution

### Filter Built-In Modules from `%base-initrd-modules`

We filter out built-in modules from the base initrd modules list using Guile's `remove` function:

```scheme
(initrd-modules
 (append '("amdgpu"      ; AMD GPU driver (critical for display) - loadable module
           "usbhid"      ; USB keyboard/mouse - loadable module
           "i2c_piix4")  ; SMBus/I2C for sensors - loadable module
         (remove (lambda (module) 
                   (or (string=? module "nvme")
                       (string=? module "xhci_pci"))) %base-initrd-modules)))
```

**Known Built-In Modules in Kernel 6.6.16:**
- `nvme` - NVMe SSD support (built-in for performance)
- `xhci_pci` - USB 3.0 host controller (built-in for USB support)

**How to Determine Built-In Modules:**

1. **Build the kernel package:**
   ```bash
   guix time-machine -C ~/channels.scm -- build linux
   ```

2. **Check module directory:**
   ```bash
   ls -R /gnu/store/*-linux-6.6.16/lib/modules/*/kernel/
   ```

3. **If a module doesn't exist as `.ko` or `.ko.gz` file**, it's built-in

4. **Add to filter list** in `framework-dual/install/03-config-dual-boot.go` and `framework/install/03-config.go`

This ensures that:
1. `nvme` is never included in the initrd-modules list, even if it appears in `%base-initrd-modules`
2. The system builds successfully with kernel 6.6.16 where NVMe is built-in
3. NVMe devices still work because the driver is part of the kernel

## Why This Matters

### 1. **Compatibility Across ISO Versions**

Different Guix ISO versions may have different expectations about kernel modules. By explicitly filtering `nvme`, we ensure compatibility regardless of:
- ISO Guix version (newer or older)
- Whether `nvme` appears in `%base-initrd-modules`
- Kernel configuration (built-in vs. loadable modules)

### 2. **Prevents Build Failures**

Without this filter, installations fail at the `guix time-machine system build` step, preventing the system from being installed.

### 3. **Future-Proof**

If `nvme` is added to `%base-initrd-modules` in future Guix versions, our filter ensures it's still excluded for framework-dual installations using wingolog-era kernels.

## Implementation

The fix is implemented in:
- `framework-dual/install/03-config-dual-boot.go` - Dual-boot configuration generator
- `framework/install/03-config.go` - Single-boot configuration generator
- `lib/check-kernel-modules.go` - Helper function `GetBuiltInModulesForKernel66()` that maintains the list

Both config generators use the same filter pattern to ensure consistency, and the filter list is maintained centrally in `lib/check-kernel-modules.go`.

## Proactively Checking Kernel Modules

To avoid discovering built-in modules during installation failures, you can proactively check which modules are available in a kernel package:

### Method 1: Using the Check Script

```bash
# Build the kernel package first
guix time-machine -C ~/channels.scm -- build linux

# Run the check script (it will auto-detect kernel 6.6.16)
./tools/check-kernel-modules.sh

# Or specify a kernel package path
./tools/check-kernel-modules.sh /gnu/store/xxxxx-linux-6.6.16
```

The script will:
- List which modules are available as loadable modules
- List which modules are built-in (not available as `.ko` files)
- Provide code snippets to update `GetBuiltInModulesForKernel66()`

### Method 2: Manual Inspection

```bash
# Build the kernel package
guix time-machine -C ~/channels.scm -- build linux

# Find the kernel package
KERNEL_PKG=$(find /gnu/store -maxdepth 1 -type d -name "*-linux-6.6.16" | head -1)

# Check for a specific module
find "$KERNEL_PKG/lib/modules" -name "nvme.ko*" -o -name "xhci-pci.ko*"

# If no results, the module is built-in
# If results found, the module is available as a loadable module
```

### Method 3: Check Kernel Config

If the kernel package includes a `.config` file:

```bash
KERNEL_PKG=$(find /gnu/store -maxdepth 1 -type d -name "*-linux-6.6.16" | head -1)
grep -E "^CONFIG_NVME=|^CONFIG_XHCI_PCI=" "$KERNEL_PKG/.config" || true

# If the value is "=y", it's built-in
# If the value is "=m", it's a loadable module
```

### Updating the Filter List

When you discover a new built-in module:

1. Add it to `lib/check-kernel-modules.go` in `GetBuiltInModulesForKernel66()`:
   ```go
   func GetBuiltInModulesForKernel66() []string {
       return []string{
           "nvme",
           "xhci_pci",
           "new_module",  // Add here
       }
   }
   ```

2. The config generators (`03-config-dual-boot.go` and `03-config.go`) will automatically use the updated list

3. Update this documentation with the new module and why it's built-in

4. Test the installation to ensure the filter works correctly

## For Existing Installations

If you have an existing `/mnt/etc/config.scm` file that was generated before this fix:

1. **Delete the old config** and regenerate:
   ```bash
   rm /mnt/etc/config.scm /root/channels.scm
   # Then rerun Step 3 of the installer
   ```

2. **Or manually edit** `/mnt/etc/config.scm`:
   - Find the `initrd-modules` line
   - Remove `"nvme"` from the list if present
   - Or replace `%base-initrd-modules` with:
     ```scheme
     (remove (lambda (module) (string=? module "nvme")) %base-initrd-modules)
     ```

## Related Documentation

- [Channel Pinning Policy](./CHANNEL_PINNING_POLICY.md) - Why we use wingolog-era pinned channels
- [Wingolog Channel Analysis](./WINGOLOG_CHANNEL_ANALYSIS.md) - Analysis of channel compatibility issues
- [Installation Knowledge](./INSTALLATION_KNOWLEDGE.md) - Framework 13 specific initrd modules section

## Technical Details

### Kernel Module vs. Built-In Driver

- **Loadable module**: Can be loaded/unloaded at runtime (`modprobe nvme`)
- **Built-in driver**: Compiled directly into the kernel binary, always available

In kernel 6.6.16, NVMe is built-in, so:
- No `/lib/modules/.../nvme.ko` file exists
- The driver is always available (no need to load it)
- Including it in `initrd-modules` causes Guix to look for a non-existent module file

### Why Filter Instead of Just Removing Explicitly?

We filter from `%base-initrd-modules` because:
1. **Defensive programming**: Ensures `nvme` is excluded even if it appears in base modules
2. **Future-proof**: Works even if Guix adds `nvme` to base modules in future versions
3. **Explicit intent**: Makes it clear that we're intentionally excluding `nvme` for compatibility reasons

## Verification

After applying this fix, `guix time-machine system build` should succeed without the "kernel module not found" error. The system will boot correctly, and NVMe devices will work because the driver is built into the kernel.
