# Guix Installation Verification

The `verify-guix-install.sh` script helps diagnose and prevent Guix installation issues.

## Purpose

This script verifies that all critical components of a Guix installation are present and properly configured. It can be run from either:
- **Guix ISO** (checks `/mnt` before first boot)
- **Installed system** (checks `/` after booting)

## What It Checks

### Critical Boot Files
- ✅ Kernel (`/boot/vmlinuz-*`) - with file size
- ✅ Initrd (`/boot/initrd-*`) - with file size
- ✅ GRUB config (`/boot/grub/grub.cfg`)
- ✅ GRUB EFI binary (`/boot/efi/EFI/Guix/grubx64.efi`)

### System Configuration
- ✅ Config file exists (`/etc/config.scm`)
- ✅ Parses hostname and timezone
- ⚠️ Warns about problematic kernel parameters

### User Accounts
- ✅ Password file exists
- ✅ Lists regular user accounts
- ✅ Checks home directories exist
- ⚠️ Warns if no regular users found

### Storage
- ✅ GNU store populated (should have >100 items)
- ✅ Filesystem labels (GUIX_ROOT, EFI)
- ✅ Available disk space

## Usage

### Before First Boot (from Guix ISO)

```bash
# After running installation, before rebooting
./verify-guix-install.sh

# Should show:
# ========================================
#   INSTALLATION VERIFIED SUCCESSFULLY!
# ========================================
```

**If verification fails:**
- DO NOT REBOOT
- Review error messages
- Re-run `guix system init /mnt/etc/config.scm /mnt`
- Run verification again

### After Boot (from installed system)

```bash
# If you encounter boot or system issues
sudo verify-guix-install

# Or with full path:
sudo /usr/local/bin/verify-guix-install
```

## Exit Codes

- `0` - All checks passed (may have warnings)
- `1` - Critical errors found (do not reboot)

## Output Examples

### Successful Verification

```
==========================================
  Guix Installation Verification
==========================================

Context: Guix ISO (checking /mnt)
Checking: /mnt

=== Critical Boot Files ===

[OK] Kernel: vmlinuz-6.6.8 (9.8 MB)
[OK] Initrd: initrd-6.6.8 (52.3 MB)
[OK] GRUB config: /boot/grub/grub.cfg

=== EFI Boot Files ===

[OK] EFI partition mounted at /boot/efi
[OK] GRUB EFI binary (Guix): /boot/efi/EFI/Guix/grubx64.efi (2.1 MB)

=== System Configuration ===

[OK] System configuration: /etc/config.scm
[INFO]   Hostname: framework-guix
[INFO]   Timezone: America/New_York
[INFO]   Kernel args: (kernel-arguments '("quiet"))

=== User Accounts ===

[OK] Password file exists
[OK]   User: durant
[INFO]     Home: /home/durant exists

=== GNU Store ===

[OK] GNU store populated: 2453 items

=== Filesystem Mounts ===

[OK] GUIX_ROOT label found: /dev/nvme0n1p4
[OK] EFI label found: /dev/nvme0n1p1

=== Disk Space ===

[OK] Available space on root: 42G

==========================================
  Verification Summary
==========================================

✓ ALL CHECKS PASSED

Installation appears complete and healthy.
System should be ready to boot.
```

### Failed Verification

```
==========================================
  Guix Installation Verification
==========================================

Context: Guix ISO (checking /mnt)
Checking: /mnt

=== Critical Boot Files ===

[ERROR] Kernel NOT FOUND: /boot/vmlinuz-* (CRITICAL)
[ERROR] Initrd NOT FOUND: /boot/initrd-* (CRITICAL)
[OK] GRUB config: /boot/grub/grub.cfg

...

==========================================
  Verification Summary
==========================================

✗ VERIFICATION FAILED

Errors: 2
Warnings: 0

CRITICAL ISSUES DETECTED!
DO NOT REBOOT until errors are resolved.

Common fixes:
  - Re-run: guix system init /mnt/etc/config.scm /mnt
  - Set user password: guix system chroot /mnt && passwd USERNAME
  - Check disk space: df -h /mnt
  - Review installation logs
```

## When to Use

### During Installation
1. Run **after** `guix system init` completes
2. **Before** rebooting from ISO
3. Verification must pass before proceeding

### After Installation
1. System won't boot - run from live ISO
2. System boots but has issues - run from installed system
3. After making system configuration changes
4. Before system upgrades to verify baseline

### During Troubleshooting
1. To confirm installation state
2. To verify fixes worked
3. To gather diagnostic information
4. To check if files got deleted/corrupted

## Integration with Installation Process

The verification script is automatically:
- **Installed** to `/usr/local/bin/verify-guix-install` during `guix system init`
- **Called** by the installer after system init completes
- **Available** in installed system for future use

You can also run it manually at any time from the repository:

```bash
./verify-guix-install.sh
```

## Troubleshooting

### Script not found in installed system

```bash
# Download from repository
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/verify-guix-install.sh -o /tmp/verify.sh
chmod +x /tmp/verify.sh
sudo /tmp/verify.sh
```

### Script shows wrong context

The script auto-detects whether it's running from ISO or installed system:
- If `/mnt` is mounted and `/mnt/boot` exists → checks `/mnt` (ISO context)
- Otherwise → checks `/` (installed system context)

### Permission denied

```bash
# Make sure it's executable
chmod +x verify-guix-install.sh

# Or run with bash explicitly
bash verify-guix-install.sh
```

## See Also

- [DEBUGGING.md](DEBUGGING.md) - Complete debugging session notes
- [CHECKLIST.md](CHECKLIST.md) - Installation implementation checklist
- [README.md](README.md) - Main installation documentation
