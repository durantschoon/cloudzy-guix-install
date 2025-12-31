# Framework 13 Startup Hang Fix

## Problem

Framework 13 AMD systems installed with framework-dual hang on startup, typically at "Loading kernel modules..." with repeating "time with localhost and MARK" messages every 20 minutes.

## Root Cause

The installer was missing critical kernel parameters required for Framework 13 AMD GPU initialization. The documentation stated these parameters were included, but the code only had `("quiet")` instead of the full set.

## Solution

### For New Installations

**Fixed in:** `framework-dual/install/03-config-dual-boot.go`

The installer now automatically includes the required kernel parameters:

```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "noapic" "nolapic"))
```

**Note:** `acpi=off` was removed because it causes USB controller initialization failures. If you still experience boot hangs, you can try adding it back, but USB devices may not work.

### For Already-Installed Systems

If your system is already installed and hanging on startup, you have two options:

#### Option 1: Temporary Fix via GRUB (Quick Recovery)

1. **At GRUB menu, press 'e' to edit** the boot entry
2. **Find the kernel line** (starts with `linux /boot/vmlinuz-...`)
3. **Add the parameters** to the end of the line:
   ```
   nomodeset acpi=off noapic nolapic
   ```
   The line should look like:
   ```
   linux /boot/vmlinuz-... quiet splash nomodeset acpi=off noapic nolapic
   ```
4. **Press Ctrl+X or F10 to boot** with these parameters
5. **After booting successfully**, proceed to Option 2 to make the fix permanent

#### Option 2: Permanent Fix via Config (Recommended)

After booting with the temporary fix:

1. **Edit `/etc/config.scm`**:
   ```bash
   sudo nano /etc/config.scm
   ```

2. **Find the `kernel-arguments` line** and update it:
   ```scheme
   ;; Change from:
   (kernel-arguments '("quiet"))
   
   ;; To:
   (kernel-arguments '("quiet" "loglevel=3" "nomodeset" "noapic" "nolapic"))
   
   ;; Note: acpi=off removed - it causes USB controller failures
   ;; If you still have boot hangs, you can try adding acpi=off back,
   ;; but USB devices (keyboard/mouse) may not work
   ```

3. **Reconfigure the system**:
   ```bash
   sudo guix system reconfigure /etc/config.scm
   ```

4. **Reboot**:
   ```bash
   sudo reboot
   ```

The system should now boot without hanging.

## What These Parameters Do

- **`nomodeset`**: Disables kernel mode setting (fixes AMD GPU display issues)
- **`acpi=off`**: Disables ACPI (prevents power management conflicts)
- **`noapic`**: Disables APIC (prevents interrupt controller issues)
- **`nolapic`**: Disables Local APIC (prevents local interrupt issues)
- **`loglevel=3`**: Reduces console verbosity (optional, for cleaner boot)

## Boot Hang Symptoms

- System hangs at "Loading kernel modules..."
- Repeating "time with localhost and MARK" messages every 20 minutes
- Never reaches login prompt
- Ctrl+C doesn't work

## Related Issues

### If System Hangs at Logo After Adding Parameters

If you added the parameters and the system hangs at the Guix logo (not reaching login), `acpi=off` may be too aggressive. Try these alternatives:

#### Step 1: Remove `quiet` to See What's Happening

1. **At GRUB menu, press 'e' to edit**
2. **Find the kernel line** and remove `quiet` to see boot messages:
   ```
   linux /boot/vmlinuz-... splash nomodeset acpi=off noapic nolapic 3
   ```
3. **Press Ctrl+X to boot** and watch for error messages

#### Step 2: Try Less Aggressive Parameters

If it still hangs, try removing `acpi=off` (ACPI is often needed for modern hardware):

1. **At GRUB menu, press 'e' to edit**
2. **Use this combination instead**:
   ```
   linux /boot/vmlinuz-... quiet nomodeset noapic nolapic
   ```
   (Removed `acpi=off` and `loglevel=3`)
3. **Press Ctrl+X to boot**

#### Step 3: Try Minimal Parameters

If still hanging, try just the essential ones:

1. **At GRUB menu, press 'e' to edit**
2. **Use minimal parameters**:
   ```
   linux /boot/vmlinuz-... quiet nomodeset
   ```
   (Only `nomodeset` - this is often sufficient for AMD GPU issues)
3. **Press Ctrl+X to boot**

#### Step 4: Check Console Output

If you can access a text console (try `Alt+F2` or `Alt+F3`), check:
- `dmesg | tail -50` - Look for errors
- `journalctl -b` - Check system logs
- Look for ACPI errors or hardware initialization failures

### If You Still Have Problems After Trying Alternatives

1. **Check that initrd modules are correct**:
   ```scheme
   (initrd-modules
    (append '("amdgpu"      ; AMD GPU driver
              "usbhid"      ; USB keyboard/mouse
              "i2c_piix4")  ; SMBus/I2C for sensors
            %base-initrd-modules))
   ```

2. **Verify you're using the correct kernel and firmware**:
   ```scheme
   (kernel linux)  ; From nonguix channel
   (firmware (list linux-firmware))  ; From nonguix channel
   ```

3. **Consider using wingolog-era channel pinning** if you have GDM login issues:
   See [GNOME_LOGIN_TROUBLESHOOTING.md](./GNOME_LOGIN_TROUBLESHOOTING.md) for details.

4. **Try booting from Pop!_OS live ISO** and chrooting to fix the config:
   ```bash
   # Boot Pop!_OS live ISO
   sudo mount /dev/nvme0n1pX /mnt  # Replace X with your Guix partition
   sudo mount /dev/nvme0n1p1 /mnt/boot/efi
   sudo chroot /mnt
   # Edit /etc/config.scm with less aggressive parameters
   ```

## USB Controller Initialization Errors

### xhci_hcd Probe Failure

If you see this error during boot:
```
xhci_hcd probe of 0000:c3:00.4 failed with error -22
```

**This is likely caused by `acpi=off`** disabling ACPI, which USB controllers need for proper initialization.

#### Root Cause

- Error code `-22` = `EINVAL` (Invalid argument)
- USB 3.0 controllers (xhci_hcd) require ACPI for proper initialization
- `acpi=off` completely disables ACPI, breaking USB controller setup
- This can cause USB devices (keyboard, mouse, USB drives) to not work

#### Solution: Remove `acpi=off`

The `acpi=off` parameter is too aggressive for Framework 13. Try these alternatives:

**Option 1: Remove `acpi=off` entirely** (Recommended)
```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "noapic" "nolapic"))
```

**Option 2: Use less aggressive ACPI options**
```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "acpi=noirq" "noapic" "nolapic"))
```

**Option 3: Minimal parameters** (if Option 1 works)
```scheme
(kernel-arguments '("quiet" "nomodeset"))
```

#### How to Fix

1. **If you can boot** (even with USB not working):
   - Edit `/etc/config.scm`
   - Remove `acpi=off` from kernel-arguments
   - Run: `sudo guix system reconfigure /etc/config.scm`
   - Reboot

2. **If you can't boot** (USB keyboard/mouse not working):
   - Boot from Pop!_OS live ISO
   - Mount your Guix partition: `sudo mount /dev/nvme0n1pX /mnt`
   - Edit `/mnt/etc/config.scm` and remove `acpi=off`
   - Chroot and reconfigure: `sudo chroot /mnt guix system reconfigure /etc/config.scm`
   - Reboot

#### Testing USB After Fix

After removing `acpi=off` and rebooting:
```bash
# Check USB controller status
dmesg | grep -i xhci
lsusb  # Should show USB devices
```

## HID BPF Error (Non-Critical)

If you see this message during boot:
```
hid_bpf: error while preloading HID BPF dispatcher: -22
```

**This is typically harmless** and does not prevent the system from booting. It's a warning about HID (Human Interface Device) BPF initialization, which is a newer Linux kernel feature for advanced input device handling.

### What It Means

- Error code `-22` = `EINVAL` (Invalid argument)
- The kernel is trying to initialize HID BPF support but encountering a compatibility issue
- This is often related to kernel version or configuration
- **Your keyboard and mouse should still work normally**

### When to Worry

Only if you experience:
- Keyboard/mouse not working
- System hangs after this message
- Other input device failures

### How to Suppress (Optional)

If the message is annoying, you can suppress it by adding to kernel arguments:
```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "noapic" "nolapic"))
```

The `quiet` and `loglevel=3` parameters will reduce console verbosity and hide non-critical messages.

### If Input Devices Don't Work

If you actually have keyboard/mouse issues:

1. **Check that `usbhid` is in initrd-modules**:
   ```scheme
   (initrd-modules
    (append '("amdgpu"
              "usbhid"      ; USB keyboard/mouse - REQUIRED
              "i2c_piix4")
            %base-initrd-modules))
   ```

2. **Try accessing a text console**: Press `Alt+F2` or `Alt+F3` to switch to a TTY

3. **Check dmesg for USB errors**:
   ```bash
   dmesg | grep -i usb
   dmesg | grep -i hid
   ```

## ext4 "Unknown parameter 'defaults'" Error

If you see repeated errors during boot:
```
ext4: Unknown parameter 'defaults'
```

**This is caused by** using `defaults` as a mount option for ext4 filesystems. The `defaults` option is valid in `/etc/fstab` but ext4 doesn't recognize it as a direct parameter.

### Solution

**Fixed in:** `framework-dual/install/03-config-dual-boot.go`

The installer now uses only valid ext4 options:
```scheme
(file-system
  (mount-point "/data")
  (device (file-system-label "DATA"))
  (type "ext4")
  (options "noatime"))  ; Removed "defaults" - not a valid ext4 option
```

### For Already-Installed Systems

If you have this error, edit `/etc/config.scm`:

1. Find the DATA partition filesystem entry (if you have one)
2. Change from:
   ```scheme
   (options "defaults,noatime")
   ```
   To:
   ```scheme
   (options "noatime")
   ```
3. Reconfigure: `sudo guix system reconfigure /etc/config.scm`
4. Reboot

**Note:** This error is harmless but annoying - it just means the `defaults` option is being ignored. The filesystem will still mount correctly.

## Prevention

This fix has been applied to the installer code. Future installations will automatically include these parameters.

## References

- [INSTALLATION_KNOWLEDGE.md](./INSTALLATION_KNOWLEDGE.md) - Framework 13 AMD GPU Boot Issues section
- [GUIDE_DUAL_BOOT.md](./GUIDE_DUAL_BOOT.md) - AMD GPU Boot Issues section
- [GNOME_LOGIN_TROUBLESHOOTING.md](./GNOME_LOGIN_TROUBLESHOOTING.md) - For GDM login issues after successful boot
