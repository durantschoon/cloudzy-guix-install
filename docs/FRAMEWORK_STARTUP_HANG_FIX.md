# Framework 13 Startup Hang Fix

> **This document has been reversed.** It used to instruct you to add
> `nomodeset`, `acpi=off`, `noapic` and `nolapic`. **Do not add them.** They were
> a misdiagnosis, and each one broke something. The historical prescription is
> kept below the fold because the reasoning is instructive, not because it should
> be followed.

## Problem

Framework 13 AMD systems installed with framework-dual appear to hang on startup,
typically at "Loading kernel modules...", with repeating "time with localhost and
MARK" messages every 20 minutes. Later variants of the same underlying fault:
GDM renders and then ignores the keyboard, or GDM accepts the password and
immediately returns to the login screen.

## Actual root cause

**The GPU firmware was older than the GPU.**

The machine is a Framework Laptop 13 (AMD Ryzen AI 300), GPU `1002:1114` (Strix
Point / Radeon 890M, gfx11.5). Framework-dual was pinned to wingolog-era channels
from Feb 2024, which predate that silicon by ~5 months. amdgpu therefore failed:

```
Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2
amdgpu: Fatal error during GPU init
```

A GPU that never finishes initializing looks a lot like a hang. It is not one.

**Fix:** move the channel pin forward, so kernel >= 6.10 and mid-2024-or-later
`linux-firmware` are available. See `docs/CHANNEL_PINNING_POLICY.md`.

## Current kernel arguments

`framework-dual/install/03-config-dual-boot.go` now generates:

```scheme
(kernel-arguments (append '("loglevel=3") %default-kernel-arguments))
```

Two properties matter:

- It **appends** to `%default-kernel-arguments` rather than replacing it. The
  default carries `modprobe.blacklist=usbmouse,usbkbd`; upstream blacklists
  `usbkbd` because it races `usbhid` (bugs.gnu.org/35574). Replacing the list
  silently drops that, which is one more way to lose a keyboard.
- It contains none of the four workaround arguments.

A regression test in `framework-dual/install/03-config-dual-boot_test.go`
fails the build if any of them reappear.

## Why each removed argument was harmful

| Argument | Claimed purpose | What it actually did |
| --- | --- | --- |
| `nomodeset` | "fixes AMD GPU display issues" | Disables kernel modesetting entirely, which contradicts loading `amdgpu` and `linux-firmware` at all. Leaves an unaccelerated console and cannot fix missing firmware. |
| `acpi=off` | "prevents power management conflicts" | Broke `xhci_hcd` init (`probe ... failed with error -22`), killing USB. Removed earlier for this reason. |
| `noapic` | "prevents interrupt controller issues" | Disables the I/O APIC, forcing legacy 8259 routing. |
| `nolapic` | "prevents local interrupt issues" | Disables the local APIC. Together with `noapic` this would starve the **internal keyboard** — an i8042 `AT Translated Set 2 keyboard` on IRQ 1 — of interrupts. Also drops the machine to a single core. |

**Do not over-attribute the keyboard failure to these arguments.** When the
Framework 13 was actually observed with a dead console keyboard on 2026-08-01,
inspection of the deployed GRUB entry showed the kernel line carried **only
`quiet`** — none of these arguments had ever reached the machine. They were a
latent defect in the config *generator*, worth removing on their own merits, but
they were not the cause of anything observed.

The real cause was the stale channel pin: `linux-6.6.16` and its firmware
predate this laptop. Repinning forward to `linux-7.1.5` fixed the keyboard,
WiFi, Bluetooth and amdgpu together — verified on hardware 2026-08-02. See
[RECOVERY_REBUILD_FROM_HOST_OS.md](RECOVERY_REBUILD_FROM_HOST_OS.md).

The lesson worth keeping: several unrelated-looking hardware failures appearing
on one boot usually means one cause underneath — a kernel and firmware set older
than the machine — not several independent problems each needing its own kernel
argument. Every argument in the table above was added to treat a symptom of that
single root cause.

`loglevel=3` was harmless and is kept.

## If your installed system has the old arguments

You do not have to reinstall.

### Step 1: confirm it at the GRUB menu (no disk write)

1. At the Guix GRUB menu, highlight the entry and press **`e`**
2. Find the line beginning `linux /gnu/store/...-linux-.../bzImage`
3. Delete `nomodeset`, `noapic`, `nolapic` and `acpi=off` if present
4. Press **Ctrl-X** to boot

Nothing is written to disk; a plain reboot restores the previous arguments. If
the keyboard works now, the diagnosis is confirmed.

### Step 2: make it permanent

```bash
sudo nano /etc/config.scm
```

Replace the kernel arguments line with:

```scheme
(kernel-arguments (append '("loglevel=3") %default-kernel-arguments))
```

Then reconfigure and reboot:

```bash
sudo guix system reconfigure /etc/config.scm
sudo reboot
```

If the GPU is still broken afterwards, the kernel arguments were never the whole
problem — go fix the channel pin, which is the actual cause.

### If you cannot get a working keyboard at all

Boot the Pop!_OS side (or a live ISO) and edit the Guix config from there:

```bash
sudo mount /dev/nvme0n1p4 /mnt      # the GUIX_ROOT partition
sudo nano /mnt/etc/config.scm
```

Then boot Guix and reconfigure.

---

## Historical prescription (superseded — do not follow)

What follows is the advice this document used to give. It is retained so that the
reasoning behind the 2025 changes is not lost.

> The installer was believed to be missing critical kernel parameters required
> for Framework 13 AMD GPU initialization, and the fix was to add:
>
> ```scheme
> (kernel-arguments '("quiet" "loglevel=3" "nomodeset" "noapic" "nolapic"))
> ```
>
> `acpi=off` had already been dropped from that set because it caused USB
> controller initialization failures.
>
> The escalation path offered was: add all four, then drop `acpi=off`, then drop
> `loglevel=3`, then fall back to `nomodeset` alone. Each step traded one broken
> subsystem for another because none of them addressed the firmware mismatch.

## Boot Hang Symptoms

- System hangs at "Loading kernel modules..."
- Repeating "time with localhost and MARK" messages every 20 minutes
- Never reaches login prompt
- Ctrl+C doesn't work

## Diagnosing a real hang

Do not start by adding kernel arguments. Start by finding out what actually
failed. The arguments this document used to recommend all hid the evidence.

### Step 1: remove `quiet`, don't add workarounds

1. At the GRUB menu, press **`e`**
2. Delete `quiet` and `loglevel=3` from the `linux` line so messages are visible
3. Press **Ctrl-X** and read what scrolls past

Boot messages are the diagnosis. `nomodeset` and friends suppress the failing
subsystem instead of reporting it.

### Step 2: reach a console

`Alt+F2` / `Alt+F3` switches to a TTY. If the keyboard does nothing there but the
firmware boot menu worked, suspect `noapic`/`nolapic` — see the table above.

Once you have a shell:

```bash
dmesg | grep -iE 'amdgpu|firmware|xhci|i8042'
```

### Step 3: match the error to a cause

| Message | Cause | Fix |
| --- | --- | --- |
| `Direct firmware load for amdgpu/... failed with error -2` | Channel pin is older than the GPU | `docs/CHANNEL_PINNING_POLICY.md` |
| `xhci_hcd probe ... failed with error -22` | `acpi=off` | Remove it |
| Greeter renders, keyboard dead | `noapic`/`nolapic` | Remove them |
| `kernel module not found "<name>"` | A module in `initrd-modules` is built in, not loadable | `docs/NVME_MODULE_FIX.md` |

### Step 4: verify config fundamentals

```scheme
(kernel linux)                   ; from nonguix
(firmware (list linux-firmware)) ; from nonguix
(initrd-modules %base-initrd-modules)
```

`initrd-modules` should be the Guix default. It already contains `usbhid` and
`hid-generic` for early-boot keyboards. Do **not** add `amdgpu` there: the initrd
only needs to mount root, and loading the GPU driver early also requires its
firmware in the initrd, which is one more way to fail before there is a console
to show it. See `docs/NVME_MODULE_FIX.md`.

### Step 5: repair from the other OS if you cannot boot Guix

```bash
# From Pop!_OS, or a live ISO
sudo mount /dev/nvme0n1p4 /mnt          # the GUIX_ROOT partition
sudo mount /dev/nvme0n1p1 /mnt/boot/efi
sudoedit /mnt/etc/config.scm
```

Fix the config there, then boot Guix and `guix system reconfigure`.

> **Note:** older revisions of this document pointed at
> [GNOME_LOGIN_TROUBLESHOOTING.md](./GNOME_LOGIN_TROUBLESHOOTING.md) and told you
> to adopt wingolog-era channel pinning for GDM login problems. **That advice is
> withdrawn.** On Ryzen AI 300 it is the cause of the failure, not the cure. See
> `docs/CHANNEL_PINNING_POLICY.md`.

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

#### Solution: remove `acpi=off`, and everything like it

`acpi=off` is too aggressive for Framework 13. The correct kernel arguments are:

```scheme
(kernel-arguments (append '("loglevel=3") %default-kernel-arguments))
```

Do not substitute `acpi=noirq` or keep `nomodeset`/`noapic`/`nolapic` — those
were the previously suggested "less aggressive" fallbacks and they trade one
broken subsystem for another. If USB or the GPU still misbehaves afterwards, the
problem is the channel pin, not the arguments.

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

`loglevel=3` already reduces console verbosity:

```scheme
(kernel-arguments (append '("loglevel=3") %default-kernel-arguments))
```

### If Input Devices Don't Work

**First check the kernel arguments, not the initrd.** On this laptop the internal
keyboard is an i8042 `AT Translated Set 2 keyboard` on IRQ 1 — not a USB device —
so `usbhid` is irrelevant to it. If `noapic` or `nolapic` are present, they are
almost certainly the cause. See the table near the top of this document.

1. **Confirm what the keyboard actually is**:
   ```bash
   grep -A4 'AT Translated' /proc/bus/input/devices
   ```

2. **Check initrd modules are the Guix default**:
   ```scheme
   (initrd-modules %base-initrd-modules)
   ```
   `%base-initrd-modules` already includes `usbhid` and `hid-generic`. Listing
   them again is noise, and adding `amdgpu` there is actively harmful.

3. **Try accessing a text console**: Press `Alt+F2` or `Alt+F3` to switch to a TTY

4. **Check dmesg for USB errors**:
   ```bash
   dmesg | grep -i usb
   dmesg | grep -i hid
   ```

## ext4 "Unknown parameter" Error

If you see repeated errors during boot:

```
ext4: Unknown parameter 'defaults'
ext4: Unknown parameter 'noatime'
```

**This is NOT harmless.** An earlier revision of this document said it was, and
that was wrong. The mount is *rejected*, so on a non-root filesystem:
`file-system-/data` fails, the `file-systems` target fails, `user-processes`
never starts, and the machine boots with **no login ttys at all** — there is no
way in to repair it.

### Cause

In a Guix `<file-system>` record, `flags` and `options` are not interchangeable:

- **`flags`** takes symbols (`no-atime`, `no-suid`, `no-dev`, `read-only`, …) and
  is converted to mount(2) flag bits
- **`options`** is a string passed verbatim as mount(2)'s `data` argument, i.e.
  **filesystem-specific parameters only**

`defaults` and `noatime` are both VFS-level tokens, so ext4 rejects them as
parameters. Guix's own `%base-file-systems` shows the split:
`(flags '(read-only bind-mount no-atime))` versus `(options "size=50%")`.

### Solution

**Fixed in:** `framework-dual/install/03-config-dual-boot.go`

```scheme
(file-system
  (mount-point "/data")
  (device (file-system-label "DATA"))
  (type "ext4")
  (flags '(no-atime)))
```

### For Already-Installed Systems

Edit `/etc/config.scm`, change:

```scheme
(options "defaults,noatime")   ; or (options "noatime")
```

to:

```scheme
(flags '(no-atime))
```

then `sudo guix system reconfigure /etc/config.scm` and reboot.

If the machine already boots with no ttys, repair it from Pop!_OS by mounting the
Guix root and editing `/mnt/etc/config.scm` (see Step 5 above).

This bit the repo twice — first `(options "defaults,noatime")`, then
`(options "noatime")` — so a regression test now guards it.

## References

- [CHANNEL_PINNING_POLICY.md](./CHANNEL_PINNING_POLICY.md) - the actual root cause of the "hang"
- [NVME_MODULE_FIX.md](./NVME_MODULE_FIX.md) - initrd module policy
- [INSTALLATION_KNOWLEDGE.md](./INSTALLATION_KNOWLEDGE.md) - Framework 13 AMD GPU Boot Issues section
- [GUIDE_DUAL_BOOT.md](./GUIDE_DUAL_BOOT.md) - AMD GPU Boot Issues section
- [GNOME_LOGIN_TROUBLESHOOTING.md](./GNOME_LOGIN_TROUBLESHOOTING.md) - GDM login issues
