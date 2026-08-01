# initrd Module Policy (formerly "the NVMe module fix")

## Current policy

Framework and framework-dual use the Guix default:

```scheme
(initrd-modules %base-initrd-modules)
```

Nothing is prepended and nothing is filtered. If you are looking for the `nvme`
filter this document used to describe, it has been removed — see below for why.

## Why the old filter was wrong

The config used to read:

```scheme
(initrd-modules
 (append '("amdgpu" "usbhid" "i2c_piix4")
         (remove (lambda (module)
                   (or (string=? module "nvme")
                       (string=? module "xhci_pci")))
                 %base-initrd-modules)))
```

Three problems.

### 1. The filter never did anything

Neither `nvme` nor `xhci_pci` is in `%base-initrd-modules`. From
`default-initrd-modules` in `gnu/system/linux-initrd.scm`, the list is:

```
ahci
usb-storage  uas
usbhid  hid-generic  hid-apple
mmc_block
dm-crypt  xts  serpent_generic  wp512
nls_iso8859-1
pata_acpi  pata_atiixp  isci        (x86 only)
virtio_pci  virtio_balloon  virtio_blk  virtio_net
virtio_console  virtio-rng  virtio_mmio  virtio_scsi
```

So `(remove <pred> %base-initrd-modules)` removed nothing. The
`kernel module not found "nvme"` error it was written to prevent came from an
earlier config that listed `nvme` in the *prepended* list — removing that entry
was the actual fix, and the filter was cargo left behind.

### 2. "Built-in to 6.6.16" is a claim about one pinned kernel

Now that framework-dual tracks a recent pin (see
[CHANNEL_PINNING_POLICY.md](./CHANNEL_PINNING_POLICY.md)), a hardcoded list of
"modules that are built in" is a latent hazard. If a future kernel builds NVMe as
a module and the filter is still stripping it, the initrd cannot find the root
device and the machine does not boot.

Filtering by hardcoded name fails in the dangerous direction. Filtering by
inspection of the actual kernel does not.

### 3. `amdgpu` does not belong in an initrd

The initrd only has to get far enough to mount the root filesystem. udev loads
`amdgpu` once the real system is up.

Putting it in the initrd also means its firmware must be in the initrd, which is a
second way to fail — before there is any working console to display the failure
on. `usbhid` was redundant (already in `%base-initrd-modules`, alongside
`hid-generic`) and `i2c_piix4` is SMBus for sensors, which has nothing to do with
booting.

## The NVMe caveat that still applies

`nvme` is absent from `initrd-modules`, and NVMe root works, because the driver is
**built into** the Guix kernel rather than being loadable.

If a future channel pin makes it modular, root will fail to mount. The symptom is
an initrd that drops to a Guile rescue REPL saying it cannot find the device for
`/`. The fix is to add `"nvme"` to `initrd-modules`.

Check before assuming:

```bash
# Build the kernel through the pinned channels
guix time-machine -C ~/channels.scm -- build linux

# Is nvme a loadable module in that build?
find /gnu/store/<hash>-linux-<version>/lib/modules -name 'nvme.ko*'
```

No result means built-in (current situation, nothing to do). A result means
modular, and it must be listed.

## If you genuinely need to filter a module

Do not hardcode it. The mechanism is already in `lib/`:

```go
// Inspect the real kernel package rather than guessing
path, _ := lib.FindKernelPackageForModules("non-libre")
avail, _ := lib.CheckKernelModulesAvailable(path, []string{"nvme", "xhci_pci"})

// Feed the result into the generated config
expr := lib.BuildInitrdModulesExpr(modulesToFilter)
```

`lib.GetBuiltInModulesToFilter()` returns an empty list today. When it is empty,
`BuildInitrdModulesExpr` emits a bare `%base-initrd-modules` rather than a no-op
`(remove (lambda (module) #f) ...)`, so the generated config never implies that
filtering is happening when it is not.

## History

- **2025-01-XX**: Added an `nvme` (later `nvme` + `xhci_pci`) filter to work around
  `kernel module not found` during `guix time-machine system build`
- **2026-08-01**: Filter removed. Established that it was a no-op against
  `%base-initrd-modules`, that the hardcoded list would become dangerous under a
  newer kernel pin, and that `amdgpu`/`usbhid`/`i2c_piix4` should not have been in
  the initrd either. Mechanism kept in `lib/check-kernel-modules.go` for the case
  where a real filter is ever needed.

## Related

- [CHANNEL_PINNING_POLICY.md](./CHANNEL_PINNING_POLICY.md) — the pin this used to depend on
- [FRAMEWORK_STARTUP_HANG_FIX.md](./FRAMEWORK_STARTUP_HANG_FIX.md) — the kernel arguments from the same misdiagnosis
