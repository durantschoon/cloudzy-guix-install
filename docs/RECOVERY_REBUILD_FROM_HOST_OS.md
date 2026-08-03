# Rebuilding a Guix System From the Host OS

How to repair or re-install the Guix side of a dual-boot machine **from the
other OS**, without booting the Guix installer ISO.

Verified end-to-end on 2026-08-02: Framework 13 (AMD Ryzen AI 300), rebuilt
from Pop!_OS, moving the Guix system from `linux-6.6.16` to `linux-7.1.5`.
Confirmed working on first boot -- keyboard responsive at the console, WiFi
connected via `nmtui`, and `dmesg | grep -i amdgpu` clean.

That single change fixed four symptoms at once: an unresponsive keyboard, no
wireless driver, Bluetooth `wmt command timed out`, and
`amdgpu/psp_14_0_4_toc.bin failed with error -2`. They were not four problems.
When a machine exhibits several unrelated-looking hardware failures on one boot,
suspect a kernel and firmware set older than the machine before you start
adding kernel arguments -- the previous config had accumulated `nomodeset`,
`noapic`, `nolapic` and `acpi=off` as workarounds, and every one of them was
treating a firmware problem with an interrupt-controller sledgehammer.

## When to use this

Use this route when the Guix side boots but is unusable, or does not boot at
all, **and** the other OS has Guix installed as a foreign-distro package (so
there is a working `guix-daemon`).

It is dramatically better than the ISO route for this case because:

- the host has working networking, so substitutes actually download
- you can inspect the target's `config.scm` before rebuilding from it
- nothing is written to the target until the very last command
- you can verify the result **before** rebooting into it

If the host OS does **not** have Guix installed, use the ISO instead.

## Prerequisites

- The other OS has `guix-daemon` running
- The Guix root partition can be mounted read-write
- The ESP is reachable
- Free space on the host's store: budget ~3 GB for a minimal system with
  nonguix `linux` + `linux-firmware`

Check space first. On many foreign-distro installs `/gnu/store` is a directory
on `/`, not its own partition, so filling it also degrades the host OS:

    df -h /gnu/store /

`guix gc` reclaims unreferenced items. On the host side, `apt clean` and
`journalctl --vacuum-size=200M` are usually worth several GB more.

## Step 0: substitutes -- BOTH halves

This is the single most common way to lose hours here.

`guix archive --authorize` grants **trust** in nonguix's signing key. It does
**not** tell the daemon to query nonguix's server. Both are required. Without
the URL, `linux` and `linux-firmware` have no substitute and the daemon
silently **builds them from source**.

Authorize the key:

    wget -qO- https://substitutes.nonguix.org/signing-key.pub \
      | sudo guix archive --authorize

Then add the URL. Passing `--substitute-urls=` on every command works but is
fragile -- one omission costs hours. Prefer a systemd drop-in at
`/etc/systemd/system/guix-daemon.service.d/nonguix-substitutes.conf`:

    [Service]
    ExecStart=
    ExecStart=/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon \
        --discover=yes \
        --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.gnu.org'

The empty `ExecStart=` is mandatory -- systemd appends to `ExecStart` by
default, and a second command on a `Type=simple` service is an error.

### WARNING: restarting guix-daemon can break the store

On a foreign distro, `gnu-store.mount` bind-mounts `/gnu/store` onto itself
read-only, and `guix-daemon` is ordered before it so it captures a read-write
view in a private mount namespace.

`systemctl restart guix-daemon` runs `ExecStartPre=systemctl stop
gnu-store.mount`. **That unmount usually fails** on a running desktop:

    umount: /gnu/store: target is busy

because your login shell, and anything else installed by Guix, executes from
`/gnu/store`. When the unmount fails the store is already read-only, so the
daemon captures it read-only. The symptom appears later, from an unrelated
command:

    guix system: error: '/gnu/store' is read-only; make sure to mount it
    read-write for proper guix-daemon operation

Fix without rebooting. `gnu-store.mount` binds `/gnu/store` onto **itself**, so
detaching that bind hides nothing -- the same files stay visible through the
underlying directory, which is mounted read-write:

    sudo umount -l /gnu/store
    sudo systemctl restart guix-daemon

Verify the daemon's own namespace, not the system-wide view:

    grep " /gnu/store " /proc/$(systemctl show guix-daemon -p MainPID --value)/mounts

You want `rw`. The system-wide view staying `ro` is correct and intended.
A reboot also fixes it deterministically: at boot `gnu-store.mount` does not
exist yet when the daemon starts, so there is nothing to unmount.

## Step 1: pin the channels

Write a channels file with an explicit commit pair. See
`docs/CHANNEL_PINNING_POLICY.md` -- in particular, the pin must be **newer than
the hardware**.

Build the pinned guix once:

    guix time-machine -C ~/channels-framework-dual.scm -- --version

Run this as your **normal user**, not root. Under `sudo`, `guix time-machine`
uses root's `~/.cache/guix` and re-clones ~800 MB of git history.

The built instance is registered as a GC root under
`/var/guix/profiles/per-user/$USER/inferiors/`. Resolve it once and invoke it
directly for everything that follows, which avoids the re-clone entirely:

    ls -l /var/guix/gcroots/auto/ | grep inferiors
    readlink -f /var/guix/profiles/per-user/$USER/inferiors/<hash>
    # -> /gnu/store/<hash>-profile ; use <that>/bin/guix

## Step 2: mount the target

Mount by **label**, never by device path -- numbering on a dual-boot disk is
whatever the other OS's installer left behind:

    sudo mkdir -p /mnt/guixroot
    sudo mount LABEL=GUIX_ROOT /mnt/guixroot

Bind the ESP into the target rather than unmounting the host's copy:

    sudo mount --bind /boot/efi /mnt/guixroot/boot/efi

## Step 3: read the existing config BEFORE rebuilding from it

    sudo cat /mnt/guixroot/etc/config.scm

Do not skip this. The deployed config may be older than the generator in this
repo and may carry defects that a rebuild would faithfully reproduce. Check at
minimum:

- `/data` and other non-root filesystems use `(flags '(no-atime))`, **not**
  `(options "noatime")` -- see `docs/NVME_MODULE_FIX.md` and the purpose file
  for `03-config-dual-boot.go`. Getting this wrong yields a system with no
  login ttys at all.
- `kernel-arguments` appends to `%default-kernel-arguments` rather than
  replacing it
- no `nomodeset` / `noapic` / `nolapic` / `acpi=off`
- there is a network stack -- `%base-services` alone gives you loopback only

## Step 4: pre-build the closure as a NORMAL user

This is the long step, it needs no root, and it writes nothing to the target:

    <pinned-guix> system build /path/to/config.scm \
      --substitute-urls="https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.gnu.org"

Watch the output. Lines like `CC crypto/sha512.o` mean substitutes are not
being reached -- stop and fix Step 0 rather than waiting out a source build.
Healthy output is `downloading from https://substitutes.nonguix.org/...`.

Doing this before `system init` means the privileged step is mostly a copy, and
any failure happens while nothing on the target has been touched.

## Step 5: install

    sudo cp /mnt/guixroot/etc/config.scm /mnt/guixroot/etc/config.scm.bak
    sudo cp /path/to/config.scm /mnt/guixroot/etc/config.scm
    sudo <pinned-guix> system init /mnt/guixroot/etc/config.scm /mnt/guixroot \
      --substitute-urls="https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.gnu.org"

## Step 6: VERIFY BEFORE REBOOTING

The advantage of this route is that the target is still mounted and inspectable.
Use it.

Which generation, and which kernel:

    readlink -f /mnt/guixroot/var/guix/profiles/system
    readlink -f /mnt/guixroot/var/guix/profiles/system/kernel/bzImage

The kernel command line GRUB will actually execute -- this is authoritative,
and it has disagreed with `config.scm` before:

    grep "linux /gnu/store" /mnt/guixroot/boot/grub/grub.cfg

Confirm `modprobe.blacklist=usbmouse,usbkbd` survived, and that none of
`nomodeset` / `noapic` / `nolapic` / `acpi=off` appear.

Drivers and firmware for your hardware are present. **Firmware ships
zstd-compressed**, so searching for the uncompressed name gives a false
negative:

    K=$(readlink -f /mnt/guixroot/var/guix/profiles/system/kernel)
    find $K/lib/modules -name 'mt7925*'          # example: this laptop's WiFi
    ls /gnu/store/*-linux-firmware-*/lib/firmware/amdgpu/psp_14_0_4*

Both bootloaders survived, and the NVRAM entries are sane:

    sudo ls /mnt/guixroot/boot/efi/EFI/Guix/ /mnt/guixroot/boot/efi/EFI/systemd/
    efibootmgr

`EFI/Guix/` containing only `grubx64.efi` is normal -- Guix keeps the menu
config at `/boot/grub/grub.cfg` on the Guix root, not on the ESP.

Finally, note how many generations exist:

    ls -l /mnt/guixroot/var/guix/profiles/ | grep system

A fresh `init` produces only `system-1-link`, meaning **there is no earlier Guix
generation to roll back to**. The other OS is your only fallback.

## After first boot

- A freshly generated config sets no password on the user account, so that
  account is locked. Log in as `root` with an empty password on tty1, then
  `passwd <user>`.
- Bring up networking with `nmtui` (curses, works on a bare console).
- Guix's GRUB menu includes a `Pop!_OS` entry that chainloads the other
  bootloader, so switching back does not need the firmware boot menu. F12 still
  works as a fallback.

## The installed system does not inherit your channels

**Symptom.** On the freshly installed machine, reconfiguring the very config
that built it fails:

```
$ sudo guix system reconfigure /etc/config.scm
failed to load '/etc/config.scm':
... in procedure resolve-interface: no code for module (nongnu packages linux)
```

**Cause.** The pin you set up in Step 1 lived on the *host* machine. `guix
system init` copies the store closure of the built system; it does not copy the
channel configuration that produced it. The target boots knowing only the
`guix` channel, so `(nongnu ...)` is unresolvable and the only mechanism for
changing the system is unavailable.

**Fix on a system already installed without it.** Both files are reachable if
you staged them on a partition shared with the host (`/data` here):

```
sudo mkdir -p /etc/guix
sudo cp /data/<user>/channels-framework-dual.scm /etc/guix/channels.scm
sudo -i guix pull                      # ~pinned commits, not HEAD
sudo -i guix system reconfigure /etc/config.scm
```

Use `sudo -i`, not plain `sudo`. `guix pull` installs root's new guix at
`/root/.config/guix/current/bin/guix`, which is only on `PATH` for a login
shell; a bare `sudo guix` keeps resolving to the older
`/run/current-system/profile/bin/guix` and fails the same way.

**Prevention.** The generated config now mirrors the pin into the target via
`guix-configuration`, so the guix service writes `/etc/guix/channels.scm` at
activation and the machine is self-sufficient from first boot. The same
override adds `https://substitutes.nonguix.org` to `substitute-urls` — see
Step 0; a system with the key authorized but no URL compiles Linux from source
instead of reporting anything.

## Related

- `docs/CHANNEL_PINNING_POLICY.md` -- the pin must be newer than the hardware
- `docs/NVME_MODULE_FIX.md` -- initrd module policy
- `docs/FRAMEWORK_STARTUP_HANG_FIX.md` -- kernel arguments
- `framework-dual/install/03-config-dual-boot_purpose.txt` -- why each setting
  in the generated config is what it is
