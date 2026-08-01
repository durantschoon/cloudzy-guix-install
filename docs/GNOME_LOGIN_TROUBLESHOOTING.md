# GNOME Login Troubleshooting Guide (Framework 13 AMD)

## Problem Statement

On a Framework 13 AMD running Guix System you see:

- **Successful TTY login** (`Alt-F2` / `Alt-Ctrl-F2`) as your user
- **GDM appears**, accepts your password, then immediately drops you back to the login screen
- **dmesg shows amdgpu firmware failures** such as:
  ```
  Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2
  amdgpu: Fatal error during GPU init
  ```
- **GDM / GNOME logs** show warnings about DRI3 and Mutter, and sessions that "never registered" and are immediately removed

**In other words:** The display stack is up, but the AMD GPU never finishes initializing because the firmware/kernel combo doesn't match what the Framework 13 AMD needs.

### Root Cause

This is **not** a "GDM is broken" issue. The display manager works and the
password is correct; the graphics stack never initializes because **the amdgpu
firmware is older than the GPU**.

Read the firmware name in the error — it identifies the silicon:

| Blob | Hardware |
| --- | --- |
| `psp_14_0_4_*` | Strix Point (Ryzen AI 300) |
| `gc_11_5_*`, `dcn_3_5_*` | gfx11.5 / Radeon 880M–890M |

Confirm what you actually have before choosing a fix:

```bash
lspci -nn | grep -iE 'vga|display'      # 1002:1114 = Radeon 890M, Strix Point
cat /sys/class/dmi/id/product_name      # e.g. Laptop 13 (AMD Ryzen AI 300 Series)
```

---

## Solution: move the channel pin FORWARD

> **Important:** this document previously told you to pin *backwards*, to
> wingolog-era commits from 2024-02-16. **On Ryzen AI 300 that is the cause of
> this error, not the cure.** See the correction below.

For Ryzen AI 300 you need:

- **kernel >= 6.10** — where amdgpu gained gfx11.5 IP support
- **linux-firmware from mid-2024 or later** — where `psp_14_0_4`, `gc_11_5_*` and
  `dcn_3_5_*` were added

The repo's pin is in `lib/common.go` (`FrameworkDualGuixCommit` /
`FrameworkDualNonguixCommit`). To move it forward, follow
[CHANNEL_PINNING_POLICY.md](./CHANNEL_PINNING_POLICY.md).

Then reconfigure through the pinned channels:

```bash
guix time-machine -C ~/channels.scm -- system reconfigure /etc/config.scm
```

### Why the old advice was wrong

Wingo's post is about the **Ryzen 7040** Framework 13. Strix Point shipped in
July 2024, about five months *after* those commits, so a Feb-2024 snapshot cannot
contain firmware for it. Pinning backwards to fix a missing-firmware error for
new hardware is self-defeating: it guarantees the error it was meant to remove.

Corroboration from the Pop!_OS side of the same laptop, where amdgpu works:
kernel 7.0.11, linux-firmware 20260221, and `psp_14_0_4_toc.bin` present.

If you are on a **Ryzen 7040** Framework 13, the wingolog pin may still be
appropriate — but verify it against your own `lspci` output rather than assuming.

---

## Historical procedure (superseded — do not follow on Ryzen AI 300)

The rest of this document walks through discovering and applying the wingolog-era
commits. It is retained because the *mechanics* of pinning and of
`guix time-machine ... system reconfigure` are still correct and useful; only the
choice of commits is wrong. Substitute a recent commit pair for the Feb-2024 ones
throughout.

---

## Step 1: Discover the Wingo-Era Commits

You can do this on any machine with git installed (Pop!_OS is fine).

### Clone the channels

```bash
mkdir -p ~/src/guix-wingolog
cd ~/src/guix-wingolog

git clone https://git.guix.gnu.org/guix.git
git clone https://gitlab.com/nonguix/nonguix.git
```

### Find "Wingo date" commits

Wingo's post is dated **2024-02-16**. We want the last commit before the end of that day.

**For guix:**

```bash
cd ~/src/guix-wingolog/guix
GUIX_COMMIT=$(git log --before="2024-02-17 00:00" -1 --format=%H)
echo "$GUIX_COMMIT"
```

**For nonguix:**

```bash
cd ~/src/guix-wingolog/nonguix
NONGUIX_COMMIT=$(git log --before="2024-02-17 00:00" -1 --format=%H)
echo "$NONGUIX_COMMIT"
```

Write both hashes down; we'll call them:
- `GUIX_COMMIT` – "Guix as of Wingo's post"
- `NONGUIX_COMMIT` – "NonGuix as of Wingo's post"

(You could just hard-code the hashes in the next step instead of exporting env vars.)

---

## Step 2: Write a Pinned Channels File

Create `~/wingolog-channels.scm` on the Framework-13 (or copy it over):

```scheme
;; wingolog-channels.scm
;; Channels pinned to the same era as Wingo's Framework 13 AMD writeup.

(list
  (channel
    (name 'guix)
    (url "https://git.guix.gnu.org/guix.git")
    (branch "master")
    (commit "GUIX_COMMIT_GOES_HERE")
    (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
  (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    ;; We pin to the commit as of Wingo's date
    (branch "master")
    (commit "NONGUIX_COMMIT_GOES_HERE")
    ;; Enable signature verification (from nonguix docs)
    (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
```

**Replace `GUIX_COMMIT_GOES_HERE` and `NONGUIX_COMMIT_GOES_HERE`** with the exact hashes you got from Step 1.

This gives you a reproducible "Wingo-era" Guix universe.

---

## Step 3: Verify Your Framework-13 Config Uses Wingo's Hardware Bits

Your `configuration.scm` for the Framework 13 AMD should have:

```scheme
(use-modules (gnu)
             (gnu packages linux)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(operating-system
  ;; ...

  ;; 1. Kernel + firmware from nonguix:
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  ;; 2. Framework-13-specific initrd modules:
  (initrd-modules
    (append '("amdgpu"      ; AMD iGPU
              "nvme"        ; NVMe drive
              "xhci_pci"    ; USB 3 controller
              "usbhid"      ; USB kbd/mouse
              "i2c_piix4")  ; sensor bus
            %base-initrd-modules))

  ;; 3. Whatever bootloader you already have working:
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (timeout 5)))

  ;; 4. Your existing file-systems + users + services...
  )
```

This is exactly the pattern Wingo describes (kernel+firmware+initrd from nonguix plus a small list of needed modules).

**Make sure:**
- You're using `(kernel linux)` and `(firmware (list linux-firmware))` from `(nongnu packages linux)`
- `initrd-modules` is present and includes `"amdgpu"`

---

## Step 4: Reconfigure via guix time-machine

Back on the Framework-13 TTY (where you can log in as your user):

1. Put `configuration.scm` somewhere sensible, e.g. `/etc/guix/framework-13.scm` (or wherever you already keep it)
2. Put `wingolog-channels.scm` in your home dir or `/etc/guix/`
3. Run:

```bash
sudo guix time-machine -C ~/wingolog-channels.scm -- \
  system reconfigure /etc/guix/framework-13.scm
```

This does **not** change your global `guix pull`; it just says:
> "Pretend I'm running the Wingo-era Guix, apply this config, and build that system."

When it finishes successfully:

```bash
sudo reboot
```

Boot into Guix, pick your usual entry in GRUB, and try logging into GNOME again.

---

## What This Buys You / Why It Matters

For a future you (or another programmer), the key conceptual points:

1. **The root bug isn't "GDM is broken"**; it's "the current mixed guix + nonguix heads don't give you working AMD firmware for this specific laptop."

2. **Wingo's blog post documents a known-good combination** (kernel + firmware + modules) for the Framework 13 AMD, but not the commits themselves.

3. **Because Guix is channel-based and declarative**, you can reconstruct that environment:
   - Use `git log` to pick commits at the time of Wingo's post
   - Pin them via `channels.scm`
   - Use `guix time-machine` to reconfigure your system in that frozen universe

4. **Once that's working and stable**, you can later experiment with upgrading channel commits (newer kernels, newer firmware) in a controlled way by editing `wingolog-channels.scm` and repeating the `guix time-machine ... system reconfigure` dance.

---

## Verification After Reboot

After rebooting with the pinned Wingo-era config:

### Check that amdgpu loaded successfully

```bash
dmesg | grep amdgpu
```

**You should see:**
- Successful firmware loading messages (not "failed with error -2")
- GPU initialization completed
- No fatal errors

### Check that GNOME session starts

1. Go to GDM login screen
2. Enter your password
3. **You should successfully enter GNOME desktop** (not get kicked back to login)

### Check graphics acceleration

Once in GNOME:

```bash
# Check that DRI3 is working
glxinfo | grep "OpenGL renderer"
```

Should show your AMD GPU, not software rendering.

---

## If It Still Doesn't Work

If you still get the same amdgpu firmware errors after pinning to Wingo's commits:

1. **Double-check the commit hashes** in `wingolog-channels.scm` match what you got from `git log`
2. **Verify your config.scm** has all three components:
   - `(kernel linux)` from nonguix
   - `(firmware (list linux-firmware))` from nonguix
   - `(initrd-modules (append '("amdgpu" ...) %base-initrd-modules))`
3. **Check that time-machine actually ran**:
   ```bash
   guix describe
   ```
   This shows your current generation, but the system generation will use the pinned commits
4. **Look at the actual system generation**:
   ```bash
   guix system describe
   ```

If none of this works, you may need to:
- Try an even older snapshot (e.g., 2024-01-01)
- Or try a newer snapshot with more recent AMD firmware support
- Or check if there's a Framework 13 AMD-specific kernel parameter needed

---

## Additional Resources

- [Wingo's Blog Post: Guix on the Framework 13 AMD](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd)
- [Guix Manual: Channels](https://guix.gnu.org/manual/en/html_node/Channels.html)
- [Guix Manual: Invoking guix time-machine](https://guix.gnu.org/manual/en/html_node/Invoking-guix-time_002dmachine.html)
- [NonGuix Channel](https://gitlab.com/nonguix/nonguix)
