# Guix OS Installation: Hard-Won Knowledge

**A Technical Tale of Discovery**

Installing Guix OS isn't just about following commands—it's about understanding a uniquely principled operating system that refuses to compromise on free software ideals. If you've installed Ubuntu or Fedora, you'll find Guix familiar yet different. It uses the same Linux kernel, the same basic filesystem layout, but approaches system management with a functional programming philosophy that makes every configuration change reproducible.

This document captures critical lessons learned from real-world installations across cloud VPS instances, modern laptops, and dual-boot scenarios. Each section tells the story of a problem discovered, investigated, and solved. Some issues took hours of debugging; others required reading Guix's Scheme source code. We've distilled these experiences into practical guidance so you don't have to repeat our mistakes.

**What makes this different from the official Guix manual?** The manual tells you what Guix *should* do. This document tells you what actually happens when you run installations on real hardware with real constraints—and how to fix things when they go wrong.

## 🎯 Free Software vs Nonguix: Platform-Specific Design Decisions

**Design Philosophy:** Different platforms have different hardware requirements and use cases.

### Cloudzy VPS Installer - Free Software Only

**Why no nonguix:**
- VPS hardware is fully virtualized (KVM/QEMU) with free software drivers
- No physical WiFi cards, Bluetooth, or proprietary GPU firmware needed
- Server workloads typically don't require proprietary software
- Keeps system fully free software compliant (important for many server deployments)
- `linux-libre` kernel works perfectly with virtualized hardware

**Code:** Uses `RunGuixSystemInitFreeSoftware()` - standard `guix system init` without time-machine or nonguix channel

**Installation approach:**
- Three-step workaround needed (same as framework-dual, due to kernel/initrd bug):
  1. Build: `guix system build /mnt/etc/config.scm`
  2. Copy: Manually copy kernel/initrd from `/gnu/store/*-system/` to `/mnt/boot/` using `cp -L` (dereference symlinks)
  3. Init: `guix system init /mnt/etc/config.scm /mnt` (installs bootloader only)
- **CRITICAL:** Kernel/initrd in system generation are symlinks - must use `cp -L` to copy actual files
- **Key insight:** Even free software installs need the 3-step workaround - `guix system init` doesn't copy kernel files to `/boot/`

### Framework 13 Installer - Requires Nonguix

**Why nonguix is mandatory:**
- **WiFi:** MediaTek MT7922 requires proprietary firmware blobs (`linux-firmware` package)
- **GPU:** AMD graphics need proprietary firmware for full functionality
- **Bluetooth:** Modern Bluetooth chips require proprietary firmware
- **Practical necessity:** Without nonguix, the laptop has no wireless connectivity
- Laptop users expect "it just works" - nonguix delivers that

**Code:** Uses `RunGuixSystemInit()` with time-machine + nonguix channel for `(kernel linux)` and `(firmware (list linux-firmware))`

**Installation approach:**
- Three-step workaround needed (due to kernel/initrd bug with time-machine):
  1. Build: `guix time-machine -C channels.scm -- system build /mnt/etc/config.scm`
  2. Copy: Manually copy kernel/initrd from `/gnu/store/*-system/` to `/mnt/boot/`
  3. Init: `guix time-machine -C channels.scm -- system init /mnt/etc/config.scm /mnt`
- **Key insight:** time-machine + nonguix creates different store structure requiring manual kernel copy

### When to Use Which Approach

**Use Free Software Only (like Cloudzy):**
- Virtual machines and VPS instances
- Servers with Intel/Realtek NICs that have free drivers
- Systems where hardware is known to work with `linux-libre`
- Deployments where free software compliance is required

**Use Nonguix (like Framework 13):**
- Modern laptops (almost all need WiFi firmware)
- Gaming systems (proprietary GPU drivers)
- Workstations with recent AMD/Nvidia GPUs
- Any system where wireless connectivity is essential

**This is a feature, not a bug:** Having separate installers lets users make informed choices based on their hardware and values.

### Framework 13 Post-Install Workflow: The Two-Pull Process

**Critical Finding:** After installing Guix on Framework 13, you must perform a two-pull process to add the nonguix channel and update your Guix installation.

**Why Two Pulls Are Required:**

1. **First `guix pull`**: Upgrades from Guix 1.4.0 (ISO) to latest master
   - Required because old Guix doesn't support channel introductions
   - Takes 10-30 minutes
   - Creates generation 1

2. **Create `~/.config/guix/channels.scm`** with nonguix channel:
   ```scheme
   (cons* (channel
           (name 'nonguix)
           (url "https://gitlab.com/nonguix/nonguix"))
          %default-channels)
   ```

3. **Second `guix pull`**: Adds nonguix channel
   - Takes 10-30 minutes
   - Creates generation 2

4. **Fix PATH** (CRITICAL):
   ```bash
   export PATH="$HOME/.config/guix/current/bin:$PATH"
   ```
   - Add to `~/.bashrc` for persistence
   - Without this, `guix describe` shows old system Guix (generation 1) instead of your pulled Guix (generation 2)
   - Nonguix packages won't be found even though second pull succeeded

**Common Pitfall - PATH Issue:**

- User's pulled Guix is in generation 2 at `~/.config/guix/current/bin/guix`
- System Guix (old) is at `/run/current-system/profile/bin/guix`
- If PATH not updated, `guix describe` shows generation 1 (system), not generation 2 (user)
- Always verify which guix binary is running: `which guix`

**Verification:**

```bash
# Check which guix is active
which guix
guix describe

# Should show generation 2 with nonguix channel
# If it shows generation 1, PATH is not set correctly
```

## 🧠 The Golden Rule: cow-store

**The Story:** Early in Guix installer development, people would try to install the system and hit "out of space" errors. The ISO boots with only a few gigabytes of RAM for its tmpfs, and `guix system init` needs to build or download many gigabytes of packages. Where does it all go? The answer is cow-store—a clever overlay filesystem that redirects new store writes to your target disk while keeping the ISO's existing store intact.

**The Rule:** **Always run `herd start cow-store /mnt` before `guix system init`.**

**Why it works:**
- Redirects Guix store writes to the target disk (`/mnt/gnu/store`) while keeping the live installer's `/gnu/store` usable
- Think of it as a "copy-on-write" overlay where new packages go to disk, existing ones stay in RAM
- Without it, the ISO's limited tmpfs fills up and the installation fails with "No space left on device"

**The tempting mistake:** **Never use `mount --bind /mnt/gnu /gnu`**
- This shadows the live system's store and breaks the `guix` command
- The ISO's `guix` command needs access to its own store to function
- If you hide it with a bind mount, even basic Guix commands fail

## 🐚 Bash Paths in Guix

**CRITICAL: All scripts MUST use `/run/current-system/profile/bin/bash` shebang.**

Guix does not follow the Filesystem Hierarchy Standard (FHS). There is NO `/bin/bash` on Guix systems.

**For all scripts (ISO and installed system):**
```bash
# CORRECT - Required for all scripts
#!/run/current-system/profile/bin/bash

# WRONG - Does not exist in Guix
#!/bin/bash

# WRONG - Not reliable on Guix ISO (DO NOT USE)
#!/usr/bin/env bash
```

**For chroot commands:**
```bash
# CORRECT
chroot /mnt /run/current-system/profile/bin/bash

# WRONG
chroot /mnt /bin/bash
```

**Why `/run/current-system/profile/bin/bash` is mandatory:**
- This is where bash exists on both Guix ISO and installed systems
- `#!/usr/bin/env bash` is NOT reliable on Guix ISO
- `/bin/bash` does not exist at all
- All critical scripts must use the explicit path

## 🧩 Partitioning & Filesystems

### Partition Table Requirements

- Use GPT partition table
- EFI System Partition (ESP): FAT32, 512MB-1GB, flags: `boot,esp`
- Root partition: ext4, labeled for reliability

### Label Convention

Use **UPPERCASE with underscores** for all partition labels:

- `EFI` → FAT32 ESP → mounted at `/boot/efi` (standard label, installer only looks for this)
- `GUIX_ROOT` → ext4 → mounted at `/`
- `DATA` (optional) → ext4/btrfs → mounted at `/data` (not `/home`)

**Important**: The installer creates EFI partitions with the `EFI` label and only attempts to mount using this exact label. Do not use `ESP`, `BOOT`, or other variations - use `EFI` consistently.

Verify labels:

```bash
e2label /dev/nvme0n1pX         # ext4 labels
fatlabel /dev/nvme0n1pY        # FAT labels
parted /dev/nvme0n1 print      # GPT partition names
```

Mount by label for reliability:

```bash
/dev/disk/by-label/GUIX_ROOT
/dev/disk/by-label/EFI
```

**Critical: Use file-system-label instead of UUID in config.scm**

Always use `(file-system-label "LABEL_NAME")` instead of `(uuid "xxxx-xxxx" 'ext4)` in your config.scm:

```scheme
;; CORRECT - Use file-system-label
(file-system
  (mount-point "/")
  (device (file-system-label "GUIX_ROOT"))
  (type "ext4"))

;; INCORRECT - Don't use UUID
(file-system
  (mount-point "/")
  (device (uuid "xxxx-xxxx" 'ext4))
  (type "ext4"))
```

**Why file-system-label is better:**

- **More reliable** - Labels don't change between boots
- **More readable** - Clear partition purpose
- **More portable** - Works across different systems
- **Easier debugging** - Can see labels with `lsblk -f`

### DATA Partition Usage

**Important**: The `DATA` partition is mounted at `/data`, not `/home`:

- **`/data`** - Shared data partition for files, documents, media
- **`/home`** - User home directories remain on the root filesystem
- **Benefits**: Cleaner separation, easier backup, shared access across systems
- **Options**: `defaults,noatime` for better performance on data storage

**Framework-dual specific**: The framework-dual installer uses the `DATA` environment variable (not `HOME_PARTITION`) to detect and mount the DATA partition. The partition should be labeled `DATA` and will be automatically detected during Step 01.

### BIOS vs UEFI Boot Modes

**Critical**: The partition layout must match the boot mode.

**UEFI Mode (most modern systems):**
```bash
# Partition layout:
# 1. EFI System Partition (ESP) - 512MB, FAT32, flags: esp
# 2. Root partition - remaining space, ext4

parted /dev/sda --script \
  mklabel gpt \
  mkpart EFI fat32 1MiB 513MiB \
  set 1 esp on \
  mkpart root ext4 513MiB 100%
```

**BIOS Mode (legacy systems, some VPS):**
```bash
# Partition layout:
# 1. BIOS Boot partition - 1MB, no filesystem, flags: bios_grub
# 2. EFI System Partition - 512MB, FAT32, flags: esp (for future UEFI boot)
# 3. Root partition - remaining space, ext4

parted /dev/sda --script \
  mklabel gpt \
  mkpart BIOSBOOT 1MiB 2MiB \
  set 1 bios_grub on \
  mkpart EFI fat32 2MiB 514MiB \
  set 2 esp on \
  mkpart root ext4 514MiB 100%
```

**Why BIOS needs a special partition:**
- GPT + BIOS boot requires a BIOS Boot Partition for GRUB's core.img
- Without it: `grub-install` fails with "GPT partition label has no BIOS boot partition"
- The partition must have the `bios_grub` flag set
- Size: 1MB is sufficient
- Filesystem: none (GRUB writes directly to it)

**How to detect boot mode:**
```bash
# Check if /sys/firmware/efi exists
if [ -d /sys/firmware/efi ]; then
    echo "UEFI mode"
else
    echo "BIOS mode"
fi
```

**Common VPS boot modes:**
- Cloudzy: BIOS mode (requires BIOS boot partition)
- Most cloud providers: UEFI mode
- Physical systems: Check BIOS/UEFI settings

## 🔧 Nonguix Channel Setup

**Critical**: Framework installers require the nonguix channel for proprietary firmware and kernel support.

### Automatic Setup (Recommended)

The framework installers automatically setup the nonguix channel:

```bash
# Creates /tmp/channels.scm with nonguix channel
# Authorizes nonguix substitutes
# Pulls Guix with nonguix channel
# Uses guix time-machine for system init
```

### Manual Setup (If Needed)

```bash
# 1. Create channels.scm
cat > /tmp/channels.scm <<'EOF'
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
       %default-channels)
EOF

# 2. Authorize nonguix substitutes (faster builds)
wget -qO- https://substitutes.nonguix.org/signing-key.pub | guix archive --authorize

# 3. Pull with nonguix channel
guix pull -C /tmp/channels.scm

# 4. Use time-machine for system init
guix time-machine -C /tmp/channels.scm -- \
  system init /mnt/etc/config.scm /mnt --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Why Nonguix Channel is Required

- **Proprietary firmware** - `linux-firmware` package for hardware support
- **Non-free kernel** - `linux` kernel (not `linux-libre`) for better hardware compatibility
- **Hardware-specific modules** - AMD GPU, NVMe, USB drivers for Framework 13

### Common Errors Without Nonguix

```text
no code for module (nongnu packages linux)
unbound variable: linux
unbound variable: linux-firmware
```

**Solution**: Ensure nonguix channel is available before config generation.

### Channels.scm Location

**Location**: The installer creates `channels.scm` in the home directory (`~/channels.scm`, typically `/root/channels.scm` when running as root).

**Why home directory instead of `/tmp`:**
- `/tmp` can be cleaned during installation, causing `channels.scm` to disappear
- Home directory persists throughout the installation process
- More reliable for multi-step installations

**The installer:**
- Creates `~/channels.scm` during nonguix channel setup
- Uses `GetChannelsPath()` helper that checks home directory first, then `/tmp` as fallback
- All scripts check both locations for backward compatibility

### Troubleshooting Nonguix Channel Issues

**Problem**: `channels.scm` missing during installer re-runs

**Symptoms:**
- Config exists but channels.scm doesn't
- "no code for module (nongnu packages linux)" error during system init
- Installer skips nonguix setup prompt

**Cause**: Installer idempotency check skips config generation (including channel setup) when config.scm already exists from a previous partial run.

**Solution**: The installer now checks for both config.scm AND channels.scm. If config exists but channels.scm is missing, it will:
1. Prompt for nonguix consent
2. Create `~/channels.scm` (home directory for persistence)
3. Skip config file writing (already exists)
4. Continue to system init with time-machine

**Manual fix if needed:**
```bash
# Remove config to trigger full re-generation
rm /mnt/etc/config.scm ~/channels.scm

# Re-run installer - will prompt for nonguix and create both files
```

## 🚀 Installer Best Practices

### Always Specify Platform

**Critical**: Always specify the platform using the GUIX_PLATFORM environment variable

```bash
# Set environment variable before running
export GUIX_PLATFORM="framework-dual"
bash bootstrap.sh

# Or inline for single command
GUIX_PLATFORM="cloudzy" bash bootstrap.sh

# WRONG - No platform specified (defaults to cloudzy, may be wrong)
bash bootstrap.sh   # Without GUIX_PLATFORM set
```

**Why this matters:**
- Different platforms have different partition layouts (BIOS vs UEFI)
- Different platforms need different channels (nonguix for framework, not for cloudzy)
- Different platforms use different installer code paths
- Without GUIX_PLATFORM, installer defaults to "cloudzy" which may be wrong for your hardware

### Setting Environment Variables

Set user variables BEFORE running the installer:

```bash
export USER_NAME="yourname"
export FULL_NAME="Your Full Name"
export TIMEZONE="America/New_York"
export HOST_NAME="my-guix-system"

# Then run installer
bash bootstrap.sh framework-dual
```

**Why set variables first:**
- Installer reads variables during config generation (step 3)
- If not set, uses defaults (USER_NAME=guix, FULL_NAME="Guix User")
- Cannot change after config is generated without deleting /mnt/etc/config.scm

### Handling Installer Interruptions

If the installer is interrupted or fails:

1. **Check what completed:**
   ```bash
   lsblk -f                    # Check partitions and labels
   ls /mnt/etc/config.scm      # Check if config exists
   ls /tmp/channels.scm        # Check if channels exist
   ```

2. **Re-run is safe (idempotent):**
   - Installer skips completed steps
   - Won't repartition formatted disks
   - Won't regenerate existing configs

3. **To force re-run of specific step:**
   ```bash
   # Force config regeneration
   rm /mnt/etc/config.scm /tmp/channels.scm

   # Force partition recreation (DESTROYS DATA!)
   wipefs -a /dev/vda
   ```

### Daemon Responsiveness Issues (VPS Systems)

**Problem**: "Connection refused" errors when validating config, especially on VPS systems

**Symptoms:**
- Config validation fails before system init
- Error: "guix system: error: failed to connect to '/var/guix/daemon-socket/socket': Connection refused"
- Daemon check says "[OK] Daemon is responsive" but then immediately fails
- System init never starts
- More common on Cloudzy VPS than bare metal (Framework 13)

**Root Cause**: Race condition in daemon startup verification - daemon process runs but socket file not ready, or connection isn't stable yet

**Why VPS Systems Are Different:**

- Daemon startup is slower on VPS than bare metal
- Virtualization layer adds complexity and latency
- Socket file initialization takes longer
- May need longer waits or different startup approach
- Standard Guix commands work better than workarounds on VPS

**✅ FIXED** (commit 4a50e50, 2025-11-11):

The installer now includes robust daemon startup verification:

1. **Socket file verification**:
   - Checks `test -S /var/guix/daemon-socket/socket` before testing daemon
   - Prevents false positives when process runs but socket not ready
   - Critical for VPS systems with slower initialization

2. **Stability verification**:
   - After first successful `guix build --version`, waits 5 seconds
   - Tests 3 more times (2 seconds apart) to ensure stable connection
   - Prevents race conditions where daemon responds once but isn't truly ready

3. **Extended wait time**:
   - Up to 2 minutes wait (40 iterations × 3 seconds)
   - Shows clear progress: "Waiting... (5/40)"
   - Separate messages for socket, process, and responsiveness checks

4. **Applied to both code paths**:
   - When daemon is already running (early check)
   - When daemon is freshly started (main startup loop)

**Previous Solutions** (still in place):
- Increased wait time (commit 44be9d8)
- Graceful validation skip (commit 059f1dd)

**Manual fix if needed:**
```bash
# Check daemon status
herd status guix-daemon
ps aux | grep guix-daemon
ls -la /var/guix/daemon-socket/socket

# Restart daemon
herd stop guix-daemon
sleep 5
herd start guix-daemon

# Wait longer on VPS
sleep 30

# Test multiple times
for i in {1..5}; do
  echo "Test $i:"
  guix build --version && break
  sleep 10
done

# If that works, continue manually
guix system init /mnt/etc/config.scm /mnt
```

**Workaround for persistent issues:**
- Answer 'y' to continue when validation warns about daemon
- System build step will start daemon if needed
- Real validation happens during system build anyway

**Lessons Learned:**

- **VPS systems require more patience**: Daemon initialization is slower, socket verification is critical
- **Don't over-apply workarounds**: The 3-step kernel/initrd workaround is ONLY for nonguix+time-machine (Framework 13). Free software installs (VPS, servers) work fine with standard `guix system init`
- **Test each platform separately**: Different Guix channels create different `/gnu/store` structures. Don't assume workarounds apply universally

### Daemon Connection Failures During guix system init

**Problem**: Daemon verification passes (`isDaemonReady()` returns success), but `guix system init` immediately fails with "Connection refused" errors.

**Symptoms:**
- `isDaemonReady()` check passes (socket exists, `guix build --version` works)
- `guix system init` immediately fails with: `failed to connect to '/var/guix/daemon-socket/socket': Connection refused`
- Retries fail with the same error
- More common on VPS systems but can occur on any platform

**Root Cause**: Race condition where:
- Daemon process is running and socket file exists
- Socket becomes unavailable between verification and actual use
- Daemon process is running but socket is not accessible to `guix system init`
- Socket may have been closed or become stale between verification and command execution

**Why This Happens:**
- Socket file exists but connection is not stable
- Daemon process running but socket not accepting connections
- Timing issue between verification check and actual command execution
- VPS systems have more latency/variability in socket initialization

**✅ FIXED** (2025-01-XX):

The installer now automatically restarts the daemon when connection failures occur:

1. **After `guix system init` fails:**
   - Checks if daemon is still responsive using `checkDaemonResponsive()`
   - If daemon connection check fails, automatically restarts the daemon
   - Retries the command after daemon restart

2. **Applied to both code paths:**
   - `RunGuixSystemInit()` (framework-dual, time-machine path)
   - `RunGuixSystemInitFreeSoftware()` (cloudzy, free software path)

3. **Prevents repeated failures:**
   - Instead of retrying with a broken socket, daemon is restarted first
   - Ensures fresh socket connection before retry

**Implementation:**

```go
// After guix system init fails:
if err := RunCommandWithSpinner(...); err != nil {
    // Check if daemon is still responsive
    if !checkDaemonResponsive() {
        fmt.Println("[WARN] Daemon connection lost, restarting...")
        EnsureGuixDaemonRunning()  // Restart daemon
    }
    // Retry command
}
```

**Manual fix if needed:**

```bash
# Check daemon status
herd status guix-daemon
ps aux | grep guix-daemon
ls -la /var/guix/daemon-socket/socket

# Restart daemon
herd stop guix-daemon
sleep 5
herd start guix-daemon

# Wait for socket to be ready
sleep 10

# Test connectivity
guix build --version

# Retry system init
guix system init /mnt/etc/config.scm /mnt
```

**Related Issues:**
- This is different from initial daemon startup verification (documented above)
- This occurs AFTER verification passes but BEFORE/DURING `guix system init`
- More common on VPS systems due to socket stability issues
- Can also occur on physical hardware under load

## 🐧 Kernel Configuration

### Always Specify the Kernel Explicitly

**Critical**: Always specify the kernel in your `config.scm` file. Do not rely on defaults.

```scheme
(use-modules (gnu)
             (gnu packages linux)  ; Required for kernel packages
             (gnu system nss))

(operating-system
  ;; ... other fields ...
  (kernel linux-libre)  ; Explicitly specify kernel
  ;; ... rest of config ...
)
```

### Common Kernel Issues

- **"linux is unbound"**: Need to import `(gnu packages linux)` module
- **"linux-libre is unbound"**: The package name is correct, ensure module import
- **Default kernel**: Never omit the kernel field - always specify explicitly

### Why Explicit Kernel Specification Matters

- Guix defaults can change between versions
- Explicit specification ensures reproducible builds
- Prevents "unbound variable" errors during `guix system init`
- Makes the configuration self-documenting

### Initrd Configuration

**For Free Software Installations (Cloudzy, VPS, servers)**:
- **Omit the initrd specification** - Let Guix use its default initrd generation
- Guix will automatically create the correct initrd for your kernel and filesystem
- Explicit initrd specification can cause "Invalid keyword" errors with `base-initrd`

```scheme
(operating-system
  ;; ... other fields ...
  (kernel linux-libre)
  ;; No initrd specification - Guix uses defaults
  ;; ... rest of config ...
)
```

**For Framework 13 (nonguix)**:
- Use `(initrd microcode-initrd)` which properly handles kernel arguments
- This is required for proprietary firmware support

**Why This Matters**:

- Default initrd generation automatically handles kernel modules
- Prevents "Invalid keyword" errors when Guix passes kernel arguments
- Ensures proper initrd generation for your specific filesystem setup

### Critical Bug: Missing Kernel/Initrd Files (FIXED 2025-11-08, UPDATED 2025-01-XX)

**⚠️ IMPORTANT: This workaround is needed for BOTH Framework 13 (nonguix + time-machine) AND Cloudzy VPS (free software only) installations.**

**CRITICAL DISCOVERY (2025-01-XX):** Kernel tracking logs confirmed that `guix system init` (free-software-only mode) also does NOT create kernel/initrd files in the system generation. The system generation only contains `['gnu','gnu.go','guix']` - no kernel or initrd. This bug affects ALL platforms, not just nonguix installations.

> **See also:** [KERNEL_TRACKING.md](./KERNEL_TRACKING.md) for details on how kernel tracking logs helped discover this issue and how to use the tracking system for debugging.

---

**Issue**: `guix system init` creates a system generation but fails to copy kernel and initrd files to `/boot/`, leaving the system unbootable. This occurs with both:
- `guix time-machine` + nonguix channel (Framework 13)
- Standard `guix system init` without time-machine (Cloudzy VPS)

**Symptoms**:
- Installation appears to succeed without errors
- `/mnt/boot/grub/grub.cfg` exists
- `/mnt/boot/vmlinuz*` and `/mnt/boot/initrd*` are MISSING
- `/mnt/run/current-system` is a broken symlink

**Root cause**: `guix system init` doesn't copy kernel/initrd to `/boot` as expected, regardless of whether time-machine or nonguix is used. This is a bug in `guix system init` itself.

**✅ Solution (implemented in installer commit b956baf, updated 2025-01-XX)**:

The installer now uses a 3-step process instead of calling `system init` directly:

1. **Build system first**:
   ```bash
   # For Framework (with time-machine):
   guix time-machine -C ~/channels.scm -- system build /mnt/etc/config.scm
   
   # For Cloudzy (free software only):
   guix system build /mnt/etc/config.scm
   ```
   This creates complete system in `/gnu/store/*-system/` with kernel and initrd.

2. **Manually copy kernel files to /boot**:
   ```bash
   # CRITICAL: Use -L flag to dereference symlinks (Cloudzy kernel/initrd are symlinks!)
   cp -L /gnu/store/*-system/kernel /mnt/boot/vmlinuz
   cp -L /gnu/store/*-system/initrd /mnt/boot/initrd
   ln -s /gnu/store/*-system /mnt/run/current-system
   ```

3. **Install bootloader**:
   ```bash
   # For Framework (with time-machine):
   guix time-machine -C ~/channels.scm -- system init /mnt/etc/config.scm /mnt
   
   # For Cloudzy (free software only):
   guix system init /mnt/etc/config.scm /mnt
   ```
   System already built, this just installs GRUB.

**Why this works**:
- `system build` creates a complete, valid system generation with all files
- Manual copy ensures kernel/initrd are in `/boot` where GRUB expects them
- `system init` then succeeds because system is already built

**For manual installations**: Always use the 3-step approach above. Don't rely on `guix system init` alone to copy kernel files.
- Required for proper kernel initialization

### Universal Verification Practices (APPLIES TO ALL PLATFORMS)

**⚠️ CRITICAL:** Even though the 3-step workaround is only needed for framework-dual (nonguix + time-machine), **all platforms** can experience silent failures where `guix system init` appears to succeed but doesn't actually create kernel/initrd files. These verification practices should be applied to **all installers** (cloudzy, framework, framework-dual, raspberry-pi).

#### Key Diagnostic: Broken System Generation Symlink

**The most reliable indicator of a failed system init** is checking if `/mnt/run/current-system` is a valid symlink pointing to an existing directory:

```bash
# Check if symlink exists and is valid
if [ ! -L /mnt/run/current-system ]; then
    echo "ERROR: /mnt/run/current-system symlink missing - system generation not created"
    exit 1
fi

# Check if symlink target exists
LINK_TARGET=$(readlink /mnt/run/current-system)
if [ ! -e "$LINK_TARGET" ]; then
    echo "ERROR: /mnt/run/current-system points to non-existent path: $LINK_TARGET"
    echo "This indicates the system generation was not built correctly"
    exit 1
fi
```

**Why this matters:**
- If the symlink is broken or missing, the system generation wasn't built correctly
- This can happen even when `guix system init` exits with success code
- Broken symlink = system won't boot, even if GRUB was installed

#### Verification After guix system init

**Always verify immediately after `guix system init` completes:**

1. **Check system generation symlink:**
   ```bash
   ls -la /mnt/run/current-system
   readlink /mnt/run/current-system  # Should point to existing directory
   ```

2. **Check kernel and initrd files exist:**
   ```bash
   ls /mnt/boot/vmlinuz-*  # Should exist
   ls /mnt/boot/initrd-*   # Should exist
   ```

3. **If files are missing but system generation exists, manually copy them:**
   ```bash
   # Find the system generation
   SYSTEM_PATH=$(readlink -f /mnt/run/current-system)
   
   # Copy kernel and initrd if they exist in system generation
   if [ -f "$SYSTEM_PATH/kernel" ] && [ ! -f /mnt/boot/vmlinuz-* ]; then
       cp "$SYSTEM_PATH/kernel" /mnt/boot/vmlinuz
       echo "Manually copied kernel from system generation"
   fi
   
   if [ -f "$SYSTEM_PATH/initrd" ] && [ ! -f /mnt/boot/initrd-* ]; then
       cp "$SYSTEM_PATH/initrd" /mnt/boot/initrd
       echo "Manually copied initrd from system generation"
   fi
   ```

**This fallback approach works for all platforms:**
- **Cloudzy (free software)**: Can recover from silent failures
- **Framework/Framework-dual (nonguix)**: Additional safety beyond 3-step workaround
- **Raspberry Pi**: Prevents boot failures from incomplete installations

#### Comprehensive Verification at End of Installation

**Before allowing reboot, run comprehensive verification:**

1. **Ensure EFI partition is mounted:**
   ```bash
   if ! mountpoint -q /mnt/boot/efi; then
       mkdir -p /mnt/boot/efi
       mount LABEL=EFI /mnt/boot/efi
   fi
   ```

2. **Run full verification script:**
   ```bash
   /root/verify-guix-install.sh
   ```

3. **If verification fails, DO NOT REBOOT:**
   - Provide clear error messages
   - Suggest running recovery script: `/root/recovery-complete-install.sh`
   - Recovery script will:
     - Mount EFI if needed
     - Re-run `guix system init` if kernel/initrd missing
     - Set user password
     - Download customization tools
     - Run verification again

**Why comprehensive verification matters:**
- Catches issues that basic checks might miss (EFI mount, password file, etc.)
- Prevents rebooting into unbootable system
- Provides clear guidance on how to fix issues
- Works consistently across all platforms

#### Implementation in Installers

**All installers should:**

1. **After `guix system init` completes:**
   - Check `/mnt/run/current-system` symlink is valid
   - Verify kernel and initrd files exist
   - If missing but system generation exists, attempt manual copy
   - Retry up to 3 times if files still missing

2. **Before reboot:**
   - Ensure EFI partition is mounted
   - Run comprehensive verification script
   - Prevent reboot if verification fails
   - Provide clear instructions to run recovery script

3. **Recovery script should:**
   - Check for broken symlink
   - Attempt manual copy if files missing
   - Re-run `guix system init` if needed
   - Verify again before completing

**This ensures all platforms benefit from the lessons learned on framework-dual.**

### GRUB EFI Bootloader Configuration

**Critical**: Always specify `grub-efi-bootloader` explicitly in your `config.scm` file.

```scheme
(bootloader
  (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (timeout 5)))
```

**Critical Bootloader Configuration Notes**:

- **DO NOT include `keyboard-layout`** in bootloader-configuration - this causes boot issues
- **DO NOT include `keyboard-layout`** in the minimal config - add it later if needed
- **Keep bootloader config minimal** - only essential fields for reliable boot

**Critical Targets Syntax Convention**:

- **Correct**: `(targets '("/boot/efi"))` - Note the single quote `'` before the parentheses
- **Incorrect**: `(targets "/boot/efi")` - Missing the quote mark
- **Incorrect**: `(targets ("/boot/efi"))` - Missing the quote mark

The single quote `'` is required in Scheme to properly quote the list. This applies to both UEFI (`'("/boot/efi")`) and BIOS (`'("/dev/sda")`) configurations.

**Why Explicit GRUB EFI Specification Matters**:

- Ensures consistent bootloader installation across different systems
- Prevents confusion between GRUB legacy and GRUB EFI
- Makes the configuration explicit and reproducible
- Required for proper EFI boot setup

### GRUB EFI Bootloader Issues

**Common Problem**: `guix system init` fails because it can't find GRUB EFI files (`grubx64.efi`, `grub.cfg`) or kernel files (`vmlinuz*`, `initrd*`) that don't exist yet.

**Root Cause**: This is a chicken-and-egg problem - `guix system init` is supposed to *create* these files, but it's looking for them before they exist.

**Solution**: Ensure proper directory structure exists before running `guix system init`:

```bash
# Create EFI directory structure
mkdir -p /mnt/boot/efi/Guix

# Clean up any existing files from previous failed attempts
rm -f /mnt/boot/efi/Guix/grubx64.efi
rm -f /mnt/boot/efi/Guix/grub.cfg
```

**Why This Happens**:

- `guix system init` needs to create GRUB EFI files in `/boot/efi/Guix/`
- It also needs to create kernel files (`vmlinuz*`, `initrd*`) in `/boot/`
- If the directory structure doesn't exist, the installation fails
- Previous failed attempts may leave partial files that cause conflicts

## ⚙️ Mount Order & Verification

### Correct Mount Sequence

```bash
# 1. Mount root
mount /dev/nvme0n1p4 /mnt

# 2. Create EFI mount point
mkdir -p /mnt/boot/efi

# 3. Mount ESP
mount /dev/nvme0n1p1 /mnt/boot/efi

# 4. Enable swap (optional)
swapon /dev/nvme0n1p2
```

### Pre-Installation Verification

Always verify ESP is correctly mounted:

```bash
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: EFI not FAT32"; exit 1; }
mount | grep "/mnt/boot/efi"
```

The ESP **must** show `Type: vfat` via `df -T /mnt/boot/efi`.

## 🚀 Running the Installation

### Command Pattern

```bash
# 1. Start cow-store FIRST
herd start cow-store /mnt

# 2. Run system init with fallback and multiple mirrors
guix system init /mnt/etc/config.scm /mnt \
  --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Key Flags

- `--fallback`: Build locally if substitutes fail (essential for reliability)
- `-v6`: Verbose output for debugging (shows what's happening)
- Multiple `--substitute-urls`: Redundancy when mirrors are flaky

### Resume After Failure

- **Do NOT reboot** if installation fails — the store is in RAM and will be lost
- Re-run the exact same `guix system init` command to resume
- It will reuse cached downloads and continue from where it failed

## 🧱 EFI & Bootloader Verification

### Most Common Error

```
"/mnt/boot/efi doesn't look like an EFI partition"
```

**Fix checklist:**

1. Ensure FAT32 format: `mkfs.vfat -F32 /dev/nvme0n1p1`
2. Verify correct flags: `parted /dev/nvme0n1 set 1 esp on`
3. Ensure empty mount point before mounting
4. Remount freshly before `guix system init`

### Post-Installation Verification

After successful installation, `/mnt/boot` should contain:

```
/mnt/boot/vmlinuz-*                    # Linux kernel
/mnt/boot/initrd-*                     # Initial RAM disk
/mnt/boot/grub/grub.cfg                # GRUB config (main config file)
/mnt/boot/efi/EFI/guix/grubx64.efi     # GRUB EFI bootloader binary
```

**Warning signs:**

- Only `grubx64.efi` present (no `grub.cfg` or kernel) → Installation incomplete, re-run
- Empty `/boot/efi` before init → ✅ Normal
- Empty `/boot/efi` after init → ❌ Installation failed

## 🖥️ Framework 13 AMD GPU Boot Issues

### Critical Kernel Parameters

**Framework 13 with AMD GPU requires specific kernel parameters to prevent boot hangs:**

```scheme
(kernel-arguments '("quiet" "loglevel=3" "nomodeset" "acpi=off" "noapic" "nolapic"))
```

### What These Parameters Do

- **`nomodeset`**: Disables kernel mode setting (fixes AMD GPU display issues)
- **`acpi=off`**: Disables ACPI (prevents power management conflicts)
- **`noapic`**: Disables APIC (prevents interrupt controller issues)
- **`nolapic`**: Disables Local APIC (prevents local interrupt issues)

### Boot Hang Symptoms

- System hangs at "Loading kernel modules..."
- Repeating "time with localhost and MARK" messages every 20 minutes
- Never reaches login prompt
- Ctrl+C doesn't work

### Manual Recovery

If you encounter boot hangs:

1. **At GRUB menu, press 'e' to edit**
2. **Find the kernel line and add parameters:**

   ```
   linux /boot/vmlinuz-... quiet splash nomodeset acpi=off noapic nolapic 3
   ```

3. **Press Ctrl+X or F10 to boot**

### Framework 13 Specific Initrd Modules

```scheme
(initrd-modules
 (append '("amdgpu"      ; AMD GPU driver (critical for display)
           "xhci_pci"    ; USB 3.0 host controller
           "usbhid"      ; USB keyboard/mouse
           "i2c_piix4")  ; SMBus/I2C for sensors
         %base-initrd-modules))
```

**Note:** `nvme` is **not** included because it's built-in to kernel 6.6.16 (wingolog-era). Including it causes "kernel module not found" errors during system build.

## 🧰 Dual-Boot with Pop!_OS

### Bootloader Coexistence

- Pop!_OS uses `systemd-boot`, Guix uses `GRUB` — they coexist peacefully in `/boot/efi/EFI/`
- Directory structure:

  ```
  /boot/efi/EFI/
  ├── Boot/           # Fallback bootloader
  ├── systemd/        # Pop!_OS systemd-boot
  └── guix/           # Guix GRUB
  ```

### Boot Order Behavior

- Whichever OS installs last controls the default boot order
- Guix's GRUB can detect Pop!_OS (via os-prober)
- Pop!_OS systemd-boot cannot detect Guix

### Dual-Boot GRUB Configuration

**Critical**: The framework-dual installer must properly configure GRUB to detect Pop!_OS:

```scheme
(bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5)))
```

**Common Issue**: If Pop!_OS disappears from boot menu after Guix installation:

1. **Boot from Pop!_OS live ISO**
2. **Reinstall Pop!_OS GRUB:**

   ```bash
   sudo mount /dev/nvme0n1p3 /mnt  # Adjust partition number
   sudo mount /dev/nvme0n1p1 /mnt/boot/efi
   sudo grub-install --target=x86_64-efi --efi-directory=/mnt/boot/efi --bootloader-id=Pop_OS
   sudo grub-mkconfig -o /mnt/boot/grub/grub.cfg
   sudo umount /mnt/boot/efi
   sudo umount /mnt
   sudo reboot
   ```

3. **Or from Guix system:**

   ```bash
   sudo os-prober
   sudo grub-mkconfig -o /boot/grub/grub.cfg
   sudo reboot
   ```

### Accessing Pop!_OS from Guix

**Method 1:** Firmware boot menu

```bash
# During boot, press F12 → select "Pop!_OS" entry
```

**Method 2:** One-time boot from Guix

```bash
sudo efibootmgr -n <PopEntry>  # Boot Pop!_OS next time only
sudo reboot
```

### Making GRUB Menu Visible

Add to `bootloader-configuration` in `/etc/config.scm`:

```scheme
(bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 5))  ; Show menu for 5 seconds
```

## 🧩 Troubleshooting Checklist

### Installation Issues

| Symptom | Diagnosis | Solution |
| -------- | --------- | -------- |
| Empty `/boot/efi` before init | ✅ Normal pre-install state | Continue normally |
| Empty `/boot/efi` after init | ❌ Installation failed | Re-run `guix system init` |
| Missing `vmlinuz` or `initrd` | Kernel missing from config | Add `linux-libre` to packages or check kernel config |
| Slow/failing install | Network flakes or small tmpfs | Re-run with `--fallback` and extra mirrors |
| Install appears frozen | Slow download or compilation | Ctrl+Alt+F3 to check; if Caps Lock toggles, wait longer |

### Verification Commands

```bash
# Verify mounts
mount | grep /mnt
df -h /mnt

# Verify EFI partition
df -T /mnt/boot/efi
parted /dev/nvme0n1 print

# Verify labels
lsblk -o NAME,LABEL,FSTYPE,MOUNTPOINT

# Check GRUB installation
ls -la /mnt/boot/efi/EFI/guix/
ls -la /mnt/boot/
```

### PATH Management Issues

**Problem**: After `guix pull`, commands still use old system Guix instead of pulled Guix

**Symptoms:**
- `guix describe` shows generation 1 (system) instead of generation 2 (user)
- Nonguix packages not found even though channel was added
- `guix search` doesn't find packages from nonguix channel

**Root Cause**: PATH environment variable not updated to include user's Guix profile

**Solution:**

```bash
# Add to ~/.bashrc for persistence
export PATH="$HOME/.config/guix/current/bin:$PATH"

# Reload shell or source bashrc
source ~/.bashrc

# Verify which guix is active
which guix
# Should show: /home/username/.config/guix/current/bin/guix

guix describe
# Should show generation 2 with nonguix channel
```

**Why This Happens:**

- Guix has multiple generations and profiles
- System Guix (old) is at `/run/current-system/profile/bin/guix`
- User's pulled Guix is at `~/.config/guix/current/bin/guix`
- Without PATH update, shell uses system Guix by default
- Always verify which guix binary is running: `which guix`

### Swap File Creation Issues

**Problem**: Confusing error messages when swap file creation fails

**Symptoms:**
- "exit status 1" appears even when swap creation succeeded
- User can't tell if swap creation succeeded or failed
- Installation continues but swap may not be active

**Root Cause**: `fallocate` fails on some filesystems (normal), but error messages weren't clear

**How Installer Handles This:**

The installer now provides clear progress reporting:

1. **`[INFO]` prefix** for fallocate failure (normal on some filesystems)
2. **Clear progress messages**: "Setting permissions...", "Formatting...", "Activating..."
3. **`[OK]` confirmation** after each step
4. **Shows `swapon --show` output** to confirm swap active

**Manual Verification:**

```bash
# Check if swap is active
swapon --show

# Check swap usage
free -h

# If swap creation failed, can continue without it
# 4G RAM is sufficient for most VPS installations
```

**VPS-Specific Notes:**

- Swap creation may fail on VPS due to I/O errors on ISO device (`sr0`)
- This is non-critical - swap is optional for VPS with adequate RAM
- Installation can continue successfully without swap

### VPS Disk Space Issues During Validation

**Problem**: "No space left on device" error during config validation on VPS

**Symptoms:**
- Config validation fails with disk space error
- Installation appears to have enough space but validation fails
- More common on smaller VPS instances

**Root Cause**: Config validation may use temporary space on ISO's tmpfs, which can fill up

**Solutions:**

1. **Set TMPDIR to target disk** (recommended):
   ```bash
   export TMPDIR=/mnt/var/tmp
   # Then continue with installation
   ```

2. **Skip validation** (if daemon is slow):
   - Answer 'y' to continue when validation warns about daemon
   - Real validation happens during system build anyway

3. **Clear substitute cache** (if low on space):
   ```bash
   rm -rf /var/guix/substitute-cache/*
   ```

4. **Make validation optional**:
   - Validation is a safety check, not required
   - System build will validate config anyway

**Prevention:**

- Ensure VPS has at least 40GB disk space (60GB+ recommended)
- Use `cow-store` to redirect store writes to target disk
- Set `TMPDIR=/mnt/var/tmp` before running installer

## 🧾 Best Practices for Automation

### Safety Checks Before Installation

```bash
#!/run/current-system/profile/bin/bash

# 1. Verify correct mount points
mount | grep -q "/mnt " || { echo "ERROR: /mnt not mounted"; exit 1; }
mount | grep -q "/mnt/boot/efi" || { echo "ERROR: ESP not mounted"; exit 1; }

# 2. Verify FAT32 ESP
df -T /mnt/boot/efi | grep -q vfat || { echo "ERROR: ESP not FAT32"; exit 1; }

# 3. Verify partition labels
blkid /dev/nvme0n1p4 | grep -q GUIX_ROOT || echo "WARNING: Root not labeled GUIX_ROOT"

# 4. Check free space
available=$(df -BG /mnt | tail -1 | awk '{print $4}' | sed 's/G//')
[[ $available -lt 40 ]] && { echo "WARNING: Less than 40GB free"; }

# 5. Start cow-store
herd start cow-store /mnt || { echo "ERROR: cow-store failed"; exit 1; }

# 6. Run installation
guix system init /mnt/etc/config.scm /mnt \
  --fallback \
  --substitute-urls="https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"
```

### Configuration Strategy

- Keep initial `/etc/config.scm` minimal for first boot
- Add services, packages, and customization after successful boot via `guix system reconfigure`
- Test configuration syntax before init: `guix system build /mnt/etc/config.scm`

## 💡 Miscellaneous Tips

### General

- **Rebooting is the only way to switch OSes** — there's no live toggle
- **F12 opens firmware boot menu** — useful when GRUB menu is hidden
- **Uppercase labels avoid UEFI quirks** — some firmware is case-sensitive
- **Never bind-mount or reformat the store mid-install** — guaranteed breakage

### Storage Management

- Set `TMPDIR=/mnt/var/tmp` to use target disk space instead of ISO tmpfs
- Set `XDG_CACHE_HOME=/mnt/var/cache` to avoid filling ISO memory
- Clear substitute cache if low on space: `rm -rf /var/guix/substitute-cache/`

### Network Issues

For slow/unreliable connections, set Git environment variables:

```bash
export GIT_HTTP_MAX_REQUESTS=2
export GIT_HTTP_LOW_SPEED_LIMIT=1000
export GIT_HTTP_LOW_SPEED_TIME=60
```

### Retry Logic

Installation can fail due to temporary network issues. Always implement retry logic:

```bash
for attempt in {1..3}; do
  if guix system init /mnt/etc/config.scm /mnt --fallback; then
    break
  fi
  echo "Attempt $attempt failed, retrying..."
  sleep 10
done
```

## 📚 Reference Commands

### Partition Management

```bash
# Create GPT table
parted /dev/nvme0n1 mklabel gpt

# Create ESP
parted /dev/nvme0n1 mkpart ESP fat32 1MiB 513MiB
parted /dev/nvme0n1 set 1 esp on

# Create root partition
parted /dev/nvme0n1 mkpart GUIX_ROOT ext4 513MiB 100%

# Format partitions
mkfs.vfat -F32 -n EFI /dev/nvme0n1p1
mkfs.ext4 -L GUIX_ROOT /dev/nvme0n1p2
```

### Boot Management

```bash
# List EFI boot entries
efibootmgr -v

# Set next boot (one-time)
efibootmgr -n <num>

# Change boot order
efibootmgr -o <num>,<num>,<num>
```

### Debugging

```bash
# Check what's using space in tmpfs
du -sh /tmp/* /var/tmp/* 2>/dev/null | sort -h

# Monitor download progress
watch -n 1 'du -sh /var/guix/substitute-cache'

# Check GRUB installation
grub-install --version
ls -la /boot/efi/EFI/*/

# Check Guix daemon status
herd status guix-daemon
guix build --version  # Test daemon connectivity
```

## 🔧 Common Issues and Fixes

### Daemon Connection Issues

**Problem:** "cannot connect to daemon-socket" error during config validation

**Root Cause:** The `ValidateGuixConfig()` function was being called before `EnsureGuixDaemonRunning()`, causing validation to fail when the daemon wasn't ready.

**Fix Applied:** Moved daemon startup before config validation in `RunGuixSystemInit()`:

1. `EnsureGuixDaemonRunning()` starts and verifies the daemon
2. `ValidateGuixConfig()` can successfully connect to the daemon
3. Installation proceeds normally

**Manual Recovery:** If you encounter this issue:

```bash
# Start the daemon manually
herd start guix-daemon

# Wait for it to be ready (test connectivity)
guix build --version

# Then continue with system init
guix system init /mnt/etc/config.scm /mnt
```

**Daemon Restart Loop:** The installer includes a robust daemon restart mechanism:

- 5 retry attempts with proper daemon stopping/starting
- Uses `herd` service manager with fallback to direct `guix-daemon` startup
- Tests daemon responsiveness with `guix build --version`
- 8-second wait periods for daemon initialization

### D-Bus Activation Failure: /var/run/dbus Symlink Issue

**Problem:** `guix system reconfigure` fails with error: `file name component is not a directory "/var/run/dbus"` in `mkdir-p/perms`

**Symptoms:**
- System reconfigure fails during activation phase
- Error mentions D-Bus directory creation
- Partial reconfigure may leave system in broken state (PAM failures, sudo broken)
- `/var/run/dbus` exists as a symlink to `/run/dbus` owned by root

**Root Cause:**

The Guix D-Bus activation script (`gnu/services/dbus.scm`) expects to manage `/var/run/dbus` creation itself:

1. **Expected flow:**
   - Activation script creates `/run/dbus` directory (owned by messagebus user, mode 0755)
   - Activation script creates symlink `/var/run/dbus` → `/run/dbus`
   - Script handles existing directories by migrating content if needed

2. **What goes wrong:**
   - Something creates `/var/run/dbus` as a symlink owned by root BEFORE activation runs
   - When activation tries to use `mkdir-p/perms "/var/run/dbus" user #o755`, it fails
   - The function expects to create a directory or manage an existing directory, not encounter a pre-existing symlink

**Why It Matters:**

If `guix system reconfigure` fails during D-Bus activation:
- The activation script runs partially, leaving some services configured and others not
- PAM configuration may be incomplete, breaking `sudo` and `su`
- System authentication and session management becomes non-functional
- **CRITICAL:** You may lose access to privileged operations until you reboot to previous generation

**Prevention (before reconfiguring):**

```bash
# Check if the problematic symlink exists
ls -la /var/run/dbus

# If it's a symlink owned by root, remove it before reconfiguring
sudo rm /var/run/dbus

# Now safe to reconfigure
sudo guix system reconfigure /etc/config.scm
```

**Recovery (if reconfigure already failed):**

If you're stuck with broken PAM/sudo after a failed reconfigure:

1. **Reboot immediately** to restore previous working generation:
   ```bash
   reboot
   ```

2. **After reboot**, select the previous working generation in GRUB menu

3. **Once system is working again**, remove the problematic symlink:
   ```bash
   sudo rm /var/run/dbus
   ```

4. **Now retry the reconfigure:**
   ```bash
   sudo guix system reconfigure /etc/config.scm
   ```

**Investigation Needed:**

The root question remains: **What is creating `/var/run/dbus` as a root-owned symlink before the D-Bus activation script runs?**

Possible culprits:
- Another service activation script running before D-Bus
- GNOME desktop services (`%desktop-services`) setup
- System initialization creating legacy compatibility symlinks
- Previous failed activation leaving state behind

This needs further investigation to find the actual source and prevent it from creating the symlink prematurely.

**Related:** When using `guix time-machine` with channel pinning (e.g., Wingo-era channels for Framework 13 AMD), this issue may appear during the reconfigure process.

## 🆘 Recovery Tool

**New in 2024:** All installers now automatically build and install a comprehensive recovery tool at `/root/recovery-complete-install.sh` before running `guix system init`.

**Note:** The recovery tool is now a **Go binary** (not a bash script) that shares code with the main installer. This ensures the recovery tool and installer stay in sync and use the same logic for system initialization, kernel recovery, and verification.

### What the Recovery Script Does

The recovery script provides a safe, idempotent way to complete or retry installation after failures:

1. **Verifies current installation state** - Checks for kernel, initrd, GRUB files
2. **Detects platform** - Automatically uses time-machine for framework installers, plain init for cloudzy
3. **Re-runs system init if needed** - Only if kernel/initrd are missing
4. **Sets user password** - Prompts if not already set
5. **Downloads customization tools** - To `/home/username/guix-customize/`
6. **Configures dual-boot GRUB** - For framework-dual, detects Pop!_OS
7. **Writes installation receipt** - Documents installation details
8. **Final verification** - Prevents reboot if critical files are missing

### When to Use the Recovery Script

Use the recovery script if:

- `guix time-machine` or `guix system init` completed but you're not sure what happened next
- The installer script was interrupted (Ctrl+C, network failure, etc.)
- You manually ran `guix time-machine` from the console
- You see "Installation incomplete" warnings
- Kernel or initrd files are missing from `/mnt/boot/`

### How to Use the Recovery Tool

**From the Guix ISO (after a failed/incomplete installation):**

```bash
# The recovery tool is already built and installed at /root/recovery-complete-install.sh
# (built during installation, before system init runs)

# Run it (same as before - it's a binary, not a script)
/root/recovery-complete-install.sh
```

**Note:** The recovery tool is a Go binary that's built during installation. If you need to rebuild it manually:

```bash
# From the repository root (if you have the source):
go build -o /root/recovery-complete-install.sh ./cmd/recovery
chmod +x /root/recovery-complete-install.sh

# Or download the bash fallback script (if Go build fails):
cd /root
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/recovery-complete-install.sh
chmod +x recovery-complete-install.sh
./recovery-complete-install.sh
```

### Recovery Script Features

**Intelligent Detection:**
- Automatically detects if `/tmp/channels.scm` exists (time-machine needed)
- Reads username from existing `/mnt/etc/config.scm`
- Skips steps that are already complete (idempotent)

**Safety Checks:**
- Verifies mounts before proceeding
- Checks daemon responsiveness
- Verifies ESP is vfat
- Won't reboot if kernel/initrd are missing

**User-Friendly:**
- Clear progress messages with [OK]/[MISSING]/[ERROR] indicators
- Prompts before destructive actions
- Provides manual recovery commands if needed
- Writes installation receipt for reference

### Common Recovery Scenarios

**Scenario 1: time-machine completed but no kernel files**

```bash
# This is your situation - run the recovery script
/root/recovery-complete-install.sh

# It will detect missing kernel/initrd and re-run system init
# Then complete all post-install steps
```

**Scenario 2: Everything installed but password not set**

```bash
# Recovery script will detect this and only prompt for password
/root/recovery-complete-install.sh

# Or set password manually:
chroot /mnt /run/current-system/profile/bin/passwd username
```

**Scenario 3: Installation complete but customization tools missing**

```bash
# Recovery script detects installed system and skips to final steps
/root/recovery-complete-install.sh

# Or download manually after first boot
```

### What Makes It Safe

The recovery script is **completely idempotent** - you can run it multiple times safely:

- ✅ Won't repartition or reformat existing partitions
- ✅ Won't overwrite `/mnt/etc/config.scm` if it exists
- ✅ Skips system init if kernel/initrd already exist
- ✅ Asks before changing already-set passwords
- ✅ Skips downloads if files already present
- ✅ Won't unmount/reboot without confirmation

### For Developers

The recovery tool is built by `lib.WriteRecoveryScript()` which compiles `cmd/recovery/main.go` into a Go binary. The binary is installed to `/root/recovery-complete-install.sh` before `guix system init` runs, so it's always available even if the installer crashes.

**Architecture:**
- The recovery tool (`cmd/recovery/main.go`) reuses functions from `lib/common.go`
- This ensures recovery and installer share the same code and stay in sync
- If Go build fails, the installer falls back to copying the bash script (`lib/recovery-complete-install.sh`)

## 🔧 Comprehensive Filesystem Recovery Script

**For systems with persistent ISO artifact issues:** Use `lib/enforce-guix-filesystem-invariants.sh` for complete recovery.

### What This Script Does

This script performs the **complete recovery procedure** for systems that were installed with ISO artifacts:

1. **Fixes filesystem layout:**
   - Empties `/run` directory (removes ISO runtime junk)
   - Creates `/var/run` → `/run` symlink
   - Creates `/var/lock` → `/run/lock` symlink
   - Fixes `/var/tmp` permissions (sticky bit)

2. **Removes ISO artifacts:**
   - Removes `/etc/machine-id`, `/etc/resolv.conf`
   - Fixes `/etc/mtab` symlink
   - Removes ISO user profiles and home directories

3. **Rebuilds system profile (optional):**
   - Chroots into the system
   - Runs `guix system reconfigure` to regenerate activation scripts
   - Ensures activation scripts have correct filesystem assumptions

### When to Use This Script

Use this script if:

- ✅ Your system boots but services fail to start (PAM errors, dbus failures)
- ✅ `sudo` fails with PAM errors
- ✅ `/var/run` keeps getting recreated as a directory after reboot
- ✅ You've already tried `lib/fix-iso-artifacts.sh` but issues persist
- ✅ Your system was installed before the ISO cleanup fixes were implemented

**Do NOT use this script if:**

- ❌ Your system hasn't been installed yet (use the installer instead)
- ❌ You just need to fix symlinks (use `lib/fix-iso-artifacts.sh` instead)
- ❌ Your system is working fine (no need to fix what isn't broken)

### How to Use the Recovery Script

**From the Guix ISO:**

```bash
# Download the script
cd /root
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/enforce-guix-filesystem-invariants.sh
chmod +x enforce-guix-filesystem-invariants.sh

# Mount your root partition first
# if you know the partition exactly:
# mount /dev/nvme0n1p4 /mnt  # Adjust partition as needed
# or use the label which you should have (confirm with `lsblk -f`)
mount $(blkid -L GUIX_ROOT) /mnt  

# Run the script
./enforce-guix-filesystem-invariants.sh
```

**From your installed Guix system (if it boots but has issues):**

```bash
# Download the script
cd ~
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/enforce-guix-filesystem-invariants.sh
chmod +x enforce-guix-filesystem-invariants.sh

# Run with sudo (it will detect it's running on installed system)
sudo ./enforce-guix-filesystem-invariants.sh
```

**Script Options:**

- `--skip-rebuild` - Skip the chroot/system rebuild step (faster, but may not fix activation script issues)
- `--help` - Show usage information

### What Makes This Different from `fix-iso-artifacts.sh`

| Feature | `fix-iso-artifacts.sh` | `enforce-guix-filesystem-invariants.sh` |
| ------- | ---------------------- | --------------------------------------- |
| Fixes symlinks | ✅ Yes | ✅ Yes |
| Empties `/run` directory | ❌ No | ✅ Yes |
| Fixes `/var/lock` symlink | ❌ No | ✅ Yes |
| Fixes `/var/tmp` permissions | ❌ No | ✅ Yes |
| Rebuilds system profile | ❌ No | ✅ Yes (optional) |
| Use case | Quick fixes | Complete recovery |

**Recommendation:** Start with `fix-iso-artifacts.sh` for quick fixes. If issues persist, use `enforce-guix-filesystem-invariants.sh` for complete recovery.

## 🔄 Idempotency Techniques

The installer implements comprehensive idempotency to allow safe reruns and recovery from failures.

### Partition Step Idempotency

**Device Unmounting:**

- Automatically detects if target device is mounted
- Unmounts all mount points in reverse order (deepest first)
- Uses `findmnt` with fallback to `mount` command parsing
- Employs lazy unmount (`umount -l`) for stubborn mounts

**Partition Existence Check:**

- Detects if partitions already exist using `lsblk`
- Skips partitioning if partitions are already present and formatted
- Verifies partition labels before proceeding

**Format Check:**

- Uses `blkid` to check if partitions are already formatted
- Supports both ext4 (root) and vfat (EFI) filesystems
- Skips formatting if partitions are already properly formatted

**Space Validation:**

- Checks device size before partitioning
- Warns if device is smaller than 40 GiB
- Displays actual device size for user awareness

### Mount Step Idempotency

**Mount Status Check:**

- Verifies if `/mnt` is already mounted using `lib.IsMounted()`
- Skips mount operations if already mounted

**Store Population Check:**

- Checks if `/mnt/gnu/store` has contents (more than 10 entries)
- Skips store copy if already populated
- Prevents unnecessary rsync/cp operations

**Label Verification:**

- Verifies required labels (`GUIX_ROOT`, `EFI`) exist before mounting
- Uses `lib.VerifyLabelsExist()` for consistent checking

### Config Step Idempotency

**Config File Check:**

- Skips config generation if `/mnt/etc/config.scm` already exists
- Provides clear message about skipping and how to regenerate
- Allows safe reruns without overwriting existing config

### System Init Step Idempotency

**Daemon Management:**

- Ensures daemon is running before config validation
- Includes robust restart mechanism with multiple fallback methods
- Tests daemon responsiveness before proceeding

**Installation Verification:**

- Verifies critical files were installed correctly
- Provides clear error messages if verification fails

### Safe Rerun Strategy

**Variable Persistence:**

- All environment variables persist between steps
- Skipping steps doesn't break downstream operations
- State information flows through all installation phases

**Graceful Degradation:**

- Each step can be run independently
- Failed steps can be rerun without affecting completed work
- Clear messaging about what's being skipped and why

**Recovery Instructions:**

- Each step provides clear instructions for manual recovery
- Error messages include specific commands to run
- Troubleshooting guidance for common failure scenarios

## ⚠️ The `/var/lock` ENOENT Error During System Init

**The Mystery of the Missing Lock Directory**

Picture this: You've partitioned your disk, formatted the filesystems, mounted everything correctly, generated a perfect `config.scm`, and started `guix system init`. Everything runs for 10 minutes, building packages, downloading substitutes... then suddenly fails with a cryptic error about `/var/lock` not existing. But `/var/lock` clearly exists—you can see it right there! What's going on?

**Critical Discovery (2025-11-25):** During `guix system init`, the system can fail with this mysterious error:

```text
error: failed to evaluate directive (directory "/var/lock" 0 0 1023)
error: chmod: No such file or directory
```

### Why This Happens

The error occurs because `guix system init` evaluates a directive to set permissions on `/var/lock`:

```scheme
(directory "/var/lock" 0 0 #o1777)  ; Create /var/lock with sticky bit
```

This directive runs **inside the target root (chroot under `/mnt`)** and fails when:

1. **`/var/lock` is a symlink to `/run/lock`** (which is the correct final state)
2. **`/run` is not mounted yet** inside the chroot during init
3. **`chmod` follows the symlink** to the non-existent `/run/lock` target
4. **Result:** `ENOENT` error, installation fails

### The Two-Phase Approach

Our installer now uses a two-phase approach to handle this:

**Phase 1 - Before `guix system init`** (in `PrepareSystemInitDirectories()`):
- Creates `/mnt/var/lock` as a **real directory** with mode `01777`
- Creates `/mnt/run` as a **real directory** with mode `0755`
- These directories must exist as **directories, not symlinks** for `system init` to succeed

**Phase 2 - After `guix system init`** (in `CleanupISOArtifacts()`):
- Converts `/var/lock` from directory → **symlink to `/run/lock`**
- Ensures `/var/run` is a **symlink to `/run`**
- This creates the correct final filesystem structure

### Why Not Just Use Symlinks From the Start?

You might wonder: "Why not just make sure `/run/lock` exists before `system init`?"

The problem is that:
- `/run` is a tmpfs that's mounted **after** `system init` completes
- During `system init`, the chroot doesn't have a mounted `/run`
- Even if you create `/mnt/run/lock`, it's on disk, not the tmpfs
- After boot, `/run` becomes a tmpfs and your pre-created structure disappears

### Implementation in Our Installer

**In `lib/common.go`:**

1. **`PrepareSystemInitDirectories()`** (called before `system init`):
   ```go
   // Creates /mnt/run and /mnt/var/lock as REAL DIRECTORIES
   // This allows guix system init directives to succeed
   {"/mnt/run", 0755},
   {"/mnt/var/lock", 01777},  // sticky bit required
   ```

2. **`CleanupISOArtifacts()`** (called after copying store, before init):
   ```go
   // Converts /var/lock from directory → symlink to /run/lock
   // This creates the correct final filesystem structure
   os.Symlink("/run/lock", "/mnt/var/lock")
   ```

**Wait, there's a conflict!** If `PrepareSystemInitDirectories()` creates `/var/lock` as a directory, but `CleanupISOArtifacts()` runs **before** `PrepareSystemInitDirectories()` and creates it as a symlink, we're back to the original problem!

### The Correct Call Order (CRITICAL)

Looking at the code flow:

1. **Step 02 (Mount):** Calls `CleanupISOArtifacts()` which creates `/var/lock` as **symlink**
2. **Step 04 (System Init):** Calls `PrepareSystemInitDirectories()` which removes symlink and creates **directory**
3. **Step 04 (System Init):** Runs `guix system init` (succeeds because `/var/lock` is now a directory)
4. **After boot:** The system expects `/var/lock` → `/run/lock` symlink (per FHS)

**Note:** After `system init` completes, the system will have `/var/lock` as a directory. This works but is not ideal. A post-install step could convert it to a symlink if needed.

### Future Improvement

For a fully correct implementation, we should:

1. **Before `system init`:** Create `/var/lock` as directory (for init directives to work)
2. **After `system init`:** Convert `/var/lock` to symlink → `/run/lock` (for FHS compliance)

This could be added to the post-install steps or the recovery script.

### Related: VPS vs Physical Systems

This issue was discovered on **framework-dual** (physical hardware) but applies to:
- ✅ All platforms using `RunGuixSystemInit()` (framework, framework-dual)
- ✅ All platforms using `RunGuixSystemInitFreeSoftware()` (cloudzy, VPS)
- ✅ Any system where Guix evaluates `(directory "/var/lock" ...)` directives

**Platform-agnostic solution:** The fix is in `lib/common.go` and benefits all platforms automatically.

## 🔧 The 3-Step Kernel/Initrd Workaround

**Status:** ✅ **SOLVED (2025-01-XX)** - Framework-dual installation now works correctly with comprehensive fixes.

**The Mystery Continues:** After fixing the `/var/lock` issue, we discovered another problem on hardware platforms (Framework 13): `guix system init` would complete successfully, but the system wouldn't boot because `/boot/vmlinuz*` and `/boot/initrd*` files were missing!

**The Rediscovery (2025-01-XX):** This issue was solved before but had to be rediscovered. Kernel tracking logs (logs 7-16) revealed the root causes through systematic hypothesis-driven debugging. This section documents the complete solution to prevent future loss of knowledge.

### The Original Problem

Running `guix system init` with nonguix (for hardware drivers) would:
1. ✅ Build the complete system with kernel in `/gnu/store`
2. ✅ Install GRUB bootloader
3. ❌ **Not copy kernel/initrd to `/boot/`**

This appears to be a bug or limitation when using `guix time-machine` with nonguix channels. The system generation exists in the store with a working kernel, but `system init` doesn't copy the files where GRUB expects them.

### The Discovery Process

**What we tried (that didn't work):**
1. Running `guix system init` alone → kernel files not copied
2. Re-running `system init` multiple times → same result
3. Checking if it was a permissions issue → no, files just weren't copied

**The breakthrough:** Manually copying kernel/initrd from the built system generation to `/boot/` before running `system init` made the system bootable!

### The 3-Step Workaround

**Implementation in `RunGuixSystemInit()` (lib/common.go:1120):**

```go
// Step 1: Build the complete system (includes kernel)
guix time-machine -C ~/channels.scm -- system build /mnt/etc/config.scm

// Step 2: Manually copy kernel and initrd from the built system
systemPath := "/gnu/store/*-system"  // Find the latest built system
cp $systemPath/kernel /mnt/boot/vmlinuz
cp $systemPath/initrd /mnt/boot/initrd

// Step 3: Run system init (now just installs bootloader)
guix time-machine -C ~/channels.scm -- system init /mnt/etc/config.scm /mnt
```

### Why This Works

**The key insight:**
1. `guix system build` creates a complete system closure in `/gnu/store/` including:
   - Kernel at `$system/kernel`
   - Initrd at `$system/initrd`
   - All packages and configurations

2. By manually copying kernel/initrd to `/boot/` **before** running `system init`, GRUB finds the files it needs

3. `system init` then:
   - Sees the system is already built (uses cached build)
   - Installs the bootloader (GRUB)
   - Sets up symlinks to the system generation
   - Doesn't try to rebuild (because it's cached)

### The Kernel File Journey: A Step-by-Step Narrative

**Understanding Where Kernel Files Come From and Where They Should Go**

When debugging why kernel files aren't appearing in `/boot/`, it's critical to understand the complete journey these files take from source to destination. This narrative breaks down each step so we can identify exactly where the process breaks.

#### Step 1: Kernel Source - Where Do Kernel Files Originate?

**The Beginning:** Kernel files don't come from "the internet" directly. Instead:

1. **Guix builds the kernel** from source code (or downloads pre-built substitutes)
   - For free software installs (cloudzy): Uses `linux-libre` kernel package
   - For nonguix installs (framework-dual): Uses `linux` kernel package from nonguix channel
   - The kernel package is specified in `/mnt/etc/config.scm` as `(kernel linux-libre)` or `(kernel linux)`

2. **During `guix system build`**, Guix:
   - Downloads or builds the kernel package
   - Compiles it into a binary executable
   - Creates an initrd (initial RAM disk) with necessary modules
   - Packages everything into a **system generation** at `/gnu/store/*-system/`

3. **The system generation** (`/gnu/store/xxxxx-system/`) contains:
   - `kernel` - **CRITICAL: This is a SYMLINK, not a direct file!** Points to `/gnu/store/xxxxx-profile/` which contains the actual kernel binary
   - `initrd` - **CRITICAL: This is also a SYMLINK!** Points to `/gnu/store/xxxxx-raw-initrd/initrd.cpio.gz`
   - `boot` directory - Contains GRUB configuration (may not exist in free-software installs)
   - Other system files (binaries, libraries, etc.)

**Key Point:** The kernel files are **created locally** during the build process, not downloaded as separate files. They're part of the system generation closure.

**⚠️ CRITICAL DISCOVERY (2025-01-XX):** On Cloudzy (free-software-only installs), the `kernel` and `initrd` entries in the system generation are **symlinks**, not direct files. This was discovered through runtime investigation:

```bash
# System generation structure (from actual investigation):
/gnu/store/xifx2s3agj121725skhp5jg1yz5b8gmj-system/
├── kernel -> /gnu/store/7bb40hayqnba9p7kcg9vavwyjvjw8lcl-profile  (SYMLINK!)
├── initrd -> /gnu/store/hza6k7h6z72y70898vxkx0ap44c9q1s5-raw-initrd/initrd.cpio.gz  (SYMLINK!)
├── boot -> /gnu/store/sayynz11yqy5z5hy0rnp16j03vxj2v1n-boot  (SYMLINK)
└── ... (other symlinks)
```

**Why This Matters:**
- When copying kernel files, you must **dereference symlinks** using `cp -L` or `cp --dereference`
- Using `cp` without `-L` will copy the symlink itself, not the actual kernel binary
- The symlink target (`/gnu/store/7bb40hayqnba9p7kcg9vavwyjvjw8lcl-profile`) contains the actual kernel binary
- The initrd symlink points to a `.cpio.gz` file inside another store path

**Correct Copy Command:**
```bash
# WRONG - Copies symlink, not actual file:
cp /gnu/store/xxxxx-system/kernel /mnt/boot/vmlinuz

# CORRECT - Dereferences symlink to copy actual kernel:
cp -L /gnu/store/xxxxx-system/kernel /mnt/boot/vmlinuz
# OR
cp --dereference /gnu/store/xxxxx-system/kernel /mnt/boot/vmlinuz
```

**Verification:**
After copying, verify the file is actually a binary (not a symlink):
```bash
file /mnt/boot/vmlinuz
# Should show: "Linux kernel x86 boot executable bzImage, version ..."
# NOT: "symbolic link to ..."
```

#### Step 2: Finding the System Generation

**⚠️ CRITICAL BUG FIX (2025-01-XX):** The installer MUST use the system path from `guix system build` output, NOT search for the newest one!

**After `guix system build` completes:**

1. **CRITICAL: Extract system path from build output:**
   ```bash
   # guix system build outputs: /gnu/store/xxxxx-system
   systemPath=$(guix time-machine -C channels.scm -- system build /mnt/etc/config.scm)
   ```
   - The build command outputs the exact system generation path
   - **DO NOT** use `ls -td /gnu/store/*-system | head -1` - this finds the WRONG (old cached) system!
   - Using the wrong system generation leads to incomplete system (only `["gnu","gnu.go","guix"]` entries)

2. **Why this matters:**
   - Build output: `/gnu/store/v4fisq22npmk12aqjbsw4la4679ld9f2-system` (NEW, complete)
   - Searching finds: `/gnu/store/0wqwbqgvw60bvc1jhry5x6axbspkz3f1-guix-system` (OLD, incomplete)
   - Result: Using wrong system generation that lacks kernel/initrd symlinks
   - **Root cause:** Cached incomplete system generations from previous failed builds

3. **Verification:** The installer lists contents of the system directory:
   ```bash
   ls -lh /gnu/store/xxxxx-system/
   ```
   - Should show: `kernel`, `initrd`, `boot/`, `activate`, `etc`, `locale`, `parameters`, `profile` (8+ entries)
   - If only `["gnu","gnu.go","guix"]` (3 entries), you're using the WRONG system generation!

**Implementation (lib/common.go:1407-1490):**
- Extract `systemPathFromOutput` from build output
- Compare with existing system paths before build to detect cached ones
- Use `systemPathFromOutput` if available, only fall back to searching if build output didn't contain path
- Log warning if paths don't match

**Potential Failure Point:** If build output doesn't contain system path, or if wrong system generation is used, kernel/initrd will be missing.

#### Step 3: Copying Kernel Files to /boot

**The Manual Copy Step (for cloudzy with 3-step workaround):**

1. **Source location:** `/gnu/store/xxxxx-system/kernel` and `/gnu/store/xxxxx-system/initrd`
   - These are the files created during Step 1 (build)

2. **Destination location:** `/mnt/boot/vmlinuz` and `/mnt/boot/initrd`
   - `/mnt` is the mount point for the target disk's root partition
   - `/mnt/boot/` is where GRUB expects to find kernel files
   - Files are named `vmlinuz` (kernel) and `initrd` (initrd) for GRUB compatibility

3. **The copy command (CRITICAL: Must handle symlinks correctly):**

   **⚠️ CRITICAL BUG FIX (2025-01-XX):** Kernel symlink points to a PROFILE DIRECTORY, not a file!

   ```bash
   # Kernel symlink structure:
   /gnu/store/xxxxx-system/kernel -> /gnu/store/...-profile/  (DIRECTORY!)
   # Actual kernel binary is inside: /gnu/store/...-profile/bzImage
   
   # Initrd symlink structure:
   /gnu/store/xxxxx-system/initrd -> /gnu/store/...-combined-initrd/initrd.img  (FILE)
   ```

   **Implementation (lib/common.go:2004-2084):**
   - Detect if kernel is symlink pointing to directory
   - Resolve symlink and search for kernel binary inside profile:
     - `bzImage` (most common)
     - `vmlinuz`
     - `Image`
     - `boot/bzImage`
     - `boot/vmlinuz`
   - Use actual kernel binary path for copy instead of symlink
   - For initrd: Use `-L` flag to dereference symlink (points to file)

   **Why this matters:**
   - `cp -L` on kernel symlink tries to copy a directory → fails
   - Need to find actual `bzImage` file inside the profile directory
   - Initrd symlink points directly to file, so `cp -L` works fine

4. **Verification after copy:**
   - Check that `/mnt/boot/vmlinuz` exists and has reasonable size (> 5 MB)
   - Check that `/mnt/boot/initrd` exists and has reasonable size (> 10 MB)
   - Verify file permissions are correct

**Potential Failure Points:**
- Copy command fails silently (permissions, disk space, path issues)
- Files copied but immediately removed/overwritten
- Files copied to wrong location
- Copy succeeds but verification fails

#### Step 4: Running guix system init

**After kernel files are copied:**

1. **`guix system init` runs** to install the bootloader:
   ```bash
   guix system init /mnt/etc/config.scm /mnt
   ```
   - This should be quick since the system is already built
   - Main job: Install GRUB bootloader and create `/mnt/boot/grub/grub.cfg`

2. **What `guix system init` does:**
   - Creates `/mnt/run/current-system` symlink → `/gnu/store/xxxxx-system`
   - Installs GRUB to `/mnt/boot/efi/EFI/guix/grubx64.efi` (UEFI) or disk MBR (BIOS)
   - Generates `/mnt/boot/grub/grub.cfg` with kernel/initrd paths
   - **May remove or overwrite** existing kernel files in `/mnt/boot/`

**Critical Issue:** `guix system init` may remove the kernel files we just copied! This is why we need to verify again after Step 4.

**⚠️ IMPORTANT:** After `guix system init` completes successfully, you MUST verify files still exist before rebooting. The installer now does this automatically, but if running manually, always check:

```bash
# Verify kernel and initrd exist
ls -lh /mnt/boot/vmlinuz* /mnt/boot/initrd*

# If missing, copy from system generation
SYSTEM_PATH=$(readlink -f /mnt/run/current-system)
cp -L "$SYSTEM_PATH/kernel" /mnt/boot/vmlinuz
cp -L "$SYSTEM_PATH/initrd" /mnt/boot/initrd
```

#### Step 5: Post-Init Verification and Recovery

**Before Rebooting:**

1. **Sync filesystems:**
   ```bash
   sync
   ```
   Ensures all writes are flushed to disk.

2. **Unmount all filesystems:**
   ```bash
   umount -R /mnt
   ```
   The `-R` flag recursively unmounts all mounts under `/mnt` (including `/mnt/boot/efi`, `/mnt/proc`, etc.)

3. **Reboot:**
   ```bash
   reboot
   ```
   Remove the ISO/USB and boot from the installed system.

**⚠️ CRITICAL:** Always sync and unmount before rebooting! Failure to unmount can cause filesystem corruption.

#### Step 6: Post-Init Verification and Recovery

**After `guix system init` completes:**

**Proactive Approach (Current Implementation):**

Instead of waiting for verification to discover missing files, we proactively fix issues immediately:

1. **Ensure symlink exists:**
   - Check if `/mnt/run/current-system` exists
   - If missing, find latest system generation and create symlink immediately
   - This ensures we have a valid path to the system generation

2. **Proactively copy kernel/initrd if missing:**
   - Right after ensuring symlink exists, check if kernel/initrd exist in `/mnt/boot/`
   - If missing, copy them immediately from system generation (which we know exists)
   - This avoids waiting for verification to discover they're missing

3. **Verification step (fallback):**
   - Check if files still exist (should already be present from proactive copy)
   - Validate file sizes are reasonable
   - Check GRUB config references correct paths
   - List all files in `/mnt/boot/` to see what's actually there

**Recovery Approach (Fallback):**

If proactive fixes didn't work:

1. **Check if files still exist:**
   ```bash
   ls /mnt/boot/vmlinuz*  # Should show vmlinuz
   ls /mnt/boot/initrd*   # Should show initrd
   ```

2. **If files are missing:**
   - Find the system generation: `readlink /mnt/run/current-system`
   - Copy again from system generation: `cp $SYSTEM_PATH/kernel /mnt/boot/vmlinuz`
   - Retry up to 3 times with delays

3. **Final verification:**
   - Check file sizes are reasonable
   - Check GRUB config references correct paths
   - List all files in `/mnt/boot/` to see what's actually there

#### Debugging Checklist: Where Are Kernel Files At Each Step?

When debugging kernel file issues, check each step:

1. **After `guix system build`:**
   - ✅ Does `/gnu/store/*-system/` directory exist?
   - ✅ Does it contain `kernel` file? (check size: should be 5-15 MB)
   - ✅ Does it contain `initrd` file? (check size: should be 10-40 MB)
   - ❌ If NO: Build failed - check build logs

2. **After finding system generation:**
   - ✅ What path was found? (log it)
   - ✅ What files are in that directory? (list them)
   - ❌ If system generation not found: Search failed - check `/gnu/store/` contents

3. **Before copying:**
   - ✅ Does source file exist? (`/gnu/store/xxxxx-system/kernel`)
   - ✅ **Is it a symlink or direct file?** (`ls -l /gnu/store/xxxxx-system/kernel`)
   - ✅ **If symlink, what does it point to?** (`readlink /gnu/store/xxxxx-system/kernel`)
   - ✅ **Does the symlink target exist?** (`ls -l $(readlink -f /gnu/store/xxxxx-system/kernel)`)
   - ✅ What is the actual file size? (symlink will be tiny, actual kernel should be 5-15 MB)
   - ✅ Does destination directory exist? (`/mnt/boot/`)
   - ✅ Is there disk space?

4. **After copying:**
   - ✅ Does `/mnt/boot/vmlinuz` exist?
   - ✅ Does `/mnt/boot/initrd` exist?
   - ✅ **Is it a real file or symlink?** (`file /mnt/boot/vmlinuz` should show "Linux kernel", not "symbolic link")
   - ✅ What are their sizes? (should be 5-15 MB for kernel, 10-40 MB for initrd - NOT a few bytes!)
   - ✅ Verify file type: `file /mnt/boot/vmlinuz` should show "Linux kernel x86 boot executable"
   - ❌ If NO: Copy failed - check permissions, disk space, errors
   - ❌ If file is tiny (< 1 KB): You copied the symlink instead of the actual file - use `cp -L`

5. **After `guix system init`:**
   - ✅ Do files still exist?
   - ✅ What files are in `/mnt/boot/`? (list all)
   - ✅ Is `/mnt/run/current-system` a valid symlink?
   - ❌ If files missing: `guix system init` removed them - need recovery

6. **After recovery:**
   - ✅ Do files exist now?
   - ✅ What are their final sizes?
   - ✅ Are they referenced in GRUB config?

**This step-by-step breakdown allows us to pinpoint exactly where kernel files are getting lost.**

### Debugging Instrumentation

**Current Status (2025-01-XX):** Comprehensive instrumentation has been added to trace the kernel file journey:

**Instrumentation Points:**

1. **Before `guix system init`**: Logs that init is about to start
2. **After `guix system init` succeeds**: Logs kernel/initrd existence, sizes, boot files present, symlink status
3. **Symlink creation**: Logs when symlink is created and what it points to
4. **Proactive copy**: Logs when kernel/initrd are copied proactively
5. **Recovery attempts**: Logs each recovery attempt, what was found, what was copied

**Log Location:**

- Kernel tracking logs written to `/tmp/kernel_tracking.log` on the install machine
- NDJSON format (one JSON object per line)
- Includes hypothesis IDs, step names, file paths, sizes, errors
- Primarily tracks kernel/initrd operations, also includes related system init operations

**Using Kernel Tracking Logs:**

After running the installer, retrieve `/tmp/kernel_tracking.log` and analyze:
- Which steps completed successfully
- Where kernel files were found (or not found)
- What recovery attempts were made
- Final state of kernel/initrd files

> **See also:** [KERNEL_TRACKING.md](./KERNEL_TRACKING.md) for comprehensive documentation on the kernel tracking system, including detailed log analysis commands and platform-specific implementation details.

**Keeping Instrumentation Active:**

- Instrumentation logs remain active during fixes
- Only remove after successful post-fix verification and explicit user confirmation
- This ensures we can trace issues across multiple runs

### Proactive Fixes: Creating Symlinks and Copying Files Immediately

**Key Insight (2025-01-XX):** Instead of iterating through multiple recovery attempts, we now proactively fix issues immediately after `guix system init` completes.

**Proactive Symlink Creation:**

After `guix system init` completes, we immediately check if `/mnt/run/current-system` symlink exists. If missing:
1. Find the latest system generation in `/gnu/store` using `ls -td /gnu/store/*-system 2>/dev/null | head -1`
2. Ensure `/mnt/run` directory exists
3. Create the symlink: `os.Symlink(systemGenPath, "/mnt/run/current-system")`

This ensures the symlink exists before any recovery logic needs it.

**Proactive Kernel/Initrd Copying:**

Right after ensuring the symlink exists (or verifying it exists), we immediately:
1. Get the system generation path from the symlink (or find it if symlink was just created)
2. Check if kernel/initrd exist in `/mnt/boot/`
3. If missing, copy them immediately from the system generation:
   ```go
   cp $systemPath/kernel /mnt/boot/vmlinuz
   cp $systemPath/initrd /mnt/boot/initrd
   ```

**Why This Approach is Better:**

- **Fewer iterations**: Files are copied proactively instead of waiting for verification to discover they're missing
- **More efficient**: Uses the system generation path we already have
- **Consistent pattern**: Follows the same proactive approach as creating the symlink
- **Avoids recovery retries**: By the time verification runs, files should already be present

**Implementation:**

```go
// In RunGuixSystemInitFreeSoftware() after guix system init succeeds:

// 1. Ensure symlink exists (create if missing)
currentSystemLink := "/mnt/run/current-system"
if _, err := os.Lstat(currentSystemLink); err != nil {
    // Find latest system generation and create symlink
    systemGenPath := findLatestSystemGeneration()
    os.Symlink(systemGenPath, currentSystemLink)
}

// 2. Proactively copy kernel/initrd if missing
systemPath := resolveSystemPath(currentSystemLink)
kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
initrds, _ := filepath.Glob("/mnt/boot/initrd*")

if len(kernels) == 0 {
    cp systemPath/kernel /mnt/boot/vmlinuz
}
if len(initrds) == 0 {
    cp systemPath/initrd /mnt/boot/initrd
}

// 3. Verification step (should find files already present)
VerifyAndRecoverKernelFiles(3)  // Fallback if proactive copy failed
```

### Automatic Recovery with Retry (Fallback)

**Fallback mechanism:** `VerifyAndRecoverKernelFiles(3)` still runs after proactive fixes as a safety net:

```go
// Automatically detects and fixes missing/corrupt kernel files
if err := lib.VerifyAndRecoverKernelFiles(3); err != nil {
    // Shows clear error message with recovery instructions
}
```

**What it does:**
1. Checks if `/mnt/boot/vmlinuz*` and `/mnt/boot/initrd*` exist
2. Validates file sizes (kernel > 5MB, initrd > 10MB)
3. If missing or too small:
   - Finds the system generation symlink
   - Copies kernel/initrd from the generation
   - Retries up to 3 times with 10-second delays
4. Reports success or provides recovery guidance

**Note:** With proactive fixes in place, this recovery function should rarely need to retry - files should already be present from the proactive copy step.

### Platform Differences

**Hardware platforms (Framework 13, Framework-dual):**
- Use `RunGuixSystemInit()` with 3-step workaround
- Needs nonguix for hardware drivers
- Kernel/initrd copy is essential
- **CRITICAL DISCOVERY (2025-01-XX):** System generation may only contain `["gnu","gnu.go","guix"]` - same kernel bug as cloudzy
- Fallback strategies (K, N, H) implemented to handle missing kernel in system generation

**VPS platforms (Cloudzy):**
- Use `RunGuixSystemInitFreeSoftware()` with 3-step workaround (re-introduced after discovery)
- Free software drivers only (`linux-libre` kernel)
- **CRITICAL:** Kernel/initrd are symlinks in system generation - must use `cp -L` to dereference
- Kernel copy IS needed - `guix system init` doesn't copy kernel files to `/boot/` even in free-software mode

**Why both need the workaround?**
- Both Framework (time-machine + nonguix) and Cloudzy (free software only) experience the same bug
- `guix system init` fails to copy kernel/initrd files to `/boot/` regardless of installation method
- The only difference: Cloudzy's kernel files are symlinks requiring `cp -L` flag, while Framework's may be direct files
- Both platforms require manual kernel/initrd copying before bootloader installation

**Cloudzy-Specific Issue:**
On Cloudzy, the system generation's `kernel` entry is a symlink to `/gnu/store/xxxxx-profile/`, not a direct kernel binary. When copying, you must use `cp -L` to dereference the symlink and copy the actual kernel file. Without `-L`, you'll copy a tiny symlink file instead of the 5-15 MB kernel binary, causing boot failures.

### Success Criteria

After this workaround, you should see:
```bash
ls -lh /mnt/boot/
# Should show:
# vmlinuz   (5-15 MB)
# initrd    (10-40 MB)
# grub/     (directory)
```

### Related Code

- **Implementation:** [lib/common.go:1120-1268](lib/common.go#L1120) - `RunGuixSystemInit()`
- **Verification:** [lib/common.go:1470-1618](lib/common.go#L1470) - `VerifyAndRecoverKernelFiles()`
- **Called by:** framework/install/04-system-init.go, framework-dual/install/04-system-init.go

### Future Investigation

Questions remaining:
- Is this a bug in `guix system init` when using time-machine?
- Does it affect other hardware platforms (Raspberry Pi, Pinebook)?
- Could we upstream a fix to Guix?

For now, the workaround is reliable and automatically recovers from failures.

## 💻 Code Structure and Development

### Platform-Specific vs Shared Code

- **Platform-specific code**: Goes in `{platform}/install/*.go` files
  - Example: `framework-dual/install/01-partition.go`
  - Contains logic unique to that platform (partition layout, hardware detection, etc.)

- **Shared code for all platforms**: Goes in `lib/common.go`
  - Example: `DownloadCustomizationTools()`, `CreateSwapFile()`, `RunGuixSystemInit()`
  - Used by multiple platforms
  - Should accept parameters for platform-specific values (usernames, paths, etc.)

### When to Factor Code into common.go

Factor code into `lib/common.go` when:

1. The code is identical across 2+ platforms
2. The code performs a common operation (mounting, downloading, verification)
3. The code can be parameterized for platform differences

Keep code platform-specific when:

1. It's only used by one platform
2. The logic is fundamentally different per platform
3. Factoring would make the code harder to understand

### Adding New Scripts to the Repository

When you create a new shell script, determine if it's a **critical script** that should be:
1. Tracked in the manifest (`SOURCE_MANIFEST.txt`)
2. Copied to `/root/` by the bootstrap installer
3. Verified during installation

**Critical scripts are:**
- Scripts that users run directly from the Guix ISO
- Recovery/diagnostic tools needed when installation fails
- Scripts that modify system state or prepare for installation

**Examples of critical scripts:**
- `lib/bootstrap-installer.sh` - Entry point for installation
- `lib/clean-install.sh` - Prepares system for clean reinstall
- `lib/verify-guix-install.sh` - Diagnostic tool for checking installation
- `lib/recovery-complete-install.sh` - Recovery tool for completing partial installations

**Steps to add a new critical script:**

1. **Create the script** in the repository root (or appropriate subdirectory)
   ```bash
   vim my-new-script.sh
   chmod +x my-new-script.sh
   ```

   **CRITICAL**: Use the correct bash shebang for Guix ISO:
   ```bash
   #!/run/current-system/profile/bin/bash
   ```

   **Never use** `#!/usr/bin/env bash` or `#!/bin/bash` for critical scripts that run on the Guix ISO. The Guix ISO has bash at `/run/current-system/profile/bin/bash` and this path must be used for scripts to execute correctly.

2. **Add to `update-manifest.sh`** in the "Critical Shell Scripts" section:
   ```bash
   # My new script (description)
   hash=$(shasum -a 256 my-new-script.sh | awk '{print $1}')
   echo "$hash  my-new-script.sh" >> "$MANIFEST_FILE"
   ```

3. **Add to `lib/bootstrap-installer.sh`** CRITICAL_SCRIPTS array:
   ```bash
   CRITICAL_SCRIPTS=(
       "lib/clean-install.sh"
       "lib/verify-guix-install.sh"
       "lib/recovery-complete-install.sh"
       "lib/my-new-script.sh"  # <-- Add your script here
   )
   ```

4. **Update the manifest**:
   ```bash
   ./update-manifest.sh
   ```

5. **Test the changes**:
   - Verify the script is in SOURCE_MANIFEST.txt
   - Check that bootstrap-installer.sh copies it to /root/
   - Confirm the manifest hash changed

6. **Commit everything together**:
   ```bash
   git add my-new-script.sh update-manifest.sh lib/bootstrap-installer.sh SOURCE_MANIFEST.txt
   git commit -m "Add my-new-script.sh as critical script"
   ```

**Non-critical scripts** (like `update-manifest.sh` or `run-tests.sh`) are development tools that don't need to be in the manifest or copied to `/root/`. They stay in the repository for developers but aren't distributed to end users during installation.

---

## Filesystem Invariants After rsync/cp Operations

**Context**: Our installer uses `rsync` (or `cp -a`) to copy `/gnu/store` and `/var/guix` from the ISO to the target disk (Step 02). This is necessary because the ISO's RAM filesystem is too small for installation. However, certain filesystem structures from the ISO must be corrected before booting the installed system.

### Critical Fixes Required

These are Guix-specific filesystem invariants that must hold for the system to boot correctly:

#### 1. `/var/run` → `/run` symlink (CRITICAL)

**Must be:**
- `/run` = directory (tmpfs at boot)
- `/var/run` → `/run` (symlink)

**Problem if wrong:** If `/var/run` is a directory (copied from ISO), services fail to start:
- guix-daemon won't start
- NetworkManager fails
- SSH server won't bind
- Most systemd-style services break

**Fix:**
```bash
rm -rf /mnt/var/run
ln -sf /run /mnt/var/run
```

**Status in our installer:** ✅ Already handled in Step 02 (mount step creates correct structure)

#### 2. `/var/guix` structure (IMPORTANT)

**ISO contains temporary runtime data** that doesn't belong on installed systems:
- `/var/guix/profiles/per-user/live-image-user` (ISO user, not real user)
- Temporary daemon state
- Wrong ownership

**Correct structure:**
```
/var/guix/
    profiles/     (must exist, created by installer)
    db/           (must exist, created by installer)
    daemon-socket/ (created at boot)
    gcroots/      (must exist, created by installer)
    userpool/     (created by installer)
```

**Our installer handles this:**
```go
// In Step 02, after copying /var/guix:
criticalDirs := []string{
    "/mnt/var/guix/profiles",
    "/mnt/var/guix/gcroots",
    "/mnt/var/guix/userpool",
}
// Ensures these exist with correct permissions
```

**Potential cleanup needed:**
```bash
# Remove ISO user profiles if they exist
rm -rf /mnt/var/guix/profiles/per-user/live-image-user

# Ensure correct ownership
chown -R root:root /mnt/var/guix
```

#### 3. `/etc/mtab` (IMPORTANT for service startup)

**Must be:**
```bash
/etc/mtab -> /proc/self/mounts  (symlink)
```

**Problem if wrong:** If it's a static file copied from ISO:
- udev may fail
- NetworkManager confused about mount state
- Filesystem services misbehave

**Fix:**
```bash
rm -f /mnt/etc/mtab
ln -sf /proc/self/mounts /mnt/etc/mtab
```

**Status:** ⚠️ **TODO** - Should add this check to our installer

### Likely Issues (Non-Critical but Recommended)

#### 4. `/etc/resolv.conf` from ISO

ISO's DNS config doesn't belong on installed system. Let NetworkManager recreate it:
```bash
rm -f /mnt/etc/resolv.conf
```

#### 5. `/etc/machine-id` from ISO

Can cause D-Bus and elogind issues:
```bash
rm -f /mnt/etc/machine-id
# System will regenerate on first boot
```

#### 6. ISO user home directory

```bash
rm -rf /mnt/home/live-image-user
```

### What `guix system init` Handles

The official `guix system init` command (which we use in Step 04) automatically:
- Creates `/etc` structure from config.scm
- Sets up bootloader
- Creates user accounts
- Initializes `/var/guix/db` (Guix package database)
- Creates system profile links

However, it does **NOT** clean up ISO-specific runtime files that were copied by rsync.

### Recommended Pre-Init Checklist

Before running `guix system init`, verify:

```bash
# Check symlinks are correct
ls -ld /mnt/var/run   # Should be symlink to /run
ls -ld /mnt/etc/mtab  # Should be symlink to /proc/self/mounts

# Check /var/guix structure
ls /mnt/var/guix/     # Should have: profiles, gcroots, userpool, db
ls /mnt/var/guix/profiles/per-user/  # Should NOT have 'live-image-user'

# Check home directories
ls /mnt/home/         # Should NOT have ISO user

# Check machine-id
ls -l /mnt/etc/machine-id  # Should not exist (will be regenerated)
```

### Root Cause: Why ISO Artifacts Cause Persistent Issues

**The Problem:**

When using `rsync` or `cp -a` to copy `/var/guix` from the ISO to the target disk, the ISO's **entire runtime filesystem structure** gets copied, including:

- `/run` directory with stale sockets, PID files, dbus stuff, etc. (should be empty on disk, tmpfs at boot)
- `/var/run` as a **real directory** (should be a symlink → `/run`)
- `/var/lock` as a directory (should be a symlink → `/run/lock`)
- Other ISO-specific runtime artifacts

**Why Simple Fixes Don't Stick:**

Even if you fix `/var/run` → `/run` symlink manually, the problem persists because:

1. `/mnt/run` still contains **a ton of junk** from the ISO (sockets, PID files, dbus runtime data)
2. Activation scripts from the rsynced system profile assume `/var/run` is a directory and keep recreating it at boot
3. The system profile was built with incorrect assumptions about the filesystem layout

**The Complete Solution Requires Two Parts:**

1. **Clean up the filesystem layout** on disk so it matches Guix's expectations
2. **Rebuild the system profile from scratch** inside a chroot so Guix regenerates activation scripts with the *correct* assumptions

### Comprehensive Recovery Plan

If your system was installed but won't boot properly (PAM errors, dbus failures, services won't start), follow this complete recovery procedure:

#### Phase A – Boot and Mount the Real System

1. **Boot from the Guix installer ISO** and get to root shell

2. **Identify the root partition:**
   ```bash
   lsblk -f
   ```
   Confirm your Guix root is `/dev/nvme0n1p4` or similar (adjust as needed)

3. **Mount the root filesystem:**
   ```bash
   mount /dev/nvme0n1p4 /mnt
   ```
   (If using LUKS, unlock it first: `cryptsetup open /dev/nvme0n1p4 root`, then mount `/dev/mapper/root`)

4. **Mount EFI partition (if needed for later):**
   ```bash
   mount /dev/nvme0n1p1 /mnt/boot/efi
   ```

5. **Verify it's the real install:**
   ```bash
   ls /mnt/gnu/store  # Should be huge (~25GB+)
   ls /mnt/var/guix/profiles/system  # Should exist
   ```

#### Phase B – Fix the Filesystem Layout

1. **Backup current `/var/run` directory:**
   ```bash
   mv /mnt/var/run /mnt/var/run.before-rebuild 2>/dev/null || true
   mv /mnt/var/run.old /mnt/var/run.old.before-rebuild 2>/dev/null || true
   ```

2. **Clear `/mnt/run` completely and recreate it empty:**
   ```bash
   rm -rf /mnt/run
   mkdir /mnt/run
   chmod 755 /mnt/run
   ```
   Verify it's empty:
   ```bash
   ls -A /mnt/run  # Should show nothing
   ```

3. **Create the correct `/var/run → /run` symlink:**
   ```bash
   ln -s /run /mnt/var/run
   ls -ld /mnt/var/run  # Should show: ... /mnt/var/run -> /run
   ```

4. **Fix `/var/lock` to be a symlink to `/run/lock`:**
   ```bash
   rm -rf /mnt/var/lock
   ln -s /run/lock /mnt/var/lock
   ls -ld /mnt/var/lock  # Should show: ... /mnt/var/lock -> /run/lock
   ```

5. **Make sure `/var/tmp` has correct sticky perms:**
    ```bash
    chmod 1777 /mnt/var/tmp
    ```

#### Phase C – Remove ISO-Specific Artifacts

1. **Fix `/etc/mtab`:**
    ```bash
    rm -f /mnt/etc/mtab
    ln -s /proc/self/mounts /mnt/etc/mtab
    ```

2. **Remove `machine-id` so it'll be regenerated:**
    ```bash
    rm -f /mnt/etc/machine-id
    ```

3. **Remove stale `resolv.conf`:**
    ```bash
    rm -f /mnt/etc/resolv.conf
    ```

4. **Clean ISO user artifacts:**
    ```bash
    rm -rf /mnt/var/guix/profiles/per-user/live-image-user
    rm -rf /mnt/home/live-image-user
    ```

5. **Ensure correct ownership:**
    ```bash
    chown -R root:root /mnt/var/guix
    ```

At this point, your **on-disk layout** is clean and consistent.

#### Phase D – Chroot and Rebuild System Profile

**Critical:** This step regenerates the system profile and activation scripts with the correct filesystem assumptions.

1. **Bind-mount proc, sys, and dev into `/mnt`:**
    ```bash
    mount -t proc none /mnt/proc
    mount --rbind /sys /mnt/sys
    mount --make-rslave /mnt/sys
    mount --rbind /dev /mnt/dev
    mount --make-rslave /mnt/dev
    ```

2. **Chroot into the real system:**
    ```bash
    chroot /mnt /run/current-system/profile/bin/bash
    ```
    If that fails because `/run/current-system/profile` is broken, use:
    ```bash
    chroot /mnt /bin/bash
    ```
    (On Guix ISO, bash is at `/run/current-system/profile/bin/bash`)

3. **Inside the chroot, rebuild the system:**

    **For first-time installs (system never booted successfully):**
    ```bash
    guix system init /etc/config.scm /
    ```

    **For existing systems (normal case):**
    ```bash
    guix system reconfigure /etc/config.scm
    ```

    If `guix system reconfigure` complains about `--root`, try:
    ```bash
    guix system reconfigure --root=/ /etc/config.scm
    ```

    This regenerates:
    - The system generations
    - Activation scripts (now with correct `/var/run` assumptions)
    - All system configuration

4. **Exit the chroot:**
    ```bash
    exit
    ```

5. **Unmount cleanly:**
    ```bash
    umount -R /mnt/proc
    umount -R /mnt/sys
    umount -R /mnt/dev
    umount /mnt/boot/efi  # If mounted
    umount /mnt
    ```

#### Phase E – Reboot and Verify

1. **Reboot without the ISO plugged in:**
    ```bash
    reboot
    ```

2. **On the real Guix system, check the key invariants:**
    ```bash
    ls -ld /run /var/run
    ls -ld /run/lock /var/lock
    ```
    You should see:
    - `/run` → `drwxr-xr-x` (directory, on tmpfs)
    - `/var/run -> /run` (symlink)
    - `/run/lock` → directory
    - `/var/lock -> /run/lock` (symlink)

3. **Test sudo/PAM:**
    ```bash
    sudo -v && echo "sudo works"
    ```
    If there's no PAM/dbus error, that part is fixed.

4. **(Optional) Clean up old backups once confident:**
    ```bash
    sudo rm -rf /var/run.before-rebuild /var/run.old.before-rebuild
    ```

**Note:** For automated recovery, use the `lib/enforce-guix-filesystem-invariants.sh` script (see below).

### Implementation Status in Our Installer

| Issue | Handled | Location | Notes |
| ----- | ------- | -------- | ----- |
| `/var/run` symlink | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` creates correct structure |
| `/var/lock` symlink | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` creates symlink to `/run/lock` |
| `/run` cleanup | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` empties directory, removes ISO runtime junk |
| `/var/tmp` permissions | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` sets sticky bit (1777) |
| `/var/guix` dirs | ✅ Yes | Step 02 mount | Creates profiles, gcroots, userpool |
| `/etc/mtab` symlink | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` fixes this |
| ISO user cleanup | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` removes ISO user artifacts |
| `/etc/machine-id` | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` removes this |
| `/etc/resolv.conf` | ✅ Yes | Step 02 mount | `CleanupISOArtifacts()` removes this |
| System profile rebuild | ⚠️ Manual | Recovery script | Requires chroot and `guix system reconfigure` |

**Current Status (2025-11-25):**

- ✅ **All filesystem invariants are now fixed** by the installer's `CleanupISOArtifacts()` function
- ✅ **Fresh installs** will have correct filesystem structure from the start
- ⚠️ **Existing systems** with ISO artifacts may still need recovery (see below)

- **For complete recovery** of systems already installed with ISO artifacts, use the comprehensive recovery script: `lib/enforce-guix-filesystem-invariants.sh`

### Why System Profile Rebuild is Necessary

**The Critical Missing Piece:**

Even after fixing filesystem symlinks, if the system profile was built with incorrect assumptions (when `/var/run` was a directory), the activation scripts will still try to recreate `/var/run` as a directory at boot.

**The Solution:**

Rebuilding the system profile inside a chroot ensures:
- Activation scripts are regenerated with correct filesystem assumptions
- System generations are created fresh
- All configuration is reapplied with the correct layout

**When to Rebuild:**

- ✅ **After fixing filesystem layout** (if system was already installed)
- ✅ **If activation scripts keep recreating `/var/run` as directory**
- ✅ **If PAM/dbus errors persist after symlink fixes**
- ❌ **Not needed** for fresh installs (our installer fixes layout before `guix system init`)

**Automated Recovery:**

Use `lib/enforce-guix-filesystem-invariants.sh` script for automated recovery (see Recovery Scripts section below).

---

## 🌐 Network and DNS Troubleshooting for Guix Build Failures

**Common Issue:** After installation, `guix install` or `guix build` commands fail with "Name or service not known" or "Network is unreachable" errors, even though the system appears to be running normally.

### Symptoms

When network/DNS issues occur, you'll see errors like:

```bash
$ guix install curl
# Error: Name or service not known
# Error: Network is unreachable

$ ping ci.guix.gnu.org
# ping: unknown host

$ guix weather curl
# warning: ci.guix.gnu.org: host not found: Name or service not known
# 0.0% substitutes available (0 out of 2)
```

### Diagnostic Script

**Use the diagnostic script** (`diagnose-guix-build.sh`) to identify the root cause:

```bash
# Download and run the diagnostic script
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/diagnose-guix-build.sh
chmod +x diagnose-guix-build.sh
./diagnose-guix-build.sh
```

**What the diagnostic script checks:**

1. **guix-daemon status** - Verifies the daemon is running
2. **Disk space** - Confirms sufficient space for builds
3. **Network connectivity** - Tests `ping` to `ci.guix.gnu.org`
4. **Substitute servers** - Checks if Guix can reach substitute servers
5. **Build logs** - Examines failed build logs for errors
6. **Build users** - Checks guix-daemon build user configuration
7. **Substitute fetching** - Tests `guix build --dry-run` to see substitute availability
8. **Fallback builds** - Suggests using `--fallback` flag

### Common Root Causes

**1. DNS Resolution Failure (Most Common)**

**Symptoms:**
- `ping ci.guix.gnu.org` reports "unknown host"
- `guix weather` shows "host not found: Name or service not known"
- `guix build` cannot fetch substitutes

**Diagnosis:**
```bash
# Check DNS configuration
cat /etc/resolv.conf
# Should show nameserver entries

# Test DNS resolution
nslookup ci.guix.gnu.org
# If this fails, DNS is not configured

# Test network connectivity (bypass DNS)
ping -c 2 8.8.8.8
# If this works but DNS doesn't, it's a DNS issue
```

**Solution:**
```bash
# Option 1: Configure DNS manually
echo "nameserver 8.8.8.8" | sudo tee /etc/resolv.conf
echo "nameserver 1.1.1.1" | sudo tee -a /etc/resolv.conf

# Option 2: Start NetworkManager (if installed)
sudo herd start network-manager
# NetworkManager will configure DNS automatically

# Option 3: Use DHCP to get DNS
sudo dhclient eth0  # Replace eth0 with your interface name
```

**2. Network Interface Not Configured**

**Symptoms:**
- `ping 8.8.8.8` fails with "Network is unreachable"
- `ip link show` shows interface is DOWN
- No network connectivity at all

**Diagnosis:**
```bash
# Check network interfaces
ip link show
# Look for interfaces marked as "DOWN"

# Check if NetworkManager is running
herd status network-manager
```

**Solution:**
```bash
# Option 1: Start NetworkManager
sudo herd start network-manager
sleep 3
herd status network-manager

# Option 2: Manual DHCP (if NetworkManager not available)
# Find your interface name
ip link show

# Bring up interface and get DHCP
sudo ip link set eth0 up  # Replace eth0 with your interface
sudo dhclient eth0
```

**3. Firewall Blocking Outbound Connections**

**Symptoms:**
- DNS works (`nslookup` succeeds)
- Ping to IPs works
- But `guix build` cannot fetch substitutes

**Diagnosis:**
```bash
# Test HTTPS connectivity to Guix servers
curl -I https://ci.guix.gnu.org
# Should return HTTP headers, not connection refused

# Check firewall rules
sudo iptables -L -n
```

**Solution:**
- Check firewall rules and allow outbound HTTPS (port 443)
- If using a VPS, check provider's firewall settings
- Temporarily disable firewall to test: `sudo iptables -F`

### Using the Fix-Network Script

**For automated diagnosis and fixes:**

```bash
# Download and run the network fix script
wget https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/fix-network.scm
chmod +x fix-network.scm
guile --no-auto-compile fix-network.scm
```

The script will:
1. Check network interfaces
2. Check network services (NetworkManager, DHCP)
3. Attempt to start NetworkManager
4. Provide manual DHCP instructions if needed

### Workarounds When Network is Unavailable

**If you cannot fix network immediately:**

**1. Use fallback builds (builds from source):**
```bash
# Install packages without substitutes (builds from source)
guix install --fallback --no-substitutes curl

# This will take much longer but doesn't require network
```

**2. Check build logs for specific errors:**
```bash
# Find build logs
find /var/log/guix -name "*curl*" -type f

# View build log
zcat /var/log/guix/drvs/39/0463zvbdvlzwr14v44jwr7y1243f55-curl-7.84.0.tar.xz.drv.gz | tail -50
```

**3. Verify daemon is running:**
```bash
# Check daemon status
herd status guix-daemon

# Restart if needed
sudo herd restart guix-daemon

# Verify daemon is responsive
guix build --version
```

### Diagnostic Output Interpretation

**From `diagnose-guix-build.sh` output:**

**✅ Good signs:**
- `guix-daemon` status shows "started" and "enabled"
- Disk space shows > 10GB available
- Build logs exist (indicates daemon is working)

**❌ Problem indicators:**
- `ping ci.guix.gnu.org` shows "unknown host" → **DNS issue**
- `guix weather` shows "0.0% substitutes available" → **Network/DNS issue**
- `guix build --dry-run` shows no substitute downloads → **Network issue**
- Build users config not found → **Secondary issue** (may affect builds but not substitute fetching)

**Action items based on diagnostic output:**

1. **If DNS fails:** Configure `/etc/resolv.conf` or start NetworkManager
2. **If network unreachable:** Start NetworkManager or configure interface manually
3. **If substitutes unavailable but network works:** Check firewall, try fallback builds
4. **If daemon not running:** Restart with `herd restart guix-daemon`

### Prevention: Network Configuration During Installation

**The installer should configure network, but if it doesn't:**

1. **Check if NetworkManager is in config.scm:**
   ```bash
   grep -i network /mnt/etc/config.scm
   ```

2. **If missing, add it:**
   ```scheme
   (use-modules (gnu services networking))
   
   (services
    (cons* (service network-manager-service-type)
           %base-services))
   ```

3. **Reconfigure:**
   ```bash
   sudo guix system reconfigure /etc/config.scm
   sudo herd start network-manager
   ```

### Related Tools

- **`diagnose-guix-build.sh`** - Comprehensive diagnostic for build failures
- **`lib/fix-network.scm`** - Automated network configuration and troubleshooting (Guile script)
- **`investigate-kernel-location.sh`** - Kernel file location investigation (different issue)

---

**Remember:** The Guix installation is more forgiving than it seems. Most failures can be recovered by re-running the same command without rebooting.
