# Pre-building the Guix Kernel with Docker

This document explains how to pre-build the Linux kernel on a powerful machine (like your Mac with Docker) and transfer it to the Framework 13 to avoid disk space issues during installation.

## Why Pre-build?

**Problem:** When the nonguix kernel substitute is unavailable, Guix tries to build the kernel from source during installation. This can cause issues:

- **cow-store may not redirect properly**: Store writes should go to `/mnt` (target disk with 60GB+), but may fill up ISO tmpfs instead if cow-store fails
- **Build failures**: "no space left on device" errors if writes go to wrong location
- **Time consuming**: 1-2 hours to compile kernel from source
- **Reliability**: Build may hang or fail silently

**Solution:** Build the kernel on your Mac (which has plenty of resources) and transfer the result to the Framework 13. This bypasses both the disk space issue AND the substitute server unreliability.

## Prerequisites

- Mac with Docker installed
- Guix Docker image: `docker pull guix/guix:latest`
- USB drive or network connection to transfer files

## Step 1: Pre-build Kernel on Mac

```bash
# In this repository directory on your Mac:
./prebuild-kernel.sh
```

This script will:
1. Create a channels.scm file with nonguix configuration
2. Start a Guix Docker container
3. Build the Linux kernel using time-machine (30-60 minutes)
4. Export the kernel as a NAR archive: `linux-kernel.nar`

**Output:** `linux-kernel.nar` (approximately 200-500 MB)

## Step 2: Transfer to Framework 13

### Option A: USB Drive

```bash
# On Mac:
cp linux-kernel.nar /Volumes/YOUR_USB_DRIVE/

# On Framework 13 Guix ISO:
mkdir /mnt/usb
mount /dev/sdb1 /mnt/usb  # Adjust device as needed
cp /mnt/usb/linux-kernel.nar /root/
umount /mnt/usb
```

### Option B: Network Transfer

```bash
# On Mac (start HTTP server):
python3 -m http.server 8000

# On Framework 13 Guix ISO:
# Find your Mac's IP (e.g., 192.168.1.100)
wget http://192.168.1.100:8000/linux-kernel.nar -O /root/linux-kernel.nar
```

### Option C: SCP (if SSH server running on Mac)

```bash
# On Framework 13 Guix ISO:
scp user@mac-hostname:~/path/to/linux-kernel.nar /root/
```

## Step 3: Run the Installer

The installer automatically detects and imports pre-built kernels:

```bash
# Boot Framework 13 from Guix ISO
# Transfer linux-kernel.nar to /root/ (see Step 2)
# Run installer normally:
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash -s -- framework-dual
```

When the installer reaches Step 4 (System Init), it will:
1. Check for `/root/linux-kernel.nar`
2. If found, import it into the Guix store
3. Use the pre-built kernel instead of building from source

## How It Works

### Pre-build Script (`prebuild-kernel.sh`)

```bash
# Creates nonguix channel configuration
# Runs in Docker:
guix time-machine -C channels.scm -- build linux --no-grafts

# Exports to NAR archive:
guix archive --export KERNEL_PATH > linux-kernel.nar
```

### Installer Integration

The installer's `04-system-init.go` calls:

```go
lib.ImportPrebuiltKernel("/root/linux-kernel.nar")
```

This runs:

```bash
guix archive --import < /root/linux-kernel.nar
```

The kernel is now in `/gnu/store/` and available for `guix system init`.

## Troubleshooting

### "Failed to import kernel archive"

**Possible causes:**
- NAR file is corrupted (re-download or rebuild)
- Not enough disk space on ISO (free up space)
- Guix daemon not running (installer handles this)

**Solution:**
```bash
# Verify NAR file exists and is readable:
ls -lh /root/linux-kernel.nar

# Check available space:
df -h

# Manually import if needed:
guix archive --import < /root/linux-kernel.nar
```

### "No pre-built kernel found"

This is not an error - just means no NAR file at `/root/linux-kernel.nar`. The installer will attempt to build the kernel during system init.

### Docker build fails on Mac

**Check Docker resources:**
- Settings → Resources → Memory: Allocate at least 4GB
- Settings → Resources → Disk: Allocate at least 20GB

**Alternative:** Build on another Linux machine with Guix installed:

```bash
# On Linux machine with Guix:
guix time-machine -C channels.scm -- build linux --no-grafts
KERNEL_PATH=$(guix time-machine -C channels.scm -- build linux --no-grafts)
guix archive --export $KERNEL_PATH > linux-kernel.nar
```

## Verification

After import, verify the kernel is in the store:

```bash
# On Guix ISO:
find /gnu/store -name "*linux-6*" -type d
```

You should see entries like:
```
/gnu/store/abc123...-linux-6.1.82
/gnu/store/def456...-linux-6.1.82-modules
```

## Benefits

✅ **Avoids disk space issues** - Build happens on Mac with plenty of space
✅ **Faster installation** - No 1-2 hour kernel compilation on Framework
✅ **More reliable** - No risk of build failure due to ISO constraints
✅ **Reusable** - Same NAR can be used for multiple installations

## Related Files

- `prebuild-kernel.sh` - Main pre-build script for Mac
- `lib/common.go` - `ImportPrebuiltKernel()` function
- `framework/install/04-system-init.go` - Framework single-boot integration
- `framework-dual/install/04-system-init.go` - Framework dual-boot integration

## Alternative: Use Substitute Servers

If the substitute servers are working, you don't need this workaround:

```bash
# Check if kernel substitute is available:
guix weather linux

# If available, the installer will download it automatically
# If not available, use the pre-build workflow
```

## See Also

- [CHECKLIST.md](CHECKLIST.md#missing-vmlinuz-and-initrd-files) - Missing kernel files debugging
- [Guix Manual: Invoking guix archive](https://guix.gnu.org/manual/en/html_node/Invoking-guix-archive.html)
- [Nonguix Repository](https://gitlab.com/nonguix/nonguix)
