# Mount by Label vs Device Path: User Experience Comparison

## Current Approach: Device Paths

### What the user does (automated by our scripts):

**Cloudzy (VPS):**
```bash
# Partition
parted /dev/vda mklabel gpt
parted /dev/vda mkpart ESP fat32 1MiB 513MiB
parted /dev/vda set 1 esp on
parted /dev/vda mkpart root ext4 513MiB 100%

# Format (no labels currently)
mkfs.vfat -F32 /dev/vda1
mkfs.ext4 /dev/vda2

# Mount
mount /dev/vda2 /mnt
mkdir -p /mnt/boot/efi
mount /dev/vda1 /mnt/boot/efi

# Config uses device paths
/dev/vda1 → /boot/efi
UUID → /
```

**Framework (single boot):**
```bash
# Partition
parted /dev/nvme0n1 mklabel gpt
parted /dev/nvme0n1 mkpart ESP fat32 1MiB 513MiB
parted /dev/nvme0n1 set 1 esp on
parted /dev/nvme0n1 mkpart primary ext4 513MiB 100%
parted /dev/nvme0n1 name 2 GUIX_ROOT

# Format with labels
mkfs.vfat -F32 -n EFI /dev/nvme0n1p1
mkfs.ext4 -L GUIX_ROOT /dev/nvme0n1p2

# Mount by device path
mount /dev/nvme0n1p2 /mnt
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi

# Config mixes approaches
/dev/nvme0n1p1 → /boot/efi  (device path)
UUID → /                     (UUID - good!)
```

**Framework-dual (existing partitions):**
```bash
# Find existing partitions
parted /dev/nvme0n1 print | grep GUIX_ROOT
parted /dev/nvme0n1 print | grep EFI

# Mount by device path
mount /dev/nvme0n1p4 /mnt
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi

# Config uses device path
/dev/nvme0n1p1 → /boot/efi
UUID → /
```

### Problems with current approach:

1. **Device enumeration can change**
   - Multiple NVMe drives: nvme0n1 vs nvme1n1 depends on detection order
   - USB drives connected: can shift device names
   - BIOS settings changes can reorder devices

2. **Inconsistent between platforms**
   - Cloudzy: `/dev/vda1, /dev/vda2`
   - Framework: `/dev/nvme0n1p1, /dev/nvme0n1p2`
   - SATA: `/dev/sda1, /dev/sda2`

3. **Config.scm mixes approaches**
   - Root: UUID (stable ✅)
   - EFI: device path (unstable ❌)

4. **We already set labels but don't use them!**
   - Framework: Sets `EFI` and `GUIX_ROOT` labels
   - Framework-dual: Looks for `GUIX_ROOT` label
   - Then... mounts by device path anyway 🤦

---

## Proposed Approach: Mount by Label

### What the user would do (automated by our scripts):

**All platforms (consistent!):**
```bash
# Partition (same as now)
parted /dev/DEVICE mklabel gpt
parted /dev/DEVICE mkpart ESP fat32 1MiB 513MiB
parted /dev/DEVICE set 1 esp on
parted /dev/DEVICE mkpart GUIX_ROOT ext4 513MiB 100%

# Format with labels (same as framework already does)
mkfs.vfat -F32 -n EFI /dev/DEVICE1
mkfs.ext4 -L GUIX_ROOT /dev/DEVICE2

# Mount by label (NEW - platform independent!)
mount /dev/disk/by-label/GUIX_ROOT /mnt
mkdir -p /mnt/boot/efi
mount /dev/disk/by-label/EFI /mnt/boot/efi

# Config uses labels (consistent!)
(file-system-label "EFI") → /boot/efi
UUID → /
```

### Advantages:

1. **Platform independent**
   - Same commands work on VPS (vda), Framework (nvme0n1), laptop (sda)
   - Scripts become simpler - no device-specific logic

2. **Stable across boots**
   - Labels don't change if device order changes
   - Safe with multiple disks or USB drives connected

3. **Self-documenting**
   - `GUIX_ROOT` clearly indicates purpose
   - Easy to identify partitions: `lsblk -o NAME,LABEL`

4. **Consistent with what we already do for root**
   - Root already uses UUID (stable identifier)
   - EFI should use label (stable identifier)
   - No more mixing approaches!

5. **Framework-dual already searches by label**
   - Just needs to mount what it finds the same way

---

## User Experience Comparison

### Scenario: User wants to manually partition before running installer

#### Current (device path):
```bash
# User must:
1. Create partitions with specific device numbers
2. Remember which partition is which (/dev/nvme0n1p1 vs p2?)
3. Know their device type (nvme vs vda vs sda)
4. Set correct labels AND remember device paths

# Error-prone:
- "Did I mount nvme0n1p1 or nvme0n1p2 to /mnt?"
- "Wait, is my drive nvme0n1 or nvme1n1?"
```

#### Proposed (label):
```bash
# User must:
1. Create partitions (any numbers work)
2. Label them: EFI and GUIX_ROOT
3. Done! Installer finds them by label

# Much clearer:
mount /dev/disk/by-label/GUIX_ROOT /mnt
mount /dev/disk/by-label/EFI /mnt/boot/efi

# Obvious what goes where!
```

---

## Manual Partition Workflow Comparison

### Current: User Pre-partitions (Framework-dual style)

**What user does in GParted:**
```
1. Create 512MB partition, format as FAT32
   → No way to set label in GParted for FAT32!
   → Must use command line: fatlabel /dev/nvme0n1p1 EFI

2. Create 60GB partition, format as ext4, label as GUIX_ROOT
   → Can set label in GParted ✓

3. Run installer:
   → Installer searches for partition named GUIX_ROOT
   → But mounts by device path anyway
   → What was the point of the label?
```

**Pain points:**
- GParted can't label FAT32 partitions (GUI limitation)
- User must remember to use `fatlabel` from command line
- Labels are searched for but not actually used for mounting
- Confusing: "Why did I set labels if it uses device paths?"

### Proposed: User Pre-partitions with Labels

**What user does in GParted + terminal:**
```
1. Create 512MB FAT32 partition
   fatlabel /dev/nvme0n1p1 EFI

2. Create 60GB ext4 partition
   → Label as GUIX_ROOT in GParted (or e2label)

3. Run installer:
   → Installer finds partitions by label
   → Mounts by label: /dev/disk/by-label/GUIX_ROOT
   → Config.scm uses label: (file-system-label "EFI")
   → Everything consistent!
```

**Improvements:**
- Labels are used end-to-end (not just searched for)
- Device paths never matter - partition numbers are arbitrary
- Clear verification: `lsblk -o NAME,LABEL` shows what installer will use
- Config file is platform-independent

---

## Automated Workflow Comparison

### Current: Fully Automated Install

**Cloudzy script:**
```bash
# Detects /dev/vda or /dev/sda
DEVICE=$(detect_device)  # Returns /dev/vda

# Partitions
parted $DEVICE mkpart ESP ...
parted $DEVICE mkpart root ...

# Formats (no labels!)
mkfs.vfat /dev/vda1
mkfs.ext4 /dev/vda2

# Mounts by device path
EFI=/dev/vda1
ROOT=/dev/vda2
mount $ROOT /mnt
mount $EFI /mnt/boot/efi

# Config hardcodes device path
device "/dev/vda1"
```

**Issues:**
- Device path fragile (what if USB drive makes it vdb?)
- No labels means no verification possible
- Can't check "is this really the EFI partition?" except by mount point

### Proposed: Fully Automated Install

**All platforms (same script!):**
```bash
# Detects device (still needed for partitioning)
DEVICE=$(detect_device)

# Partitions
parted $DEVICE mkpart ESP ...
parted $DEVICE mkpart GUIX_ROOT ...

# Formats WITH LABELS
mkfs.vfat -n EFI ${DEVICE}1 or ${DEVICE}p1
mkfs.ext4 -L GUIX_ROOT ${DEVICE}2 or ${DEVICE}p2

# Mounts by label (platform independent!)
mount /dev/disk/by-label/GUIX_ROOT /mnt
mount /dev/disk/by-label/EFI /mnt/boot/efi

# Config uses label (portable!)
(file-system-label "EFI")
```

**Benefits:**
- Same script works on any platform
- Labels enable verification: `blkid -L GUIX_ROOT`
- Can check "did formatting work?" with `lsblk -o LABEL`
- Config.scm works anywhere

---

## Code Changes Required

### Minimal changes needed:

**1. Update mounting (all 02-mount*.go files):**
```go
// OLD:
mount /dev/nvme0n1p1 /mnt/boot/efi

// NEW:
mount /dev/disk/by-label/EFI /mnt/boot/efi
```

**2. Update config generation (all 03-config*.go files):**
```go
// OLD:
(device "%s")  // state.EFI = "/dev/nvme0n1p1"

// NEW:
(device (file-system-label "EFI"))
```

**3. Add label to cloudzy formatting (01-partition.go):**
```go
// Already done for framework, add to cloudzy:
mkfs.vfat -F32 -n EFI /dev/vda1
mkfs.ext4 -L GUIX_ROOT /dev/vda2
```

**4. Verify labels exist before mounting:**
```go
// Safety check:
if [ ! -e /dev/disk/by-label/EFI ]; then
    echo "ERROR: No partition labeled 'EFI' found"
    exit 1
fi
```

---

## Edge Cases & Considerations

### What if labels already exist on disk?

**Current behavior:** Overwrites, could cause conflicts

**Proposed behavior:**
```bash
# Check before formatting
existing=$(blkid -L GUIX_ROOT 2>/dev/null)
if [ -n "$existing" ]; then
    echo "WARNING: Partition labeled GUIX_ROOT already exists: $existing"
    echo "This will be overwritten. Continue? [y/N]"
fi
```

### What if user has multiple Guix installations?

**Problem:** Can't have two partitions with same label

**Solutions:**
1. Allow custom labels: `GUIX_ROOT_WORK` vs `GUIX_ROOT_HOME`
2. Detect and prompt: "Label exists, use GUIX_ROOT_2?"
3. Document: "Only one Guix install per disk supported"

**Recommendation:** Document limitation, most users don't need multiple Guix

### Backward compatibility

**Question:** What about existing installations using device paths?

**Answer:**
- Existing installs keep working (device paths still valid)
- New installs use labels
- Users can migrate by:
  1. Setting labels: `fatlabel /dev/nvme0n1p1 EFI`
  2. Updating config.scm to use `(file-system-label "EFI")`
  3. Running `guix system reconfigure`

---

## Recommendation

**✅ DO implement mount-by-label:**

**Pros:**
- Platform independent
- Self-documenting
- Stable across boots
- Consistent with UUID usage for root
- We already set labels, just not using them!

**Cons:**
- Requires unique labels (can't have two GUIX_ROOT on same disk)
- Need to handle label conflicts
- Slight learning curve for users used to device paths

**Implementation complexity:** Low
- We already set labels
- Just need to mount by label instead of device path
- Update config.scm to use `(file-system-label "EFI")`

**Impact:** Medium-High improvement in reliability and user experience

---

## Bottom Line

**Current state:** We're 80% there!
- ✅ Set uppercase labels
- ✅ Use UUID for root
- ❌ Mount by device path (why?!)
- ❌ Config uses device path for EFI

**One small change gets us to 100%:**
```diff
- mount /dev/nvme0n1p1 /mnt/boot/efi
+ mount /dev/disk/by-label/EFI /mnt/boot/efi

- (device "/dev/nvme0n1p1")
+ (device (file-system-label "EFI"))
```

This makes everything consistent, stable, and platform-independent.
