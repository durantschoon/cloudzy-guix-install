# /home vs /data Strategy: Analysis and Migration Path

## The Argument for /data with Symlinks

### Why Separate /data from /home?

**Problem with shared /home:**
- Different distros have different user/group IDs
- Config files conflict (`.bashrc`, `.profile`, desktop settings)
- Application caches collide
- Permission issues between OSes

**Benefits of /data + symlinks:**
- Clean separation: OS configs stay in OS-specific /home
- Shared data is explicit (Documents, Music, etc.)
- No permission conflicts
- Each OS has independent user environment
- Easy to see what's shared vs what's not

### Directory Structure Comparison

**Current approach (shared /home):**
```
/home/durant/              ← Shared between Pop!_OS and Guix
├── .bashrc                ← CONFLICT: Different shells/configs
├── .config/               ← CONFLICT: Desktop settings differ
├── Documents/             ← Shared data ✓
├── Downloads/             ← Shared data ✓
├── Music/                 ← Shared data ✓
└── Pictures/              ← Shared data ✓
```

**Proposed approach (/data + symlinks):**
```
# Pop!_OS
/home/durant/              ← Pop!_OS specific
├── .bashrc                ← Pop!_OS config
├── .config/               ← Pop!_OS desktop settings
├── Documents -> /data/Documents    ← Symlink to shared
├── Downloads -> /data/Downloads    ← Symlink to shared
├── Music -> /data/Music            ← Symlink to shared
└── Pictures -> /data/Pictures      ← Symlink to shared

# Guix
/home/durant/              ← Guix specific
├── .bashrc                ← Guix config
├── .config/               ← Guix desktop settings
├── Documents -> /data/Documents    ← Symlink to shared
├── Downloads -> /data/Downloads    ← Symlink to shared
├── Music -> /data/Music            ← Symlink to shared
└── Pictures -> /data/Pictures      ← Symlink to shared

# Shared data partition
/data/
├── Documents/
├── Downloads/
├── Music/
├── Pictures/
├── Videos/
└── Projects/
```

---

## Current Pop!_OS Setup Analysis

### What you have now (framework-dual detects this):

Looking at [framework-dual/install/01-partition-check.go:319](framework-dual/install/01-partition-check.go#L319):

```go
func (s *Step01PartitionCheck) findHomePartition(state *State) {
    cmd := exec.Command("parted", state.Device, "print")
    output, _ := cmd.Output()

    for _, line := range strings.Split(string(output), "\n") {
        if strings.Contains(strings.ToLower(line), "home") {
            // Found partition with "home" in GPT name
            state.HomePartition = s.makePartitionPath(state.Device, partNum)
            // This gets mounted at /home in config.scm
        }
    }
}
```

**If you have a separate "home" partition:**
- Pop!_OS likely has it mounted at `/home`
- Guix installer would detect it and share it
- **This is the conflict scenario** - both OSes using same /home

---

## Migration Scenarios

### Scenario 1: You Currently Have Separate /home Partition

**Current state:**
```
/dev/nvme0n1p1  → EFI (shared)
/dev/nvme0n1p2  → Pop!_OS root
/dev/nvme0n1p3  → /home (shared) ← PROBLEM
/dev/nvme0n1p4  → Guix root
```

**Migration path to /data:**

1. **Back up everything!**

2. **Boot Pop!_OS, create /data partition** (if space available):
   ```bash
   # Shrink home partition
   sudo resize2fs /dev/nvme0n1p3 50G  # Keep 50G for /home
   sudo parted /dev/nvme0n1 resizepart 3 50G

   # Create new /data partition
   sudo parted /dev/nvme0n1 mkpart DATA ext4 XXXGiB 100%
   sudo mkfs.ext4 -L DATA /dev/nvme0n1pX
   ```

3. **Move data from /home to /data:**
   ```bash
   sudo mount /dev/disk/by-label/DATA /mnt

   # Move shared folders
   sudo mv /home/durant/Documents /mnt/
   sudo mv /home/durant/Downloads /mnt/
   sudo mv /home/durant/Music /mnt/
   sudo mv /home/durant/Pictures /mnt/
   sudo mv /home/durant/Videos /mnt/

   # Create symlinks in Pop!_OS
   ln -s /data/Documents ~/Documents
   ln -s /data/Downloads ~/Downloads
   ln -s /data/Music ~/Music
   ln -s /data/Pictures ~/Pictures
   ln -s /data/Videos ~/Videos
   ```

4. **Update /etc/fstab in Pop!_OS:**
   ```
   LABEL=DATA  /data  ext4  defaults  0  2
   ```

5. **Install Guix:**
   - Installer detects no "home" partition → Good!
   - Guix creates /home/durant in its root partition
   - After first Guix boot, create same symlinks

**Pros:**
- Clean separation
- No config conflicts
- Can have different usernames if desired

**Cons:**
- Requires partition resizing (risky!)
- Need free space for /data partition
- Migration effort

---

### Scenario 2: You Don't Have Separate /home Partition

**Current state:**
```
/dev/nvme0n1p1  → EFI (shared)
/dev/nvme0n1p2  → Pop!_OS root (includes /home)
/dev/nvme0n1p3  → Guix root (will include /home)
```

**Two options:**

**Option A: Keep separate /home, create /data (RECOMMENDED)**

1. **Create /data partition in free space:**
   ```bash
   sudo parted /dev/nvme0n1 mkpart DATA ext4 XXXGiB YYYGiB
   sudo mkfs.ext4 -L DATA /dev/nvme0n1pX
   ```

2. **Move shared data to /data:**
   - Same steps as Scenario 1, step 3

3. **Both OSes use /data via symlinks:**
   - Pop!_OS: symlinks from `/home/durant/` → `/data/`
   - Guix: symlinks from `/home/durant/` → `/data/`

**Option B: Share /home (CURRENT FRAMEWORK-DUAL BEHAVIOR)**

Just create a partition labeled "home" before running installer:
```bash
sudo parted /dev/nvme0n1 mkpart home ext4 XXXGiB YYYGiB
sudo mkfs.ext4 -L home /dev/nvme0n1pX
e2label /dev/nvme0n1pX home  # Installer searches for this
```

Installer will detect it and mount at /home in both OSes.

**Pros:** Simple, automatic
**Cons:** Config file conflicts, permission issues

---

## Implementation: How to Support /data in Installer

### Current framework-dual installer:

```go
// Searches for partition with "home" in name
if strings.Contains(strings.ToLower(line), "home") {
    state.HomePartition = s.makePartitionPath(state.Device, partNum)
}
```

Then in config.scm:
```scheme
(file-system
  (mount-point "/home")
  (device (uuid "..."))
  (type "ext4"))
```

### Proposed enhancement:

**Option 1: Search for "DATA" label too**
```go
// In findHomePartition, also check for DATA
if strings.Contains(strings.ToLower(line), "home") {
    state.HomePartition = s.makePartitionPath(state.Device, partNum)
    state.HomeMount = "/home"
} else if strings.Contains(strings.ToLower(line), "data") {
    state.DataPartition = s.makePartitionPath(state.Device, partNum)
    state.DataMount = "/data"
}
```

Then mount it in config.scm:
```scheme
(file-system
  (mount-point "/data")
  (device (file-system-label "DATA"))
  (type "ext4"))
```

**Option 2: Let user choose via environment variable**
```bash
export SHARED_PARTITION_TYPE=data  # or "home"
export SHARED_PARTITION_MOUNT=/data  # or "/home"
```

**Option 3: Detect both, ask user in interactive mode**
```
Found shared partitions:
  1. /dev/nvme0n1p3 labeled "home" (50GB)
  2. /dev/nvme0n1p5 labeled "DATA" (200GB)

How should these be mounted in Guix?
  [1] Mount "home" at /home (share home directory)
  [2] Mount "DATA" at /data (use symlinks for sharing)
  [3] Don't mount either (Guix uses own /home)
```

---

## Recommended Approach

### For You (Durant)

**If you currently have a separate /home partition:**

1. **Check what you have:**
   ```bash
   lsblk -o NAME,LABEL,MOUNTPOINT,SIZE
   sudo parted /dev/nvme0n1 print
   ```

2. **Decide:**
   - **Option A:** Migrate to /data (clean but requires work)
   - **Option B:** Keep shared /home (simple but potential conflicts)

3. **If migrating to /data:**
   - Boot Pop!_OS
   - Shrink home partition (scary!) or use free space
   - Create DATA partition
   - Move Documents/Downloads/etc to /data
   - Create symlinks in Pop!_OS
   - Install Guix (won't detect "home", will create own)
   - Create same symlinks in Guix

4. **Post-install script:**
   ```bash
   #!/bin/bash
   # Run this in Guix after first boot
   ln -s /data/Documents ~/Documents
   ln -s /data/Downloads ~/Downloads
   ln -s /data/Music ~/Music
   # etc.
   ```

### For the Installer

**Enhancement to add:**

```go
// Detect DATA partition
func (s *Step01PartitionCheck) findDataPartition(state *State) {
    // Similar to findHomePartition but searches for "DATA" label
    // Sets state.DataPartition if found
}

// In config generation:
if state.DataPartition != "" {
    // Add /data mount point to config.scm
    dataFS = fmt.Sprintf(`
         (file-system
          (mount-point "/data")
          (device (file-system-label "DATA"))
          (type "ext4"))`)
}
```

**Post-install customization script:**
```bash
# In ~/guix-customize/recipes/setup-data-symlinks.sh
if [ -d /data ]; then
    echo "Setting up symlinks to /data..."
    ln -s /data/Documents ~/Documents
    ln -s /data/Downloads ~/Downloads
    ln -s /data/Music ~/Music
    ln -s /data/Pictures ~/Pictures
    ln -s /data/Videos ~/Videos
fi
```

---

## Questions to Answer

1. **Do you currently have a separate /home partition?**
   - Run: `lsblk -o NAME,LABEL,MOUNTPOINT | grep home`
   - Or: `df -h | grep home`

2. **How much free space do you have?**
   - For /data partition creation
   - Run: `sudo parted /dev/nvme0n1 print free`

3. **Are you willing to resize partitions?**
   - Risk vs reward
   - Backup strategy

4. **Preference:**
   - **Simple:** Keep shared /home (live with potential conflicts)
   - **Clean:** Migrate to /data (more work, cleaner result)

---

## Summary

**The /data approach is architecturally cleaner:**
- ✅ No config conflicts
- ✅ Explicit sharing
- ✅ Independent /home per OS
- ❌ Requires migration/setup
- ❌ Need partition space

**Shared /home is simpler:**
- ✅ Works out of box (installer already supports it)
- ✅ No migration needed
- ❌ Config file conflicts
- ❌ Permission issues

**My recommendation:**
1. Check your current partition layout
2. If you have free space AND are comfortable with partitioning:
   - Create /data partition
   - Move shared data there
   - Use symlinks
3. If not:
   - Live with shared /home for now
   - Migrate to /data later when comfortable

The installer can support both! Just need to:
- Add DATA partition detection (10 lines of code)
- Add post-install symlink setup script (already have ~/guix-customize/)
