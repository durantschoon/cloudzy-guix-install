# NetworkManager Test Plan for Framework-Dual

## Overview
Testing the new NetworkManager integration in the framework-dual customize script.

## Current State
- ✅ Fresh Framework 13 boot successful
- ✅ Wired ethernet works (dhclient)
- ❌ No WiFi (no NetworkManager)
- ❌ No WiFi firmware (linux-firmware not installed)

## Test Steps

### Step 1: Run Customize Script
```bash
cd ~/guix-customize
./customize
```

**Expected:** Main menu shows option 0 "Add NetworkManager service"

### Step 2: Add NetworkManager (Option 0)
Select option 0

**Expected:**
- Backs up /etc/config.scm
- Adds `(gnu services networking)` to use-modules
- Adds `(service network-manager-service-type)` to services list
- Shows success message with nmcli usage tips

### Step 3: Add WiFi Firmware (Option 4)
Select option 4

**Expected:**
- Backs up /etc/config.scm again
- Adds `(gnu packages linux)` to use-modules
- Adds `(firmware (list linux-firmware))` to operating-system
- Shows success message about WiFi/Bluetooth firmware

### Step 4: Verify Config Changes
Select option 'v' to view config

**Expected config.scm should contain:**
```scheme
(use-modules (gnu)
             (gnu services networking)    ; <-- Added by step 2
             (gnu packages linux)          ; <-- Added by step 4
             (gnu system nss))

(operating-system
 (firmware (list linux-firmware))         ; <-- Added by step 4
 ;; ... other fields ...
 (services
  (append
   (list (service network-manager-service-type))  ; <-- Added by step 2
   %base-services)))
```

### Step 5: Apply Changes (Option r)
Select option 'r' to reconfigure

**Expected:**
- Prompts for confirmation
- Runs `sudo guix system reconfigure /etc/config.scm`
- Downloads and builds NetworkManager and linux-firmware
- Takes 5-15 minutes
- Completes successfully

### Step 6: Verify NetworkManager is Running
```bash
# Check service status
sudo herd status network-manager

# Should show: running

# List WiFi networks
nmcli device wifi list

# Should show available WiFi networks
```

### Step 7: Connect to WiFi
```bash
nmcli device wifi connect "YOUR_SSID" password "YOUR_PASSWORD"

# Should show: successfully activated connection
```

### Step 8: Verify WiFi Connection
```bash
# Check connection
nmcli connection show

# Should show active WiFi connection

# Test internet
ping -c 3 gnu.org

# Should get responses
```

## Success Criteria

✅ **Complete Success:**
- NetworkManager service added to config
- WiFi firmware added to config
- System reconfigure completes without errors
- NetworkManager service is running
- Can list WiFi networks with nmcli
- Can connect to WiFi
- Internet connectivity works over WiFi

## Troubleshooting

### If reconfigure fails:
- Check error messages carefully
- Verify config.scm syntax with: `guix system build /etc/config.scm`
- Look for typos or missing parentheses
- Restore backup from `~/.config/guix-customize/backups/`

### If NetworkManager doesn't start:
```bash
# Check service logs
sudo herd log network-manager

# Try starting manually
sudo herd start network-manager

# Check for errors
journalctl -u network-manager
```

### If WiFi devices not detected:
```bash
# Check if firmware is loaded
dmesg | grep -i firmware

# Check if WiFi device exists
ip link show

# Should see something like wlp1s0 or similar
```

## Post-Test Actions

After successful test:
1. Document any issues encountered
2. Update customize script if needed
3. Commit changes
4. Update CHECKLIST.md to mark NetworkManager as ✅ Complete
5. Consider framework-dual ready for v1.0.0 release

## Notes

- The customize script creates backups in `~/.config/guix-customize/backups/`
- Each backup is timestamped: `config.scm.YYYYMMDD-HHMMSS`
- Can always restore with: `sudo cp backup-file /etc/config.scm`
- NetworkManager and wired dhclient may conflict - NetworkManager takes over all interfaces
