# Guix Minimal Install - Quick Start

Complete installation workflow from ISO to customized system.

## Overview

1. **Install** (from ISO) → Get minimal bootable Guix
2. **Boot** (from disk) → Free of ISO, console login
3. **Customize** (from installed system) → Add features you need

---

## Phase 1: Install Minimal Guix (From ISO)

### 1. Boot Guix Live ISO

- Download from <https://guix.gnu.org/>
- Boot on your VPS or Framework 13

### 2. Set Installation Variables

```bash
# Required
export USER_NAME="yourname"
export FULL_NAME="Your Full Name"
export TIMEZONE="America/New_York"
export HOST_NAME="my-guix"
```

### 3. Download and Run Installer

**One command to install:**

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh | bash -s -- <platform>
```

**Platforms:**

- `cloudzy` - VPS fresh install (wipes entire disk)
- `framework` - Framework 13 single-boot (wipes entire disk)
- `framework-dual` - Framework 13 dual-boot with Pop!_OS (keeps existing OS)

**Examples:**

```bash
# VPS installation
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh | bash -s -- cloudzy

# Framework 13 dual-boot
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh | bash -s -- framework-dual
```

**What this does:**

1. Downloads the repository as a tarball
2. Verifies checksums against SOURCE_MANIFEST.txt
3. Prompts you to verify the manifest hash
4. Builds the installer from Go source
5. Runs the interactive installation

### 4. Wait for Installation

The installer runs 4 main steps with time estimates:

| Step | Description | Estimated Time |
|------|-------------|----------------|
| **1. Partition** | Create/verify partitions and labels | 1-2 minutes |
| **2. Mount** | Mount filesystems and copy store | 2-5 minutes |
| **3. Config** | Generate system configuration | < 1 minute |
| **4. Install** | Run `guix system init` | 5-15 minutes |

**Total time: 10-25 minutes** (depends on network speed and whether substitutes are available)

**What happens during Step 4 (Install):**

- Downloads Guix packages (or builds locally with `--fallback`)
- Installs bootloader (GRUB)
- Creates kernel and initrd
- Sets up system profile

You'll see lots of output - this is normal! The installer is downloading and installing packages.

### 5. Reboot

```bash
# Remove ISO
# Reboot into installed system
sudo reboot
```

---

## Phase 2: First Boot (Free of ISO!)

### What to Expect on First Boot

After rebooting from the installed system, you'll see:

**✅ What Works Immediately:**

- Console login prompt (your password works!)
- Basic system utilities (`ls`, `cd`, `nano`, etc.)
- Root access via `sudo`
- Pre-installed customize tool at `~/guix-customize/`

**⚠️ What's NOT Configured Yet:**

- **No SSH server** - can't SSH in until you add it
- **No desktop environment** - console only
- **No WiFi** (Framework 13) - firmware not installed yet
- **Network may need manual setup** - especially on some VPS providers

**📝 Your Next Steps:**

1. Login at console
2. Get network working (if needed)
3. Add SSH (for VPS) or desktop (for laptop)

---

### 1. Login at Console

```text
Guix System
Login: yourname
Password: [password you set during installation]
```

The installer prompts you to set your user password before the system reboots, so you can log in immediately after first boot.

### 2. Set Root Password (Optional but Recommended)

```bash
sudo passwd root
```

### 3. Get Network Working

**Check if network is already working:**

```bash
ping -c 3 guix.gnu.org
```

If this works, skip to Phase 3 (Customize).

**If network doesn't work, try these quick fixes:**

#### Option A: DHCP (Most VPS and Wired Connections)

```bash
# Check interface name
ip link show

# Bring up interface and get DHCP
sudo ip link set eth0 up      # or your interface name (enp0s3, ens3, etc.)
sudo dhclient eth0
```

#### Option B: Manual IP Configuration (VPS/Static IP)

```bash
# Set static IP (example)
sudo ip addr add 192.168.1.100/24 dev eth0
sudo ip link set eth0 up
sudo ip route add default via 192.168.1.1

# Set DNS
echo "nameserver 8.8.8.8" | sudo tee /etc/resolv.conf
```

#### Option C: WiFi Setup (Framework 13 - After Adding Firmware)

**Note:** WiFi won't work until you add `linux-firmware` package. You'll need:

- Wired ethernet connection, or
- USB tethering from phone, or
- USB WiFi adapter with free drivers

Once you have network via ethernet/USB:

```bash
cd ~/guix-customize
./customize
# Select option to add WiFi firmware
# Reconfigure and reboot
```

After reboot with firmware:

```bash
# Scan for networks
sudo iwlist wlan0 scan | grep ESSID

# Connect to WPA network
wpa_passphrase "YourSSID" "YourPassword" | sudo tee /etc/wpa_supplicant.conf
sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant.conf
sudo dhclient wlan0
```

**Better WiFi management:** Add NetworkManager service (see Phase 3 below).

---

### 4. Quick Path: Add NetworkManager (Recommended for Laptops)

NetworkManager provides easy WiFi/network management with a simple command-line interface.

**Add to `/etc/config.scm`:**

```scheme
;; Add to use-modules section (near top of file)
(use-modules (gnu)
             (gnu system nss)
             (gnu services networking))  ; Add this line

;; Replace the services section
(services
  (append
   (list (service network-manager-service-type))
   %base-services))
```

**Reconfigure and reboot:**

```bash
sudo guix system reconfigure /etc/config.scm
sudo reboot
```

**After reboot, connect to WiFi:**

```bash
# Interactive mode (easiest, most secure)
nmtui

# Or command line
nmcli device wifi list
nmcli device wifi connect "YourSSID" --ask  # Prompts for password securely
```

---

## Phase 3: Customize (From Installed System)

Now you're booted into your minimal Guix installation. Time to add features!

**✨ Good news:** The customize tool is pre-installed at `~/guix-customize/` so you can use it immediately after first boot, even without network access!

### Option A: Manual Customization (Works for all platforms)

This is the most reliable method and works without network access:

```bash
sudo nano /etc/config.scm
```

Add features you need (see examples below and CUSTOMIZATION.md for more).

**Step 1: Add SSH (Critical for VPS!)**

```scheme
;; Add to use-modules section (near top of file)
(use-modules (gnu)
             (gnu system nss)
             (gnu services ssh))  ; Add this line

;; Replace the services section
(services
  (append
   (list (service openssh-service-type))
   %base-services))
```

Apply changes:

```bash
sudo guix system reconfigure /etc/config.scm
```

Now SSH will be running on port 22. You can SSH in from another machine.

**Step 2: Add other features** (optional, can do via SSH now)

**Add Desktop (GNOME):**

```scheme
(use-modules (gnu)
             (gnu system nss)
             (gnu services desktop))  ; Add this

(services
  (append
   (list (service gnome-desktop-service-type))
   %base-services))
```

**Add Packages:**

```scheme
(packages
  (append (list (specification->package "git")
                (specification->package "vim")
                (specification->package "emacs"))
          %base-packages))
```

For desktop, reboot after reconfiguring:

```bash
sudo guix system reconfigure /etc/config.scm
sudo reboot
```

### Option B: Interactive Customization Tool (Pre-installed!)

The customize tool was automatically installed during setup. Just run it:

```bash
cd ~/guix-customize
./customize
```

The customize tool provides a friendly menu for:

- Adding SSH service (critical for VPS!)
- Adding desktop environments
- Adding common packages
- Viewing/editing config
- Running shared recipes (Spacemacs, dev tools, fonts)

After making changes, select `r` to reconfigure.

**Bonus:** Shared recipes are also pre-installed in `~/guix-customize/recipes/`:

- `add-spacemacs.sh` - Install Spacemacs (Emacs with Vim keybindings)
- `add-doom-emacs.sh` - Install Doom Emacs (modern, fast Emacs framework)
- `add-vanilla-emacs.sh` - Install vanilla Emacs with minimal configuration
- `add-development.sh` - Install dev tools (git, vim, gcc, etc.)
- `add-fonts.sh` - Install programming fonts

All Emacs recipes support importing your existing config from a Git repository! See `~/guix-customize/EMACS_IMPORT_GUIDE.md` for details.

### Option C: Alternative - Download Fresh Copy (if needed)

If you want to update the customize tool to the latest version:

```bash
# Download fresh copy (requires network)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/cloudzy/postinstall/customize -o ~/customize-latest
chmod +x ~/customize-latest
./customize-latest
```

Or use USB transfer if no network is available (same method as before).

---

## Common Workflows

### VPS Server (SSH Only)

**Step 1: Add SSH at console using customize tool**

```bash
# Login at console after first boot
cd ~/guix-customize
./customize
# Select option 1 to add SSH
# Select 'r' to reconfigure
```

Or manually edit config.scm (see "Manual Customization" section above).

**Step 2: Now you can SSH in**

```bash
# From another machine
ssh yourname@your-vps-ip
```

**Step 3: Add more features via SSH**

Now you can use the customize tool remotely or edit config via SSH.

### Framework 13 Laptop (Desktop + WiFi)

The customize tool is already installed! Just run it:

```bash
cd ~/guix-customize
./customize
# Use menu to add WiFi firmware and desktop
# Select 'r' to reconfigure
sudo reboot  # Desktop starts after reboot
```

Or manually edit config.scm to add:

- linux-firmware (for WiFi)
- Desktop environment (GNOME/Xfce)
- Common packages

See CUSTOMIZATION.md for complete examples.

### Development Workstation

Use the pre-installed customize tool:

```bash
cd ~/guix-customize
./customize
# Use menu to add desktop and common packages
# Or run individual recipes:
./recipes/add-development.sh  # Adds git, vim, gcc, etc.
./recipes/add-spacemacs.sh    # Adds Spacemacs editor
```

Or manually edit /etc/config.scm to add development tools.

See CUSTOMIZATION.md for more recipes and examples.

---

## Troubleshooting

### Can't Login After Install

**Problem:** No password set during installation

**Solution:**

1. Switch to a text console: Press `Alt+F3` or `Alt+F4`
2. Login as `root` (no password)
3. Set user password: `passwd yourname`
4. Return to graphical login: Press `Alt+F7`

**Note on virtual terminals:**
- `Alt+F1`: ISO installer (original boot screen)
- `Alt+F2`: Info page
- `Alt+F3`, `Alt+F4`, `Alt+F5`, `Alt+F6`: Text consoles (useful for running background services like `python -m http.server`)
- `Alt+F7`: Graphical login (if GUI installed)

### No Network After Boot

**Problem:** Network not configured

**Solution:**

```bash
# For DHCP
sudo dhclient

# Or check interface name
ip link show

# Bring up interface
sudo ip link set eth0 up  # or your interface name
sudo dhclient eth0
```

### WiFi Not Working (Framework 13)

**Problem:** Missing firmware

**Solution:** Add `linux-firmware` to config.scm (see CUSTOMIZATION.md)

---

## Next Steps

- Read `CUSTOMIZATION.md` for detailed customization recipes
- Set up nonguix channel for proprietary software
- Install your favorite packages
- Configure dotfiles

Enjoy your minimal Guix system! 🎉

### Notes on Safety Checks and Logs

- Mounts use filesystem labels for reliability; labels must exist before mounting
- Installer warns if free space on `/mnt` is below 40GiB
- During system init, command output is logged to `/tmp/guix-install.log`
- If something fails, check that log first
