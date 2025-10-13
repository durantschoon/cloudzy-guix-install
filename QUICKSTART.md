# Guix Minimal Install - Quick Start

Complete installation workflow from ISO to customized system.

## Overview

1. **Install** (from ISO) → Get minimal bootable Guix
2. **Boot** (from disk) → Free of ISO, console login
3. **Customize** (from installed system) → Add features you need

---

## Phase 1: Install Minimal Guix (From ISO)

### 1. Boot Guix Live ISO
- Download from https://guix.gnu.org/
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
- Script runs 4 steps: partition → mount → config → install
- Takes ~10-15 minutes
- Creates minimal bootable system

### 5. Reboot

```bash
# Remove ISO
# Reboot into installed system
sudo reboot
```

---

## Phase 2: First Boot (Free of ISO!)

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

### 3. Verify Network

```bash
# Check network connectivity
ping -c 3 guix.gnu.org

# If no network, configure:
sudo dhclient  # For DHCP
# or manually configure network
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
1. Press `Ctrl+Alt+F2` for root console
2. Login as `root` (no password)
3. Set user password: `passwd yourname`
4. Return to normal login: `Ctrl+Alt+F1`

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
