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

**For VPS (Cloudzy):**
```bash
# Install prerequisites
guix install curl perl

# Download installer
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
chmod +x run-remote-steps.sh

# Run installer (will install scripts 01-04 only)
bash run-remote-steps.sh
```

**For Framework 13 Dual-Boot:**
```bash
# (Coming soon - separate runner for framework-dual/)
```

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

```
Guix System
Login: yourname
Password: [the password you'll set in a moment]
```

**Wait, I don't have a password yet!**

The installer creates your user account but **doesn't set a password**. You need to:

1. At login prompt, press `Ctrl+Alt+F2` to get root console
2. Login as `root` (no password yet)
3. Set your user password: `passwd yourname`
4. Press `Ctrl+Alt+F1` to return to normal login
5. Now login with your username and new password

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

### Option A: Interactive Customization Tool

```bash
# Download the tool
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/guix-customize -o ~/guix-customize
chmod +x ~/guix-customize

# Run interactive menu
./guix-customize
```

Use the menu to add:
- SSH service
- Desktop environment (GNOME, Xfce, MATE, LXQt)
- Common packages (git, vim, emacs, etc.)
- Framework 13 hardware support
- Nonguix channel info

After making changes, select `r` to reconfigure.

### Option B: Manual Customization

Edit the config directly:

```bash
sudo nano /etc/config.scm
```

Add features (see CUSTOMIZATION.md for examples):

**Add SSH:**
```scheme
(use-modules (gnu)
             (gnu system nss)
             (gnu services ssh))  ; Add this

(services
  (append
   (list (service openssh-service-type))
   %base-services))
```

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

Apply changes:
```bash
sudo guix system reconfigure /etc/config.scm
```

For desktop, reboot after reconfiguring:
```bash
sudo reboot
```

---

## Common Workflows

### VPS Server (SSH Only)

```bash
# After booting into minimal system
sudo nano /etc/config.scm
# Add SSH service (see CUSTOMIZATION.md)
sudo guix system reconfigure /etc/config.scm

# Now you can SSH in
ssh yourname@your-vps-ip
```

### Framework 13 Laptop (Desktop + WiFi)

```bash
# After booting into minimal system
sudo nano /etc/config.scm
# Add:
#   - GNOME desktop
#   - linux-firmware (for WiFi)
#   - Common packages
# (see CUSTOMIZATION.md for complete example)

sudo guix system reconfigure /etc/config.scm
sudo reboot  # Desktop starts after reboot
```

### Development Workstation

```bash
# After booting into minimal system
./guix-customize
# Use menu to add:
#   - Desktop environment
#   - Common packages
# Then manually add development tools:

sudo nano /etc/config.scm
# Add: gcc-toolchain, make, python, node, docker
# (see CUSTOMIZATION.md)

sudo guix system reconfigure /etc/config.scm
```

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
