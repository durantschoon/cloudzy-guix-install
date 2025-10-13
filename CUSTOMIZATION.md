# Guix System Customization Guide

After installing the minimal Guix system, use this guide to customize your installation.

## Philosophy

The installation scripts create a **truly minimal** bootable Guix system with:

- ✅ Base system packages only (`%base-packages`)
- ✅ Base services only (`%base-services`)
- ✅ User account with sudo access
- ✅ Network support
- ❌ No SSH server
- ❌ No desktop environment
- ❌ No extra packages

**Why minimal?** Get a working system fast, then customize for your specific needs (VPS server, Framework laptop, development workstation, etc.).

---

## Quick Start: `guix-customize` Tool

After first boot, copy the customization tool to your system:

```bash
# Download the tool
curl -fsSL https://raw.githubusercontent.com/YOUR_USERNAME/cloudzy-guix-install/main/lib/guix-customize -o ~/guix-customize
chmod +x ~/guix-customize

# Run interactive customization
./guix-customize
```

### Interactive Menu Options

```text
Services & Features:
  1) Add SSH service
  2) Add desktop environment (GNOME, Xfce, MATE, LXQt)
  3) Add common packages (git, vim, emacs, htop, curl, wget)

Hardware:
  4) Add Framework 13 hardware support (WiFi/BT firmware)
  5) Show nonguix channel info (proprietary software)

Actions:
  r) Apply changes (reconfigure system)
  e) Edit config manually
  v) View current config
  q) Quit
```

---

## Manual Customization Workflows

### Workflow 1: VPS Server Setup

**Goal:** Headless server with SSH access

```bash
# 1. Add SSH service
sudo nano /etc/config.scm
```

Add to `use-modules`:
```scheme
(use-modules (gnu)
             (gnu system nss)
             (gnu services ssh))  ; Add this line
```

Change services:
```scheme
(services
  (append
   (list (service openssh-service-type))
   %base-services))
```

```bash
# 2. Apply changes
sudo guix system reconfigure /etc/config.scm

# 3. SSH is now running on port 22
```

---

### Workflow 2: Framework 13 Desktop

**Goal:** Laptop with GNOME, WiFi, and common tools

```bash
sudo nano /etc/config.scm
```

Add modules:
```scheme
(use-modules (gnu)
             (gnu system nss)
             (gnu services desktop)  ; Desktop environments
             (gnu packages linux))   ; Firmware
```

Add firmware (for WiFi/Bluetooth):
```scheme
(operating-system
 (firmware (list linux-firmware))  ; Add this line
 (host-name "framework-guix")
 ...
```

Add desktop and packages:
```scheme
(packages
  (append (list (specification->package "emacs")
                (specification->package "git")
                (specification->package "vim")
                (specification->package "firefox")
                (specification->package "htop"))
          %base-packages))

(services
  (append
   (list (service gnome-desktop-service-type))
   %base-services))
```

Apply:
```bash
sudo guix system reconfigure /etc/config.scm
sudo reboot  # Desktop will start on next boot
```

---

### Workflow 3: Development Workstation

**Goal:** Full development environment with Docker-like tools

```bash
sudo nano /etc/config.scm
```

Add development packages:
```scheme
(packages
  (append (list (specification->package "git")
                (specification->package "emacs")
                (specification->package "vim")
                (specification->package "gcc-toolchain")
                (specification->package "make")
                (specification->package "python")
                (specification->package "node")
                (specification->package "docker")
                (specification->package "docker-compose"))
          %base-packages))
```

Add Docker service:
```scheme
(use-modules (gnu)
             (gnu system nss)
             (gnu services docker))

(services
  (append
   (list (service openssh-service-type)
         (service docker-service-type))
   %base-services))
```

---

## Adding Proprietary Software (Nonguix)

For proprietary firmware, drivers, or software (Steam, Discord, etc.):

### 1. Add Nonguix Channel

Create `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
```

### 2. Update and Reconfigure

```bash
guix pull
sudo guix system reconfigure /etc/config.scm
```

### 3. Install Nonguix Packages

```bash
# Install proprietary NVIDIA drivers
guix install nvidia-driver

# Install Steam
guix install steam
```

---

## Platform-Specific Optimizations

### Framework 13

**Better battery life:**
```scheme
(use-modules (gnu services pm))

(services
  (append
   (list (service tlp-service-type))  ; Laptop power management
   %base-services))
```

**Better audio:**
```scheme
(use-modules (gnu services audio))

(services
  (append
   (list (service pipewire-service-type))  ; Modern audio stack
   %base-services))
```

### VPS/Server

**Faster boot:**
```scheme
(bootloader-configuration
 (bootloader grub-efi-bootloader)
 (targets '("/boot/efi"))
 (timeout 1)  ; Fast boot (1 second GRUB timeout)
 (terminal-outputs '(console)))  ; No splash screen
```

**Minimal kernel:**
```scheme
(kernel linux-libre-minimal)  ; Smaller kernel, faster boot
```

---

## Common Customization Recipes

### Installing Emacs (Multiple Options)

The guix-customize tool includes three Emacs recipe options pre-installed in `~/guix-customize/recipes/`:

**Option 1: Spacemacs** - Emacs distribution with Vim keybindings and layers
```bash
cd ~/guix-customize
./recipes/add-spacemacs.sh
```

**Option 2: Doom Emacs** - Modern, fast Emacs framework with great defaults
```bash
cd ~/guix-customize
./recipes/add-doom-emacs.sh
```

**Option 3: Vanilla Emacs** - Plain Emacs with sensible minimal configuration
```bash
cd ~/guix-customize
./recipes/add-vanilla-emacs.sh
```

All three recipes:
- Add Emacs and dependencies to your config.scm using idiomatic Guix style
- Support importing your existing config from a Git repository
- Create appropriate configuration files for first-time users
- Provide clear next steps after installation

**Importing Your Existing Config:**

Each recipe will ask if you want to import an existing configuration from a Git repository. You can:
- Provide your dotfiles repo URL to import automatically during setup
- Skip import and do it manually later

For detailed import instructions, see: `~/guix-customize/EMACS_IMPORT_GUIDE.md`

**Example: Importing Doom Emacs config**
```bash
# Recipe will prompt for repo URL like:
# https://github.com/yourusername/doom-config

# Or import manually later:
git clone https://github.com/yourusername/doom-config ~/.config/doom
~/.config/emacs/bin/doom sync
```

---

### Add User to Docker Group

```scheme
(user-account
 (name "yourname")
 (comment "Your Name")
 (group "users")
 (supplementary-groups '("wheel" "netdev" "audio" "video" "docker")))
```

### Change Timezone

```scheme
(timezone "America/Los_Angeles")  ; Or your timezone
```

### Add Swap File

```bash
# Create 4GB swap file
sudo dd if=/dev/zero of=/swapfile bs=1G count=4
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
```

Add to config.scm:
```scheme
(swap-devices
 (list (swap-space
        (target "/swapfile"))))
```

---

## Troubleshooting

### WiFi Not Working (Framework 13)

1. Add `linux-firmware` package (see Workflow 2 above)
2. If still not working, add nonguix channel for proprietary firmware
3. Reboot after reconfiguring

### Can't SSH In

1. Check SSH service is added to config.scm
2. Check firewall: `sudo guix install nftables`
3. Verify SSH is running: `sudo herd status ssh-daemon`

### Desktop Won't Start

1. Make sure you added desktop service to config.scm
2. Reboot after reconfiguring (desktop services need clean boot)
3. Check logs: `sudo journalctl -u gdm` (for GNOME)

---

## Further Reading

- [Official Guix Manual](https://guix.gnu.org/manual/)
- [Guix System Configuration Examples](https://guix.gnu.org/cookbook/en/html_node/System-Configuration.html)
- [Nonguix Documentation](https://gitlab.com/nonguix/nonguix)
