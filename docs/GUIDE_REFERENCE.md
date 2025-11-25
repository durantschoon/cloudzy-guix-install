# Reference Documentation Guide

**Technical deep-dives, troubleshooting, and "how things work" documentation.**

This guide organizes all reference documentation by topic. Use this when you need detailed technical information, are troubleshooting issues, or want to understand how the installer works internally.

---

## Table of Contents

1. [Installation Troubleshooting](#installation-troubleshooting)
2. [System Configuration](#system-configuration)
3. [Hardware-Specific Issues](#hardware-specific-issues)
4. [Recovery and Automation](#recovery-and-automation)
5. [Installation Internals](#installation-internals)

---

## Installation Troubleshooting

### General Troubleshooting

- **[Troubleshooting Guide](TROUBLESHOOTING.md)** - Common issues and solutions
  - Network problems
  - Boot issues
  - Installation failures
  - Configuration errors

- **[Verification Guide](VERIFICATION.md)** - Verify installation integrity
  - Check system generation
  - Verify boot files
  - Validate configuration
  - Post-installation checks

### Deep Technical Reference

- **[Installation Knowledge](INSTALLATION_KNOWLEDGE.md)** - Comprehensive technical reference
  - Hard-won lessons from real installations
  - Problem discovery and solutions
  - Guix internals explained (cow-store, system init, etc.)
  - Platform-specific quirks and workarounds
  - Kernel/initrd workarounds
  - Channel management details
  - PATH management issues

**Key Sections:**
- Free Software vs Nonguix design decisions
- Nonguix channel setup
- Kernel/initrd workaround (time-machine + nonguix)
- Dual-boot with Pop!_OS
- Framework 13 AMD GPU boot issues
- PATH management after `guix pull`
- Filesystem invariants

---

## System Configuration

### Console and Display

- **[Console Font Tips](CONSOLE_FONT_TIPS.md)** - High-DPI display font configuration
  - Setting larger fonts during installation
  - Permanent font configuration
  - Available fonts on ISO vs installed system

### System Services

- **[Disable rsyslog MARK](DISABLE_RSYSLOG_MARK.md)** - Disable rsyslog keepalive messages
  - Fix periodic "localhost -- MARK --" messages
  - Configure rsyslog service

---

## Hardware-Specific Issues

### Desktop Environment

- **[GNOME Login Troubleshooting](GNOME_LOGIN_TROUBLESHOOTING.md)** - Desktop environment issues
  - GDM login loops
  - AMD GPU firmware failures
  - Channel pinning solutions
  - Framework 13 AMD specific fixes

### Platform-Specific

See [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) for platform-specific sections:

- **Cloudzy VPS**: Free software only approach
- **Framework 13**: Nonguix channel requirements
- **Framework 13 AMD**: GPU boot issues, channel pinning
- **Dual-Boot**: GRUB configuration, bootloader coexistence

---

## Recovery and Automation

### Recovery Scripts

- **[Automation Analysis](AUTOMATION_ANALYSIS.md)** - Recovery script details
  - What can be automated vs manual
  - resolv.conf handling
  - PATH setup for chroot
  - Custom channels support
  - Usage examples with CHANNELS_PATH

**Key Topics:**
- Filesystem invariant setup
- Chroot operations
- Custom channel detection
- Recovery workflow

### Manual Recovery

See [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) for manual recovery procedures:

- Kernel/initrd manual copy process
- System generation verification
- Chroot networking setup
- Filesystem invariant fixes

---

## Installation Internals

### How the Installer Works

- **[Installation Knowledge](INSTALLATION_KNOWLEDGE.md)** - Deep technical details
  - Cow-store mechanism
  - System init process
  - Channel pinning and time-machine
  - Partition detection and mounting
  - Label-based mounting strategy

### Platform Differences

**Free Software Only (Cloudzy):**
- Uses `RunGuixSystemInitFreeSoftware()`
- Standard `guix system init` without time-machine
- No kernel/initrd workaround needed
- Simpler, more reliable for VPS/servers

**Nonguix Required (Framework):**
- Uses `RunGuixSystemInit()` with time-machine + nonguix
- Three-step kernel/initrd workaround
- Requires proprietary firmware for WiFi
- Channel pinning for AMD GPU stability

**Dual-Boot (Framework-Dual):**
- Preserves existing Pop!_OS
- Reuses existing ESP
- GRUB chainloading configuration
- Partition detection without data loss

---

## Quick Reference

### Essential Commands

**Verification:**
```bash
# Check system generation
ls -la /var/guix/profiles/system

# Verify boot files
ls -la /boot/vmlinuz* /boot/initrd*

# Check mounts
mount | grep /mnt
```

**Recovery:**
```bash
# Run recovery script with custom channels
export CHANNELS_PATH="/path/to/channels.scm"
./lib/enforce-guix-filesystem-invariants.sh

# Manual kernel/initrd copy
SYSTEM=$(readlink -f /mnt/var/guix/profiles/system)
cp "$SYSTEM/boot/vmlinuz"* /mnt/boot/
cp "$SYSTEM/boot/initrd"* /mnt/boot/
```

**Troubleshooting:**
```bash
# Check installation log
cat /tmp/guix-install.log

# Verify PATH
which guix
guix describe

# Check channel configuration
cat ~/.config/guix/channels.scm
```

---

## When to Use This Guide

**Use this reference guide when:**

- ✅ You're troubleshooting a specific issue
- ✅ You need to understand how something works internally
- ✅ You want detailed technical information
- ✅ You're recovering from a failed installation
- ✅ You need platform-specific workarounds

**For installation workflows, use:**
- [Quickstart](../QUICKSTART.md) - Beginners
- [Seasoned Guix Guide](GUIDE_SEASONED_GUIX.md) - Experienced users
- [Dual-Boot Guide](GUIDE_DUAL_BOOT.md) - Dual-boot setup

---

## Further Reading

- [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) - Most comprehensive reference
- [Troubleshooting Guide](TROUBLESHOOTING.md) - Common issues
- [Automation Analysis](AUTOMATION_ANALYSIS.md) - Recovery automation
- [Platform READMEs](../README.md#quick-navigation) - Platform-specific details

---

**Not finding what you need?** Check the [main documentation index](README.md) or search [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) for specific topics.

