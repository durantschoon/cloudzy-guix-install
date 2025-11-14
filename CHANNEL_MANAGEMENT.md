# Channel Management for Guix Installations

This document explains how to manage Guix channels during installation, including support for custom channel repositories and templates.

## Overview

Guix channels allow you to extend the package collection with additional software. The installer now supports:

- **Automatic channel detection** - Detects existing channel configurations
- **Remote channel repositories** - Downloads channels from Git repositories
- **Channel templates** - Pre-configured channel setups for common use cases
- **Channel validation** - Validates channel configurations before installation

## Quick Start

### Using Default Channels

The installer automatically sets up default channels (nonguix + official) if no custom configuration is found:

```bash
# Run installer with default channels
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash
```

### Using Custom Channel Repository

If you have a Git repository with your channel configuration (supports any repository name like `guix-config`, `guix-channels`, `my-guix-setup`, etc.):

```bash
# Set environment variables
export GUIX_CHANNEL_REPO="https://github.com/yourusername/guix-config"
export GUIX_CHANNEL_BRANCH="main"
export GUIX_CHANNEL_PATH="channels/"

# Run installer
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash
```

### Using Bootstrap Script with Channel Parameters

```bash
# Download and run with channel parameters
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash -s -- \
  --channel-repo "https://github.com/yourusername/guix-config" \
  --channel-branch "main" \
  --channel-path "channels/" \
  cloudzy
```

## Channel Repository Structure

Your repository can have any name (`guix-config`, `guix-channels`, `my-guix-setup`, etc.) and should contain a `channels.scm` file. The installer is flexible and looks for it in these locations:

1. `channels/channels.scm` (if `GUIX_CHANNEL_PATH` is set)
2. `channels.scm` (root of repository)
3. `config/channels.scm`
4. `.config/guix/channels.scm`

### Example Repository Structures

**Option 1: `guix-config` repository (recommended - room to grow)**

```
your-guix-config/
├── channels/
│   └── channels.scm
├── systems/
│   ├── desktop.scm
│   └── server.scm
├── services/
│   └── ssh.scm
├── README.md
└── LICENSE
```

**Option 2: `guix-channels` repository (focused on channels only)**

```
your-guix-channels/
├── channels/
│   └── channels.scm
├── README.md
└── LICENSE
```

**Option 3: Simple structure (channels.scm in root)**

```
your-guix-setup/
├── channels.scm
├── README.md
└── LICENSE
```

### Example channels.scm

```scheme
;; Custom channel configuration
(cons* 
 ;; Your custom channel
 (channel
  (name 'my-channel)
  (url "https://github.com/yourusername/my-guix-channel")
  (branch "main"))
 
 ;; Nonguix for proprietary firmware
 (channel
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

## Channel Templates

The installer includes several pre-configured channel templates:

### Minimal Template

- Official Guix channels
- Nonguix channel for proprietary firmware
- Good for basic installations

### Development Template

- All minimal channels
- Additional development-focused channels
- Good for software development

### Gaming Template

- All minimal channels
- Gaming-specific channels
- Good for gaming setups

### Custom Template

- Template for adding your own channels
- Includes examples and documentation

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GUIX_CHANNEL_REPO` | Git URL for channel repository | None |
| `GUIX_CHANNEL_BRANCH` | Branch/tag to use | `main` |
| `GUIX_CHANNEL_PATH` | Path within repository | `channels/` |
| `GUIX_PLATFORM` | Installation platform | `cloudzy` |

## Channel Management Script

The installer includes a channel management utility that works with any repository name:

```bash
# Show current channel status
./lib/channel-utils.sh status

# Set up default channels
./lib/channel-utils.sh setup-default

# Download channels from any repository (guix-config, guix-channels, etc.)
./lib/channel-utils.sh download https://github.com/user/guix-config main channels/
./lib/channel-utils.sh download https://github.com/user/my-guix-setup main config/

# Validate channel configuration
./lib/channel-utils.sh validate

# Interactive setup
./lib/channel-utils.sh interactive
```

## Channel Detection Logic

The installer detects channels in this order:

1. **Existing Configuration**: If `~/.config/guix/channels.scm` exists, use it
2. **Environment Variables**: If `GUIX_CHANNEL_REPO` is set, download from repository
3. **Default Setup**: Create minimal channel configuration

## Troubleshooting

### Channel Download Fails

```bash
# Check repository URL and branch
git clone --branch main https://github.com/yourusername/guix-channels /tmp/test

# Verify channels.scm exists
ls -la /tmp/test/channels.scm
```

### Channel Validation Fails

```bash
# Check channel file syntax
guix describe --format=channels

# Validate specific channel
guix pull --channels=~/.config/guix/channels.scm
```

### Missing Channel Files

The installer will show helpful error messages if `channels.scm` is not found:

```
ERROR: channels.scm not found in repository. Tried:
  - /tmp/guix-channels/channels/channels.scm
  - /tmp/guix-channels/channels.scm
  - /tmp/guix-channels/config/channels.scm
  - /tmp/guix-channels/.config/guix/channels.scm
```

## Best Practices

1. **Pin Channel Commits**: Use specific commit hashes for reproducibility
2. **Test Channels**: Validate channel configurations before installation
3. **Backup Configurations**: Keep copies of working channel configurations
4. **Document Dependencies**: Document any special requirements for your channels
5. **Use Aliases**: Add convenient aliases to your shell profile

### Useful Guix Aliases

Add these to your shell profile (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
# Guix aliases (g for guix - no conflict with git since you use Magit)
alias g='guix'
alias gs='guix shell'
alias gp='guix pull'
alias gi='guix install'
alias gu='guix upgrade'
alias gr='guix remove'
alias gd='guix describe'
alias gt='guix time-machine'
```

**Usage examples:**
```bash
g shell go -- go build
gp
gi emacs
gu
gd
gt -- system reconfigure /etc/config.scm
```

## Examples

### Minimal Setup

```bash
# Use default channels
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash
```

### Custom Development Setup

```bash
# Set up development channels (using guix-config repository)
export GUIX_CHANNEL_REPO="https://github.com/yourusername/guix-config"
export GUIX_CHANNEL_BRANCH="main"
export GUIX_CHANNEL_PATH="channels/"

# Run installer
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash
```

### Framework Laptop with Custom Channels

```bash
# Framework laptop with custom channels (any repository name works)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh | bash -s -- \
  --channel-repo "https://github.com/yourusername/my-guix-setup" \
  --channel-branch "framework-13" \
  --channel-path "config/" \
  framework
```

### Different Repository Naming Patterns

The installer works with any repository name:

```bash
# guix-config repository (recommended - room to grow)
export GUIX_CHANNEL_REPO="https://github.com/yourusername/guix-config"

# guix-channels repository (focused on channels)
export GUIX_CHANNEL_REPO="https://github.com/yourusername/guix-channels"

# Custom naming
export GUIX_CHANNEL_REPO="https://github.com/yourusername/my-guix-setup"
export GUIX_CHANNEL_REPO="https://github.com/yourusername/dev-environment"
export GUIX_CHANNEL_REPO="https://github.com/yourusername/system-config"
```

## Repository Naming Recommendations

### **`guix-config` (Recommended)**

- **Best choice** for most users
- **Room to grow** - can include channels, system configs, services, etc.
- **Future-proof** - won't need to rename if you add more content
- **Common convention** - widely used in the Guix community

### **`guix-channels`**

- **Good choice** if you only plan to manage channels
- **Clear purpose** - no ambiguity about contents
- **Focused** - single responsibility

### **Custom Names**

- **`my-guix-setup`** - Personal preference
- **`dev-environment`** - Development-focused
- **`system-config`** - System configuration focused
- **Any name works** - The installer is flexible!

## Integration with Existing Workflows

The channel system integrates seamlessly with existing installation workflows:

- **VPS Installations**: Use `cloudzy` platform with custom channels
- **Framework Laptops**: Use `framework` or `framework-dual` with custom channels
- **Dual Boot**: Use `framework-dual` with custom channels

## Security Considerations

- **Verify Channel Sources**: Only use channels from trusted sources
- **Check Signatures**: Verify channel introduction signatures
- **Pin Commits**: Use specific commit hashes for reproducibility
- **Review Changes**: Review channel updates before applying them

## Future Enhancements

- **Channel Caching**: Cache downloaded channels for offline use
- **Channel Validation**: More comprehensive channel validation
- **Channel Templates**: Additional pre-configured templates
- **Channel Management**: Web interface for channel management
