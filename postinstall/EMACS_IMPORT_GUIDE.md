# Importing Your Existing Emacs Configuration

This guide explains how to import your existing Emacs setup (Spacemacs, Doom Emacs, or vanilla) into your new Guix system.

---

## Quick Import Methods

### Method 1: Import from Git Repository (Recommended)

If your Emacs config is in a Git repository (GitHub, GitLab, etc.), this is the easiest method:

```bash
# For vanilla Emacs or Spacemacs (.emacs.d)
git clone https://github.com/YOUR_USERNAME/YOUR_EMACS_CONFIG ~/.emacs.d

# For Doom Emacs (.config/doom)
mkdir -p ~/.config/doom
git clone https://github.com/YOUR_USERNAME/YOUR_DOOM_CONFIG ~/.config/doom

# For Doom, you also need to install Doom itself first:
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Method 2: Copy from Another Machine

If you have your config on another machine:

```bash
# From the other machine, create a tarball
tar czf emacs-config.tar.gz ~/.emacs.d    # or ~/.config/doom for Doom

# Copy to new machine (via scp, USB, etc.)
scp emacs-config.tar.gz user@new-machine:~

# On new machine, extract
cd ~
tar xzf emacs-config.tar.gz
```

### Method 3: Manual Setup with Recipe, Then Import

Use our recipes to install the framework, then overlay your config:

```bash
# 1. Run the appropriate recipe to install base Emacs
cd ~/guix-customize
./recipes/add-doom-emacs.sh    # or add-spacemacs.sh or add-vanilla-emacs.sh

# 2. Apply the system configuration
sudo guix system reconfigure /etc/config.scm

# 3. Backup the generated config
mv ~/.config/doom ~/.config/doom.default    # for Doom
# or
mv ~/.emacs.d ~/.emacs.d.default            # for Spacemacs/vanilla

# 4. Clone your config
git clone https://github.com/YOUR_USERNAME/YOUR_CONFIG ~/.config/doom
# or
git clone https://github.com/YOUR_USERNAME/YOUR_CONFIG ~/.emacs.d
```

---

## Detailed Instructions by Emacs Type

### Spacemacs

**Option A: Import Existing Repository**

```bash
# Remove default if it exists
rm -rf ~/.emacs.d

# Clone Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# Import your .spacemacs config
curl -o ~/.spacemacs https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/.spacemacs
# or clone your entire dotfiles repo
git clone https://github.com/YOUR_USERNAME/dotfiles ~/dotfiles
ln -s ~/dotfiles/.spacemacs ~/.spacemacs

# Ensure Emacs is in config.scm
grep -q "emacs" /etc/config.scm || echo "Run: ./recipes/add-spacemacs.sh to add Emacs"

# Apply and launch
sudo guix system reconfigure /etc/config.scm
emacs  # First launch will install packages
```

**Option B: Start Fresh with Recipe, Then Customize**

```bash
cd ~/guix-customize
./recipes/add-spacemacs.sh
sudo guix system reconfigure /etc/config.scm

# Edit .spacemacs with your layers and preferences
emacs ~/.spacemacs
```

---

### Doom Emacs

**Option A: Import Existing Repository**

```bash
# Install Doom framework
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Import your Doom config
git clone https://github.com/YOUR_USERNAME/YOUR_DOOM_CONFIG ~/.config/doom

# Or copy specific files
curl -o ~/.config/doom/init.el https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/doom/init.el
curl -o ~/.config/doom/config.el https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/doom/config.el
curl -o ~/.config/doom/packages.el https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/doom/packages.el

# Ensure Emacs and dependencies are in config.scm
grep -q "emacs" /etc/config.scm || echo "Run: ./recipes/add-doom-emacs.sh to add Emacs"

# Apply configuration
sudo guix system reconfigure /etc/config.scm

# Sync Doom packages
~/.config/emacs/bin/doom sync
```

**Option B: Use Recipe, Then Overlay Your Config**

```bash
cd ~/guix-customize
./recipes/add-doom-emacs.sh
sudo guix system reconfigure /etc/config.scm

# Backup generated config
mv ~/.config/doom ~/.config/doom.backup

# Import yours
git clone https://github.com/YOUR_USERNAME/YOUR_DOOM_CONFIG ~/.config/doom

# Sync
~/.config/emacs/bin/doom sync
```

---

### Vanilla Emacs

**Option A: Import Existing Repository**

```bash
# Clone your Emacs config
git clone https://github.com/YOUR_USERNAME/YOUR_EMACS_CONFIG ~/.emacs.d

# Or import single init file
curl -o ~/.emacs https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/.emacs

# Ensure Emacs is in config.scm
grep -q "emacs" /etc/config.scm || echo "Run: ./recipes/add-vanilla-emacs.sh to add Emacs"

# Apply and launch
sudo guix system reconfigure /etc/config.scm
emacs
```

**Option B: Use Recipe as Template**

```bash
cd ~/guix-customize
./recipes/add-vanilla-emacs.sh
sudo guix system reconfigure /etc/config.scm

# Edit the generated init.el with your customizations
emacs ~/.emacs.d/init.el

# Or replace entirely
mv ~/.emacs.d/init.el ~/.emacs.d/init.el.backup
curl -o ~/.emacs.d/init.el https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_DOTFILES/main/init.el
```

---

## Common Import Scenarios

### Import from Dotfiles Repository

Many users keep their Emacs config in a larger dotfiles repo:

```bash
# Clone dotfiles
cd ~
git clone https://github.com/YOUR_USERNAME/dotfiles

# Symlink Emacs config
ln -s ~/dotfiles/emacs.d ~/.emacs.d          # Vanilla/Spacemacs
# or
ln -s ~/dotfiles/doom ~/.config/doom         # Doom Emacs
ln -s ~/dotfiles/.spacemacs ~/.spacemacs     # Spacemacs config file

# Use stow (if you use it)
cd ~/dotfiles
stow emacs
```

### Import from Tarball/Backup

```bash
# Extract your backup
tar xzf my-emacs-backup.tar.gz -C ~

# Or extract to specific location
tar xzf doom-config.tar.gz -C ~/.config/

# Ensure Emacs is installed in config.scm
cd ~/guix-customize
./recipes/add-doom-emacs.sh  # Choose appropriate recipe
sudo guix system reconfigure /etc/config.scm
```

### Import with GNU Stow

If you use GNU Stow for dotfile management:

```bash
# Add stow to your config.scm first
sudo guix install stow

# Clone dotfiles
cd ~
git clone https://github.com/YOUR_USERNAME/dotfiles

# Use stow to symlink
cd ~/dotfiles
stow emacs    # This symlinks emacs/.* to ~/.*
stow doom     # This symlinks doom/.config/doom to ~/.config/doom
```

---

## Important Notes

### Package Dependencies

Make sure your config.scm includes Emacs and any required system packages:

```scheme
(packages
  (append
    (list (specification->package "emacs")
          (specification->package "git")
          (specification->package "ripgrep")  ; For Doom search
          (specification->package "fd")       ; For Doom file finding
          )
    %base-packages))
```

Our recipes handle this automatically, but if importing manually, check your config.

### First Launch

After importing your config:

1. **Spacemacs**: First launch will install all packages (can take 5-10 minutes)
2. **Doom Emacs**: Run `~/.config/emacs/bin/doom sync` before launching
3. **Vanilla Emacs**: May need `M-x package-refresh-contents` then `M-x package-install` for MELPA packages

### Troubleshooting

**"Package not found" errors:**
```bash
# Ensure Emacs is installed
guix package -i emacs

# Or add to system config
sudo guix system reconfigure /etc/config.scm
```

**"git not found" (for Spacemacs/Doom):**
```bash
# Add git to config.scm or install to user profile
guix package -i git
```

**Doom sync fails:**
```bash
# Install dependencies
guix package -i git ripgrep fd

# Re-run sync
~/.config/emacs/bin/doom sync -!
```

---

## Recommended Workflow

The recommended approach for importing existing configs:

1. **Install base system** using our installer
2. **First boot**, run the appropriate recipe to ensure Emacs + dependencies are in config.scm:
   ```bash
   cd ~/guix-customize
   ./recipes/add-doom-emacs.sh  # or your preferred variant
   ```
3. **Apply system config** to install Emacs:
   ```bash
   sudo guix system reconfigure /etc/config.scm
   ```
4. **Backup generated config** (optional):
   ```bash
   mv ~/.config/doom ~/.config/doom.default
   ```
5. **Import your config**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/YOUR_CONFIG ~/.config/doom
   ```
6. **Sync/launch**:
   ```bash
   ~/.config/emacs/bin/doom sync  # Doom only
   emacs
   ```

---

## Version Control Your Config

If you don't already have your Emacs config in version control, now is a great time to start:

```bash
# Initialize git repo
cd ~/.emacs.d  # or ~/.config/doom
git init
git add .
git commit -m "Initial Emacs configuration"

# Push to GitHub
gh repo create my-emacs-config --private --source=. --remote=origin --push

# Or manually
git remote add origin https://github.com/YOUR_USERNAME/my-emacs-config
git push -u origin main
```

Now you can easily sync your config across machines!

---

## Platform-Specific Considerations

### Framework 13 AMD (Framework/Framework-Dual)

No special considerations - all Emacs variants work normally.

### Cloudzy VPS

If using Emacs in terminal mode over SSH:
- Consider using `emacs -nw` (no window) for terminal-only mode
- Or set up X11 forwarding: `ssh -X user@server`
- Or use `emacsclient` for faster startup

### Raspberry Pi

Performance considerations:
- Doom Emacs may be slower on Pi 3
- Vanilla Emacs is lightest weight
- Consider disabling heavy packages for better performance

---

## Additional Resources

- [Spacemacs Documentation](https://www.spacemacs.org/doc/DOCUMENTATION.html)
- [Doom Emacs Documentation](https://docs.doomemacs.org)
- [GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Guix Emacs Packages](https://packages.guix.gnu.org/search/?query=emacs)
