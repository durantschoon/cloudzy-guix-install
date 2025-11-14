# Console Font Tips for High-DPI Displays

This guide helps you make the text larger during installation and after boot on systems with high-resolution displays (like the Framework 13 AMD).

## During Installation (Guix ISO)

**TODO: Test on next ISO boot to confirm which fonts are available**

If fonts are available on the ISO, you can make the text larger immediately after booting:

```bash
# List available fonts (if this directory exists on ISO)
ls /run/current-system/profile/share/consolefonts/

# Try a larger font (example - adjust based on what's available)
sudo setfont /run/current-system/profile/share/consolefonts/solar24x32
```

**Possible scenarios:**
1. **Fonts available on ISO** - Use `setfont` command above
2. **Fonts not on ISO** - Wait until after installation, then set font
3. **Minimal fonts on ISO** - May have basic larger fonts but not all options

## After Installation (Installed Guix System)

### Quick Temporary Change

```bash
# List all available console fonts
ls /run/current-system/profile/share/consolefonts/

# Recommended fonts for high-DPI displays:
# - solar24x32 - Large, clean (recommended for Framework 13)
# - ter-v32n - Terminus 32pt
# - ter-v32b - Terminus 32pt bold

# Set font temporarily (until reboot)
sudo setfont /run/current-system/profile/share/consolefonts/solar24x32
```

### Make Font Change Permanent

Add console font service to your `/etc/config.scm`:

```scheme
(use-modules (gnu)
             (gnu services base)  ; Required for console-font-service-type
             (gnu services networking)
             ...)

(operating-system
  (host-name "your-hostname")
  (timezone "America/New_York")
  (locale "en_US.utf8")

  ;; ... other configuration ...

  (services
   (append
    (list (service wpa-supplicant-service-type)
          (service network-manager-service-type)

          ;; Set large console font for all TTYs
          (service console-font-service-type
                   (map (lambda (tty)
                          (cons tty "solar24x32"))
                        '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6"))))
    %base-services)))
```

Then apply the changes:

```bash
sudo guix system reconfigure /etc/config.scm
```

### Testing Different Fonts

Try different fonts to find what works best for your display:

```bash
# Try Terminus fonts (designed for programming/terminals)
sudo setfont ter-v32n  # 32pt normal
sudo setfont ter-v32b  # 32pt bold

# Try solar fonts
sudo setfont solar24x32

# If fonts are too small, try even larger:
ls /run/current-system/profile/share/consolefonts/ | grep -E '32|24'
```

## Framework 13 AMD Recommendation

For the Framework 13 AMD with its 2256x1504 display:
- **Best choice**: `solar24x32` - Clean, readable, good size
- **Alternative**: `ter-v32b` - Terminus bold for extra clarity

## Adding Console Font to Customize Script

The framework-dual customize script could offer this as an option in the future:

```bash
# Option: Set larger console font for high-DPI displays
# Adds console-font-service-type to config.scm
```

## Notes

- Console fonts are `.psf` or `.psfu` files (PC Screen Font format)
- Fonts are provided by the `kbd` package (included in `%base-packages`)
- Virtual terminals (tty1-tty6) use console fonts, not TTF fonts
- Font persists across virtual terminals once set
- Graphical terminals (X11/Wayland) use different font systems

## Troubleshooting

**Problem**: Font command not found
```bash
# Install kbd package if missing
guix package -i kbd
```

**Problem**: Fonts directory doesn't exist
```bash
# Check if kbd is in base packages
grep kbd /etc/config.scm
```

**Problem**: Font looks garbled after setting
```bash
# Reset to default font
sudo setfont
```
