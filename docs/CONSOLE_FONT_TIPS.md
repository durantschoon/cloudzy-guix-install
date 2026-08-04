# Console Font Tips for High-DPI Displays

This guide helps you make the text larger during installation and after boot on systems with high-resolution displays (like the Framework 13 AMD).

> **framework-dual already does this for you.** The generated `config.scm` sets
> `solar24x32` on tty1-tty6 via `modify-services`, so a system installed from
> this repo boots with a readable console. The rest of this page is for the
> installer ISO, for other platforms, and for changing the font by hand.

## Which fonts actually exist

Only fonts in the **`kbd`** package resolve by bare name, because that is the
package `console-font-service-type` runs `setfont` from. In `kbd` 2.5.1 there
are exactly two at high-DPI size:

| Font | Notes |
|------|-------|
| `solar24x32` | Recommended. What framework-dual configures. |
| `latarcyrheb-sun32` | The only other 32px option. |

**`ter-v32n` and `ter-v32b` are not in `kbd`.** They come from the separate
`font-terminus` package. Earlier revisions of this document recommended them;
that was wrong. Naming a font `kbd` does not ship raises no configuration error
— `setfont` just fails at boot and the console silently stays at the default.
Install `font-terminus` first if you want them.

Confirm what your system has:

```bash
ls /run/current-system/profile/share/consolefonts/ | grep -E '24|32|36'
```

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
# Set font temporarily (until reboot) -- use the BARE NAME, not a path
sudo setfont solar24x32

# Reset to the default
sudo setfont
```

Use the bare name. `setfont` searches `kbd`'s own font directory, and the fonts
are stored gzipped (`solar24x32.psfu.gz`), so a hand-written absolute path that
omits the suffix is an easy way to get a confusing failure.

### Make Font Change Permanent

**Use `modify-services`, not another `(service console-font-service-type ...)`.**

`%base-services` *already* instantiates this service type, mapping tty1-tty6 to
`%default-console-font`. Its Shepherd services are named `console-font-tty1`
through `console-font-tty6`, so adding a second instance over the same ttys
collides on those provisions and the system fails to build.

```scheme
(services
 (append
  (list (service network-manager-service-type)
        (service wpa-supplicant-service-type))
  (modify-services %base-services
    ;; Rewrite only the font, keeping whatever tty list the base defines.
    (console-font-service-type
     config => (map (lambda (tty+font)
                      (cons (car tty+font) "solar24x32"))
                    config)))))
```

`console-font-service-type` comes from `(gnu services base)`, which `(gnu)`
re-exports — no extra `use-modules` entry is needed.

This is safe to add to an otherwise minimal config. The font service declares
`(requirement (list 'term-ttyN))`, meaning it depends on mingetty rather than the
reverse, so a font that fails to load **cannot** leave you without a login
prompt. Its start code also treats `setfont`'s `EX_OSERR` (71) as success.

Then apply the changes:

```bash
sudo guix system reconfigure /etc/config.scm
```

### Testing Different Fonts

```bash
sudo setfont solar24x32          # recommended
sudo setfont latarcyrheb-sun32   # the other 32px font in kbd
sudo setfont                     # back to the default

# What this system actually has:
ls /run/current-system/profile/share/consolefonts/ | grep -E '32|24'
```

To try Terminus, install it first — it is not in `kbd`:

```bash
guix install font-terminus
sudo setfont ter-v32b
```

## Framework 13 AMD Recommendation

For the Framework 13 AMD with its 2256x1504 display: **`solar24x32`**. This is
what the generated framework-dual config sets, and it needs no extra package.

## Console Font in the Customize Script

`framework-dual/postinstall/customize` offers this as **option 6) Configure
console font**. Against a framework-dual config it now reports "Console font
already configured" and does nothing, which is correct — the font is set at
install time.

On platforms whose generated config does *not* mention
`console-font-service-type`, be aware that `add_console_font`
(`postinstall/lib.sh`) **appends** a new service rather than modifying the base
one. If that config builds on `%base-services`, the result collides on
`console-font-tty1`..`tty6` as described above.

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
