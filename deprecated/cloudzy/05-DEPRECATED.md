# Scripts 05-06: DEPRECATED for Minimal Install

These scripts are **no longer used** with the minimal installation approach.

## Why Deprecated?

**Old workflow:** Add SSH/desktop/channels while still in ISO
**New workflow:** Boot into minimal system first, then customize

## What They Did

- **05-postinstall-console**: Set root password, start SSH (while in ISO)
- **06-postinstall-own-terminal**: Add nonguix channel, reconfigure (while in ISO)

## What to Do Instead

After booting into the installed minimal system:

1. **Login at console** with your USER_NAME
2. **Set root password**: `sudo passwd root`
3. **Customize**: Use `guix-customize` tool or manually edit `/etc/config.scm`
4. **Reconfigure**: `sudo guix system reconfigure /etc/config.scm`

See `CUSTOMIZATION.md` for details.
