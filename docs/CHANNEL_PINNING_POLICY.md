# Channel Pinning Policy for Framework-Dual

## CRITICAL: Do Not Change Framework-Dual Channel Pinning

**This document explains why framework-dual MUST use wingolog-era pinned channels and must NOT be changed to use unpinned channels.**

## The Problem

Framework-dual installations were failing to generate initrd files during `guix time-machine system build`. After investigation, we discovered that:

1. **Unpinned channels cause initrd generation failures**: Using current master commits for guix+nonguix results in incompatible versions that don't properly generate initrd files
2. **Wingolog-era channels work**: Pinning both guix and nonguix to Feb 2024 commits (wingolog era) ensures compatible versions that properly generate initrd

## The Solution

Framework-dual installations **MUST** use wingolog-era pinned channels:

- **Guix commit**: `91d80460296e2d5a01704d0f34fb966a45a165ae` (2024-02-16)
- **Nonguix commit**: `10318ef7dd53c946bae9ed63f7e0e8bb8941b6b1` (2024-02-14)

This is implemented in `lib/common.go:SetupNonguixChannel()` which checks `platform == "framework-dual"` and creates pinned channels.

## Why This Matters

1. **Initrd generation requires compatible versions**: The initrd generation process in Guix requires specific compatibility between guix and nonguix versions
2. **Current master may have regressions**: Wingolog's blog post specifically mentions avoiding "newer guix/nonguix commits" due to firmware loading failures
3. **Reproducibility**: Pinned channels ensure the same behavior every time

## Implementation Details

The channel setup is in `lib/common.go:SetupNonguixChannel(platform string)`:

- **For framework-dual**: Creates `(list ...)` with both guix and nonguix explicitly pinned
- **For other platforms**: Uses `(cons* ... %default-channels)` with unpinned nonguix

## Preventing Future Regressions

### DO NOT:
- ❌ Change framework-dual to use unpinned channels
- ❌ Remove the platform check in `SetupNonguixChannel()`
- ❌ "Update" the commits to newer versions without testing initrd generation
- ❌ Use `%default-channels` for framework-dual (it doesn't pin guix)

### DO:
- ✅ Keep the wingolog-era commits for framework-dual
- ✅ Test initrd generation if you must update commits
- ✅ Document any changes to channel pinning strategy
- ✅ Use explicit `(list ...)` structure for framework-dual

## Testing

If you need to verify that channel pinning is working:

1. Check the generated `~/channels.scm` file during installation
2. Verify it contains both guix and nonguix channels with pinned commits
3. Confirm that `guix time-machine -C ~/channels.scm -- system build` creates initrd files

## References

- [Wingolog Blog Post](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd)
- [Wingolog Channel Analysis](./WINGOLOG_CHANNEL_ANALYSIS.md)
- `framework-dual/wingolog-channels.scm` - Reference implementation

## History

- **2025-01-XX**: Discovered that unpinned channels cause initrd generation failures
- **2025-01-XX**: Implemented wingolog-era channel pinning for framework-dual
- **2025-01-XX**: Added this policy document to prevent future regressions
