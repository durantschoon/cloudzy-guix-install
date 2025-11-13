# Exploratory Approaches Archive

This directory contains approaches we explored but didn't end up implementing in the final installer.

## Why Archive These?

These files represent valid ideas and working code, but they weren't needed for the final solution. We keep them here for:

1. **Historical reference** - Understanding what we tried and why
2. **Future inspiration** - These approaches might be useful for different scenarios
3. **Learning** - Shows the iterative process of finding the right solution

## Contents

### prebuild-kernel.sh / PREBUILD_KERNEL.md

**Explored:** Pre-building the Linux kernel on a powerful machine (Mac with Docker) and transferring it to the target system.

**Why we explored this:**
- Substitute servers sometimes unreliable (kernel not always available)
- Building kernel from source takes 1-2 hours
- Disk space concerns during kernel compilation on ISO
- cow-store reliability concerns

**Why we didn't use it:**
- Found a simpler solution: 3-step kernel/initrd workaround (build, copy, init)
- The workaround works reliably on the ISO without external pre-building
- Keeps installation self-contained (no external dependencies)
- Pre-building would require users to have Docker/Mac setup

**Current solution:** Framework installer uses the 3-step approach documented in INSTALLATION_KNOWLEDGE.md.

---

## Using Archived Approaches

If you're facing a scenario where the current installer doesn't work well (e.g., extremely limited ISO resources, completely offline installation), these archived approaches might be worth revisiting.

Always check the main documentation first - these archived approaches are outdated and may not work with current code.
