# Documentation Paths Analysis

This document categorizes all markdown files in the repository and recommends documentation paths.

## Current Paths

1. **🚀 New to Guix** - Beginners guide
2. **🔧 Experienced Guix Users** - Advanced configuration
3. **💻 Dual-Boot Setup** - Framework 13 + Pop!_OS

## Recommended Additional Paths

### 4. **📚 Reference Documentation** (Technical Deep-Dives)
For users who need detailed technical information, troubleshooting, or want to understand internals.

### 5. **👨‍💻 Contributing to This Project** (Developer Documentation)
For contributors, maintainers, and developers working on the installer itself.

### 6. **🖥️ Platform-Specific Guides** (Hardware/Platform Focus)
For users installing on specific platforms (VPS, Framework, Raspberry Pi).

---

## Complete File Categorization

### Path 1: 🚀 New to Guix (Beginners)

**Primary Guide:**
- `QUICKSTART.md` - Complete workflow from ISO to customized system

**Supporting Docs:**
- `postinstall/CUSTOMIZATION.md` - Adding features after install
- `postinstall/CHANNEL_MANAGEMENT.md` - Configure Guix channels
- `lib/mirrors.md` - Optimize download speeds globally
- `postinstall/EMACS_IMPORT_GUIDE.md` - Import existing Emacs config

**Status:** ✅ Well-organized

---

### Path 2: 🔧 Experienced Guix Users

**Primary Guide:**
- `docs/GUIDE_SEASONED_GUIX.md` - Advanced configuration guide

**Supporting Docs:**
- `docs/INSTALLATION_KNOWLEDGE.md` - Deep technical details (reference)
- `docs/AUTOMATION_ANALYSIS.md` - Recovery script details
- `postinstall/CHANNEL_MANAGEMENT.md` - Channel management (also useful for beginners)

**Status:** ✅ Well-organized

---

### Path 3: 💻 Dual-Boot Setup

**Primary Guide:**
- `docs/GUIDE_DUAL_BOOT.md` - Framework 13 + Pop!_OS dual-boot guide

**Supporting Docs:**
- `framework-dual/README.md` - Installer-specific documentation
- `docs/INSTALLATION_KNOWLEDGE.md` - Sections on dual-boot (reference)

**Status:** ✅ Well-organized

---

### Path 4: 📚 Reference Documentation (NEW PATH)

**Purpose:** Deep technical reference, troubleshooting, and "how things work" documentation.

**Core Reference:**
- `docs/INSTALLATION_KNOWLEDGE.md` - Comprehensive technical reference
- `docs/TROUBLESHOOTING.md` - Common issues and solutions
- `docs/VERIFICATION.md` - Verify installation integrity

**Specific Issue References:**
- `docs/GNOME_LOGIN_TROUBLESHOOTING.md` - Desktop environment issues
- `docs/CONSOLE_FONT_TIPS.md` - High-DPI display font configuration
- `docs/DISABLE_RSYSLOG_MARK.md` - Disable rsyslog keepalive messages
- `docs/AUTOMATION_ANALYSIS.md` - Recovery script automation details

**Status:** ⚠️ Needs organization - currently scattered in docs/README.md under "Reference Documentation"

**Recommendation:** Create `docs/GUIDE_REFERENCE.md` that organizes these by topic:
- Installation troubleshooting
- System configuration
- Hardware-specific issues
- Recovery and automation

---

### Path 5: 👨‍💻 Contributing to This Project (NEW PATH)

**Purpose:** For developers contributing code, documentation, or maintaining the project.

**Core Developer Docs:**
- `CLAUDE.md` - Development notes for AI assistants
- `docs/TESTING.md` - Test suite documentation
- `docs/POSTINSTALL_DEV.md` - Postinstall script development guide
- `docs/STRUCTURE.md` - Repository structure and architecture
- `CHECKLIST.md` - Development status and testing checklist

**Guile/Scheme Development:**
- `docs/GUILE_BEST_PRACTICES.md` - Scheme/Guile patterns
- `docs/GUILE_GOTCHAS.md` - Common Guile pitfalls
- `docs/GUILE_KNOWLEDGE.md` - Quick reference and patterns
- `docs/GUILE_CONVERSION.md` - Bash to Guile conversion strategy

**Process Documentation:**
- `docs/TIME_TRACKING_RETROSPECTIVE.md` - Process lessons and best practices
- `docs/BATCH_CONVERSION_BEST_PRACTICES.md` - Batch conversion workflow

**Tool Documentation:**
- `tools/README.md` - Batch conversion tools
- `tools/DEPLOYMENT_CHECKLIST.md` - Deployment process
- `tools/GENERALIZATION_PLAN.md` - Generalization strategy
- `tools/BATCH_CONVERSION_PLAN.md` - Conversion planning

**Status:** ⚠️ Needs organization - currently mentioned in docs/README.md under "For Developers" but not well-structured

**Recommendation:** Create `docs/GUIDE_CONTRIBUTING.md` that organizes these by:
- Getting started (structure, testing)
- Development workflows (postinstall scripts, Guile conversion)
- Code patterns (Guile best practices, gotchas)
- Process (time tracking, batch conversion)

---

### Path 6: 🖥️ Platform-Specific Guides (NEW PATH)

**Purpose:** Platform-specific installation and configuration details.

**Platform Docs:**
- `cloudzy/README.md` - VPS installation (Cloudzy, DigitalOcean, AWS)
- `framework/README.md` - Framework 13 single-boot installation
- `framework-dual/README.md` - Framework 13 dual-boot installation
- `raspberry-pi/README.md` - Raspberry Pi installation
- `raspberry-pi/install/README.md` - Raspberry Pi install process
- `raspberry-pi/CHANGELOG.md` - Raspberry Pi changelog

**Status:** ✅ Well-organized - each platform has its own README

**Recommendation:** These are already well-organized. Could add a platform comparison guide if needed.

---

### Archive/Historical Documentation

**Archive Files (not part of active paths):**
- `archive/CHECKLIST_COMPLETED.md` - Historical checklist
- `archive/batch/README.md` - Historical batch conversion docs
- `archive/exploratory-approaches/PREBUILD_KERNEL.md` - Exploratory work
- `archive/exploratory-approaches/GUIX_CONTAINER_TESTING.md` - Exploratory work
- `archive/exploratory-approaches/README.md` - Archive index

**Status:** ✅ Properly archived - not part of active documentation paths

---

### Meta Documentation

**Navigation/Index:**
- `README.md` - Main repository entry point
- `docs/README.md` - Documentation navigation hub
- `postinstall/README.md` - Postinstall recipes overview

**Status:** ✅ These serve as navigation/index files

---

## Recommendations

### 1. Create Path 4: Reference Documentation Guide

**File:** `docs/GUIDE_REFERENCE.md`

**Content:**
- Organize all reference docs by topic
- Quick links to troubleshooting guides
- Cross-reference with user guides
- Index of technical deep-dives

**Structure:**
```markdown
# Reference Documentation Guide

## Installation Troubleshooting
- [Troubleshooting Guide](TROUBLESHOOTING.md)
- [Verification Guide](VERIFICATION.md)
- [Installation Knowledge](INSTALLATION_KNOWLEDGE.md)

## System Configuration
- [Console Font Tips](CONSOLE_FONT_TIPS.md)
- [Disable rsyslog MARK](DISABLE_RSYSLOG_MARK.md)

## Hardware-Specific Issues
- [GNOME Login Troubleshooting](GNOME_LOGIN_TROUBLESHOOTING.md)
- [Installation Knowledge - Platform Sections](INSTALLATION_KNOWLEDGE.md)

## Recovery and Automation
- [Automation Analysis](AUTOMATION_ANALYSIS.md)
```

### 2. Create Path 5: Contributing Guide

**File:** `docs/GUIDE_CONTRIBUTING.md`

**Content:**
- Getting started with the codebase
- Development workflows
- Testing procedures
- Code patterns and best practices
- Process documentation

**Structure:**
```markdown
# Contributing Guide

## Getting Started
- [Repository Structure](STRUCTURE.md)
- [Testing Guide](TESTING.md)
- [Development Checklist](../CHECKLIST.md)

## Development Workflows
- [Postinstall Script Development](POSTINSTALL_DEV.md)
- [Guile Conversion Strategy](GUILE_CONVERSION.md)

## Code Patterns
- [Guile Best Practices](GUILE_BEST_PRACTICES.md)
- [Guile Gotchas](GUILE_GOTCHAS.md)
- [Guile Knowledge](GUILE_KNOWLEDGE.md)

## Process Documentation
- [Time Tracking Retrospective](TIME_TRACKING_RETROSPECTIVE.md)
- [Batch Conversion Best Practices](BATCH_CONVERSION_BEST_PRACTICES.md)
- [AI Assistant Notes](../CLAUDE.md)

## Tools
- [Batch Conversion Tools](../tools/README.md)
```

### 3. Update docs/README.md

**Add new paths:**
- Path 4: Reference Documentation
- Path 5: Contributing to This Project
- Path 6: Platform-Specific Guides (already exists but could be clearer)

### 4. Update Main README.md

**Add link to contributing guide:**
```markdown
## 📖 Documentation Guide

**Choose your documentation path:**

- 🚀 **New to Guix?** → [Quickstart](QUICKSTART.md)
- 🔧 **Experienced with Guix?** → [Seasoned Guix Guide](docs/GUIDE_SEASONED_GUIX.md)
- 💻 **Setting up dual-boot?** → [Dual-Boot Guide](docs/GUIDE_DUAL_BOOT.md)
- 📚 **Need technical reference?** → [Reference Guide](docs/GUIDE_REFERENCE.md)
- 👨‍💻 **Contributing to this project?** → [Contributing Guide](docs/GUIDE_CONTRIBUTING.md)
- 🖥️ **Platform-specific help?** → See platform READMEs (cloudzy/, framework/, raspberry-pi/)

**Not sure where to start?** See [docs/README.md](docs/README.md) for complete navigation.
```

---

## Summary

**Current Status:**
- ✅ Paths 1-3: Well-organized
- ⚠️ Path 4: Exists but needs organization
- ⚠️ Path 5: Mentioned but not well-structured
- ✅ Path 6: Well-organized (platform READMEs)

**Action Items:**
1. ✅ Create `docs/GUIDE_REFERENCE.md` (Path 4) - **COMPLETED**
2. ✅ Create `docs/GUIDE_CONTRIBUTING.md` (Path 5) - **COMPLETED**
3. ✅ Update `docs/README.md` with new paths - **COMPLETED**
4. ✅ Update main `README.md` with contributing guide link - **COMPLETED**

**Files Not Yet Categorized:**
- All files have been categorized ✅

**Status:** All recommendations have been implemented. Documentation paths are now complete and organized.

