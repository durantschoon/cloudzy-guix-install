# Troubleshooting Guide

## Kernel Build and Discovery System

> **See also:** [KERNEL_TRACKING.md](./KERNEL_TRACKING.md) for comprehensive documentation on the kernel tracking system, including platform-specific implementations and log analysis.

### Overview

The installer uses a sophisticated hypothesis-driven system to find and install Linux kernels across different environments. Each hypothesis represents a different strategy for locating kernel files, with automatic fallbacks when one approach fails.

### Why This System Exists

Guix installations can fail in multiple ways depending on:
- **Platform** (cloudzy VPS, Framework laptop, Oracle Cloud, etc.)
- **Build Type** (libre free-software-only vs. non-libre with proprietary drivers)
- **Network Availability** (some cloud environments have DNS or network issues)
- **Guix Version** (different versions may structure packages differently)
- **Environment** (ISO vs. installed system, different hardware)

The hypothesis system provides **systematic debugging** and **automatic recovery** through multiple fallback strategies.

### Hypothesis System Architecture

#### Hypothesis ID Policy

**Consistency Rule:** Hypothesis IDs (letters) must be consistent across all platforms. The same letter always means the same hypothesis strategy, regardless of platform.

**Current Hypothesis Assignments:**
- **G**: Standard Build Path (both platforms)
- **M**: Network Diagnostics (both platforms)
- **H**: Build Kernel Package (both platforms)
- **K**: Deep System Generation Search (both platforms)
- **N**: Store-Wide Kernel Search (both platforms)
- **E**: Error Recovery (both platforms)

**When Adding New Hypotheses:**
- Use the next available letter alphabetically
- It's OK to skip letters if a hypothesis is platform-specific (e.g., if cloudzy needs something framework-dual doesn't)
- Document any platform-specific hypotheses clearly
- Ensure the same letter is never reused for different purposes across platforms

#### Hypothesis G: Standard Build Path
**Purpose:** Use `guix system build` to create system generation

**Steps:**
1. Run `guix system build /mnt/etc/config.scm`
2. Check if system generation was created
3. List system generation contents
4. Search for kernel in standard locations

**Tracking Fields:**
- `hypothesisId`: "G"
- `step`: `before_guix_system_build`, `after_guix_system_build`, `system_path_found`, `list_system_contents`, `kernel_search_complete`, `kernel_not_found_anywhere`

**Common Outcomes:**
- **Success:** System generation contains kernel → proceed to copy
- **Partial Success:** System generation created but no kernel → try Hypothesis H
- **Failure:** Build fails → error and retry

#### Hypothesis M: Network Diagnostics
**Purpose:** Check if network/DNS is working before attempting network-based builds

**Steps:**
1. Test DNS resolution for `ci.guix.gnu.org`
2. Return success/failure to guide next steps

**Tracking Fields:**
- `hypothesisId`: "M"
- `platform`: Installation target (cloudzy, framework, framework-dual)
- `buildType`: Software freedom level (libre, non-libre)
- `step`: `check_network_start`, `network_ok`, `dns_failed`

**Common Outcomes:**
- **Network OK:** Proceed with Hypothesis H (build kernel package)
- **Network Failed:** Skip network-based builds, try Hypotheses K and N

**When This Helps:**
- Oracle Cloud instances with DNS issues
- Cloud VPS with restrictive firewalls
- Offline installations
- Guix substitute servers unreachable

#### Hypothesis H: Build Kernel Package
**Purpose:** Build `linux-libre` package separately and locate kernel binary

**Steps:**
1. Run `guix build --substitute-urls='...' linux-libre`
2. Find kernel package output path
3. Validate output is a real path (not error message)
4. Search for kernel binary in package (bzImage, vmlinuz, etc.)
5. List package contents if kernel not found

**Tracking Fields:**
- `hypothesisId`: "H"
- `platform`: Installation target
- `buildType`: Software freedom level
- `step`: `before_guix_build_kernel`, `kernel_package_path_found`, `kernel_build_output_invalid`, `kernel_found_in_package`, `kernel_not_found_in_package`
- `kernelPackagePath`: Store path of linux-libre package
- `checkedPaths`: Locations searched for kernel
- `packageEntries`: Contents of package (for debugging)

**Common Outcomes:**
- **Success:** Kernel found in package → use it
- **Build Failed:** Command error → try Hypotheses K and N
- **Invalid Output:** Error message instead of path → try K and N
- **No Kernel in Package:** Package exists but empty → try K and N

**When This Helps:**
- Guix system build doesn't include kernel
- Free-software-only builds (cloudzy, VPS)
- Environments where kernel is packaged separately

#### Hypothesis K: Deep System Generation Search
**Purpose:** Explore system generation subdirectories and configuration files for kernel paths

**Steps:**
1. Parse `parameters` file for store paths containing "linux" or "initrd"
2. Check each path for actual kernel/initrd files
3. Explore subdirectories (`gnu/`, `guix/`, `bin/`, `boot/`)
4. Follow symlinks to resolve actual file locations
5. Validate found files exist and are accessible

**Tracking Fields:**
- `hypothesisId`: "K"
- `platform`: Installation target
- `buildType`: Software freedom level
- `step`: `deep_search_start`, `parameters_found`, `kernel_from_parameters`, `explore_subdir`, `kernel_from_subdir_symlink`, `deep_search_success`, `deep_search_empty`
- `systemPath`: Path to system generation
- `subdirPath`: Subdirectory being explored
- `kernelPath/initrdPath`: Discovered file locations

**Common Outcomes:**
- **Success:** Kernel/initrd found in subdirectories or via symlinks
- **Partial Success:** Found kernel but not initrd (or vice versa)
- **Failure:** No files found → try Hypothesis N

**When This Helps:**
- Kernel embedded in system generation but not in expected location
- Different Guix versions with alternate directory structures
- Symlink-based kernel references
- Non-standard packaging

#### Hypothesis N: Store-Wide Kernel Search
**Purpose:** Last-resort search through entire `/gnu/store` for kernel packages

**Steps:**
1. Find all `/gnu/store/*-linux-libre-*` packages (most recent first)
2. Search each package for kernel binaries (bzImage, vmlinuz, Image, vmlinux)
3. Validate files by size (> 1MB = reasonable kernel size)
4. Search for initrd packages (`/gnu/store/*-initrd*`)
5. Return first valid kernel found

**Tracking Fields:**
- `hypothesisId`: "N"
- `platform`: Installation target
- `buildType`: Software freedom level
- `step`: `store_search_start`, `packages_found`, `kernel_from_store`, `store_search_success`, `store_search_failed`
- `packageCount`: Number of linux-libre packages found
- `packages`: List of package paths
- `size`: Kernel file size
- `kernelPath/initrdPath`: Discovered file locations

**Common Outcomes:**
- **Success:** Valid kernel found in store → use it
- **Failure:** No kernel packages or all packages empty → installation fails

**When This Helps:**
- Previous build left kernel in store
- Cached packages from earlier installs
- Mixed Guix version environments
- Recovery after partial failures

### Decision Flow

```
Start: guix system build /mnt/etc/config.scm
  ↓
[Hypothesis G: Check system generation]
  ↓
Kernel found? → YES → Copy and continue
  ↓ NO
  ↓
[Hypothesis M: Check network]
  ↓
Network OK? → NO → Try K, then N
  ↓ YES
  ↓
[Hypothesis H: Build linux-libre]
  ↓
Success? → YES → Extract kernel from package
  ↓ NO
  ↓
Try K, then N → Found? → YES → Use kernel
  ↓ NO
  ↓
FAIL: No kernel found via any method
```

### Logging and Tracking

Every hypothesis logs structured JSON data to `/tmp/kernel_tracking.log`:

```json
{
  "location": "lib/common.go:3343",
  "message": "Starting deep system generation search",
  "hypothesisId": "K",
  "step": "deep_search_start",
  "platform": "cloudzy",
  "buildType": "libre",
  "systemPath": "/gnu/store/xxx-system",
  "timestamp": 1766026324013
}
```

**Key Fields:**
- `hypothesisId`: Which strategy (G, M, H, K, N)
- `platform`: cloudzy, framework, framework-dual, raspberry-pi
- `buildType`: libre (free-software), non-libre (future)
- `step`: Specific operation within hypothesis

### Using Logs for Debugging

#### Check Network Issues
```bash
grep '"hypothesisId":"M"' /tmp/kernel_tracking.log | jq .
```
Look for `dns_failed` or `network_ok`

#### Check Kernel Build
```bash
grep '"hypothesisId":"H"' /tmp/kernel_tracking.log | jq .
```
Look for `guix_build_kernel_failed`, `kernel_build_output_invalid`, or `kernel_not_found_in_package`

#### Check Deep Search
```bash
grep '"hypothesisId":"K"' /tmp/kernel_tracking.log | jq .
```
Check which `subdirPath` was explored and if `parameters_found`

#### Check Store Search
```bash
grep '"hypothesisId":"N"' /tmp/kernel_tracking.log | jq .
```
See `packageCount` and which `packages` were found

### Platform-Specific Behaviors

#### Cloudzy (VPS)
- **Platform:** `cloudzy`
- **Build Type:** `libre`
- **Common Path:** G fails → H succeeds
- **Network:** Usually works
- **Fallback:** K and N for cached kernels

#### Oracle Cloud
- **Platform:** `cloudzy`
- **Build Type:** `libre`
- **Common Path:** M fails (DNS) → K finds kernel
- **Network:** Often broken
- **Fallback:** K and N essential

### Related Documentation

- [INSTALLATION_KNOWLEDGE.md](INSTALLATION_KNOWLEDGE.md) - Background on kernel/initrd issues
- [CHECKLIST.md](CHECKLIST.md) - Installation status and recovery procedures

### Getting Help

If kernel discovery fails:

1. Collect logs: `cat /tmp/kernel_tracking.log > kernel_debug.json`
2. Check system: `ls -la /gnu/store/*-guix-system/`
3. Verify store: `guix gc --verify`
4. Report with platform, logs, `guix describe` output

## Remote Debugging & Log Collection

When an installation fails on a remote machine (like a Cloudzy VPS) where copying text from the terminal is difficult, you can use the built-in log serving tool.

### Using the Log Server

The `tools/serve-logs.scm` script (installed to `/root/serve-logs.scm` by the bootstrap script) gathers all relevant logs and serves them over HTTP.

1.  **Run the tool:**
    ```bash
    guile tools/serve-logs.scm
    # Or if running from /root after bootstrap:
    guile /root/serve-logs.scm
    ```

2.  **Access the logs:**
    The script will print a URL, usually:
    ```
    http://<YOUR_IP>:8000/
    ```
    Open this URL in your local browser to view and download:
    - `guix-install.log` (Main installation log)
    - `dmesg.txt` (Kernel ring buffer, critical for OOM kills)
    - `config.scm` (System configuration)
    - `guix-daemon.log` (Daemon logs)

### What to Look For

- **OOM Kills:** Check `dmesg.txt` for "Out of memory: Kill process".
- **Daemon Crashes:** Check `guix-daemon.log` for unexpected stops.
- **Build Errors:** Check `guix-install.log` for the specific build phase failure.

## Installation Media Issues

### Ventoy Incompatibility

If you see an error like:
```
/ventoy/init:65:1 unterminated #! ... !# comment
```
or other syntax errors during boot, it indicates a compatibility issue between Ventoy's boot script and the Guix ISO structure. While recent Ventoy versions claim support, regressions are common.

#### Workarounds (Try these first)
1.  **Use GRUB2 Mode:** When in the Ventoy menu, select the Guix ISO and press **Ctrl+R** to toggle "GRUB2 Mode" before booting. This often bypasses the buggy script.
2.  **Update Ventoy:** Ensure you are on the absolute latest version.

#### The Reliable Fix
If the above fail, you **must** write the ISO directly to the USB drive using `dd`. This eliminates Ventoy entirely for this installation.

**On Linux/macOS (using dd):**
```bash
# Replace /dev/sdX with your USB drive device
sudo dd if=guix-system-install-1.4.0.x86_64-linux.iso of=/dev/sdX bs=4M status=progress && sync
```

**On Windows:**
Use **Rufus** (select "DD Mode" if asked) or **Balena Etcher**.
