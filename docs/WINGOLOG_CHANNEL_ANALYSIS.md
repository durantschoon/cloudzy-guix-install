# Wingolog Channel Analysis: Why Initrd Isn't Being Created

## Problem Summary

The framework-dual installation is failing to create initrd files during `guix time-machine system build`. After analyzing the wingolog-era approach and comparing it to our implementation, we've identified several critical discrepancies.

## Key Differences: Our Implementation vs. Wingolog Approach

### 1. **Channel Pinning: Missing Commit Pins**

**Our Current Implementation (`SetupNonguixChannel()`):**
```scheme
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; NO COMMIT PINNING - uses current master!
        (introduction ...))
       %default-channels)
```

**Wingolog Approach (`wingolog-channels.scm`):**
```scheme
(list
  (channel
    (name 'guix)
    (url "https://git.savannah.gnu.org/git/guix.git")
    (commit "91d80460296e2d5a01704d0f34fb966a45a165ae")  ; PINNED!
    ...)
  (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (commit "10318ef7dd53c946bae9ed63f7e0e8bb8941b6b1")  ; PINNED!
    ...))
```

**Critical Issue:**
- We're using **current master** commits (whatever is latest at install time)
- Wingolog uses **fixed commits from Feb 2024** that are known to work together
- Current master commits may have incompatible changes that break initrd generation

### 2. **Channel Structure: Missing Explicit Guix Channel**

**Our Implementation:**
- Uses `(cons* ... %default-channels)` which relies on the ISO's default guix channel
- Only explicitly defines nonguix channel
- Guix channel version is **uncontrolled** (whatever the ISO has)

**Wingolog Approach:**
- Uses `(list ...)` with **both** guix and nonguix explicitly listed
- Both channels are pinned to specific commits
- Complete control over the channel combination

**Why This Matters:**
- The ISO's guix version may be incompatible with current nonguix master
- Initrd generation requires specific compatibility between guix and nonguix versions
- Without pinning both, we get unpredictable combinations

### 3. **Initrd Generation Requirements**

According to the documentation and wingolog approach:

1. **Initrd is created during `guix system build`** - it should be in the system generation
2. **Initrd is a symlink** pointing to `/gnu/store/*-raw-initrd/initrd.cpio.gz`
3. **Initrd requires compatible kernel + firmware + initrd modules**

**Our Config (`framework-dual/install/03-config-dual-boot.go`):**
```scheme
(kernel linux)                    ; ✓ Correct (from nonguix)
(initrd microcode-initrd)         ; ✓ Correct
(firmware (list linux-firmware))  ; ✓ Correct
(initrd-modules ...)              ; ✓ Correct (includes amdgpu, nvme, etc.)
```

The config looks correct, but **the channel versions may be incompatible**.

## Hypotheses for Why Initrd Isn't Being Created

### Hypothesis 1: Channel Version Incompatibility (MOST LIKELY)
**Problem:** Current nonguix master + ISO guix version don't work together for initrd generation.

**Evidence:**
- Wingolog specifically pins to Feb 2024 commits to avoid "firmware loading failures seen with newer guix/nonguix commits"
- Our logs show system generation exists but has no initrd
- Kernel is found (via Hypothesis H fallback), but initrd never appears

**Solution:** Use wingolog-era pinned commits for both channels.

### Hypothesis 2: Missing Explicit Guix Channel Pin
**Problem:** We're relying on ISO's guix version, which may be too old or too new.

**Evidence:**
- We only pin nonguix, not guix
- `%default-channels` uses whatever guix version the ISO has
- This could be from any era, potentially incompatible with current nonguix

**Solution:** Explicitly list and pin both guix and nonguix channels.

### Hypothesis 3: Initrd Generation Bug in Current Master
**Problem:** There's a bug in current guix/nonguix master that prevents initrd creation.

**Evidence:**
- Wingolog's post specifically mentions avoiding "newer guix/nonguix commits"
- The workaround (3-step process) exists because of this bug
- Our system generation only contains `["gnu","gnu.go","guix"]` - no initrd

**Solution:** Pin to known-good commits (wingolog era).

### Hypothesis 4: Time-Machine Not Using Channels Correctly
**Problem:** `guix time-machine -C channels.scm` might not be respecting our channel file properly.

**Evidence:**
- We create `~/channels.scm` but it only has nonguix
- Time-machine might be falling back to defaults when guix channel isn't explicit
- The build command uses `--substitute-urls` but channels might not match

**Solution:** Ensure channels.scm explicitly lists both channels with commits.

## Recommended Fix: Use Wingolog-Era Channels

### Option 1: Use Existing `wingolog-channels.scm` File

We already have `framework-dual/wingolog-channels.scm` with the correct pins. We should:

1. **Copy it to the installation location** instead of generating a new one
2. **Use it for `guix time-machine`** during system build/init

### Option 2: Modify `SetupNonguixChannel()` to Create Wingolog-Era Channels

Update the function to create a channels.scm that:
- Explicitly lists both guix and nonguix channels
- Pins both to wingolog-era commits (Feb 2024)
- Uses `(list ...)` structure instead of `(cons* ... %default-channels)`

### Option 3: Add Platform-Specific Channel Setup

For `framework-dual` platform:
- Use wingolog-era pinned channels (known-good combination)
- For other platforms, use current approach

## Implementation Plan

1. **Immediate Fix:** Copy `wingolog-channels.scm` to `~/channels.scm` during framework-dual installation
2. **Long-term Fix:** Update `SetupNonguixChannel()` to create wingolog-era channels for framework-dual
3. **Verification:** Add instrumentation to log which channel commits are actually being used
4. **Documentation:** Update installation docs to explain why we pin channels

## Why Wingolog Approach Works

Wingolog's approach works because:

1. **Reproducible:** Same commits = same behavior every time
2. **Tested:** Feb 2024 commits are known to work with Framework 13 AMD
3. **Stable:** Avoids breaking changes in newer commits
4. **Complete:** Both channels explicitly defined and pinned

Our current approach fails because:

1. **Unpredictable:** Different commits every time = different behavior
2. **Untested:** Current master commits may have regressions
3. **Incomplete:** Only nonguix pinned, guix version uncontrolled
4. **Incompatible:** ISO guix + current nonguix master may not work together

## Next Steps

1. Modify `SetupNonguixChannel()` to create wingolog-era channels for framework-dual
2. Add instrumentation to verify which channel commits are actually used
3. Test with pinned channels to confirm initrd generation works
4. Update documentation to explain the channel pinning strategy
