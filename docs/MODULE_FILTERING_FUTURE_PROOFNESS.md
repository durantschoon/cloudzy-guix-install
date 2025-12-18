# Module Filtering Future-Proofness Analysis

## Current Implementation

### Protection Status

**✅ framework-dual: PROTECTED**
- Uses wingolog-era pinned channels (Feb 2024)
- Kernel version locked to 6.6.16
- Built-in modules list (`nvme`, `xhci_pci`) will remain valid as long as channels stay pinned
- **Future-proof**: As long as channel pinning policy is maintained

**⚠️ framework (single-boot): POTENTIALLY FRAGILE**
- Uses unpinned channels (current master)
- Kernel version can change with Guix updates
- If Guix 1.5.0+ changes kernel version, built-in modules might change
- **Risk**: Function name `GetBuiltInModulesForKernel66()` becomes misleading
- **Risk**: Filter list might become outdated or incorrect for newer kernels

### Current Fragility Points

1. **Function Name Tied to Specific Kernel Version**
   - `GetBuiltInModulesForKernel66()` assumes kernel 6.6.16
   - If kernel changes, function name becomes misleading
   - No automatic kernel version detection

2. **No Kernel Version Detection**
   - Code doesn't detect which kernel version will be used
   - Relies on static list regardless of actual kernel
   - Could filter wrong modules if kernel version changes

3. **Hardcoded Module List**
   - List is static: `["nvme", "xhci_pci"]`
   - No mechanism to update based on kernel version
   - Could become outdated or incorrect

4. **Platform-Specific Behavior**
   - framework-dual: Protected by pinning
   - framework: Not protected, could break with kernel updates

## What Happens If Guix Moves to 1.5.0?

### Scenario 1: Kernel Version Changes

**If Guix 1.5.0 uses kernel 6.8.x or 7.x:**
- Built-in modules might be different
- `nvme` might become a loadable module again
- `xhci_pci` might change status
- **framework-dual**: ✅ Still protected (pinned channels)
- **framework**: ❌ Could break (wrong modules filtered)

### Scenario 2: Guile/Guix API Changes

**If Guix 1.5.0 changes initrd-modules API:**
- Filter predicate generation might break
- `remove` function usage might change
- **Risk**: Low - Guile Scheme is stable

### Scenario 3: Channel Pinning Policy Changes

**If someone updates framework-dual to use newer channels:**
- Kernel version would change
- Built-in modules list would become incorrect
- **Risk**: Medium - Policy document exists but could be ignored

## Recommendations for Future-Proofing

### Option 1: Kernel Version Detection (RECOMMENDED)

Add kernel version detection and version-specific module lists:

```go
// Detect kernel version from channels or store
func DetectKernelVersion(platform, buildType string) (string, error)

// Get built-in modules for specific kernel version
func GetBuiltInModulesForKernel(version string) []string {
    switch {
    case strings.HasPrefix(version, "6.6"):
        return []string{"nvme", "xhci_pci"}
    case strings.HasPrefix(version, "6.8"):
        return []string{"xhci_pci"} // nvme might be loadable
    default:
        return []string{} // Unknown version, no filtering
    }
}
```

**Pros:**
- Handles kernel version changes automatically
- Works for both pinned and unpinned channels
- Future-proof for new kernel versions

**Cons:**
- Requires kernel version detection logic
- Need to maintain version-specific lists

### Option 2: Runtime Module Detection (BEST)

Use the existing `CheckKernelModulesAvailable()` function to detect built-in modules at runtime:

```go
// Check kernel package in store and filter dynamically
func GetBuiltInModulesToFilter(platform, buildType string) []string {
    kernelPkg, err := FindKernelPackageForModules(buildType)
    if err != nil {
        // Fallback to static list for kernel 6.6.16
        return GetBuiltInModulesForKernel66()
    }
    
    modulesToCheck := []string{"nvme", "xhci_pci", "usbhid", "i2c_piix4", "amdgpu"}
    available, err := CheckKernelModulesAvailable(kernelPkg, modulesToCheck)
    if err != nil {
        return GetBuiltInModulesForKernel66() // Fallback
    }
    
    // Return modules that are NOT available (i.e., built-in)
    builtIn := []string{}
    for mod, avail := range available {
        if !avail {
            builtIn = append(builtIn, mod)
        }
    }
    return builtIn
}
```

**Pros:**
- Fully dynamic - works with any kernel version
- No hardcoded lists needed
- Future-proof for any kernel changes

**Cons:**
- Requires kernel package to exist in store (might not during config generation)
- More complex logic

### Option 3: Platform-Specific Functions (SIMPLE)

Keep current approach but make it explicit:

```go
// For framework-dual: always use kernel 6.6.16 list (protected by pinning)
func GetBuiltInModulesForFrameworkDual() []string {
    return []string{"nvme", "xhci_pci"}
}

// For framework: detect or use conservative list
func GetBuiltInModulesForFramework() []string {
    // Could detect kernel version or use empty list (let Guix handle it)
    return []string{} // Conservative: don't filter unless we're sure
}
```

**Pros:**
- Simple, explicit platform behavior
- framework-dual stays protected
- framework doesn't risk filtering wrong modules

**Cons:**
- Doesn't solve framework fragility
- Still relies on static lists

## Current Protection Mechanisms

### ✅ What's Already Protected

1. **Channel Pinning Policy** (`docs/CHANNEL_PINNING_POLICY.md`)
   - Explicit policy document
   - Prevents accidental changes to framework-dual channels
   - Documents why pinning is necessary

2. **Platform-Specific Channel Setup** (`lib/common.go`)
   - framework-dual: Explicitly pinned channels
   - framework: Unpinned channels
   - Clear separation of concerns

3. **Documentation**
   - `NVME_MODULE_FIX.md`: Explains why filtering is needed
   - `CHANNEL_PINNING_POLICY.md`: Prevents channel changes
   - Code comments explain kernel version assumptions

### ⚠️ What's Not Protected

1. **framework (single-boot) platform**
   - Uses unpinned channels
   - Could get newer kernel with different built-in modules
   - No detection or adaptation mechanism

2. **Function naming**
   - `GetBuiltInModulesForKernel66()` assumes specific version
   - Could become misleading if used with different kernels

3. **Static module list**
   - No way to adapt to kernel changes
   - Requires manual updates

## Recommendations

### Short Term (Current State)

**Status: ACCEPTABLE for framework-dual, FRAGILE for framework**

- ✅ framework-dual is protected by channel pinning
- ⚠️ framework could break if kernel changes
- ✅ Filtering mechanism itself is sound
- ⚠️ Function name is version-specific

### Medium Term (Recommended Improvements)

1. **Rename function** to be version-agnostic:
   ```go
   GetBuiltInModulesForKernel66() → GetBuiltInModulesForWingologKernel()
   ```
   Makes it clear this is for wingolog-era kernels specifically

2. **Add platform check** in config generation:
   ```go
   if platform == "framework-dual" {
       // Use wingolog kernel list (protected by pinning)
       builtInModules = lib.GetBuiltInModulesForWingologKernel()
   } else {
       // For other platforms, use runtime detection or empty list
       builtInModules = lib.GetBuiltInModulesDynamic(platform, buildType)
   }
   ```

3. **Document framework fragility**:
   - Add note that framework uses unpinned channels
   - Explain that module list might need updates if kernel changes

### Long Term (Best Solution)

Implement **Option 2: Runtime Module Detection**:
- Use `CheckKernelModulesAvailable()` to detect built-in modules
- Fallback to static list if kernel package not available
- Works with any kernel version automatically

## Conclusion

**Current State:**
- ✅ framework-dual: Well protected by channel pinning
- ⚠️ framework: Fragile but acceptable (uses unpinned channels intentionally)
- ✅ Filtering mechanism: Sound and future-proof
- ⚠️ Function naming: Version-specific but acceptable

**If Guix 1.5.0 changes kernel:**
- framework-dual: ✅ Protected (pinned channels keep kernel 6.6.16)
- framework: ⚠️ Might need manual update to module list
- Filtering code: ✅ Will continue to work (just need updated list)

**Recommendation:**
- Keep current approach for framework-dual (it's protected)
- Consider runtime detection for framework platform
- Rename function to clarify it's for wingolog-era kernels
- Document that framework might need updates if kernel changes
