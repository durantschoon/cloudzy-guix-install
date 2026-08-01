package lib

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// GetBuiltInModulesToFilter returns module names to strip from
// %base-initrd-modules before they reach the generated config.
//
// It is deliberately EMPTY, and that is not an oversight.
//
// The list used to be []string{"nvme", "xhci_pci"} on the theory that those are
// built into kernel 6.6.16 and would raise "kernel module not found" during
// initrd generation. Two things are wrong with that:
//
//  1. Neither name is in %base-initrd-modules in the first place. Guix's
//     default-initrd-modules (gnu/system/linux-initrd.scm) lists ahci,
//     usb-storage, uas, usbhid, hid-generic, hid-apple, mmc_block, dm-crypt,
//     xts, serpent_generic, wp512, nls_iso8859-1, pata_acpi, pata_atiixp, isci
//     and the virtio set. So (remove <pred> %base-initrd-modules) never removed
//     anything: the filter has always been a no-op.
//
//  2. "built-in to 6.6.16" is a statement about one pinned kernel. Now that
//     framework-dual tracks a recent kernel, hardcoding it would eventually be
//     wrong in the dangerous direction -- stripping a module that the new
//     kernel really does need to mount root.
//
// The mechanism is kept rather than deleted because it is the correct shape for
// a real fix: if a module ever must be filtered, determine it from the built
// kernel with FindKernelPackageForModules + CheckKernelModulesAvailable below,
// which inspect the actual store path instead of guessing.
func GetBuiltInModulesToFilter() []string {
	return []string{}
}

// BuildInitrdModulesExpr returns the Guile expression to use as the value of
// the operating-system 'initrd-modules' field.
//
// With no modules to filter (the normal case) this is the bare
// %base-initrd-modules, so the generated config says plainly that it uses the
// Guix default. With modules to filter it emits a 'remove' over a predicate.
// The caller must have (srfi srfi-1) in scope for 'remove'.
func BuildInitrdModulesExpr(filterModules []string) string {
	if len(filterModules) == 0 {
		return "%base-initrd-modules"
	}

	conditions := make([]string, 0, len(filterModules))
	for _, mod := range filterModules {
		conditions = append(conditions, fmt.Sprintf(`(string=? module "%s")`, mod))
	}

	return fmt.Sprintf("(remove (lambda (module) (or %s))\n          %%base-initrd-modules)",
		strings.Join(conditions, " "))
}

// CheckKernelModulesAvailable checks which modules are available as loadable modules
// in the kernel package. Returns a map of module name -> available (true/false)
// This helps filter out built-in modules from initrd-modules list
func CheckKernelModulesAvailable(kernelPackagePath string, moduleNames []string) (map[string]bool, error) {
	result := make(map[string]bool)
	
	// Initialize all modules as unavailable
	for _, name := range moduleNames {
		result[name] = false
	}
	
	if kernelPackagePath == "" {
		return result, fmt.Errorf("kernel package path is empty")
	}
	
	// Check if kernel package exists
	if _, err := os.Stat(kernelPackagePath); os.IsNotExist(err) {
		return result, fmt.Errorf("kernel package path does not exist: %s", kernelPackagePath)
	}
	
	// Look for modules in /lib/modules/<version>/kernel/ subdirectories
	modulesBasePath := filepath.Join(kernelPackagePath, "lib", "modules")
	if _, err := os.Stat(modulesBasePath); os.IsNotExist(err) {
		// No modules directory - all modules might be built-in
		return result, nil
	}
	
	// Find kernel version directory (usually one subdirectory)
	entries, err := os.ReadDir(modulesBasePath)
	if err != nil {
		return result, fmt.Errorf("failed to read modules directory: %w", err)
	}
	
	var kernelVersionDir string
	for _, entry := range entries {
		if entry.IsDir() {
			kernelVersionDir = filepath.Join(modulesBasePath, entry.Name())
			break
		}
	}
	
	if kernelVersionDir == "" {
		return result, nil
	}
	
	// Search for each module
	for _, moduleName := range moduleNames {
		// Module files can be:
		// - <module>.ko
		// - <module>.ko.gz
		// - In subdirectories like kernel/drivers/usb/host/xhci-pci.ko
		// - Module name might have underscores or dashes
		
		// Try direct name match
		moduleVariants := []string{
			moduleName + ".ko",
			moduleName + ".ko.gz",
			strings.ReplaceAll(moduleName, "_", "-") + ".ko",
			strings.ReplaceAll(moduleName, "_", "-") + ".ko.gz",
		}
		
		found := false
		for _, variant := range moduleVariants {
			// Search recursively in kernel version directory
			findCmd := exec.Command("find", kernelVersionDir, "-name", variant, "-type", "f")
			output, err := findCmd.Output()
			if err == nil && len(output) > 0 {
				found = true
				break
			}
		}
		
		result[moduleName] = found
	}
	
	return result, nil
}

// FindKernelPackageForModules finds the kernel package and checks module availability
// Returns a map of module name -> available (true/false)
func FindKernelPackageForModules(buildType string) (string, error) {
	var findCmd *exec.Cmd
	if buildType == "non-libre" {
		// Search for 'linux' package (nonguix)
		findCmd = exec.Command("bash", "-c", "for p in /gnu/store/*-linux-*; do [ -d \"$p\" ] && [[ \"$p\" != *linux-libre* ]] && [[ \"$p\" != *.drv ]] && [[ \"$p\" != *.scm ]] && [[ \"$p\" != *.patch ]] && [[ \"$p\" != *.tar.* ]] && echo \"$p\"; done | xargs ls -td 2>/dev/null | head -1")
	} else {
		// Search for 'linux-libre' package
		findCmd = exec.Command("bash", "-c", "for p in /gnu/store/*-linux-libre-*; do [ -d \"$p\" ] && [[ \"$p\" != *.drv ]] && [[ \"$p\" != *.scm ]] && [[ \"$p\" != *.patch ]] && [[ \"$p\" != *.tar.* ]] && echo \"$p\"; done | xargs ls -td 2>/dev/null | head -1")
	}
	
	output, err := findCmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to find kernel package: %w", err)
	}
	
	kernelPackagePath := strings.TrimSpace(string(output))
	if kernelPackagePath == "" {
		return "", fmt.Errorf("no kernel package found in store")
	}
	
	return kernelPackagePath, nil
}
