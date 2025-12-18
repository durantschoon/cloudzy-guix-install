package lib

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// GetBuiltInModulesForKernel66 returns a list of modules known to be built-in
// to kernel 6.6.16 (wingolog-era). These should be filtered from initrd-modules.
// This is based on kernel 6.6.16 configuration where many drivers are built-in
// rather than loadable modules for better performance and reliability.
//
// To determine which modules are built-in:
// 1. Build the kernel package: guix build linux
// 2. Check /gnu/store/<hash>-linux-6.6.16/lib/modules/<version>/kernel/
// 3. If a module doesn't exist there, it's built-in
// 4. Add it to this list to prevent "kernel module not found" errors
func GetBuiltInModulesForKernel66() []string {
	return []string{
		"nvme",      // NVMe SSD support - built-in for better performance
		"xhci_pci",  // USB 3.0 host controller - built-in for USB support
		// Add more as discovered through installation failures or kernel package inspection
	}
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
