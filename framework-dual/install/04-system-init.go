package install

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

// Step04SystemInit performs the final system initialization
type Step04SystemInit struct{}

func (s *Step04SystemInit) RunWarnings(state *State) error {
	fmt.Println("=== Step 4: System Initialization (Final Step) ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Create swap file in /mnt/swapfile (size from SWAP_SIZE env var)")
	fmt.Println("  2. Activate swap for installation process")
	fmt.Println("  3. Optionally run 'guix pull' (if RUN_GUIX_PULL env var is set)")
	fmt.Println("  4. Download customization tools to /mnt/root/guix-customize/")
	fmt.Println("  5. Run 'guix system init /mnt/etc/config.scm /mnt'")
	fmt.Println("  6. Unmount all partitions")
	fmt.Println("  7. Reboot into your new Guix system")
	fmt.Println()
	fmt.Println("Environment variables used by this step:")
	fmt.Printf("  SWAP_SIZE       - %s (default: 4G)\n", getEnvOrDefault(state.SwapSize, "4G"))
	fmt.Printf("  RUN_GUIX_PULL   - %s (default: not set, skip pull)\n", getEnvOrDefault(os.Getenv("RUN_GUIX_PULL"), "not set"))
	fmt.Printf("  GUIX_GIT_URL    - %s\n", getEnv("GUIX_GIT_URL", "https://git.savannah.gnu.org/git/guix.git"))
	fmt.Printf("  GUIX_VERSION    - %s\n", getEnv("GUIX_VERSION", "v1.4.0"))
	fmt.Println()
	fmt.Println("Estimated time: 5-10 minutes (or 15-30 min if RUN_GUIX_PULL is set)")
	fmt.Println()
	fmt.Println("After reboot, you'll have a minimal bootable Guix system.")
	fmt.Println("Use ~/guix-customize/customize to add SSH, desktop, packages, etc.")
	fmt.Println()

	return nil
}

func (s *Step04SystemInit) RunClean(state *State) error {
	// Set up temporary directory on the target partition (has more space than ISO)
	tmpDir := "/mnt/var/tmp"
	if err := os.MkdirAll(tmpDir, 0777); err != nil {
		return err
	}
	os.Setenv("TMPDIR", tmpDir)

	// Also set XDG_CACHE_HOME to avoid filling up ISO's limited space
	cacheDir := "/mnt/var/cache"
	if err := os.MkdirAll(cacheDir, 0755); err != nil {
		return err
	}
	os.Setenv("XDG_CACHE_HOME", cacheDir)

	// Clear substitute cache
	exec.Command("rm", "-rf", "/var/guix/substitute-cache/").Run()

	// Create swap file
	if err := s.createSwapFile(state); err != nil {
		return err
	}

	// guix pull is now a post-install step (run after first boot)
	// The ISO version is good enough to install the system
	// To enable during install: set RUN_GUIX_PULL=1 (not recommended, takes 10-30 min)
	runPull := getEnv("RUN_GUIX_PULL", "")
	if runPull != "" {
		fmt.Println()
		fmt.Println("=== Guix Pull ===")
		fmt.Println("Updating Guix to latest version (takes 10-30 minutes)...")
		fmt.Println()

		// Set up Git environment variables for slow connections
		os.Setenv("GIT_HTTP_MAX_REQUESTS", "2")
		os.Setenv("GIT_HTTP_LOW_SPEED_LIMIT", "1000")
		os.Setenv("GIT_HTTP_LOW_SPEED_TIME", "60")

		// Determine mirror URL
		guixGitURL := getEnv("GUIX_GIT_URL", "https://git.savannah.gnu.org/git/guix.git")
		guixVersion := getEnv("GUIX_VERSION", "v1.4.0")

		fmt.Printf("Pulling Guix from configured mirror: %s\n", guixGitURL)
		if err := runCommand("guix", "pull", "--url="+guixGitURL, "--commit="+guixVersion); err != nil {
			return fmt.Errorf("guix pull failed: %w", err)
		}
	} else {
		fmt.Println()
		fmt.Println("Skipping guix pull - this is now a post-install step")
		fmt.Println("After first boot, run: guix pull && guix package -u")
		fmt.Println()
	}

	// Download customization tools
	if err := s.downloadCustomizationTools(state); err != nil {
		fmt.Printf("Warning: Failed to download customization tools: %v\n", err)
		fmt.Println("  You can manually download them after first boot")
	}

	// Verify ESP is properly mounted as vfat
	fmt.Println()
	fmt.Println("=== Verifying EFI System Partition ===")
	cmd := exec.Command("df", "-T", "/mnt/boot/efi")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to check ESP: %w", err)
	}
	if !strings.Contains(string(output), "vfat") {
		fmt.Println("ERROR: /mnt/boot/efi is not mounted as vfat filesystem")
		fmt.Println("Current mount info:")
		runCommand("df", "-T", "/mnt/boot/efi")
		runCommand("mount", "|", "grep", "/mnt/boot/efi")
		return fmt.Errorf("ESP verification failed: not a vfat filesystem")
	}
	fmt.Println("[OK] ESP is correctly mounted as vfat")
	fmt.Println()

	// Start cow-store to redirect store writes to target disk
	fmt.Println("=== Starting cow-store ===")
	fmt.Println("Redirecting store writes to /mnt to avoid filling ISO space...")
	if err := runCommand("herd", "start", "cow-store", "/mnt"); err != nil {
		return fmt.Errorf("failed to start cow-store: %w", err)
	}
	fmt.Println()

	// Run guix system init with retry logic
	fmt.Println("=== Running guix system init ===")
	fmt.Println("This will take several minutes...")
	fmt.Println()

	maxRetries := 3
	var lastErr error
	for attempt := 1; attempt <= maxRetries; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Retrying guix system init after substitute failure...\n", attempt, maxRetries)
			fmt.Println("Waiting 10 seconds before retry...")
			fmt.Println()
			exec.Command("sleep", "10").Run()
		}

		if err := runCommand("guix", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt"); err != nil {
			lastErr = err
			fmt.Printf("\n[WARN] Attempt %d failed: %v\n", attempt, err)
			if attempt < maxRetries {
				fmt.Println("This is often caused by temporary substitute server issues.")
				fmt.Println("The command will automatically retry...")
			}
			continue
		}

		// Success
		lastErr = nil
		break
	}

	if lastErr != nil {
		fmt.Println()
		fmt.Println("All retry attempts failed. You can:")
		fmt.Println("  1. Wait a few minutes and run: guix system init /mnt/etc/config.scm /mnt")
		fmt.Println("  2. Try with --fallback to build from source: guix system init --fallback /mnt/etc/config.scm /mnt")
		return fmt.Errorf("guix system init failed after %d attempts: %w", maxRetries, lastErr)
	}

	// Verify installation succeeded
	fmt.Println()
	fmt.Println("=== Verifying Installation ===")
	allGood := true

	// Check for kernel
	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz-*")
	if len(kernels) == 0 {
		fmt.Println("[WARN] No kernel found in /mnt/boot/vmlinuz-*")
		allGood = false
	} else {
		fmt.Printf("[OK] Kernel installed: %s\n", filepath.Base(kernels[0]))
	}

	// Check for initrd
	initrds, _ := filepath.Glob("/mnt/boot/initrd-*")
	if len(initrds) == 0 {
		fmt.Println("[WARN] No initrd found in /mnt/boot/initrd-*")
		allGood = false
	} else {
		fmt.Printf("[OK] Initrd installed: %s\n", filepath.Base(initrds[0]))
	}

	// Check for GRUB config
	if _, err := os.Stat("/mnt/boot/grub/grub.cfg"); err != nil {
		fmt.Println("[WARN] No GRUB config at /mnt/boot/grub/grub.cfg")
		allGood = false
	} else {
		fmt.Println("[OK] GRUB config installed")
	}

	// Check for GRUB EFI binary
	if _, err := os.Stat("/mnt/boot/efi/EFI/guix/grubx64.efi"); err != nil {
		fmt.Println("[WARN] No GRUB EFI binary at /mnt/boot/efi/EFI/guix/grubx64.efi")
		allGood = false
	} else {
		fmt.Println("[OK] GRUB EFI binary installed")
	}

	// Check for EFI GRUB config
	if _, err := os.Stat("/mnt/boot/efi/EFI/guix/grub.cfg"); err != nil {
		fmt.Println("[WARN] No EFI GRUB config at /mnt/boot/efi/EFI/guix/grub.cfg")
		allGood = false
	} else {
		fmt.Println("[OK] EFI GRUB config installed")
	}

	if !allGood {
		fmt.Println()
		fmt.Println("WARNING: Installation may be incomplete!")
		fmt.Println("Missing files detected. System may not boot properly.")
		fmt.Println("You can try re-running: guix system init /mnt/etc/config.scm /mnt")
		fmt.Println()
		fmt.Print("Continue anyway? [y/N] ")

		var response string
		fmt.Scanln(&response)
		if response != "y" && response != "Y" {
			return fmt.Errorf("installation verification failed, aborting")
		}
	} else {
		fmt.Println()
		fmt.Println("[OK] All critical files verified successfully!")
	}
	fmt.Println()

	// Sync and unmount
	fmt.Println("Syncing filesystems...")
	runCommand("sync")

	fmt.Println("Unmounting /mnt...")
	runCommand("umount", "-R", "/mnt")

	fmt.Println()
	fmt.Println("=== Installation Complete ===")
	fmt.Println("System will reboot now...")
	fmt.Println()

	runCommand("reboot")

	return nil
}

func (s *Step04SystemInit) createSwapFile(state *State) error {
	swapSize := state.SwapSize
	if swapSize == "" {
		swapSize = "4G"
	}

	// Parse swap size
	re := regexp.MustCompile(`^(\d+)([GMK]?)$`)
	matches := re.FindStringSubmatch(swapSize)
	if matches == nil {
		fmt.Printf("Warning: Invalid SWAP_SIZE format '%s', using default 4G\n", swapSize)
		swapSize = "4G"
		matches = re.FindStringSubmatch(swapSize)
	}

	sizeNum, _ := strconv.Atoi(matches[1])
	sizeUnit := matches[2]
	if sizeUnit == "" {
		sizeUnit = "G"
	}

	var sizeBytes int64
	switch sizeUnit {
	case "G":
		sizeBytes = int64(sizeNum) * 1024 * 1024 * 1024
	case "M":
		sizeBytes = int64(sizeNum) * 1024 * 1024
	case "K":
		sizeBytes = int64(sizeNum) * 1024
	}

	// Remove existing swapfile if present (from previous run)
	if _, err := os.Stat("/mnt/swapfile"); err == nil {
		fmt.Println("Removing existing swap file from previous run...")
		// Turn off swap if it's active
		exec.Command("swapoff", "/mnt/swapfile").Run()
		os.Remove("/mnt/swapfile")
	}

	fmt.Printf("Creating %s swap file...\n", swapSize)

	// Try fallocate first, fall back to dd
	cmd := exec.Command("fallocate", "-l", fmt.Sprintf("%d", sizeBytes), "/mnt/swapfile")
	if err := cmd.Run(); err != nil {
		fmt.Println("fallocate failed, using dd instead...")
		sizeMB := sizeBytes / 1024 / 1024
		cmd = exec.Command("dd", "if=/dev/zero", "of=/mnt/swapfile", "bs=1M", fmt.Sprintf("count=%d", sizeMB))
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("failed to create swap file: %w", err)
		}
	}

	if err := os.Chmod("/mnt/swapfile", 0600); err != nil {
		return err
	}

	if err := runCommand("mkswap", "/mnt/swapfile"); err != nil {
		return err
	}

	if err := runCommand("swapon", "/mnt/swapfile"); err != nil {
		return err
	}

	fmt.Println("Verify swap is active and memory is available:")
	runCommand("swapon", "--show")
	runCommand("free", "-h")

	return nil
}

func (s *Step04SystemInit) downloadCustomizationTools(state *State) error {
	fmt.Println()
	fmt.Println("=== Installing Customization Tools ===")
	fmt.Println("Copying customize script and recipes to /mnt/root/guix-customize/")
	fmt.Println()

	platform := state.GuixPlatform
	if platform == "" {
		platform = "cloudzy"
	}

	repoOwner := getEnv("GUIX_INSTALL_REPO", "durantschoon/cloudzy-guix-install")
	repoRef := getEnv("GUIX_INSTALL_REF", "main")
	rawBase := fmt.Sprintf("https://raw.githubusercontent.com/%s/%s", repoOwner, repoRef)

	// Create destination directory
	destDir := "/mnt/root/guix-customize"
	if err := os.MkdirAll(filepath.Join(destDir, "recipes"), 0755); err != nil {
		return err
	}

	// Download customize script
	fmt.Printf("Downloading %s customize script...\n", platform)
	customizeURL := fmt.Sprintf("%s/%s/postinstall/customize", rawBase, platform)
	if err := downloadFile(customizeURL, filepath.Join(destDir, "customize")); err != nil {
		return fmt.Errorf("failed to download customize script: %w", err)
	}
	os.Chmod(filepath.Join(destDir, "customize"), 0755)
	fmt.Println("Customize script installed")

	// Download shared recipes
	fmt.Println("Downloading shared recipes...")
	recipes := []string{"add-spacemacs.sh", "add-development.sh", "add-fonts.sh"}
	for _, recipe := range recipes {
		recipeURL := fmt.Sprintf("%s/postinstall/recipes/%s", rawBase, recipe)
		recipePath := filepath.Join(destDir, "recipes", recipe)
		if err := downloadFile(recipeURL, recipePath); err != nil {
			fmt.Printf("Skipped %s\n", recipe)
			continue
		}
		os.Chmod(recipePath, 0755)
		fmt.Printf("Downloaded %s\n", recipe)
	}

	// Create README
	readmeContent := `# Guix System Customization Tools

This directory contains tools to customize your minimal Guix installation.

## Quick Start

After first boot, run:

    cd ~/guix-customize
    ./customize

This will launch an interactive menu to add:
- SSH service (critical for VPS!)
- Desktop environments
- Common packages
- And more...

## Manual Customization

You can also edit /etc/config.scm directly:

    sudo nano /etc/config.scm
    sudo guix system reconfigure /etc/config.scm

## Shared Recipes

The recipes/ directory contains modular scripts:
- add-spacemacs.sh - Install Spacemacs editor
- add-development.sh - Install dev tools (git, vim, gcc, etc.)
- add-fonts.sh - Install programming and system fonts

Run them individually:

    ./recipes/add-spacemacs.sh

## Documentation

For more examples and detailed guides, see:
https://github.com/durantschoon/cloudzy-guix-install/blob/main/CUSTOMIZATION.md
`

	if err := os.WriteFile(filepath.Join(destDir, "README.txt"), []byte(readmeContent), 0644); err != nil {
		return err
	}

	fmt.Println()
	fmt.Println("Customization tools installed to /root/guix-customize/")
	fmt.Println("  After first boot, run: cd ~/guix-customize && ./customize")
	fmt.Println()

	return nil
}

func downloadFile(url, dest string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return fmt.Errorf("HTTP %d", resp.StatusCode)
	}

	out, err := os.Create(dest)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, resp.Body)
	return err
}
