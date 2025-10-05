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
)

// Step04SystemInit performs the final system initialization
type Step04SystemInit struct{}

func (s *Step04SystemInit) RunWarnings(state *State) error {
	fmt.Println("=== System Initialization ===")
	fmt.Println("This is the final step that will:")
	fmt.Println("  1. Set up swap file")
	fmt.Println("  2. Pull Guix from configured mirror")
	fmt.Println("  3. Download customization tools")
	fmt.Println("  4. Run 'guix system init'")
	fmt.Println("  5. Reboot into the new system")
	fmt.Println()
	fmt.Println("This step will take 10-30 minutes depending on network speed.")
	fmt.Println()

	return nil
}

func (s *Step04SystemInit) RunClean(state *State) error {
	// Set up temporary directory
	tmpDir := "/mnt/var/tmp"
	if err := os.MkdirAll(tmpDir, 0777); err != nil {
		return err
	}
	os.Setenv("TMPDIR", tmpDir)

	// Clear substitute cache
	exec.Command("rm", "-rf", "/var/guix/substitute-cache/").Run()

	// Create swap file
	if err := s.createSwapFile(state); err != nil {
		return err
	}

	// Optional: guix pull (can skip to save time, do it after first boot)
	// Set SKIP_GUIX_PULL=1 to skip this step
	skipPull := getEnv("SKIP_GUIX_PULL", "")
	if skipPull != "" {
		fmt.Println("Skipping guix pull (SKIP_GUIX_PULL is set)")
		fmt.Println("You can run 'guix pull' after first boot")
	} else {
		fmt.Println()
		fmt.Println("=== Guix Pull ===")
		fmt.Println("Updating Guix to latest version (takes 10-30 minutes)...")
		fmt.Println("To skip: set SKIP_GUIX_PULL=1 before running installer")
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
	}

	// Download customization tools
	if err := s.downloadCustomizationTools(state); err != nil {
		fmt.Printf("Warning: Failed to download customization tools: %v\n", err)
		fmt.Println("  You can manually download them after first boot")
	}

	// Run guix system init
	fmt.Println()
	fmt.Println("=== Running guix system init ===")
	fmt.Println("This will take several minutes...")
	if err := runCommand("guix", "system", "init", "/mnt/etc/config.scm", "/mnt"); err != nil {
		return fmt.Errorf("guix system init failed: %w", err)
	}

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
