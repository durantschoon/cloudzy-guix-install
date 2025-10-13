package install

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/durantschoon/cloudzy-guix-install/lib"
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
	fmt.Println("  4. Run 'guix system init /mnt/etc/config.scm /mnt'")
	fmt.Println("  5. Set user password for first login")
	fmt.Println("  6. Download customization tools to user's home directory")
	fmt.Println("  7. Unmount all partitions")
	fmt.Println("  8. Reboot into your new Guix system")
	fmt.Println()
	fmt.Println("Environment variables used by this step:")
	fmt.Printf("  SWAP_SIZE       - %s (default: 4G)\n", lib.GetEnvOrDefault(state.SwapSize, "4G"))
	fmt.Printf("  RUN_GUIX_PULL   - %s (default: not set, skip pull)\n", lib.GetEnvOrDefault(os.Getenv("RUN_GUIX_PULL"), "not set"))
	fmt.Printf("  GUIX_GIT_URL    - %s\n", lib.GetEnv("GUIX_GIT_URL", "https://git.savannah.gnu.org/git/guix.git"))
	fmt.Printf("  GUIX_VERSION    - %s\n", lib.GetEnv("GUIX_VERSION", "v1.4.0"))
	fmt.Println()
	fmt.Println("Estimated time: 5-10 minutes (or 15-30 min if RUN_GUIX_PULL is set)")
	fmt.Println()
	fmt.Println("After reboot:")
	fmt.Println("  1. Log in with your username and password")
	fmt.Println("  2. Run: ~/guix-customize/customize")
	fmt.Println("  3. Add SSH, desktop, packages, etc.")
	fmt.Println()

	return nil
}

func (s *Step04SystemInit) RunClean(state *State) error {
    // Enable command logging
    lib.EnableCommandLogging("/tmp/guix-install.log")
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
	if err := lib.CreateSwapFile(state.SwapSize); err != nil {
		return err
	}

	// guix pull is now a post-install step (run after first boot)
	// The ISO version is good enough to install the system
	// To enable during install: set RUN_GUIX_PULL=1 (not recommended, takes 10-30 min)
	runPull := lib.GetEnv("RUN_GUIX_PULL", "")
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
		guixGitURL := lib.GetEnv("GUIX_GIT_URL", "https://git.savannah.gnu.org/git/guix.git")
		guixVersion := lib.GetEnv("GUIX_VERSION", "v1.4.0")

		fmt.Printf("Pulling Guix from configured mirror: %s\n", guixGitURL)
		if err := lib.RunCommand("guix", "pull", "--url="+guixGitURL, "--commit="+guixVersion); err != nil {
			return fmt.Errorf("guix pull failed: %w", err)
		}
	} else {
		fmt.Println()
		fmt.Println("Skipping guix pull - this is now a post-install step")
		fmt.Println("After first boot, run: guix pull && guix package -u")
		fmt.Println()
	}

	// Verify ESP is properly mounted as vfat
	if err := lib.VerifyESP(); err != nil {
		return err
	}

	// Start cow-store to redirect store writes to target disk
	if err := lib.StartCowStore(); err != nil {
		return err
	}

	// Ensure guix-daemon is running with bind mounts
	if err := lib.EnsureGuixDaemonRunning(); err != nil {
		return err
	}

	// Run guix system init with retry logic
	if err := lib.RunGuixSystemInit(); err != nil {
		return err
	}

	// Verify installation succeeded
	if err := lib.VerifyInstallation(); err != nil {
		return err
	}

	// Set user password
	if err := lib.SetUserPassword(state.UserName); err != nil {
		return err
	}

	// Download customization tools to user's home directory
	if err := lib.DownloadCustomizationTools(state.GuixPlatform, state.UserName); err != nil {
		fmt.Printf("Warning: Failed to download customization tools: %v\n", err)
		fmt.Println("  You can manually download them after first boot")
	}

    // Write installation receipt and logs
    if err := lib.WriteInstallReceipt(state.GuixPlatform, state.UserName); err != nil {
        fmt.Printf("Warning: Failed to write installation receipt: %v\n", err)
    }

    // Sync and unmount
	fmt.Println("Syncing filesystems...")
	lib.RunCommand("sync")

	fmt.Println("Unmounting /mnt...")
	lib.RunCommand("umount", "-R", "/mnt")

	fmt.Println()
	fmt.Println("=== Installation Complete ===")
	fmt.Println("System will reboot now...")
	fmt.Println()

	lib.RunCommand("reboot")

	return nil
}
