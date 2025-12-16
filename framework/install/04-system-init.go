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
	lib.PrintStepHeader(4, "System Initialization (Final Step)")
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
	fmt.Printf("  SWAP_SIZE       - %s (default: 4G)\n", lib.GetEnvOrDefault(state.SwapSize, "4G"))
	fmt.Printf("  RUN_GUIX_PULL   - %s (default: not set, skip pull)\n", lib.GetEnvOrDefault(os.Getenv("RUN_GUIX_PULL"), "not set"))
	fmt.Printf("  GUIX_GIT_URL    - %s\n", lib.GetEnv("GUIX_GIT_URL", "https://git.savannah.gnu.org/git/guix.git"))
	fmt.Printf("  GUIX_VERSION    - %s\n", lib.GetEnv("GUIX_VERSION", "v1.4.0"))
	fmt.Println()
	fmt.Println("Estimated time: 5-10 minutes (or 15-30 min if RUN_GUIX_PULL is set)")
	fmt.Println()
	fmt.Println("After reboot, you'll have a minimal bootable Guix system.")
	fmt.Println("Use ~/guix-customize/customize to add SSH, desktop, packages, etc.")
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

	// Verify EFI is properly mounted as vfat
	if err := lib.VerifyESP(); err != nil {
		return err
	}

	// Start cow-store to redirect store writes to target disk
	if err := lib.StartCowStore(); err != nil {
		return err
	}

	// Write helper scripts for manual recovery
	// 1. Simple time-machine retry script
	helperPath := "/root/guix-init-time-machine.sh"
	if err := lib.WriteTimeMachineHelperScript(helperPath); err != nil {
		fmt.Printf("Warning: Failed to write helper script %s: %v\n", helperPath, err)
	}

	// 2. Comprehensive recovery script (for post-time-machine completion)
	recoveryPath := "/root/recovery-complete-install.sh"
	if err := lib.WriteRecoveryScript(recoveryPath, state.GuixPlatform); err == nil {
		fmt.Printf("Recovery script written: %s\n", recoveryPath)
		fmt.Println("If time-machine succeeds but post-install steps fail, run:")
		fmt.Printf("  %s\n", recoveryPath)
	} else {
		fmt.Printf("Warning: Failed to write recovery script %s: %v\n", recoveryPath, err)
	}

	// Run guix system init with retry logic (includes daemon startup)
	if err := lib.RunGuixSystemInit(); err != nil {
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  SYSTEM INIT FAILED")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Printf("Error: %v\n", err)
		fmt.Println()
		fmt.Println("You can try to recover by running:")
		fmt.Printf("  %s\n", recoveryPath)
		fmt.Println()
		fmt.Println("Or verify what succeeded:")
		fmt.Println("  /root/verify-guix-install.sh")
		fmt.Println()
		return err
	}

	// Verify and recover kernel/initrd files with auto-retry
	if err := lib.VerifyAndRecoverKernelFiles(3); err != nil {
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  KERNEL/INITRD VERIFICATION FAILED")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Printf("Error: %v\n", err)
		fmt.Println()
		fmt.Println("CRITICAL: System will not boot without kernel/initrd files!")
		fmt.Println()
		fmt.Println("To recover, run:")
		fmt.Printf("  %s\n", recoveryPath)
		fmt.Println()
		fmt.Println("This will:")
		fmt.Println("  1. Attempt to copy kernel/initrd from system generation")
		fmt.Println("  2. Complete remaining installation steps")
		fmt.Println("  3. Verify everything before reboot")
		fmt.Println()
		return err
	}

	// Install verification script (for use after boot and by recovery script)
	if err := lib.InstallVerificationScript(); err != nil {
		fmt.Printf("Warning: Failed to install verification script: %v\n", err)
	}

	// Verify installation succeeded
	if err := lib.VerifyInstallation(); err != nil {
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  INSTALLATION VERIFICATION FAILED")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Printf("Error: %v\n", err)
		fmt.Println()
		fmt.Println("To complete installation, run:")
		fmt.Printf("  %s\n", recoveryPath)
		fmt.Println()
		return err
	}

	// Set user password
	if err := lib.SetUserPassword(state.UserName); err != nil {
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  PASSWORD SETUP FAILED")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Printf("Error: %v\n", err)
		fmt.Println()
		fmt.Println("You can set the password later by running:")
		fmt.Printf("  %s\n", recoveryPath)
		fmt.Println()
		fmt.Println("Or manually after reboot with:")
		fmt.Println("  sudo passwd " + state.UserName)
		fmt.Println()
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
	fmt.Println("========================================")
	fmt.Println("  INSTALLATION COMPLETE!")
	fmt.Println("========================================")
	fmt.Println()
	fmt.Println("Your Guix system is ready to boot.")
	fmt.Println()
	fmt.Println("Helpful scripts available on the ISO:")
	fmt.Println("  - Verify installation: /root/verify-guix-install.sh")
	fmt.Println("  - Recovery/completion: " + recoveryPath)
	fmt.Println("  - Filesystem invariants: lib/enforce-guix-filesystem-invariants.sh")
	fmt.Println()
	fmt.Println("After first boot:")
	fmt.Println("  - Verification: sudo verify-guix-install (installed in /usr/local/bin)")
	fmt.Println("  - Customization: ~/guix-customize/customize")
	fmt.Println()
	fmt.Println("System will reboot now...")
	fmt.Println()

	lib.RunCommand("reboot")

	return nil
}
