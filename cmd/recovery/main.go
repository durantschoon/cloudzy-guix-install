package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

const maxRetryAttempts = 3

func main() {
	retryCount := 0
	if retryEnv := os.Getenv("RECOVERY_RETRY_COUNT"); retryEnv != "" {
		fmt.Sscanf(retryEnv, "%d", &retryCount)
	}

	if retryCount > 0 {
		fmt.Printf("Automatically continuing (retry attempt %d/%d)...\n", retryCount, maxRetryAttempts)
		fmt.Println()
	} else {
		fmt.Println("========================================")
		fmt.Println("  Guix Installation Recovery")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Println("This tool will complete or retry installation steps.")
		fmt.Println()
		fmt.Print("Continue? [Y/n] ")
		var response string
		fmt.Scanln(&response)
		if response == "n" || response == "N" {
			fmt.Println("Recovery cancelled.")
			os.Exit(0)
		}
		fmt.Println()
	}

	if err := runRecovery(retryCount); err != nil {
		fmt.Printf("\n[ERROR] Recovery failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println()
	fmt.Println("========================================")
	fmt.Println("  RECOVERY COMPLETE")
	fmt.Println("========================================")
	fmt.Println()
	fmt.Println("The system is ready to boot.")
	fmt.Println()
	fmt.Println("Next steps:")
	fmt.Println("  1. Sync and unmount:")
	fmt.Println("     sync")
	fmt.Println("     umount -R /mnt")
	fmt.Println("  2. Reboot:")
	fmt.Println("     reboot")
	fmt.Println()
}

func runRecovery(retryCount int) error {
	// Step 1: Verify mounts
	if err := verifyMounts(); err != nil {
		return fmt.Errorf("mount verification failed: %w", err)
	}

	// Step 2: Verify config exists
	if err := verifyConfig(); err != nil {
		return fmt.Errorf("config verification failed: %w", err)
	}

	// Step 3: Detect platform
	platform := detectPlatform()

	// Step 4: Check installation state
	state, err := checkInstallationState()
	if err != nil {
		return fmt.Errorf("failed to check installation state: %w", err)
	}

	// Step 5: Run system init if needed
	if state.NeedSystemInit {
		if err := runSystemInit(platform); err != nil {
			return fmt.Errorf("system init failed: %w", err)
		}
	} else {
		fmt.Println()
		fmt.Println("[OK] System init already complete - skipping")
	}

	// Step 6: Set user password (CRITICAL - must always run)
	fmt.Println()
	fmt.Println("========================================")
	fmt.Println("  SETTING USER PASSWORD")
	fmt.Println("========================================")
	fmt.Println()
	username, err := detectUsername()
	if err != nil {
		fmt.Printf("[ERROR] Failed to detect username: %v\n", err)
		fmt.Println("        Password setup will be skipped.")
		fmt.Println("        You can set it manually after first boot with:")
		fmt.Println("          sudo passwd <username>")
		fmt.Println()
		// Don't fail the entire recovery - password can be set manually
	} else {
		fmt.Printf("Detected username: %s\n", username)
		fmt.Println("You will be prompted to enter a password for this user.")
		fmt.Println("This password is required to log in after first boot.")
		fmt.Println()
		
		if err := lib.SetUserPassword(username); err != nil {
			fmt.Printf("[ERROR] Failed to set password: %v\n", err)
			fmt.Println("        You can set it manually after first boot with:")
			fmt.Printf("          sudo passwd %s\n", username)
			fmt.Println()
			// Don't fail the entire recovery - password can be set manually
		} else {
			fmt.Printf("[OK] Password set successfully for user: %s\n", username)
			fmt.Println()
		}
	}

	// Step 7: Download customization tools
	if err := lib.DownloadCustomizationTools(platform, username); err != nil {
		fmt.Printf("[WARN] Failed to download customization tools: %v\n", err)
		fmt.Println("You can download manually after first boot.")
	}

	// Step 8: Configure dual-boot GRUB (if framework-dual)
	if platform == "framework-dual" {
		if err := configureDualBootGRUB(); err != nil {
			fmt.Printf("[WARN] Failed to configure dual-boot GRUB: %v\n", err)
			fmt.Println("You can configure it manually after first boot.")
		}
	}

	// Step 9: Write installation receipt
	if err := lib.WriteInstallReceipt(platform, username); err != nil {
		fmt.Printf("[WARN] Failed to write installation receipt: %v\n", err)
	}

	// Step 10: Final verification
	if err := runFinalVerification(retryCount); err != nil {
		return err
	}

	return nil
}

type installationState struct {
	HasKernel   bool
	HasInitrd   bool
	HasGRUBEFI  bool
	NeedSystemInit bool
}

func verifyMounts() error {
	fmt.Println()
	fmt.Println("=== Verifying Mounts ===")

	if !lib.IsMounted("/mnt") {
		return fmt.Errorf("/mnt is not mounted! Please run: mount LABEL=GUIX_ROOT /mnt")
	}
	fmt.Println("[OK] /mnt is mounted")

	// Check if EFI partition should be mounted (UEFI systems)
	bootMode := lib.DetectBootModeFromConfig("/mnt/etc/config.scm")
	if bootMode == "uefi" {
		if !lib.IsMounted("/mnt/boot/efi") {
			return fmt.Errorf("/mnt/boot/efi is not mounted! Please run: mount LABEL=EFI /mnt/boot/efi")
		}
		fmt.Println("[OK] /mnt/boot/efi is mounted")
	}

	return nil
}

func verifyConfig() error {
	fmt.Println()
	fmt.Println("=== Verifying Config ===")

	if _, err := os.Stat("/mnt/etc/config.scm"); os.IsNotExist(err) {
		return fmt.Errorf("/mnt/etc/config.scm not found! Please generate config first (step 3)")
	}
	fmt.Println("[OK] Config exists: /mnt/etc/config.scm")

	return nil
}

func detectPlatform() string {
	// Use environment variable if set
	if platform := os.Getenv("GUIX_PLATFORM"); platform != "" {
		return platform
	}

	// Check for channels.scm (framework-dual/framework use nonguix)
	channelsPath := lib.GetChannelsPath()
	if channelsPath != "" {
		return "framework-dual"
	}

	// Check for dual-boot (GUIX_ROOT filesystem with separate EFI)
	if lib.IsMounted("/mnt") {
		if lib.IsMounted("/mnt/boot/efi") {
			// Check if this looks like a dual-boot setup
			if _, err := os.Stat("/mnt/etc/config.scm"); err == nil {
				content, _ := os.ReadFile("/mnt/etc/config.scm")
				if strings.Contains(string(content), "os-prober") {
					return "framework-dual"
				}
			}
		}
	}

	// Check for VPS vendors (cloudzy)
	if vendor, err := os.ReadFile("/sys/class/dmi/id/sys_vendor"); err == nil {
		vendorStr := strings.ToLower(string(vendor))
		if strings.Contains(vendorStr, "cloudzy") || strings.Contains(vendorStr, "ovh") ||
			strings.Contains(vendorStr, "digitalocean") || strings.Contains(vendorStr, "vultr") {
			return "cloudzy"
		}
	}

	// Default to cloudzy (most common VPS case)
	return "cloudzy"
}

func checkInstallationState() (*installationState, error) {
	fmt.Println()
	fmt.Println("=== Checking Installation State ===")

	state := &installationState{}

	// Check for kernel files
	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
	if len(kernels) > 0 {
		fmt.Printf("[OK] Kernel found: %s\n", filepath.Base(kernels[0]))
		state.HasKernel = true
	} else {
		fmt.Println("[MISSING] No kernel in /mnt/boot/")
	}

	// Check for initrd files
	initrds, _ := filepath.Glob("/mnt/boot/initrd*")
	if len(initrds) > 0 {
		fmt.Printf("[OK] Initrd found: %s\n", filepath.Base(initrds[0]))
		state.HasInitrd = true
	} else {
		fmt.Println("[MISSING] No initrd in /mnt/boot/")
	}

	// Check for GRUB EFI bootloader (only for UEFI systems)
	bootMode := lib.DetectBootModeFromConfig("/mnt/etc/config.scm")
	if bootMode == "uefi" {
		if _, err := os.Stat("/mnt/boot/efi/EFI/Guix/grubx64.efi"); err == nil {
			fmt.Println("[OK] GRUB EFI bootloader found")
			state.HasGRUBEFI = true
		} else if _, err := os.Stat("/mnt/boot/efi/EFI/guix/grubx64.efi"); err == nil {
			fmt.Println("[OK] GRUB EFI bootloader found")
			state.HasGRUBEFI = true
		} else {
			fmt.Println("[MISSING] No GRUB EFI bootloader")
		}
	}

	// Determine if we need to run system init
	if !state.HasKernel || !state.HasInitrd {
		fmt.Println()
		fmt.Println("[!] Installation is INCOMPLETE - system init must be run")
		state.NeedSystemInit = true
	}

	return state, nil
}

func runSystemInit(platform string) error {
	fmt.Println()
	fmt.Println("=== Running System Init ===")

	// Setup environment
	if err := setupEnvironment(); err != nil {
		return fmt.Errorf("failed to setup environment: %w", err)
	}

	// Verify ESP before init (for UEFI systems)
	bootMode := lib.DetectBootModeFromConfig("/mnt/etc/config.scm")
	if bootMode == "uefi" {
		if err := lib.VerifyESP(); err != nil {
			return fmt.Errorf("ESP verification failed: %w", err)
		}
	}

	// Start cow-store
	if err := lib.StartCowStore(); err != nil {
		return fmt.Errorf("failed to start cow-store: %w", err)
	}

	// Ensure guix-daemon is responsive
	if err := lib.EnsureGuixDaemonRunning(); err != nil {
		return fmt.Errorf("failed to ensure guix-daemon is running: %w", err)
	}

	// Check if we should use time-machine (has channels.scm)
	channelsPath := lib.GetChannelsPath()
	useTimeMachine := channelsPath != ""

	// Platform is already provided as parameter for tracking purposes

	if useTimeMachine {
		// Use time-machine path (framework-dual)
		if err := lib.RunGuixSystemInit(platform); err != nil {
			return fmt.Errorf("guix time-machine system init failed: %w", err)
		}
	} else {
		// Use free-software path (cloudzy)
		if err := lib.RunGuixSystemInitFreeSoftware(platform); err != nil {
			return fmt.Errorf("guix system init failed: %w", err)
		}
	}

	return nil
}

func setupEnvironment() error {
	fmt.Println()
	fmt.Println("=== Setting Up Environment ===")

	os.Setenv("TMPDIR", "/mnt/var/tmp")
	os.Setenv("XDG_CACHE_HOME", "/mnt/var/cache")

	if err := os.MkdirAll("/mnt/var/tmp", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/var/tmp: %w", err)
	}
	if err := os.MkdirAll("/mnt/var/cache", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/var/cache: %w", err)
	}

	fmt.Println("TMPDIR=/mnt/var/tmp")
	fmt.Println("XDG_CACHE_HOME=/mnt/var/cache")

	// Clear substitute cache to free space
	fmt.Println("Clearing substitute cache...")
	os.RemoveAll("/var/guix/substitute-cache/")

	return nil
}

func detectUsername() (string, error) {
	fmt.Println()
	fmt.Println("=== Detecting Username ===")

	configPath := "/mnt/etc/config.scm"
	content, err := os.ReadFile(configPath)
	if err != nil {
		return "", fmt.Errorf("failed to read config.scm: %w", err)
	}

	// Simple regex-like extraction: look for (name "username")
	lines := strings.Split(string(content), "\n")
	for _, line := range lines {
		if strings.Contains(line, "(name") {
			// Extract username from line like: (name "username")
			start := strings.Index(line, `"`)
			if start != -1 {
				end := strings.Index(line[start+1:], `"`)
				if end != -1 {
					username := line[start+1 : start+1+end]
					fmt.Printf("Username: %s\n", username)
					return username, nil
				}
			}
		}
	}

	// Fallback: prompt user
	fmt.Print("Could not detect username from config.scm. Enter your username: ")
	var username string
	fmt.Scanln(&username)
	return username, nil
}

func configureDualBootGRUB() error {
	fmt.Println()
	fmt.Println("=== Configuring Dual-Boot GRUB ===")

	// Check if os-prober is available in the installed system
	cmd := exec.Command("chroot", "/mnt", "/run/current-system/profile/bin/bash", "-c", "command -v os-prober")
	if err := cmd.Run(); err != nil {
		fmt.Println("[WARN] os-prober not found in installed system")
		fmt.Println("You'll need to manually configure dual-boot after first boot")
		return nil
	}

	fmt.Println("Enabling os-prober...")
	cmd = exec.Command("chroot", "/mnt", "/run/current-system/profile/bin/bash", "-c",
		`echo "GRUB_DISABLE_OS_PROBER=false" >> /etc/default/grub`)
	cmd.Run() // Ignore errors

	fmt.Println("Running os-prober to detect Pop!_OS...")
	cmd = exec.Command("chroot", "/mnt", "/run/current-system/profile/bin/os-prober")
	cmd.Run() // Ignore errors

	fmt.Println("Updating GRUB configuration...")
	cmd = exec.Command("chroot", "/mnt", "/run/current-system/profile/bin/grub-mkconfig", "-o", "/boot/grub/grub.cfg")
	if err := cmd.Run(); err != nil {
		fmt.Println("[WARN] Failed to update GRUB config")
		fmt.Println("You can run this after first boot:")
		fmt.Println("  sudo os-prober && sudo grub-mkconfig -o /boot/grub/grub.cfg")
		return nil
	}

	fmt.Println("[OK] Dual-boot GRUB configured")
	return nil
}

func runFinalVerification(retryCount int) error {
	fmt.Println()
	fmt.Println("========================================")
	fmt.Println("  FINAL VERIFICATION")
	fmt.Println("========================================")
	fmt.Println()

	// Always verify kernel/initrd are present (critical check)
	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
	initrds, _ := filepath.Glob("/mnt/boot/initrd*")

	kernelMissing := len(kernels) == 0
	initrdMissing := len(initrds) == 0

	if !kernelMissing {
		fmt.Printf("[OK] Kernel present: %s\n", filepath.Base(kernels[0]))
	} else {
		fmt.Println("[ERROR] Kernel file missing from /mnt/boot/")
	}

	if !initrdMissing {
		fmt.Printf("[OK] Initrd present: %s\n", filepath.Base(initrds[0]))
	} else {
		fmt.Println("[ERROR] Initrd file missing from /mnt/boot/")
	}

	if kernelMissing || initrdMissing {
		fmt.Println()
		fmt.Println("CRITICAL: Kernel or initrd files are missing!")
		fmt.Println("The system will NOT boot without these files.")
		fmt.Println()

		// Check if we've exceeded retry limit
		if retryCount >= maxRetryAttempts {
			fmt.Println("========================================")
			fmt.Printf("  RECOVERY FAILED AFTER %d ATTEMPTS\n", maxRetryAttempts)
			fmt.Println("========================================")
			fmt.Println()
			fmt.Println("The recovery script has been run", maxRetryAttempts, "times but kernel/initrd files")
			fmt.Println("are still missing. Manual intervention is required.")
			fmt.Println()
			fmt.Println("Next steps:")
			fmt.Println("  1. Check logs: cat /tmp/guix-install.log | tail -100")
			fmt.Println("  2. Verify config.scm has kernel specified: grep -A 5 kernel /mnt/etc/config.scm")
			fmt.Println("  3. Check system generation: ls -la /gnu/store/*-system | head -5")
			fmt.Println("  4. Try manual recovery:")
			fmt.Println("     SYSTEM=$(ls -td /gnu/store/*-system | head -1)")
			fmt.Println("     cp -L $SYSTEM/kernel /mnt/boot/vmlinuz")
			fmt.Println("     cp -L $SYSTEM/initrd /mnt/boot/initrd")
			fmt.Println()
			return fmt.Errorf("recovery failed after %d attempts", maxRetryAttempts)
		}

		// Increment retry counter and automatically retry
		retryCount++
		fmt.Printf("Automatically retrying recovery (attempt %d/%d)...\n", retryCount, maxRetryAttempts)
		fmt.Println()

		// Re-execute this program with incremented retry count
		os.Setenv("RECOVERY_RETRY_COUNT", fmt.Sprintf("%d", retryCount))
		executable, _ := os.Executable()
		cmd := exec.Command(executable)
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("retry failed: %w", err)
		}
		return nil
	}

	// Run comprehensive verification script if available
	verifyScript := "/root/verify-guix-install.sh"
	if _, err := os.Stat(verifyScript); err == nil {
		fmt.Println("Running comprehensive verification script...")
		cmd := exec.Command(verifyScript)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Println()
			fmt.Println("[WARN] Comprehensive verification found issues")
			fmt.Println("Please review the output above")
			// Don't fail here - kernel/initrd are present, which is the critical check
		} else {
			fmt.Println("[OK] Comprehensive verification passed")
		}
	}

	fmt.Println()
	fmt.Println("[OK] Final verification passed - system is ready to boot")
	return nil
}

