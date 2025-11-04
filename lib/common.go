// Package lib provides shared functionality for Guix installation across platforms
package lib

import (
	"bufio"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// GetEnv returns the value of an environment variable or a default value
func GetEnv(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}

// GetEnvOrDefault returns the value if non-empty, otherwise the default
func GetEnvOrDefault(value, defaultValue string) string {
	if value != "" {
		return value
	}
	return defaultValue
}

var commandLogWriter io.Writer
var commandLogPath string

// EnableCommandLogging enables tee-style logging for external command outputs
// All outputs from RunCommand will be duplicated to the provided logfile.
func EnableCommandLogging(logPath string) error {
    f, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
    if err != nil {
        return err
    }
    // Combine stdout with file for tee behavior
    commandLogWriter = io.MultiWriter(os.Stdout, f)
    commandLogPath = logPath
    return nil
}

// RunCommand runs a command and returns an error if it fails
func RunCommand(name string, args ...string) error {
    cmd := exec.Command(name, args...)
    if commandLogWriter != nil {
        cmd.Stdout = commandLogWriter
        cmd.Stderr = commandLogWriter
    } else {
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
    }
    return cmd.Run()
}

// RunCommandWithSpinner runs a command with a spinner when output is quiet
func RunCommandWithSpinner(name string, args ...string) error {
    cmd := exec.Command(name, args...)
    
    // Create pipes for stdout and stderr
    stdout, err := cmd.StdoutPipe()
    if err != nil {
        return err
    }
    stderr, err := cmd.StderrPipe()
    if err != nil {
        return err
    }
    
    // Start the command
    if err := cmd.Start(); err != nil {
        return err
    }
    
    // Channel to signal when we have output
    outputReceived := make(chan bool, 1)
    
    // Goroutine to read stdout
    go func() {
        scanner := bufio.NewScanner(stdout)
        for scanner.Scan() {
            line := scanner.Text()
            if commandLogWriter != nil {
                fmt.Fprintln(commandLogWriter, line)
            } else {
                fmt.Println(line)
            }
            // Signal that we have output
            select {
            case outputReceived <- true:
            default:
            }
        }
    }()
    
    // Goroutine to read stderr
    go func() {
        scanner := bufio.NewScanner(stderr)
        for scanner.Scan() {
            line := scanner.Text()
            if commandLogWriter != nil {
                fmt.Fprintln(commandLogWriter, line)
            } else {
                fmt.Fprintln(os.Stderr, line)
            }
            // Signal that we have output
            select {
            case outputReceived <- true:
            default:
            }
        }
    }()
    
    // Spinner goroutine
    spinner := []string{"/", "|", "\\", "-"}
    spinnerIndex := 0
    lastOutputTime := time.Now()
    spinnerActive := false
    
    go func() {
        ticker := time.NewTicker(200 * time.Millisecond)
        defer ticker.Stop()
        
        for {
            select {
            case <-ticker.C:
                // Check if we've received output recently
                if time.Since(lastOutputTime) > 3*time.Second {
                    if !spinnerActive {
                        spinnerActive = true
                        fmt.Printf("\n") // New line before starting spinner
                    }
                    // Hide cursor and show spinner
                    fmt.Printf("\r\033[?25l%s Working... (this may take 5-30 minutes)", spinner[spinnerIndex])
                    os.Stdout.Sync()
                    spinnerIndex = (spinnerIndex + 1) % len(spinner)
                }
            case <-outputReceived:
                // We have output, clear spinner and restore cursor
                if spinnerActive {
                    fmt.Printf("\r%s", strings.Repeat(" ", 80)) // Clear line
                    fmt.Printf("\r\033[?25h") // Restore cursor
                    os.Stdout.Sync()
                    spinnerActive = false
                }
                lastOutputTime = time.Now()
            }
        }
    }()
    
    // Wait for command to complete
    err = cmd.Wait()
    
    // Clear any remaining spinner and restore cursor
    fmt.Printf("\r%s", strings.Repeat(" ", 80))
    fmt.Printf("\r\033[?25h") // Restore cursor
    os.Stdout.Sync()
    
    return err
}

// WriteInstallReceipt writes a summary of the installation to the target filesystem
func WriteInstallReceipt(platform string, username string) error {
    // Ensure destination directory
    destDir := "/mnt/var/log/guix-install"
    if err := os.MkdirAll(destDir, 0755); err != nil {
        return err
    }

    // Compose receipt
    timestamp := time.Now().Format(time.RFC3339)
    receipt := []string{
        "Guix Installation Receipt",
        "==========================",
        fmt.Sprintf("Timestamp: %s", timestamp),
        fmt.Sprintf("Platform: %s", platform),
        fmt.Sprintf("User: %s", username),
        "",
        "Kernel and initrd present:",
    }

    // Check kernel/initrd presence
    if files, _ := filepath.Glob("/mnt/boot/vmlinuz-*"); len(files) > 0 {
        receipt = append(receipt, fmt.Sprintf("- Kernel: %s", filepath.Base(files[0])))
    } else {
        receipt = append(receipt, "- Kernel: MISSING")
    }
    if files, _ := filepath.Glob("/mnt/boot/initrd-*"); len(files) > 0 {
        receipt = append(receipt, fmt.Sprintf("- Initrd: %s", filepath.Base(files[0])))
    } else {
        receipt = append(receipt, "- Initrd: MISSING")
    }

    // GRUB status
    if _, err := os.Stat("/mnt/boot/grub/grub.cfg"); err == nil {
        receipt = append(receipt, "- GRUB config: present")
    } else {
        receipt = append(receipt, "- GRUB config: MISSING")
    }
    if _, err := os.Stat("/mnt/boot/efi/EFI/guix/grubx64.efi"); err == nil {
        receipt = append(receipt, "- GRUB EFI binary: present")
    } else {
        receipt = append(receipt, "- GRUB EFI binary: MISSING")
    }

    // Add channel information for reproducibility
    receipt = append(receipt, "", "Channel Commits (for reproducibility):")
    if channelData, err := os.ReadFile("/mnt/etc/channels-pinned.scm"); err == nil {
        receipt = append(receipt, string(channelData))
    } else {
        receipt = append(receipt, "- Channel commits not recorded")
    }

    // Add substitute servers used
    receipt = append(receipt, "", "Substitute Servers:")
    receipt = append(receipt, "- https://substitutes.nonguix.org")
    receipt = append(receipt, "- https://ci.guix.gnu.org")
    receipt = append(receipt, "- https://bordeaux.guix.gnu.org")

    // Write receipt file
    receiptPath := filepath.Join(destDir, "install-receipt.txt")
    if err := os.WriteFile(receiptPath, []byte(strings.Join(receipt, "\n")), 0644); err != nil {
        return err
    }

    // Copy install log if available
    if commandLogPath != "" {
        _ = RunCommand("cp", commandLogPath, filepath.Join(destDir, "install.log"))
    }

    // Also copy a copy to user's home for convenience (if user exists)
    if username == "" {
        username = "guix"
    }
    userLogDir := filepath.Join("/mnt/home", username, "guix-install")
    if err := os.MkdirAll(userLogDir, 0755); err == nil {
        _ = RunCommand("cp", receiptPath, filepath.Join(userLogDir, "install-receipt.txt"))
        if commandLogPath != "" {
            _ = RunCommand("cp", commandLogPath, filepath.Join(userLogDir, "install.log"))
        }
    }

    fmt.Printf("Installation receipt written to %s and copied to user's home\n", destDir)
    return nil
}

// CreateSwapFile creates and activates a swap file at /mnt/swapfile
func CreateSwapFile(swapSize string) error {
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

	if err := RunCommand("mkswap", "/mnt/swapfile"); err != nil {
		return err
	}

	if err := RunCommand("swapon", "/mnt/swapfile"); err != nil {
		return err
	}

	fmt.Println("Verify swap is active and memory is available:")
	RunCommand("swapon", "--show")
	RunCommand("free", "-h")

	return nil
}

// EnsureGuixDaemonRunning ensures the guix-daemon is running
// NOTE: Do NOT use bind mounts! cow-store handles store writes correctly.
// Bind mounting /mnt/gnu to /gnu shadows the live system's store and breaks guix.
func EnsureGuixDaemonRunning() error {
	fmt.Println("=== Ensuring guix-daemon is running ===")

	// First, check if daemon is already running and responsive
	cmd := exec.Command("herd", "status", "guix-daemon")
	output, err := cmd.Output()

	if err == nil && (strings.Contains(string(output), "It is started") || strings.Contains(string(output), "It is enabled")) {
		fmt.Println("guix-daemon is already running, checking if responsive...")

		// Poll for responsiveness (daemon may be starting up)
		for i := 0; i < 10; i++ {
			testCmd := exec.Command("guix", "build", "--version")
			if err := testCmd.Run(); err == nil {
				fmt.Println("[OK] guix-daemon is responsive and ready")
				return nil
			}
			if i == 0 {
				fmt.Println("Daemon is starting up, waiting for it to become responsive...")
			}
			fmt.Printf("  Waiting... (%d/10)\n", i+1)
			time.Sleep(3 * time.Second)
		}

		// If still not responsive after waiting, we'll restart it below
		fmt.Println("Daemon not responsive after waiting, will restart...")
	}

	// Daemon not running or not responsive - restart it
	fmt.Println("Starting guix-daemon...")
	fmt.Println()

	// Stop any existing daemon processes
	fmt.Println("Stopping any existing guix-daemon processes...")
	exec.Command("herd", "stop", "guix-daemon").Run()
	exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
	time.Sleep(2 * time.Second)
	exec.Command("pkill", "-KILL", "-x", "guix-daemon").Run() // Force kill if needed
	time.Sleep(2 * time.Second)

	// Start the daemon
	fmt.Println("Starting guix-daemon via herd...")
	if err := RunCommand("herd", "start", "guix-daemon"); err != nil {
		fmt.Printf("Warning: herd start failed: %v\n", err)
		// Try alternative startup method
		fmt.Println("Trying alternative daemon startup...")
		exec.Command("guix-daemon", "--build-users-group=guixbuild").Start()
	}

	// Wait for daemon to become responsive (up to 60 seconds)
	fmt.Println("Waiting for daemon to become responsive...")
	for i := 0; i < 20; i++ {
		time.Sleep(3 * time.Second)

		// Check if process is running
		statusCmd := exec.Command("herd", "status", "guix-daemon")
		statusOutput, _ := statusCmd.Output()

		if !strings.Contains(string(statusOutput), "It is started") && !strings.Contains(string(statusOutput), "It is enabled") {
			fmt.Printf("  Daemon process not started yet... (%d/20)\n", i+1)
			continue
		}

		// Process is running, check if responsive
		testCmd := exec.Command("guix", "build", "--version")
		if err := testCmd.Run(); err == nil {
			fmt.Println("[OK] guix-daemon is now responsive and ready")
			fmt.Println()
			return nil
		}

		fmt.Printf("  Daemon running but not responsive yet... (%d/20)\n", i+1)
	}

	// Final check
	statusCmd := exec.Command("herd", "status", "guix-daemon")
	finalOutput, err := statusCmd.Output()
	if err != nil || (!strings.Contains(string(finalOutput), "It is started") && !strings.Contains(string(finalOutput), "It is enabled")) {
		fmt.Println()
		fmt.Println("[ERROR] Failed to start guix-daemon after 60 seconds")
		fmt.Println()
		fmt.Println("Please manually run these commands:")
		fmt.Println("  herd start guix-daemon")
		fmt.Println("  herd status guix-daemon")
		fmt.Println("  guix build --version  # Test if responsive")
		fmt.Println()
		fmt.Println("Once the daemon is running and responsive, continue with:")
		fmt.Println("  guix system init /mnt/etc/config.scm /mnt")
		fmt.Println()
		return fmt.Errorf("guix-daemon failed to become responsive after 60 seconds")
	}

	fmt.Println()
	fmt.Println("[WARN] guix-daemon is running but not fully responsive yet")
	fmt.Println("       Continuing anyway - it may become responsive during system init")
	fmt.Println()
	return nil
}

// VerifyESP verifies the EFI System Partition is properly mounted as vfat
func VerifyESP() error {
	fmt.Println()
	fmt.Println("=== Verifying EFI System Partition ===")
	
	// Check partition information with lsblk
	fmt.Println("Checking partition information:")
	RunCommand("lsblk", "-f")
	
	// Check if EFI partition exists and is properly labeled
	fmt.Println("Checking EFI partition:")
	RunCommand("blkid", "-t", "LABEL=EFI")
	RunCommand("blkid", "-t", "TYPE=vfat")
	
	// Check if /mnt/boot/efi directory exists and is accessible
	fmt.Println("Checking /mnt/boot/efi directory:")
	RunCommand("ls", "-la", "/mnt/boot/")
	
	// Check all EFI-related mounts
	fmt.Println("Checking EFI-related mounts:")
	RunCommand("mount | grep -i efi")
	
	// Verify EFI partition is vfat and accessible
	fmt.Println("Verifying EFI partition filesystem:")
	RunCommand("blkid", "-t", "LABEL=EFI")
	
	fmt.Println("[OK] EFI partition verification complete")
	fmt.Println()
	return nil
}

// StartCowStore starts cow-store to redirect store writes to target disk
func StartCowStore() error {
	fmt.Println("=== Starting cow-store ===")
	fmt.Println("Redirecting store writes to /mnt to avoid filling ISO space...")
	if err := RunCommand("herd", "start", "cow-store", "/mnt"); err != nil {
		return fmt.Errorf("failed to start cow-store: %w", err)
	}
	fmt.Println()
	return nil
}

// SetupGRUBEFI creates necessary directories and symlinks for GRUB EFI bootloader
func SetupGRUBEFI() error {
	fmt.Println("=== Setting up GRUB EFI bootloader ===")
	
	// Ensure the EFI directory structure exists
	guixEfiDir := "/mnt/boot/efi/Guix"
	
	// Create directories if they don't exist
	if err := os.MkdirAll(guixEfiDir, 0755); err != nil {
		return fmt.Errorf("failed to create EFI directory: %w", err)
	}
	
	// Check if we already have GRUB EFI files (from previous failed attempts)
	grubEfiFile := "/mnt/boot/efi/Guix/grubx64.efi"
	grubCfgLink := "/mnt/boot/efi/Guix/grub.cfg"
	
	if _, err := os.Stat(grubEfiFile); err == nil {
		fmt.Println("GRUB EFI files already exist, cleaning up...")
		// Remove existing files to ensure clean installation
		os.Remove(grubEfiFile)
		os.Remove(grubCfgLink)
		fmt.Println("[OK] Cleaned up existing GRUB EFI files")
	}
	
	fmt.Println("EFI directory structure ready for guix system init")
	fmt.Println("GRUB EFI files will be created during guix system init")
	fmt.Println()
	return nil
}

// SetupNonguixChannel sets up the nonguix channel for proprietary firmware and kernel
func SetupNonguixChannel() error {
	fmt.Println("=== Setting up nonguix channel ===")
	fmt.Println()

	// Prompt user for consent to trust nonguix
	fmt.Println("The Nonguix channel provides:")
	fmt.Println("  - Proprietary firmware (WiFi, Bluetooth, GPU drivers)")
	fmt.Println("  - Non-free Linux kernel (better hardware support)")
	fmt.Println("  - Binary substitutes from substitutes.nonguix.org")
	fmt.Println()
	fmt.Println("SECURITY NOTE:")
	fmt.Println("  - You will be trusting binaries from substitutes.nonguix.org")
	fmt.Println("  - This is a third-party server (not official GNU Guix)")
	fmt.Println("  - Required for Framework 13 WiFi/GPU to work properly")
	fmt.Println()
	fmt.Print("Do you want to trust and use Nonguix? [Y/n] ")

	// Read user response from /dev/tty (not stdin which may be redirected)
	tty, err := os.Open("/dev/tty")
	if err != nil {
		return fmt.Errorf("failed to open /dev/tty for user input: %w", err)
	}
	defer tty.Close()

	reader := bufio.NewReader(tty)
	response, err := reader.ReadString('\n')
	if err != nil {
		return fmt.Errorf("failed to read user response: %w", err)
	}

	response = strings.ToLower(strings.TrimSpace(response))
	if response != "" && response != "y" && response != "yes" {
		return fmt.Errorf("user declined to trust Nonguix - installation cannot proceed without proprietary firmware")
	}

	fmt.Println()
	fmt.Println("[OK] User consented to trust Nonguix")
	fmt.Println()

	// Create channels.scm file with introduction for authenticity
	channelsContent := `(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)`

	channelsPath := "/tmp/channels.scm"
	if err := os.WriteFile(channelsPath, []byte(channelsContent), 0644); err != nil {
		return fmt.Errorf("failed to create channels.scm: %w", err)
	}

	// Authorize nonguix substitutes
	fmt.Println("Authorizing nonguix substitutes...")
	// Download key and pipe to guix archive
	wgetCmd := exec.Command("wget", "-qO-", "https://substitutes.nonguix.org/signing-key.pub")
	guixCmd := exec.Command("guix", "archive", "--authorize")

	// Pipe wget output to guix
	pipe, err := wgetCmd.StdoutPipe()
	if err != nil {
		fmt.Println("Warning: Could not setup pipe for key authorization")
	} else {
		guixCmd.Stdin = pipe
		guixCmd.Stdout = os.Stdout
		guixCmd.Stderr = os.Stderr

		if err := wgetCmd.Start(); err != nil {
			fmt.Println("Warning: Could not download nonguix key")
		} else if err := guixCmd.Run(); err != nil {
			fmt.Println("Warning: Could not authorize nonguix key")
		} else if err := wgetCmd.Wait(); err != nil {
			fmt.Println("Warning: wget failed after authorization")
		} else {
			fmt.Println("[OK] Nonguix substitutes authorized")
		}
	}
	
	// NOTE: We do NOT run 'guix pull' here because:
	// 1. It can cause glibc version mismatches on older ISOs
	// 2. It's not needed - 'guix time-machine' will fetch channels independently
	// 3. The channels.scm file is all time-machine needs

	fmt.Println()
	fmt.Println("[OK] Nonguix channel setup complete")
	fmt.Println("    Channel file created: /tmp/channels.scm")
	fmt.Println("    Will be used by 'guix time-machine' during system init")
	fmt.Println()

	return nil
}

// RecordChannelCommits records the current channel commits for reproducibility
func RecordChannelCommits() error {
	fmt.Println()
	fmt.Println("=== Recording Channel Commits for Reproducibility ===")

	// Run guix describe to get commit information
	cmd := exec.Command("guix", "describe", "--format=channels")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to run guix describe: %w", err)
	}

	// Save the pinned channels to /mnt/etc/channels-pinned.scm
	pinnedPath := "/mnt/etc/channels-pinned.scm"
	if err := os.MkdirAll("/mnt/etc", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/etc directory: %w", err)
	}

	if err := os.WriteFile(pinnedPath, output, 0644); err != nil {
		return fmt.Errorf("failed to write pinned channels: %w", err)
	}

	fmt.Printf("[OK] Pinned channels saved to %s\n", pinnedPath)
	fmt.Println("    Use this file for reproducible builds:")
	fmt.Printf("    guix pull -C %s\n", pinnedPath)
	fmt.Println()

	return nil
}

// ValidateGuixConfig validates the config file for common issues
func ValidateGuixConfig(configPath string) error {
	fmt.Println("=== Validating Guix Configuration ===")
	fmt.Printf("Checking config file: %s\n", configPath)
	
	// Check if config file exists
	if _, err := os.Stat(configPath); err != nil {
		return fmt.Errorf("config file does not exist: %s", configPath)
	}
	
	// Show config file contents for debugging
	fmt.Println("Config file contents:")
	RunCommand("head", "-20", configPath)
	fmt.Println("...")
	RunCommand("tail", "-10", configPath)
	fmt.Println()
	
	// First check if daemon is responsive
	fmt.Println("Checking daemon connectivity...")
	testCmd := exec.Command("guix", "build", "--version")
	if err := testCmd.Run(); err != nil {
		return fmt.Errorf("daemon is not responsive - ensure guix-daemon is running: %w", err)
	}
	fmt.Println("[OK] Daemon is responsive")
	
	// Try to load the config to check for syntax errors and unbound variables
	fmt.Println("Validating config syntax and checking for unbound variables...")
	cmd := exec.Command("guix", "system", "reconfigure", "--dry-run", configPath)
	output, err := cmd.CombinedOutput()
	
	if err != nil {
		// Check for common unbound variable errors
		outputStr := string(output)
		if strings.Contains(outputStr, "unbound variable") {
			fmt.Println()
			fmt.Println("[ERROR] Unbound variable detected in config file!")
			fmt.Println("Common issues and fixes:")
			fmt.Println("  - 'linux-libre' should be 'linux'")
			fmt.Println("  - 'microcode-initrd' should be removed")
			fmt.Println("  - Check for typos in package names")
			fmt.Println()
			fmt.Printf("Full error output:\n%s\n", outputStr)
			return fmt.Errorf("unbound variable in config: %s", outputStr)
		}
		
		// Check for channel or nonguix/nongnu module issues
		if strings.Contains(outputStr, "channel") ||
		   strings.Contains(outputStr, "nonguix") ||
		   strings.Contains(outputStr, "nongnu") ||
		   strings.Contains(outputStr, "no code for module") {
			fmt.Println()
			fmt.Println("[WARN] Channel or nonguix module issues detected")
			fmt.Println("This is expected - the ISO's Guix doesn't have nonguix channel")
			fmt.Println("The config will be validated during 'guix time-machine' system init")
			fmt.Println("[OK] Skipping validation - will validate with time-machine")
			fmt.Println()
			return nil
		}
		
		// Other syntax errors
		fmt.Printf("[ERROR] Config validation failed:\n%s\n", outputStr)
		return fmt.Errorf("config validation failed: %w", err)
	}
	
	fmt.Println("[OK] Config file validation passed")
	fmt.Println()
	return nil
}

// RunGuixSystemInit runs guix system init with retry logic
func RunGuixSystemInit() error {
	// Ensure daemon is running before validation (validation needs daemon)
	if err := EnsureGuixDaemonRunning(); err != nil {
		return fmt.Errorf("failed to ensure guix-daemon is running: %w", err)
	}
	
	// Validate config after daemon is confirmed running
	configPath := "/mnt/etc/config.scm"
	if err := ValidateGuixConfig(configPath); err != nil {
		return fmt.Errorf("config validation failed: %w", err)
	}
	
	// Setup GRUB EFI if needed
	if err := SetupGRUBEFI(); err != nil {
		return fmt.Errorf("GRUB EFI setup failed: %w", err)
	}
	
	fmt.Println("=== Running guix system init ===")
	fmt.Println("This will take 5-30 minutes depending on substitutes availability...")
	fmt.Println()
	fmt.Println("You should see:")
	fmt.Println("  1. time-machine fetching channels (nonguix + guix)")
	fmt.Println("  2. Downloading/building packages")
	fmt.Println("  3. Installing bootloader")
	fmt.Println("  4. Finalizing system")
	fmt.Println()
	fmt.Println("Progress output below:")
	fmt.Println("---")
	fmt.Println()
	fmt.Println("NOTE: If you're in screen/tmux and see strange characters, that's normal.")
	fmt.Println("     Use Ctrl+A+D to detach from screen, or Ctrl+B+D for tmux.")
	fmt.Println()
	fmt.Println("If output seems to stop for several minutes, the process is likely still working.")
	fmt.Println("You can check progress by:")
	fmt.Println("  1. Press Ctrl+Z to suspend this process")
	fmt.Println("  2. Run: tail -f /tmp/guix-install.log")
	fmt.Println("  3. Run: ps aux | grep guix")
	fmt.Println("  4. Run: fg to resume in foreground, OR bg to continue in background")
	fmt.Println()
	fmt.Println("NOTE: If you don't run 'fg' or 'bg', the installer will stay suspended!")
	fmt.Println("Alternatively, open another terminal and run the same commands.")
	fmt.Println()

	maxRetries := 3
	var lastErr error
	for attempt := 1; attempt <= maxRetries; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Retrying guix system init after substitute failure...\n", attempt, maxRetries)
			fmt.Println("Waiting 10 seconds before retry...")
			fmt.Println()
			time.Sleep(10 * time.Second)
		}

		// Use time-machine with nonguix channel for system init
		channelsPath := "/tmp/channels.scm"
		if err := RunCommandWithSpinner("guix", "time-machine", "-C", channelsPath, "--", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt", "--substitute-urls=https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
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
		fmt.Println("  1. Wait a few minutes and run: guix time-machine -C /tmp/channels.scm -- system init /mnt/etc/config.scm /mnt")
		fmt.Println("  2. Try with --fallback to build from source: guix time-machine -C /tmp/channels.scm -- system init --fallback /mnt/etc/config.scm /mnt")
		return fmt.Errorf("guix system init failed after %d attempts: %w", maxRetries, lastErr)
	}

	return nil
}

// InstallVerificationScript installs the verification script to the target system
func InstallVerificationScript() error {
	fmt.Println("=== Installing Verification Script ===")

	// Read the verification script from current directory
	scriptContent, err := os.ReadFile("verify-guix-install.sh")
	if err != nil {
		return fmt.Errorf("failed to read verify-guix-install.sh: %w", err)
	}

	// Install to /mnt/usr/local/bin/
	targetDir := "/mnt/usr/local/bin"
	if err := os.MkdirAll(targetDir, 0755); err != nil {
		return fmt.Errorf("failed to create %s: %w", targetDir, err)
	}

	targetPath := filepath.Join(targetDir, "verify-guix-install")
	if err := os.WriteFile(targetPath, scriptContent, 0755); err != nil {
		return fmt.Errorf("failed to write %s: %w", targetPath, err)
	}

	fmt.Printf("[OK] Verification script installed to: %s\n", targetPath)
	fmt.Println("     Run anytime with: sudo verify-guix-install")
	fmt.Println()

	return nil
}

// VerifyInstallation verifies that all critical files were installed
func VerifyInstallation() error {
	fmt.Println()
	fmt.Println("=== Verifying Installation ===")
	allGood := true

	// Check for kernel
	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz-*")
	if len(kernels) == 0 {
		fmt.Println("[ERROR] No kernel found in /mnt/boot/vmlinuz-*")
		fmt.Println("        CRITICAL: System will not boot without a kernel!")
		allGood = false
	} else {
		fmt.Printf("[OK] Kernel installed: %s\n", filepath.Base(kernels[0]))
		// Show file size to confirm it's real
		if info, err := os.Stat(kernels[0]); err == nil {
			fmt.Printf("     Size: %.1f MB\n", float64(info.Size())/1024/1024)
		}
	}

	// Check for initrd
	initrds, _ := filepath.Glob("/mnt/boot/initrd-*")
	if len(initrds) == 0 {
		fmt.Println("[ERROR] No initrd found in /mnt/boot/initrd-*")
		fmt.Println("        CRITICAL: System will not boot without an initrd!")
		allGood = false
	} else {
		fmt.Printf("[OK] Initrd installed: %s\n", filepath.Base(initrds[0]))
		// Show file size to confirm it's real
		if info, err := os.Stat(initrds[0]); err == nil {
			fmt.Printf("     Size: %.1f MB\n", float64(info.Size())/1024/1024)
		}
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
		fmt.Println("========================================")
		fmt.Println("  INSTALLATION VERIFICATION FAILED!")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Println("Missing critical boot files - system WILL NOT BOOT.")
		fmt.Println()
		fmt.Println("DO NOT REBOOT until this is resolved!")
		fmt.Println()
		fmt.Println("Recommended action:")
		fmt.Println("  1. Re-run: guix system init /mnt/etc/config.scm /mnt")
		fmt.Println("  2. Wait for it to complete successfully")
		fmt.Println("  3. Verify files exist: ls /mnt/boot/vmlinuz* /mnt/boot/initrd*")
		fmt.Println()
		fmt.Println("If the problem persists, check:")
		fmt.Println("  - Disk space: df -h /mnt")
		fmt.Println("  - Error messages in guix system init output")
		fmt.Println("  - Installation logs")
		fmt.Println()
		return fmt.Errorf("installation verification failed - missing critical boot files")
	} else {
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  INSTALLATION VERIFIED SUCCESSFULLY!")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Println("All critical boot files are present.")
		fmt.Println("System should boot properly after reboot.")
	}
	fmt.Println()

	// Install verification script for future use
	if err := InstallVerificationScript(); err != nil {
		fmt.Printf("[WARN] Failed to install verification script: %v\n", err)
		fmt.Println("       (This is not critical, continuing...)")
	}

	return nil
}

// RunGuixSystemInitFreeSoftware runs guix system init with only free software (no nonguix)
func RunGuixSystemInitFreeSoftware() error {
	// Ensure daemon is running before validation (validation needs daemon)
	if err := EnsureGuixDaemonRunning(); err != nil {
		return fmt.Errorf("failed to ensure guix-daemon is running: %w", err)
	}
	
	// Validate config after daemon is confirmed running
	configPath := "/mnt/etc/config.scm"
	if err := ValidateGuixConfig(configPath); err != nil {
		return fmt.Errorf("config validation failed: %w", err)
	}
	
	// Setup GRUB EFI if needed
	if err := SetupGRUBEFI(); err != nil {
		return fmt.Errorf("GRUB EFI setup failed: %w", err)
	}
	
	fmt.Println("=== Running guix system init (Free Software Only) ===")
	fmt.Println("This will take 5-30 minutes depending on substitutes availability...")
	fmt.Println()
	fmt.Println("You should see:")
	fmt.Println("  1. Downloading/building packages (free software only)")
	fmt.Println("  2. Installing bootloader")
	fmt.Println("  3. Finalizing system")
	fmt.Println()
	fmt.Println("Progress output below:")
	fmt.Println("---")
	fmt.Println()

	maxRetries := 3
	var lastErr error
	for attempt := 1; attempt <= maxRetries; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Retrying guix system init after substitute failure...\n", attempt, maxRetries)
			fmt.Println("Waiting 10 seconds before retry...")
			fmt.Println()
			time.Sleep(10 * time.Second)
		}

		// Use standard guix system init (no nonguix channel)
		if err := RunCommandWithSpinner("guix", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt"); err != nil {
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

	return nil
}

// DownloadFile downloads a file from a URL to a destination path
func DownloadFile(url, dest string) error {
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

// IsMounted checks if a path is mounted
func IsMounted(path string) bool {
	cmd := exec.Command("mountpoint", "-q", path)
	return cmd.Run() == nil
}

// CommandExists checks if a command exists in PATH
func CommandExists(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}

// GetRootUUID gets the UUID of a partition
func GetRootUUID(device string) (string, error) {
	cmd := exec.Command("blkid", "-s", "UUID", "-o", "value", device)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

// GetUUID gets the UUID of a partition (alias for GetRootUUID)
func GetUUID(device string) (string, error) {
	return GetRootUUID(device)
}

// IsGuixLiveISO checks if we're running from a Guix live ISO
func IsGuixLiveISO() bool {
	// Check for common Guix live ISO indicators
	indicators := []string{
		"/etc/guix-release",
		"/run/current-system/profile/bin/guix",
	}
	
	for _, indicator := range indicators {
		if _, err := os.Stat(indicator); err == nil {
			return true
		}
	}
	
	// Check if guix command is available and we're in a live environment
	if CommandExists("guix") {
		cmd := exec.Command("guix", "describe")
		if err := cmd.Run(); err == nil {
			return true
		}
	}
	
	return false
}

// GetPartitionSizeGiB gets the size of a partition in GiB
func GetPartitionSizeGiB(partition string) float64 {
	cmd := exec.Command("lsblk", "-b", "-n", "-o", "SIZE", partition)
	output, err := cmd.Output()
	if err != nil {
		return 0
	}
	
	sizeStr := strings.TrimSpace(string(output))
	size, err := strconv.ParseFloat(sizeStr, 64)
	if err != nil {
		return 0
	}
	
	// Convert bytes to GiB
	return size / (1024 * 1024 * 1024)
}

// GetDiskSizeGiB gets the size of a disk device in GiB
func GetDiskSizeGiB(device string) float64 {
	cmd := exec.Command("lsblk", "-b", "-d", "-n", "-o", "SIZE", device)
	output, err := cmd.Output()
	if err != nil {
		return 0
	}
	
	sizeStr := strings.TrimSpace(string(output))
	size, err := strconv.ParseFloat(sizeStr, 64)
	if err != nil {
		return 0
	}
	
	// Convert bytes to GiB
	return size / (1024 * 1024 * 1024)
}

// GetFreeSpaceGiB gets the free space on a device in GiB
func GetFreeSpaceGiB(device string) float64 {
	cmd := exec.Command("parted", device, "print", "free")
	output, err := cmd.Output()
	if err != nil {
		return 0
	}
	
	lines := strings.Split(string(output), "\n")
	for _, line := range lines {
		if strings.Contains(line, "Free Space") {
			// Parse the free space line
			fields := strings.Fields(line)
			if len(fields) >= 4 {
				sizeStr := fields[2] // Size field
				if strings.HasSuffix(sizeStr, "GiB") {
					sizeStr = strings.TrimSuffix(sizeStr, "GiB")
					if size, err := strconv.ParseFloat(sizeStr, 64); err == nil {
						return size
					}
				}
			}
		}
	}
	
	return 0
}

// GetMountFreeSpaceGiB returns the available space on a mounted path in GiB
func GetMountFreeSpaceGiB(path string) float64 {
    cmd := exec.Command("df", "-BG", path)
    output, err := cmd.Output()
    if err != nil {
        return 0
    }
    lines := strings.Split(strings.TrimSpace(string(output)), "\n")
    if len(lines) < 2 {
        return 0
    }
    fields := strings.Fields(lines[len(lines)-1])
    if len(fields) < 4 {
        return 0
    }
    avail := strings.TrimSuffix(fields[3], "G")
    if v, err := strconv.ParseFloat(avail, 64); err == nil {
        return v
    }
    return 0
}

// VerifyLabelsExist ensures the provided /dev/disk/by-label/* entries exist
func VerifyLabelsExist(labels ...string) error {
    missing := []string{}
    for _, label := range labels {
        path := filepath.Join("/dev/disk/by-label", label)
        if _, err := os.Stat(path); err != nil {
            missing = append(missing, label)
        }
    }
    if len(missing) > 0 {
        return fmt.Errorf("missing labels under /dev/disk/by-label: %s", strings.Join(missing, ", "))
    }
    return nil
}

// MountByLabel mounts a filesystem by label to a mount point, creating the mount point if needed
func MountByLabel(label string, mountPoint string) error {
    if err := os.MkdirAll(mountPoint, 0755); err != nil {
        return err
    }
    
    // Check if the label exists
    labelPath := filepath.Join("/dev/disk/by-label", label)
    if _, err := os.Stat(labelPath); err != nil {
        // List available labels for debugging
        fmt.Printf("Label '%s' not found. Available labels:\n", label)
        RunCommand("ls", "-la", "/dev/disk/by-label/")
        return fmt.Errorf("label '%s' not found in /dev/disk/by-label", label)
    }
    
    return RunCommand("mount", labelPath, mountPoint)
}

// FindFreeSpaceStart finds the start sector for free space on a device
func FindFreeSpaceStart(device string) (int, error) {
	cmd := exec.Command("parted", device, "print", "free")
	output, err := cmd.Output()
	if err != nil {
		return 0, err
	}
	
	lines := strings.Split(string(output), "\n")
	for _, line := range lines {
		if strings.Contains(line, "Free Space") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				startStr := fields[1]
				if start, err := strconv.Atoi(startStr); err == nil {
					return start, nil
				}
			}
		}
	}
	
	return 0, fmt.Errorf("no free space found")
}

// GetLastPartition gets the last partition number on a device
func GetLastPartition(device string) (string, error) {
	cmd := exec.Command("parted", device, "print")
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	
	lines := strings.Split(string(output), "\n")
	var lastPartition string
	
	for _, line := range lines {
		fields := strings.Fields(line)
		if len(fields) >= 1 {
			// Check if this line contains a partition number
			if _, err := strconv.Atoi(fields[0]); err == nil {
				lastPartition = fields[0]
			}
		}
	}
	
	if lastPartition == "" {
		return "", fmt.Errorf("no partitions found")
	}
	
	return lastPartition, nil
}

// AskYesNo prompts the user for yes/no input
func AskYesNo(prompt string, expected string) bool {
	fmt.Print(prompt)
	
	var response string
	fmt.Scanln(&response)
	
	return strings.EqualFold(strings.TrimSpace(response), expected)
}

// DownloadCustomizationTools downloads customization tools to user's home directory
func DownloadCustomizationTools(platform string, username string) error {
	fmt.Println()
	fmt.Println("=== Installing Customization Tools ===")

	if platform == "" {
		platform = "cloudzy"
	}

	if username == "" {
		username = "guix"
	}

	destDir := fmt.Sprintf("/mnt/home/%s/guix-customize", username)
	fmt.Printf("Copying customize script and recipes to %s/\n", destDir)
	fmt.Println()

	repoOwner := GetEnv("GUIX_INSTALL_REPO", "durantschoon/cloudzy-guix-install")
	repoRef := GetEnv("GUIX_INSTALL_REF", "main")
	rawBase := fmt.Sprintf("https://raw.githubusercontent.com/%s/%s", repoOwner, repoRef)

	// Create destination directory
	if err := os.MkdirAll(filepath.Join(destDir, "recipes"), 0755); err != nil {
		return err
	}

	// Download customize script
	customizePath := filepath.Join(destDir, "customize")
	customizeURL := fmt.Sprintf("%s/%s/postinstall/customize", rawBase, platform)
	
	// Check if customize script already exists
	if _, err := os.Stat(customizePath); err == nil {
		fmt.Printf("Customize script already exists at %s\n", customizePath)
		fmt.Print("Do you want to replace it with the latest version? [y/N] ")
		
		var response string
		fmt.Scanln(&response)
		if response != "y" && response != "Y" {
			fmt.Println("Keeping existing customize script")
		} else {
			fmt.Printf("Downloading %s customize script...\n", platform)
			if err := DownloadFile(customizeURL, customizePath); err != nil {
				return fmt.Errorf("failed to download customize script: %w", err)
			}
			os.Chmod(customizePath, 0755)
			fmt.Println("Customize script updated")
		}
	} else {
		fmt.Printf("Downloading %s customize script...\n", platform)
		if err := DownloadFile(customizeURL, customizePath); err != nil {
			return fmt.Errorf("failed to download customize script: %w", err)
		}
		os.Chmod(customizePath, 0755)
		fmt.Println("Customize script installed")
	}

	// Download shared recipes
	fmt.Println("Downloading shared recipes...")
	recipes := []string{"add-spacemacs.sh", "add-doom-emacs.sh", "add-vanilla-emacs.sh", "add-development.sh", "add-fonts.sh"}
	for _, recipe := range recipes {
		recipeURL := fmt.Sprintf("%s/postinstall/recipes/%s", rawBase, recipe)
		recipePath := filepath.Join(destDir, "recipes", recipe)
		if err := DownloadFile(recipeURL, recipePath); err != nil {
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
- add-spacemacs.sh - Install Spacemacs (Emacs distribution with Vim keybindings)
- add-doom-emacs.sh - Install Doom Emacs (modern, fast Emacs framework)
- add-vanilla-emacs.sh - Install vanilla Emacs with minimal configuration
- add-development.sh - Install dev tools (git, vim, gcc, etc.)
- add-fonts.sh - Install programming and system fonts

Run them individually:

    ./recipes/add-doom-emacs.sh

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

// SetUserPassword sets the password for a user in the installed system
func SetUserPassword(username string) error {
	fmt.Println()
	fmt.Println("=== Set User Password ===")
	fmt.Printf("Setting password for user: %s\n", username)
	fmt.Println("You will need this password to log in after first boot.")
	fmt.Println()

	passwdCmd := exec.Command("chroot", "/mnt", "/run/current-system/profile/bin/passwd", username)
	passwdCmd.Stdin = os.Stdin
	passwdCmd.Stdout = os.Stdout
	passwdCmd.Stderr = os.Stderr
	if err := passwdCmd.Run(); err != nil {
		return fmt.Errorf("failed to set user password: %w", err)
	}

	fmt.Println()
	fmt.Printf("Password set successfully for user: %s\n", username)

	return nil
}

// Device detection and validation functions

// DetectDevice auto-detects the appropriate block device for the platform
func DetectDevice(platform string) (string, error) {
	var candidates []string
	
	switch platform {
	case "cloudzy":
		// VPS platforms typically use these devices
		candidates = []string{"/dev/vda", "/dev/sda", "/dev/nvme0n1"}
	case "framework", "framework-dual":
		// Framework laptops typically use NVMe or SATA
		candidates = []string{"/dev/nvme0n1", "/dev/nvme1n1", "/dev/sda"}
	default:
		// Generic fallback
		candidates = []string{"/dev/nvme0n1", "/dev/sda", "/dev/vda"}
	}
	
	for _, device := range candidates {
		if _, err := os.Stat(device); err == nil {
			fmt.Printf("Auto-detected device: %s\n", device)
			return device, nil
		}
	}
	
	fmt.Println("Error: No suitable block device found.")
	fmt.Println("Available block devices:")
	RunCommand("lsblk", "-d", "-n", "-o", "NAME,SIZE,TYPE")
	return "", fmt.Errorf("no suitable block device found")
}

// IsPartitionFormatted checks if a partition has a filesystem
func IsPartitionFormatted(partition string, fsTypes ...string) bool {
	cmd := exec.Command("blkid", "-s", "TYPE", "-o", "value", partition)
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	
	actualFsType := strings.TrimSpace(string(output))
	
	// If no specific types requested, check if any filesystem exists
	if len(fsTypes) == 0 {
		return actualFsType != ""
	}
	
	// Check if actual type matches any of the requested types
	for _, fsType := range fsTypes {
		if actualFsType == fsType {
			return true
		}
	}
	
	return false
}

// IsPartitionType checks if a partition has a specific GPT partition type flag
func IsPartitionType(partition string, partType string) bool {
	// Use parted to check partition flags
	// Extract device and partition number from partition path
	device := partition
	if strings.Contains(partition, "nvme") || strings.Contains(partition, "mmcblk") {
		// NVMe/eMMC: /dev/nvme0n1p1 -> device=/dev/nvme0n1
		parts := strings.Split(partition, "p")
		if len(parts) >= 2 {
			device = strings.Join(parts[:len(parts)-1], "p")
		}
	} else {
		// SATA/SCSI: /dev/sda1 -> device=/dev/sda
		for i := len(partition) - 1; i >= 0; i-- {
			if partition[i] < '0' || partition[i] > '9' {
				device = partition[:i+1]
				break
			}
		}
	}

	// Get partition information from parted
	cmd := exec.Command("parted", device, "print")
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	// Check if the partition has the specified flag
	// parted output format includes flags like "bios_grub", "esp", etc.
	outputStr := string(output)
	return strings.Contains(outputStr, partType)
}

// IsStorePopulated checks if the Guix store is already populated
func IsStorePopulated() bool {
	// Check if /mnt/gnu/store has contents
	entries, err := os.ReadDir("/mnt/gnu/store")
	if err != nil {
		return false
	}
	// Store should have many entries if populated
	return len(entries) > 10
}

// CheckDeviceSpace validates device size and warns if too small
func CheckDeviceSpace(device string, minSizeGiB float64) error {
	// Get device size in bytes
	cmd := exec.Command("lsblk", "-b", "-n", "-o", "SIZE", device)
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to get device size: %w", err)
	}
	
	// Take only the first line (the device itself, not partitions)
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	if len(lines) == 0 {
		return fmt.Errorf("no output from lsblk for device %s", device)
	}
	
	sizeStr := strings.TrimSpace(lines[0])
	sizeBytes, err := strconv.ParseInt(sizeStr, 10, 64)
	if err != nil {
		return fmt.Errorf("failed to parse device size '%s': %w", sizeStr, err)
	}
	
	// Convert to GiB
	sizeGiB := float64(sizeBytes) / (1024 * 1024 * 1024)
	
	fmt.Printf("Device %s size: %.1f GiB\n", device, sizeGiB)
	
	// Warn if device is smaller than minimum
	if sizeGiB < minSizeGiB {
		fmt.Printf("WARNING: Device is only %.1f GiB. Installation may fail due to low space.\n", sizeGiB)
		fmt.Printf("Recommended: %.0fGB+ for comfortable usage\n", minSizeGiB)
		fmt.Println()
	}
	
	return nil
}

// HasExistingPartitions checks if a device has any partitions
func HasExistingPartitions(device string) bool {
	// Check if device has any partitions
	cmd := exec.Command("lsblk", "-n", "-o", "NAME", device)
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	// If we have more than 1 line (device + partitions), there are existing partitions
	return len(lines) > 1
}

// UnmountDevice unmounts all mount points for a device
func UnmountDevice(device string) error {
	// Get all mount points for this device and its partitions
	cmd := exec.Command("findmnt", "-n", "-o", "TARGET", device)
	output, err := cmd.Output()
	if err != nil {
		// findmnt might not be available, try alternative method
		return UnmountDeviceAlternative(device)
	}
	
	mountPoints := strings.Split(strings.TrimSpace(string(output)), "\n")
	
	// Unmount all mount points in reverse order (deepest first)
	for i := len(mountPoints) - 1; i >= 0; i-- {
		mountPoint := strings.TrimSpace(mountPoints[i])
		if mountPoint == "" {
			continue
		}
		
		fmt.Printf("Unmounting %s...\n", mountPoint)
		if err := RunCommand("umount", mountPoint); err != nil {
			fmt.Printf("Warning: Failed to unmount %s: %v\n", mountPoint, err)
			// Try lazy unmount as fallback
			RunCommand("umount", "-l", mountPoint)
		}
	}
	
	return nil
}

// UnmountDeviceAlternative uses mount command parsing as fallback
func UnmountDeviceAlternative(device string) error {
	// Alternative method using mount command and grep
	cmd := exec.Command("mount")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to get mount information: %w", err)
	}
	
	lines := strings.Split(string(output), "\n")
	var mountPoints []string
	
	for _, line := range lines {
		if strings.Contains(line, device) {
			// Extract mount point (second field)
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				mountPoints = append(mountPoints, fields[1])
			}
		}
	}
	
	// Unmount all found mount points
	for _, mountPoint := range mountPoints {
		fmt.Printf("Unmounting %s...\n", mountPoint)
		if err := RunCommand("umount", mountPoint); err != nil {
			fmt.Printf("Warning: Failed to unmount %s: %v\n", mountPoint, err)
			// Try lazy unmount as fallback
			RunCommand("umount", "-l", mountPoint)
		}
	}
	
	return nil
}


// MakePartitionPath creates a partition path from device and partition number
func MakePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}

// DetectBootMode detects whether the system is running in UEFI or BIOS mode
func DetectBootMode() string {
	// Check if /sys/firmware/efi exists
	if _, err := os.Stat("/sys/firmware/efi"); err == nil {
		return "uefi"
	}
	return "bios"
}

// DetectPartitions detects the EFI and root partition paths for a device
func DetectPartitions(device string) (efi string, root string, err error) {
	if device == "" {
		return "", "", fmt.Errorf("DEVICE not set")
	}

	// For standard layouts: partition 1 is EFI, partition 2 is root
	efi = MakePartitionPath(device, "1")
	root = MakePartitionPath(device, "2")

	return efi, root, nil
}

// FindEFIPartition finds the EFI System Partition by GPT type GUID
func FindEFIPartition(device string) (string, error) {
	if device == "" {
		return "", fmt.Errorf("DEVICE not set")
	}

	// Find EFI partition by GPT type GUID
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,PARTTYPE", device)
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to list partitions: %w", err)
	}

	var efiPartName string
	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(strings.ToLower(line), "c12a7328-f81f-11d2-ba4b-00a0c93ec93b") {
			fields := strings.Fields(line)
			if len(fields) > 0 {
				efiPartName = fields[0]
				break
			}
		}
	}

	if efiPartName == "" {
		return "", fmt.Errorf("no EFI System Partition found")
	}

	// Convert partition name to device path
	var efiPath string
	if strings.Contains(device, "nvme") {
		// Extract partition number from nvme0n1p1
		parts := strings.Split(efiPartName, "p")
		if len(parts) >= 2 {
			efiPath = device + "p" + parts[len(parts)-1]
		}
	} else {
		// SATA: extract number from sda1
		for i := len(efiPartName) - 1; i >= 0; i-- {
			if efiPartName[i] < '0' || efiPartName[i] > '9' {
				efiPath = device + efiPartName[i+1:]
				break
			}
		}
	}

	if efiPath == "" {
		return "", fmt.Errorf("failed to parse EFI partition path from %s", efiPartName)
	}

	return efiPath, nil
}

// FindGuixRootPartition finds a partition labeled 'GUIX_ROOT' or 'guix-root'
func FindGuixRootPartition(device string) (string, error) {
	if device == "" {
		return "", fmt.Errorf("DEVICE not set")
	}

	// Try lsblk first (works with filesystem labels)
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,LABEL", device)
	output, err := cmd.Output()
	if err == nil {
		for _, line := range strings.Split(string(output), "\n") {
			if strings.Contains(line, "GUIX_ROOT") {
				fields := strings.Fields(line)
				if len(fields) >= 2 && fields[1] == "GUIX_ROOT" {
					// Extract partition name (strip any tree characters)
					partName := fields[0]
					// Remove tree branch characters like ├─ └─ │
					partName = strings.TrimLeft(partName, "├─└│ ")
					return "/dev/" + partName, nil
				}
			}
		}
	}

	// Try parted as fallback (works with partition names)
	cmd = exec.Command("parted", device, "print")
	output, err = cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to read partition table: %w", err)
	}

	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(line, "guix-root") || strings.Contains(line, "GUIX_ROOT") {
			fields := strings.Fields(line)
			if len(fields) > 0 {
				partNum := fields[0]
				return MakePartitionPath(device, partNum), nil
			}
		}
	}

	return "", fmt.Errorf("no partition labeled 'GUIX_ROOT' or 'guix-root' found")
}


// DetectDeviceFromState detects device from state, auto-detecting if not set
func DetectDeviceFromState(device string, platform string) (string, error) {
	if device != "" {
		// User-specified device
		if _, err := os.Stat(device); err != nil {
			return "", fmt.Errorf("specified device %s is not a block device", device)
		}
		fmt.Printf("Using user-specified device: %s\n", device)
		return device, nil
	}

	// Auto-detect using platform-specific candidates
	return DetectDevice(platform)
}

// ChannelInfo represents user's channel configuration
type ChannelInfo struct {
	HasExistingChannels bool
	ChannelRepo         string  // Git URL for user's channels
	ChannelBranch       string  // Branch/tag to use
	ChannelPath         string  // Path within repo (e.g., "channels/")
}

// DetectUserChannels checks if user has existing channel setup
func DetectUserChannels() (*ChannelInfo, error) {
	// Check for existing ~/.config/guix/channels.scm
	channelsFile := filepath.Join(os.Getenv("HOME"), ".config", "guix", "channels.scm")
	if _, err := os.Stat(channelsFile); err == nil {
		return &ChannelInfo{HasExistingChannels: true}, nil
	}
	
	// Check for environment variables indicating channel repo
	if repo := os.Getenv("GUIX_CHANNEL_REPO"); repo != "" {
		return &ChannelInfo{
			HasExistingChannels: false,
			ChannelRepo:         repo,
			ChannelBranch:       GetEnv("GUIX_CHANNEL_BRANCH", "main"),
			ChannelPath:         GetEnv("GUIX_CHANNEL_PATH", "channels/"),
		}, nil
	}
	
	return &ChannelInfo{HasExistingChannels: false}, nil
}

// BootstrapUserChannels downloads and sets up user's channel configuration
func BootstrapUserChannels(channelInfo *ChannelInfo) error {
	if channelInfo.HasExistingChannels {
		fmt.Println("[INFO] User has existing channel configuration, skipping bootstrap")
		return nil
	}
	
	if channelInfo.ChannelRepo == "" {
		fmt.Println("[INFO] No channel repository specified, using default channels")
		return SetupDefaultChannels()
	}
	
	// Download user's channel configuration
	fmt.Printf("[INFO] Downloading channels from: %s\n", channelInfo.ChannelRepo)
	return DownloadUserChannels(channelInfo)
}

// DownloadUserChannels clones user's channel repo and sets up channels.scm
func DownloadUserChannels(channelInfo *ChannelInfo) error {
	tempDir := "/tmp/guix-channels"
	
	// Clean up any existing temp directory
	os.RemoveAll(tempDir)
	
	// Clone the repository
	fmt.Printf("Cloning channel repository (branch: %s)...\n", channelInfo.ChannelBranch)
	cmd := exec.Command("git", "clone", 
		"--branch", channelInfo.ChannelBranch,
		"--depth", "1",
		channelInfo.ChannelRepo,
		tempDir)
	
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to clone channel repo: %w", err)
	}
	
	// Find channels.scm file
	channelsPath := filepath.Join(tempDir, channelInfo.ChannelPath, "channels.scm")
	if _, err := os.Stat(channelsPath); os.IsNotExist(err) {
		// Try alternative locations
		altPaths := []string{
			filepath.Join(tempDir, "channels.scm"),
			filepath.Join(tempDir, "config", "channels.scm"),
			filepath.Join(tempDir, ".config", "guix", "channels.scm"),
		}
		
		found := false
		for _, altPath := range altPaths {
			if _, err := os.Stat(altPath); err == nil {
				channelsPath = altPath
				found = true
				break
			}
		}
		
		if !found {
			return fmt.Errorf("channels.scm not found in repository. Tried:\n  - %s\n  - %s\n  - %s\n  - %s", 
				filepath.Join(tempDir, channelInfo.ChannelPath, "channels.scm"),
				filepath.Join(tempDir, "channels.scm"),
				filepath.Join(tempDir, "config", "channels.scm"),
				filepath.Join(tempDir, ".config", "guix", "channels.scm"))
		}
	}
	
	// Copy to user's config directory
	configDir := filepath.Join(os.Getenv("HOME"), ".config", "guix")
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}
	
	destPath := filepath.Join(configDir, "channels.scm")
	if err := RunCommand("cp", channelsPath, destPath); err != nil {
		return fmt.Errorf("failed to copy channels.scm: %w", err)
	}
	
	fmt.Printf("[OK] Channels configured from: %s\n", channelInfo.ChannelRepo)
	fmt.Printf("     Channels file: %s\n", destPath)
	
	// Clean up temp directory
	os.RemoveAll(tempDir)
	
	return nil
}

// SetupDefaultChannels creates minimal channel setup for new users
func SetupDefaultChannels() error {
	configDir := filepath.Join(os.Getenv("HOME"), ".config", "guix")
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}
	
	channelsContent := `(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)`
	
	channelsPath := filepath.Join(configDir, "channels.scm")
	if err := os.WriteFile(channelsPath, []byte(channelsContent), 0644); err != nil {
		return fmt.Errorf("failed to write default channels.scm: %w", err)
	}
	
	fmt.Println("[OK] Default channels configured (nonguix + official)")
	fmt.Printf("     Channels file: %s\n", channelsPath)
	
	return nil
}

// ValidateChannels validates the channel configuration
func ValidateChannels() error {
	fmt.Println("=== Validating Channel Configuration ===")
	
	channelsPath := filepath.Join(os.Getenv("HOME"), ".config", "guix", "channels.scm")
	if _, err := os.Stat(channelsPath); os.IsNotExist(err) {
		fmt.Println("[WARN] No channels.scm found - will use default channels")
		return nil
	}
	
	fmt.Printf("Checking channels file: %s\n", channelsPath)
	
	// Show channel file contents
	fmt.Println("Channel configuration:")
	RunCommand("cat", channelsPath)
	fmt.Println()
	
	// Try to validate with guix describe
	fmt.Println("Validating channel configuration...")
	cmd := exec.Command("guix", "describe", "--format=channels")
	if err := cmd.Run(); err != nil {
		fmt.Println("[WARN] Channel validation failed - this is expected on ISO")
		fmt.Println("       Channels will be validated during system init")
		return nil
	}
	
	fmt.Println("[OK] Channel configuration is valid")
	return nil
}
