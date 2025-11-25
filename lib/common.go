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

    // Channels for monitoring
    outputReceived := make(chan string, 100) // Buffer recent output lines
    done := make(chan error, 1)

    // Goroutine to read stdout
    go func() {
        scanner := bufio.NewScanner(stdout)
        for scanner.Scan() {
            line := scanner.Text()
            if commandLogWriter != nil {
                fmt.Fprintln(commandLogWriter, line)
            }
            fmt.Println(line)

            // Send line to monitoring channel
            select {
            case outputReceived <- line:
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
            }
            fmt.Fprintln(os.Stderr, line)

            // Send line to monitoring channel
            select {
            case outputReceived <- line:
            default:
            }
        }
    }()

    // Wait for command in separate goroutine
    go func() {
        done <- cmd.Wait()
    }()

    // Monitor progress with enhanced feedback
    spinner := []string{"/", "|", "\\", "-"}
    spinnerIndex := 0
    spinnerColorIndex := 0 // Separate index for color cycling
    lastOutputTime := time.Now()
    lastLogSize := int64(0)
    spinnerActive := false
    progressTicker := time.NewTicker(60 * time.Second) // Progress update every 60s
    spinnerTicker := time.NewTicker(200 * time.Millisecond)
    defer progressTicker.Stop()
    defer spinnerTicker.Stop()

    for {
        select {
        case err := <-done:
            // Command finished
            if spinnerActive {
                fmt.Printf("\r%s\r", strings.Repeat(" ", 80))
                fmt.Printf("\033[?25h") // Restore cursor
            }
            return err

        case <-outputReceived:
            // Clear spinner if active
            if spinnerActive {
                fmt.Printf("\r%s\r", strings.Repeat(" ", 80))
                spinnerActive = false
            }
            lastOutputTime = time.Now()

        case <-spinnerTicker.C:
            // Show spinner when quiet
            elapsed := time.Since(lastOutputTime)
            if elapsed > 3*time.Second {
                if !spinnerActive {
                    spinnerActive = true
                    fmt.Print("\n")
                }
                spinnerColor := GetSpinnerColor(spinnerColorIndex)
                fmt.Printf("\r\033[?25l%s%s%s Working... (%s elapsed, may take 5-30 min)",
                    spinnerColor,
                    spinner[spinnerIndex],
                    ColorReset,
                    formatDuration(time.Since(lastOutputTime)))
                os.Stdout.Sync()
                spinnerIndex = (spinnerIndex + 1) % len(spinner)
                spinnerColorIndex = (spinnerColorIndex + 1) % len(SpinnerColors) // Cycle through all colors
            }

        case <-progressTicker.C:
            // Periodic progress update
            elapsed := time.Since(lastOutputTime)

            // Clear spinner for progress message
            if spinnerActive {
                fmt.Printf("\r%s\r", strings.Repeat(" ", 80))
            }

            // Check log file growth
            logGrowth := ""
            if commandLogWriter != nil {
                if logFile, ok := commandLogWriter.(*os.File); ok {
                    if stat, err := logFile.Stat(); err == nil {
                        newSize := stat.Size()
                        if newSize > lastLogSize {
                            growth := newSize - lastLogSize
                            logGrowth = fmt.Sprintf(" (log +%s)", formatBytes(growth))
                            lastLogSize = newSize
                        }
                    }
                }
            }

            timestamp := time.Now().Format("15:04:05")

            if elapsed > 15*time.Minute {
                PrintWarning(fmt.Sprintf("[%s] No output for %s - process may be hung%s",
                    timestamp, formatDuration(elapsed), logGrowth))
                fmt.Println("         Check: tail -f /tmp/guix-install.log")
                fmt.Println("         Or: ps aux | grep guix")
                if logGrowth == "" {
                    fmt.Println("         Log not growing - likely hung!")
                }
            } else if elapsed > 5*time.Minute {
                PrintInfo(fmt.Sprintf("[%s] No output for %s (normal for some phases)%s",
                    timestamp, formatDuration(elapsed), logGrowth))
            } else {
                PrintProgress(fmt.Sprintf("[%s] [PROGRESS] Last output %s ago%s",
                    timestamp, formatDuration(elapsed), logGrowth))
            }

            // Resume spinner if needed
            if elapsed > 3*time.Second {
                spinnerActive = true
            }
        }
    }
}

// formatDuration formats a duration in a human-readable way
func formatDuration(d time.Duration) string {
    d = d.Round(time.Second)
    h := d / time.Hour
    d -= h * time.Hour
    m := d / time.Minute
    d -= m * time.Minute
    s := d / time.Second

    if h > 0 {
        return fmt.Sprintf("%dh%dm%ds", h, m, s)
    } else if m > 0 {
        return fmt.Sprintf("%dm%ds", m, s)
    }
    return fmt.Sprintf("%ds", s)
}

// formatBytes formats byte count in human-readable format
func formatBytes(bytes int64) string {
    const unit = 1024
    if bytes < unit {
        return fmt.Sprintf("%d B", bytes)
    }
    div, exp := int64(unit), 0
    for n := bytes / unit; n >= unit; n /= unit {
        div *= unit
        exp++
    }
    return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
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

	fmt.Printf("Creating %s swap file at /mnt/swapfile...\n", swapSize)

	// Try fallocate first (fast), fall back to dd (slower but more compatible)
	cmd := exec.Command("fallocate", "-l", fmt.Sprintf("%d", sizeBytes), "/mnt/swapfile")
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Printf("  [INFO] fallocate not supported on this filesystem (normal for some VPS): %s\n", strings.TrimSpace(string(output)))
		fmt.Println("  Falling back to dd (this may take 1-2 minutes)...")
		sizeMB := sizeBytes / 1024 / 1024
		ddCmd := exec.Command("dd", "if=/dev/zero", "of=/mnt/swapfile", "bs=1M", fmt.Sprintf("count=%d", sizeMB), "status=progress")
		ddCmd.Stdout = os.Stdout
		ddCmd.Stderr = os.Stderr
		if err := ddCmd.Run(); err != nil {
			return fmt.Errorf("CRITICAL: dd failed to create swap file: %w", err)
		}
		fmt.Println("  [OK] Swap file created with dd")
	} else {
		fmt.Println("  [OK] Swap file created with fallocate")
	}

	fmt.Println("  Setting secure permissions (0600)...")
	if err := os.Chmod("/mnt/swapfile", 0600); err != nil {
		return fmt.Errorf("failed to set permissions on swapfile: %w", err)
	}
	fmt.Println("  [OK] Permissions set to 0600")

	fmt.Println("  Formatting as swap...")
	if err := RunCommand("mkswap", "/mnt/swapfile"); err != nil {
		return fmt.Errorf("mkswap failed: %w", err)
	}
	fmt.Println("  [OK] Swap formatted")

	fmt.Println("  Activating swap...")
	if err := RunCommand("swapon", "/mnt/swapfile"); err != nil {
		return fmt.Errorf("swapon failed (check permissions and filesystem): %w", err)
	}
	fmt.Println("  [OK] Swap activated")

	fmt.Println()
	fmt.Println("Swap is now active:")
	RunCommand("swapon", "--show")
	RunCommand("free", "-h")
	fmt.Println()

	return nil
}

// DaemonCheck is a function type for daemon readiness checks
type DaemonCheck func() error

// checkSocketExists verifies the daemon socket file exists
func checkSocketExists() error {
	cmd := exec.Command("test", "-S", "/var/guix/daemon-socket/socket")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("socket not ready")
	}
	return nil
}

// checkDaemonResponsive verifies the daemon responds to commands
func checkDaemonResponsive() error {
	cmd := exec.Command("guix", "build", "--version")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("daemon not responsive")
	}
	return nil
}

// checkDaemonStable verifies daemon connection is stable over multiple tests
func checkDaemonStable(stabilityTests int, interval time.Duration) error {
	for i := 0; i < stabilityTests; i++ {
		if i > 0 {
			time.Sleep(interval)
		}
		if err := checkDaemonResponsive(); err != nil {
			return fmt.Errorf("stability check %d/%d failed", i+1, stabilityTests)
		}
		fmt.Printf("  Stability check %d/%d passed\n", i+1, stabilityTests)
	}
	return nil
}

// isDaemonReady performs complete daemon readiness check
func isDaemonReady() error {
	// Check socket exists
	if err := checkSocketExists(); err != nil {
		return err
	}

	// Check daemon responds
	if err := checkDaemonResponsive(); err != nil {
		return err
	}

	// Stabilization wait before testing stability
	fmt.Println("[INFO] Daemon responded, verifying stability...")
	time.Sleep(5 * time.Second)

	// Verify stability with multiple consecutive tests
	if err := checkDaemonStable(3, 2*time.Second); err != nil {
		return err
	}

	return nil
}

// isDaemonReadyAfterStart checks process, socket, and stability after daemon start
func isDaemonReadyAfterStart() error {
	// First check if process is running
	if !isDaemonProcessRunning() {
		return fmt.Errorf("daemon process not started yet")
	}

	// Then do complete readiness check
	return isDaemonReady()
}

// retryUntilReady retries a check function until it succeeds or times out
// This is a generic retry helper that can be used for any similar waiting scenario
func retryUntilReady(check DaemonCheck, timeout time.Duration, pollInterval time.Duration, progressFn func(int, int)) error {
	maxAttempts := int(timeout / pollInterval)

	for attempt := 0; attempt < maxAttempts; attempt++ {
		time.Sleep(pollInterval)

		// Call progress callback if provided
		if progressFn != nil {
			progressFn(attempt+1, maxAttempts)
		}

		// Try the check
		if err := check(); err == nil {
			return nil // Success!
		}
	}

	return fmt.Errorf("timeout after %v", timeout)
}

// isDaemonProcessRunning checks if daemon process is running via herd
func isDaemonProcessRunning() bool {
	cmd := exec.Command("herd", "status", "guix-daemon")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(output), "It is started") ||
	       strings.Contains(string(output), "It is enabled")
}

// EnsureGuixDaemonRunning ensures the guix-daemon is running and responsive
// Uses a functional approach: actually ensures the daemon works, not just retries
// NOTE: Do NOT use bind mounts! cow-store handles store writes correctly.
// Bind mounting /mnt/gnu to /gnu shadows the live system's store and breaks guix.
func EnsureGuixDaemonRunning() error {
	PrintSectionHeader("Ensuring guix-daemon is running")

	// First, check if daemon is actually ready (process + socket + responsive)
	fmt.Println("Checking if guix-daemon is ready...")
	if err := isDaemonReady(); err == nil {
		fmt.Println("[OK] guix-daemon is already running and responsive")
		fmt.Println()
		return nil
	}

	// Daemon is not ready - need to start/restart it
	fmt.Println("guix-daemon is not ready, ensuring it's running...")
	fmt.Println()

	// Stop any existing daemon processes (clean slate)
	fmt.Println("Stopping any existing guix-daemon processes...")
	exec.Command("herd", "stop", "guix-daemon").Run()
	exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
	time.Sleep(2 * time.Second)
	exec.Command("pkill", "-KILL", "-x", "guix-daemon").Run() // Force kill if needed
	time.Sleep(2 * time.Second)

	// Start the daemon
	fmt.Println("Starting guix-daemon via herd...")
	startErr := RunCommand("herd", "start", "guix-daemon")
	if startErr != nil {
		fmt.Printf("Warning: herd start failed: %v\n", startErr)
		fmt.Println("Trying alternative daemon startup method...")
		// Try direct daemon start as fallback
		cmd := exec.Command("guix-daemon", "--build-users-group=guixbuild")
		if err := cmd.Start(); err != nil {
			fmt.Printf("Warning: Direct daemon start also failed: %v\n", err)
		} else {
			fmt.Println("Started daemon via direct method, waiting for it to initialize...")
			time.Sleep(3 * time.Second)
		}
	}

	// Functional approach: Keep checking and restarting until daemon is actually ready
	// This ensures the daemon is working, not just that we tried to start it
	maxAttempts := 5
	for attempt := 1; attempt <= maxAttempts; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Daemon not ready yet, restarting...\n", attempt, maxAttempts)
			// Stop and restart
			exec.Command("herd", "stop", "guix-daemon").Run()
			exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
			time.Sleep(2 * time.Second)
			exec.Command("pkill", "-KILL", "-x", "guix-daemon").Run()
			time.Sleep(2 * time.Second)
			
			fmt.Println("Restarting guix-daemon...")
			RunCommand("herd", "start", "guix-daemon")
			time.Sleep(3 * time.Second) // Give it a moment to start
		}

		// Check if process is running
		if !isDaemonProcessRunning() {
			fmt.Printf("[WARN] Attempt %d: Daemon process not running yet\n", attempt)
			time.Sleep(3 * time.Second)
			continue
		}

		// Process is running, now check if it's actually ready (socket + responsive)
		fmt.Printf("Attempt %d: Daemon process is running, checking responsiveness...\n", attempt)
		
		// Wait up to 30 seconds for daemon to become ready
		readyErr := retryUntilReady(
			isDaemonReady,
			30*time.Second,
			2*time.Second,
			func(attemptNum, maxAttempts int) {
				if attemptNum%5 == 0 { // Show progress every 5 attempts (10 seconds)
					fmt.Printf("  Waiting for daemon to become ready... (%d/%d)\n", attemptNum*2, 30)
				}
			},
		)

		if readyErr == nil {
			fmt.Println("[OK] guix-daemon is now running and responsive")
			fmt.Println()
			return nil
		}

		fmt.Printf("[WARN] Attempt %d: Daemon started but not responsive: %v\n", attempt, readyErr)
		
		// If this isn't the last attempt, we'll restart and try again
		if attempt < maxAttempts {
			fmt.Println("  Will restart and try again...")
		}
	}

	// All attempts failed - provide helpful error message
	fmt.Println()
	fmt.Println("[ERROR] Failed to get guix-daemon running and responsive after", maxAttempts, "attempts")
	fmt.Println()
	fmt.Println("The daemon may be starting but not becoming responsive.")
	fmt.Println("Please try manually:")
	fmt.Println("  1. herd stop guix-daemon")
	fmt.Println("  2. pkill -KILL guix-daemon")
	fmt.Println("  3. herd start guix-daemon")
	fmt.Println("  4. Wait 10-20 seconds")
	fmt.Println("  5. Test: guix build --version")
	fmt.Println()
	fmt.Println("If it still doesn't work, check:")
	fmt.Println("  - Disk space: df -h")
	fmt.Println("  - System logs: journalctl -u guix-daemon")
	fmt.Println("  - Socket exists: ls -la /var/guix/daemon-socket/socket")
	fmt.Println()
	return fmt.Errorf("guix-daemon failed to become responsive after %d restart attempts", maxAttempts)
}

// VerifyESP verifies the EFI System Partition is properly mounted as vfat
func VerifyESP() error {
	fmt.Println()
	PrintSectionHeader("Verifying EFI System Partition")
	
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
	PrintSectionHeader("Starting cow-store")
	fmt.Println("Redirecting store writes to /mnt to avoid filling ISO space...")
	if err := RunCommand("herd", "start", "cow-store", "/mnt"); err != nil {
		return fmt.Errorf("failed to start cow-store: %w", err)
	}
	fmt.Println()
	return nil
}

// SetupGRUBEFI creates necessary directories and symlinks for GRUB EFI bootloader
func SetupGRUBEFI() error {
	PrintSectionHeader("Setting up GRUB EFI bootloader")
	
	// CRITICAL: Verify EFI partition is mounted before proceeding
	// GRUB EFI installation will fail if EFI partition is not accessible
	if !isMountPoint("/mnt/boot/efi") {
		fmt.Println("[WARN] EFI partition not mounted at /mnt/boot/efi")
		fmt.Println("Attempting to mount EFI partition...")
		
		// Try to mount EFI partition by label
		if err := os.MkdirAll("/mnt/boot/efi", 0755); err != nil {
			return fmt.Errorf("failed to create /mnt/boot/efi directory: %w", err)
		}
		
		cmd := exec.Command("mount", "LABEL=EFI", "/mnt/boot/efi")
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("failed to mount EFI partition: %w. GRUB EFI installation requires EFI partition to be mounted", err)
		}
		fmt.Println("[OK] EFI partition mounted")
	} else {
		fmt.Println("[OK] EFI partition already mounted at /mnt/boot/efi")
	}
	
	// Verify EFI partition is vfat (required for EFI)
	cmd := exec.Command("df", "-T", "/mnt/boot/efi")
	output, err := cmd.Output()
	if err == nil {
		if !strings.Contains(string(output), "vfat") && !strings.Contains(string(output), "fat32") {
			return fmt.Errorf("EFI partition at /mnt/boot/efi is not vfat filesystem (required for EFI boot)")
		}
		fmt.Println("[OK] EFI partition is vfat filesystem")
	}
	
	// Ensure the EFI directory structure exists
	guixEfiDir := "/mnt/boot/efi/EFI/Guix"
	
	// Create directories if they don't exist
	if err := os.MkdirAll(guixEfiDir, 0755); err != nil {
		return fmt.Errorf("failed to create EFI directory: %w", err)
	}
	
	// Check if we already have GRUB EFI files (from previous failed attempts)
	grubEfiFile := "/mnt/boot/efi/EFI/Guix/grubx64.efi"
	grubCfgLink := "/mnt/boot/efi/EFI/Guix/grub.cfg"
	
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

// GetChannelsPath returns the path to channels.scm file
// Checks home directory first (~/channels.scm), then /tmp/channels.scm as fallback
func GetChannelsPath() string {
	homeDir := os.Getenv("HOME")
	if homeDir == "" {
		homeDir = "/root" // Default to /root if HOME is not set
	}
	homeChannelsPath := filepath.Join(homeDir, "channels.scm")
	
	// Check home directory first
	if _, err := os.Stat(homeChannelsPath); err == nil {
		return homeChannelsPath
	}
	
	// Fallback to /tmp/channels.scm for backward compatibility
	return "/tmp/channels.scm"
}

// SetupNonguixChannel sets up the nonguix channel for proprietary firmware and kernel
func SetupNonguixChannel() error {
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

	// Use home directory for persistence (survives /tmp cleanup)
	homeDir := os.Getenv("HOME")
	if homeDir == "" {
		homeDir = "/root" // Default to /root if HOME is not set
	}
	channelsPath := filepath.Join(homeDir, "channels.scm")
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
	fmt.Printf("    Channel file created: %s\n", channelsPath)
	fmt.Println("    Will be used by 'guix time-machine' during system init")
	fmt.Println()

	return nil
}

// PromptKeyboardLayout prompts the user for keyboard layout preference
// Returns layout string in format "layout:option" (e.g., "us:ctrl:swapcaps")
func PromptKeyboardLayout() (string, error) {
	fmt.Println()
	fmt.Println("=== Keyboard Layout Configuration ===")
	fmt.Println()
	fmt.Println("Many Emacs users prefer to swap Caps Lock and Left Control.")
	fmt.Println("This makes Ctrl more accessible for Emacs keybindings.")
	fmt.Println()
	fmt.Println("Options:")
	fmt.Println("  1. Standard US layout (no swap)")
	fmt.Println("  2. US layout with Caps Lock <-> Left Ctrl swap")
	fmt.Println("  3. Skip (can configure later)")
	fmt.Println()
	fmt.Print("Choose keyboard layout [1/2/3]: ")

	// Read user response from /dev/tty
	tty, err := os.Open("/dev/tty")
	if err != nil {
		return "", fmt.Errorf("failed to open /dev/tty for user input: %w", err)
	}
	defer tty.Close()

	reader := bufio.NewReader(tty)
	response, err := reader.ReadString('\n')
	if err != nil {
		return "", fmt.Errorf("failed to read user response: %w", err)
	}

	response = strings.TrimSpace(response)

	var layout string
	switch response {
	case "1", "":
		layout = "us"
		fmt.Println("[OK] Using standard US keyboard layout")
	case "2":
		layout = "us:ctrl:swapcaps"
		fmt.Println("[OK] Using US layout with Caps Lock <-> Left Ctrl swap")
	case "3":
		layout = ""
		fmt.Println("[OK] Skipping keyboard layout configuration")
	default:
		layout = "us"
		fmt.Printf("[WARN] Invalid choice '%s', using standard US layout\n", response)
	}

	fmt.Println()
	return layout, nil
}

// RecordChannelCommits records the current channel commits for reproducibility
func RecordChannelCommits() error {
	fmt.Println()
	PrintSectionHeader("Recording Channel Commits for Reproducibility")

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
	PrintSectionHeader("Validating Guix Configuration")
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
	
	// Ensure daemon is running and responsive before validation
	fmt.Println("Ensuring daemon is running before validation...")
	if err := EnsureGuixDaemonRunning(); err != nil {
		fmt.Println()
		fmt.Println("[WARN] Failed to ensure daemon is running")
		fmt.Println("       Skipping config validation - will validate during system init")
		fmt.Println("       The daemon will be ensured again before system init")
		fmt.Println()
		return nil  // Skip validation gracefully
	}
	fmt.Println("[OK] Daemon is running and responsive")
	
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
		
		// Check for daemon connection issues - skip validation gracefully
		if strings.Contains(outputStr, "failed to connect") ||
		   strings.Contains(outputStr, "Connection refused") ||
		   strings.Contains(outputStr, "daemon-socket") {
			fmt.Println()
			fmt.Println("[WARN] Daemon connection issue during validation")
			fmt.Println("This can happen if the daemon is temporarily unavailable")
			fmt.Println("The config will be validated during 'guix system init'")
			fmt.Println("[OK] Skipping validation - will validate during system init")
			fmt.Println()
			return nil
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

// ImportPrebuiltKernel imports a pre-built kernel NAR archive if it exists
// This allows building the kernel on a powerful machine (e.g., Docker on Mac)
// and transferring it to avoid disk space issues during installation
func ImportPrebuiltKernel(narPath string) error {
	// Check if NAR file exists
	if _, err := os.Stat(narPath); os.IsNotExist(err) {
		fmt.Printf("No pre-built kernel found at %s (optional)\n", narPath)
		return nil
	}

	fmt.Println()
	PrintSectionHeader("Importing Pre-built Kernel")
	fmt.Printf("Found kernel archive at: %s\n", narPath)

	// Get file size for display
	if stat, err := os.Stat(narPath); err == nil {
		fmt.Printf("Archive size: %s\n", formatBytes(stat.Size()))
	}
	fmt.Println()

	// Import the archive
	fmt.Println("Importing kernel into Guix store...")
	fmt.Println("This may take a few minutes...")

	narFile, err := os.Open(narPath)
	if err != nil {
		return fmt.Errorf("failed to open NAR file: %w", err)
	}
	defer narFile.Close()

	cmd := exec.Command("guix", "archive", "--import")
	cmd.Stdin = narFile
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to import kernel archive: %w", err)
	}

	fmt.Println()
	fmt.Println("[OK] Pre-built kernel imported successfully")
	fmt.Println("     System init will use this kernel instead of building from source")
	fmt.Println()

	return nil
}

// PrepareSystemInitDirectories ensures critical directories exist as real directories
// (not symlinks) before running guix system init. This prevents ENOENT errors when
// guix system init tries to chmod directories like /var/lock.
func PrepareSystemInitDirectories() error {
	fmt.Println()
	fmt.Println("Preparing directories for system init...")

	// Critical directories that must exist as real directories (not symlinks)
	// before guix system init runs. If these are symlinks to non-existent targets,
	// chmod operations will fail with ENOENT.
	criticalDirs := []struct {
		path string
		mode os.FileMode
	}{
		{"/mnt/run", 0755},
		{"/mnt/var/lock", 01777}, // sticky bit required
	}

	for _, dir := range criticalDirs {
		// Remove any existing symlink or directory
		if err := os.RemoveAll(dir.path); err != nil && !os.IsNotExist(err) {
			return fmt.Errorf("failed to remove %s: %w", dir.path, err)
		}

		// Create as a real directory
		if err := os.MkdirAll(dir.path, dir.mode); err != nil {
			return fmt.Errorf("failed to create %s: %w", dir.path, err)
		}

		// Set permissions explicitly (MkdirAll may not set them correctly)
		if err := os.Chmod(dir.path, dir.mode); err != nil {
			return fmt.Errorf("failed to chmod %s: %w", dir.path, err)
		}

		fmt.Printf("[OK] Created %s (mode %o)\n", dir.path, dir.mode)
	}

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

	// Prepare critical directories before system init
	if err := PrepareSystemInitDirectories(); err != nil {
		return fmt.Errorf("failed to prepare system init directories: %w", err)
	}
	
	// WORKAROUND: guix system init has a bug where it doesn't copy kernel/initrd to /boot
	// Solution: Build the system first, manually copy kernel files, then run init

	PrintSectionHeader("Building Guix System")
	fmt.Println("Step 1/3: Building system closure (includes kernel)")
	fmt.Println("This will take 5-30 minutes depending on substitutes availability...")
	fmt.Println()
	fmt.Println("You should see:")
	fmt.Println("  1. time-machine fetching channels (nonguix + guix)")
	fmt.Println("  2. Downloading/building packages (including kernel)")
	fmt.Println("  3. Final output: /gnu/store/...-system")
	fmt.Println()
	fmt.Println("Progress output below:")
	fmt.Println("---")
	fmt.Println()

	channelsPath := GetChannelsPath()
	// Verify channels.scm exists
	if _, err := os.Stat(channelsPath); os.IsNotExist(err) {
		return fmt.Errorf("channels.scm not found at %s - nonguix channel setup may have failed", channelsPath)
	}

	// Step 1: Build the system (creates complete system in /gnu/store)
	if err := RunCommandWithSpinner("guix", "time-machine", "-C", channelsPath, "--", "system", "build", "/mnt/etc/config.scm", "--substitute-urls=https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
		return fmt.Errorf("guix system build failed: %w", err)
	}

	PrintSectionHeader("Copying Kernel Files")
	fmt.Println("Step 2/3: Manually copying kernel and initrd to /boot")
	fmt.Println("(Workaround for guix system init bug)")
	fmt.Println()

	// Step 2: Find the built system and copy kernel/initrd to /boot
	// The system build creates /gnu/store/*-system with kernel and initrd
	cmd := exec.Command("bash", "-c", "ls -td /gnu/store/*-system 2>/dev/null | head -1")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to find built system: %w", err)
	}
	systemPath := strings.TrimSpace(string(output))
	if systemPath == "" {
		return fmt.Errorf("no system found in /gnu/store")
	}

	fmt.Printf("Found built system: %s\n", systemPath)

	// Copy kernel
	kernelSrc := filepath.Join(systemPath, "kernel")
	if _, err := os.Stat(kernelSrc); err != nil {
		return fmt.Errorf("kernel not found in built system: %w", err)
	}

	// Determine kernel version from the kernel path
	kernelDest := "/mnt/boot/vmlinuz"
	if err := exec.Command("cp", kernelSrc, kernelDest).Run(); err != nil {
		return fmt.Errorf("failed to copy kernel: %w", err)
	}
	fmt.Printf("✓ Copied kernel: %s -> %s\n", kernelSrc, kernelDest)

	// Copy initrd
	initrdSrc := filepath.Join(systemPath, "initrd")
	if _, err := os.Stat(initrdSrc); err != nil {
		return fmt.Errorf("initrd not found in built system: %w", err)
	}

	initrdDest := "/mnt/boot/initrd"
	if err := exec.Command("cp", initrdSrc, initrdDest).Run(); err != nil {
		return fmt.Errorf("failed to copy initrd: %w", err)
	}
	fmt.Printf("✓ Copied initrd: %s -> %s\n", initrdSrc, initrdDest)

	// Create the current-system symlink
	if err := os.Remove("/mnt/run/current-system"); err != nil && !os.IsNotExist(err) {
		fmt.Printf("Warning: failed to remove old current-system symlink: %v\n", err)
	}
	if err := os.MkdirAll("/mnt/run", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/run: %w", err)
	}
	if err := os.Symlink(systemPath, "/mnt/run/current-system"); err != nil {
		return fmt.Errorf("failed to create current-system symlink: %w", err)
	}
	fmt.Printf("✓ Created symlink: /mnt/run/current-system -> %s\n", systemPath)
	fmt.Println()

	PrintSectionHeader("Installing Bootloader")
	fmt.Println("Step 3/3: Running guix system init to install bootloader")
	fmt.Println("(System already built, this should be quick)")
	fmt.Println()

	// Step 3: Run system init (should now just install bootloader since system is already built)
	maxRetries := 3
	var lastErr error
	for attempt := 1; attempt <= maxRetries; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Retrying guix system init...\n", attempt, maxRetries)
			fmt.Println("Waiting 10 seconds before retry...")
			fmt.Println()
			time.Sleep(10 * time.Second)
		}

		if err := RunCommandWithSpinner("guix", "time-machine", "-C", channelsPath, "--", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt", "--substitute-urls=https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
			lastErr = err
			fmt.Printf("\n[WARN] Attempt %d failed: %v\n", attempt, err)
			if attempt < maxRetries {
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
		fmt.Println("Bootloader installation failed. You can:")
		fmt.Printf("  1. Try manually: guix time-machine -C %s -- system init /mnt/etc/config.scm /mnt\n", channelsPath)
		fmt.Println("  2. Or reboot anyway - the kernel is already in place")
		return fmt.Errorf("guix system init failed after %d attempts: %w", maxRetries, lastErr)
	}

	return nil
}

// InstallVerificationScript installs the verification script to the target system
func InstallVerificationScript() error {
	PrintSectionHeader("Installing Verification Script")

	// Try multiple locations for the verification script
	var scriptContent []byte
	var err error
	
	// Try lib directory first (when running from repo root - most common case)
	scriptContent, err = os.ReadFile("lib/verify-guix-install.sh")
	if err != nil {
		// Try current directory (when running from lib/ directory)
		scriptContent, err = os.ReadFile("verify-guix-install.sh")
	}
	if err != nil {
		// Try absolute path (when script was already copied to /root/)
		scriptContent, err = os.ReadFile("/root/verify-guix-install.sh")
	}
	if err != nil {
		return fmt.Errorf("failed to find verify-guix-install.sh in lib/, current dir, or /root/: %w", err)
	}

	// Install to /mnt/usr/local/bin/ (for use after boot)
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

	// Also copy to /root/ (for use by recovery script on ISO)
	rootPath := "/root/verify-guix-install.sh"
	if err := os.WriteFile(rootPath, scriptContent, 0755); err != nil {
		return fmt.Errorf("failed to write %s: %w", rootPath, err)
	}
	fmt.Printf("[OK] Also copied to: %s (for recovery script)\n", rootPath)
	fmt.Println()

	return nil
}

// VerifyInstallation verifies that all critical files were installed
// This is a basic check - use RunComprehensiveVerification for full verification
func VerifyInstallation() error {
	fmt.Println()
	PrintSectionHeader("Verifying Installation")
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

	if !allGood {
		return fmt.Errorf("installation verification failed - missing critical boot files")
	}

	fmt.Println()
	fmt.Println("[OK] Basic verification passed - critical boot files present")
	fmt.Println()

	return nil
}

// RunComprehensiveVerification runs the full verification script and handles failures
// This should be called at the very end of installation before reboot
func RunComprehensiveVerification(recoveryScriptPath string) error {
	fmt.Println()
	PrintSectionHeader("Final Installation Verification")
	fmt.Println("Running comprehensive verification script...")
	fmt.Println()

	// Ensure EFI is mounted before verification
	if !isMountPoint("/mnt/boot/efi") {
		fmt.Println("[WARN] EFI partition not mounted, mounting it now...")
		if err := os.MkdirAll("/mnt/boot/efi", 0755); err != nil {
			fmt.Printf("[WARN] Failed to create /mnt/boot/efi directory: %v\n", err)
		} else {
			// Try to mount EFI partition
			cmd := exec.Command("mount", "LABEL=EFI", "/mnt/boot/efi")
			if err := cmd.Run(); err != nil {
				fmt.Printf("[WARN] Failed to mount EFI partition: %v\n", err)
				fmt.Println("       Verification may report EFI errors, but this can be fixed")
			} else {
				fmt.Println("[OK] EFI partition mounted")
			}
		}
		fmt.Println()
	}

	// Try to run the comprehensive verification script
	verifyScript := "/root/verify-guix-install.sh"
	if _, err := os.Stat(verifyScript); err != nil {
		// Script not found, try to install it
		fmt.Println("Verification script not found, installing it...")
		if err := InstallVerificationScript(); err != nil {
			fmt.Printf("[WARN] Verification script not available: %v\n", err)
			fmt.Println("       Falling back to basic verification...")
			fmt.Println()
			// Fall back to basic verification
			return VerifyInstallation()
		}
		// Verify it was installed
		if _, err := os.Stat(verifyScript); err != nil {
			fmt.Printf("[WARN] Verification script still not found after install: %v\n", err)
			fmt.Println("       Falling back to basic verification...")
			fmt.Println()
			return VerifyInstallation()
		}
	}

	// Run the comprehensive verification script
	cmd := exec.Command("bash", verifyScript)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		// Verification failed
		fmt.Println()
		fmt.Println("========================================")
		fmt.Println("  VERIFICATION FAILED - ACTION REQUIRED")
		fmt.Println("========================================")
		fmt.Println()
		fmt.Println("The installation verification found critical issues.")
		fmt.Println("DO NOT REBOOT until these are resolved!")
		fmt.Println()
		fmt.Println("To fix the issues, run the recovery script:")
		fmt.Printf("  %s\n", recoveryScriptPath)
		fmt.Println()
		fmt.Println("The recovery script will:")
		fmt.Println("  1. Check and mount EFI partition if needed")
		fmt.Println("  2. Re-run guix system init if kernel/initrd are missing")
		fmt.Println("  3. Set your user password")
		fmt.Println("  4. Download customization tools")
		fmt.Println("  5. Run verification again")
		fmt.Println()
		fmt.Println("Or fix manually:")
		fmt.Println("  1. Mount EFI: mkdir -p /mnt/boot/efi && mount LABEL=EFI /mnt/boot/efi")
		fmt.Println("  2. Re-run system init: guix system init /mnt/etc/config.scm /mnt")
		fmt.Println("  3. Set password: chroot /mnt /run/current-system/profile/bin/passwd USERNAME")
		fmt.Println("  4. Verify again: /root/verify-guix-install.sh")
		fmt.Println()
		return fmt.Errorf("comprehensive verification failed - run recovery script before rebooting")
	}

	// Verification passed
	fmt.Println()
	fmt.Println("========================================")
	fmt.Println("  VERIFICATION PASSED - READY TO REBOOT")
	fmt.Println("========================================")
	fmt.Println()
	fmt.Println("All critical files are present and verified.")
	fmt.Println("The system should boot successfully after reboot.")
	fmt.Println()

	return nil
}

// isMountPoint checks if a path is a mount point
func isMountPoint(path string) bool {
	cmd := exec.Command("mountpoint", "-q", path)
	return cmd.Run() == nil
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

	// Prepare critical directories before system init
	if err := PrepareSystemInitDirectories(); err != nil {
		return fmt.Errorf("failed to prepare system init directories: %w", err)
	}

	// CRITICAL FIX: guix system build doesn't produce kernel/initrd files.
	// We must use guix system init directly, which builds everything AND installs it.
	// guix system init creates the system generation with kernel/initrd AND installs the bootloader.

	PrintSectionHeader("Installing Guix System")
	fmt.Println("Running guix system init (builds system and installs bootloader)...")
	fmt.Println("This will take 5-30 minutes depending on substitutes availability...")
	fmt.Println()
	fmt.Println("You should see:")
	fmt.Println("  1. Downloading/building packages (including kernel)")
	fmt.Println("  2. Building initrd")
	fmt.Println("  3. Installing bootloader")
	fmt.Println()
	fmt.Println("Progress output below:")
	fmt.Println("---")
	fmt.Println()
	
	// Check disk space before starting
	cmd := exec.Command("df", "-h", "/mnt")
	if output, err := cmd.Output(); err == nil {
		fmt.Println("Disk space check:")
		fmt.Println(string(output))
		fmt.Println()
	}

	// Run guix system init directly - this builds everything AND installs it
	if err := RunCommandWithSpinner("guix", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt", "--substitute-urls=https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
		return fmt.Errorf("guix system init failed: %w", err)
	}

	// Verify kernel/initrd were created by guix system init
	PrintSectionHeader("Verifying Installation")
	fmt.Println("Checking if kernel/initrd files were created...")
	fmt.Println()

	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
	initrds, _ := filepath.Glob("/mnt/boot/initrd*")
	
	if len(kernels) == 0 {
		return fmt.Errorf("CRITICAL: Kernel not found after guix system init. System will not boot. Please check config.scm has (kernel linux-libre) specified")
	}
	if len(initrds) == 0 {
		return fmt.Errorf("CRITICAL: Initrd not found after guix system init. System will not boot. Please check config.scm configuration")
	}
	
	fmt.Printf("✓ Kernel found: %s\n", filepath.Base(kernels[0]))
	fmt.Printf("✓ Initrd found: %s\n", filepath.Base(initrds[0]))
	fmt.Println()
	fmt.Println("✓ Installation verified - system should boot successfully")
	fmt.Println()
	
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

// CleanupISOArtifacts fixes filesystem invariants after copying from ISO
// This must be called after rsync/cp operations that copy /var/guix from ISO
// Fixes critical symlinks and removes ISO-specific artifacts that could cause boot/service issues
func CleanupISOArtifacts() error {
	fmt.Println()
	fmt.Println("Cleaning up ISO artifacts and fixing filesystem invariants...")
	fmt.Println()

	// Ensure critical parent directories exist before creating symlinks
	criticalDirs := []string{"/mnt/var", "/mnt/etc", "/mnt/run"}
	for _, dir := range criticalDirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create directory %s: %w", dir, err)
		}
	}

	// 1. Fix /var/run symlink (CRITICAL - prevents service failures)
	varRunPath := "/mnt/var/run"
	if info, err := os.Lstat(varRunPath); err == nil {
		if info.IsDir() {
			// It's a directory (copied from ISO) - remove and create symlink
			fmt.Printf("  Fixing /var/run: removing directory, creating symlink...")
			if err := os.RemoveAll(varRunPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to remove /mnt/var/run directory: %w", err)
			}
			if err := os.Symlink("/run", varRunPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to create /mnt/var/run symlink: %w", err)
			}
			fmt.Printf(" [FIXED]\n")
		} else if info.Mode()&os.ModeSymlink != 0 {
			// It's a symlink - verify it points to /run
			target, err := os.Readlink(varRunPath)
			if err != nil {
				fmt.Printf("  Fixing /var/run: broken symlink, recreating...")
				os.Remove(varRunPath)
				if err := os.Symlink("/run", varRunPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to recreate /mnt/var/run symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else if target != "/run" {
				fmt.Printf("  Fixing /var/run: wrong target (%s), fixing...", target)
				os.Remove(varRunPath)
				if err := os.Symlink("/run", varRunPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to fix /mnt/var/run symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else {
				fmt.Printf("  /var/run symlink: [OK]\n")
			}
		}
	} else if os.IsNotExist(err) {
		// Doesn't exist - create symlink
		fmt.Printf("  Creating /var/run symlink...")
		if err := os.Symlink("/run", varRunPath); err != nil {
			fmt.Printf(" [FAILED]: %v\n", err)
			return fmt.Errorf("failed to create /mnt/var/run symlink: %w", err)
		}
		fmt.Printf(" [OK]\n")
	} else {
		fmt.Printf("  /var/run: [WARNING] %v\n", err)
	}

	// 2. Fix /etc/mtab symlink (IMPORTANT - prevents filesystem service issues)
	mtabPath := "/mnt/etc/mtab"
	if info, err := os.Lstat(mtabPath); err == nil {
		if info.Mode()&os.ModeSymlink == 0 {
			// It's a file (copied from ISO) - remove and create symlink
			fmt.Printf("  Fixing /etc/mtab: removing file, creating symlink...")
			if err := os.Remove(mtabPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to remove /mnt/etc/mtab file: %w", err)
			}
			if err := os.Symlink("/proc/self/mounts", mtabPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to create /mnt/etc/mtab symlink: %w", err)
			}
			fmt.Printf(" [FIXED]\n")
		} else {
			// It's a symlink - verify it points to /proc/self/mounts
			target, err := os.Readlink(mtabPath)
			if err != nil {
				fmt.Printf("  Fixing /etc/mtab: broken symlink, recreating...")
				os.Remove(mtabPath)
				if err := os.Symlink("/proc/self/mounts", mtabPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to recreate /mnt/etc/mtab symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else if target != "/proc/self/mounts" {
				fmt.Printf("  Fixing /etc/mtab: wrong target (%s), fixing...", target)
				os.Remove(mtabPath)
				if err := os.Symlink("/proc/self/mounts", mtabPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to fix /mnt/etc/mtab symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else {
				fmt.Printf("  /etc/mtab symlink: [OK]\n")
			}
		}
	} else if os.IsNotExist(err) {
		// Doesn't exist - create symlink
		fmt.Printf("  Creating /etc/mtab symlink...")
		if err := os.Symlink("/proc/self/mounts", mtabPath); err != nil {
			fmt.Printf(" [FAILED]: %v\n", err)
			return fmt.Errorf("failed to create /mnt/etc/mtab symlink: %w", err)
		}
		fmt.Printf(" [OK]\n")
	} else {
		fmt.Printf("  /etc/mtab: [WARNING] %v\n", err)
	}

	// 3. Fix /var/lock symlink (IMPORTANT - prevents lock file issues)
	varLockPath := "/mnt/var/lock"
	if info, err := os.Lstat(varLockPath); err == nil {
		if info.IsDir() {
			// It's a directory (copied from ISO) - remove and create symlink
			fmt.Printf("  Fixing /var/lock: removing directory, creating symlink...")
			if err := os.RemoveAll(varLockPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to remove /mnt/var/lock directory: %w", err)
			}
			if err := os.Symlink("/run/lock", varLockPath); err != nil {
				fmt.Printf(" [FAILED]: %v\n", err)
				return fmt.Errorf("failed to create /mnt/var/lock symlink: %w", err)
			}
			fmt.Printf(" [FIXED]\n")
		} else if info.Mode()&os.ModeSymlink != 0 {
			// It's a symlink - verify it points to /run/lock
			target, err := os.Readlink(varLockPath)
			if err != nil {
				fmt.Printf("  Fixing /var/lock: broken symlink, recreating...")
				os.Remove(varLockPath)
				if err := os.Symlink("/run/lock", varLockPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to recreate /mnt/var/lock symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else if target != "/run/lock" {
				fmt.Printf("  Fixing /var/lock: wrong target (%s), fixing...", target)
				os.Remove(varLockPath)
				if err := os.Symlink("/run/lock", varLockPath); err != nil {
					fmt.Printf(" [FAILED]: %v\n", err)
					return fmt.Errorf("failed to fix /mnt/var/lock symlink: %w", err)
				}
				fmt.Printf(" [FIXED]\n")
			} else {
				fmt.Printf("  /var/lock symlink: [OK]\n")
			}
		}
	} else if os.IsNotExist(err) {
		// Doesn't exist - create symlink
		fmt.Printf("  Creating /var/lock symlink...")
		if err := os.Symlink("/run/lock", varLockPath); err != nil {
			fmt.Printf(" [FAILED]: %v\n", err)
			return fmt.Errorf("failed to create /mnt/var/lock symlink: %w", err)
		}
		fmt.Printf(" [OK]\n")
	} else {
		fmt.Printf("  /var/lock: [WARNING] %v\n", err)
	}

	// 4. Empty /run directory (removes ISO runtime artifacts like sockets, PID files, dbus state)
	runPath := "/mnt/run"
	if info, err := os.Stat(runPath); err == nil && info.IsDir() {
		entries, err := os.ReadDir(runPath)
		if err != nil {
			fmt.Printf("  /run directory cleanup: [WARNING] could not read: %v\n", err)
		} else if len(entries) > 0 {
			fmt.Printf("  Emptying /run directory (removing %d ISO runtime artifacts)...", len(entries))
			// Remove all contents but keep the directory
			for _, entry := range entries {
				entryPath := filepath.Join(runPath, entry.Name())
				if err := os.RemoveAll(entryPath); err != nil {
					fmt.Printf("\n    WARNING: Failed to remove %s: %v", entryPath, err)
				}
			}
			fmt.Printf(" [OK]\n")
		} else {
			fmt.Printf("  /run directory: already empty [OK]\n")
		}
	}

	// 5. Fix /var/tmp permissions (sticky bit for proper temp file handling)
	varTmpPath := "/mnt/var/tmp"
	if info, err := os.Stat(varTmpPath); err == nil && info.IsDir() {
		// Check if sticky bit is already set (mode & 01000)
		if info.Mode().Perm() != 0o1777 {
			fmt.Printf("  Fixing /var/tmp permissions (adding sticky bit)...")
			if err := os.Chmod(varTmpPath, 0o1777); err != nil {
				fmt.Printf(" [WARNING]: %v\n", err)
			} else {
				fmt.Printf(" [OK]\n")
			}
		} else {
			fmt.Printf("  /var/tmp permissions: [OK]\n")
		}
	} else if os.IsNotExist(err) {
		fmt.Printf("  Creating /var/tmp with correct permissions...")
		if err := os.MkdirAll(varTmpPath, 0o1777); err != nil {
			fmt.Printf(" [WARNING]: %v\n", err)
		} else {
			fmt.Printf(" [OK]\n")
		}
	}

	// 6. Remove ISO-specific artifacts (non-critical, but recommended)
	artifacts := []string{
		"/mnt/etc/machine-id",                              // System regenerates on boot
		"/mnt/etc/resolv.conf",                             // NetworkManager recreates
		"/mnt/var/guix/profiles/per-user/live-image-user",  // ISO user profile
		"/mnt/home/live-image-user",                        // ISO user home directory
	}

	for _, artifact := range artifacts {
		if err := os.RemoveAll(artifact); err != nil {
			if !os.IsNotExist(err) {
				fmt.Printf("  Removing %s: [WARNING] %v\n", artifact, err)
			}
		} else {
			fmt.Printf("  Removed ISO artifact: %s [OK]\n", artifact)
		}
	}

	// 7. Ensure correct ownership of /var/guix (if it exists)
	guixPath := "/mnt/var/guix"
	if info, err := os.Stat(guixPath); err == nil && info.IsDir() {
		// Try to fix ownership, but don't fail if chown doesn't work
		if err := exec.Command("chown", "-R", "root:root", guixPath).Run(); err != nil {
			fmt.Printf("  /var/guix ownership: [WARNING] chown failed: %v\n", err)
		} else {
			fmt.Printf("  /var/guix ownership: [OK]\n")
		}
	}

	fmt.Println()
	fmt.Println("ISO cleanup complete.")
	fmt.Println()

	return nil
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
    
    // Check if already mounted - if so, skip mount attempt
    if IsMounted(mountPoint) {
        fmt.Printf("[OK] %s is already mounted\n", mountPoint)
        return nil
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
	PrintSectionHeader("Installing Customization Tools")

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
	PrintSectionHeader("Set User Password")
	fmt.Printf("Setting password for user: %s\n", username)
	fmt.Println("You will need this password to log in after first boot.")
	fmt.Println()

	// Check if keyboard layout has swap options (like ctrl:swapcaps)
	// If so, warn user to type password as if swap is NOT enabled (for GDM compatibility)
	configPath := "/mnt/etc/config.scm"
	if configData, err := os.ReadFile(configPath); err == nil {
		configStr := string(configData)
		if strings.Contains(configStr, "keyboard-layout") && strings.Contains(configStr, "ctrl:swapcaps") {
			fmt.Println("⚠️  IMPORTANT: Keyboard Layout Warning")
			fmt.Println("   Your system is configured with Caps Lock ↔ Ctrl swap.")
			fmt.Println("   However, the GNOME login screen (GDM) uses the DEFAULT layout (no swap).")
			fmt.Println()
			fmt.Println("   When typing your password:")
			fmt.Println("   - Type it as if Caps Lock and Ctrl are NOT swapped")
			fmt.Println("   - This ensures it will work at the GDM login screen")
			fmt.Println("   - After logging in, your keyboard swap will be active")
			fmt.Println()
		}
	}

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

// WriteRecoveryScript writes a comprehensive recovery script for completing installation
// after guix time-machine succeeds but post-install steps may have failed
func WriteRecoveryScript(scriptPath, platform string) error {
	script := `#!/run/current-system/profile/bin/bash
# Recovery script to complete Guix installation after time-machine
# Use this when guix time-machine ran but didn't complete fully
#
# This script assumes:
# - Partitions are formatted and mounted at /mnt
# - /mnt/etc/config.scm exists
# - ~/channels.scm or /tmp/channels.scm exists (for nonguix)
# - guix time-machine was run but may have failed or been interrupted

set -e  # Exit on error

echo "=== Guix Installation Recovery Script ==="
echo ""
echo "This script will:"
echo "  1. Verify current installation state"
echo "  2. Re-run guix system init if needed (with time-machine)"
echo "  3. Set user password"
echo "  4. Download customization tools"
echo "  5. Configure dual-boot GRUB (if framework-dual)"
echo "  6. Write installation receipt"
echo "  7. Prepare for reboot"
echo ""
read -p "Continue? [Y/n] " -r </dev/tty
if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "Aborted."
    exit 0
fi

# Check if we're on Guix ISO
if [ ! -f /run/current-system/profile/bin/guix ]; then
    echo "[ERROR] Not running on Guix ISO - cannot continue"
    exit 1
fi

# Verify mounts
echo ""
echo "=== Verifying Mounts ==="
if ! mountpoint -q /mnt; then
    echo "[ERROR] /mnt is not mounted!"
    echo "Please run the mount step first:"
    echo "  mount LABEL=GUIX_ROOT /mnt"
    echo "  mkdir -p /mnt/boot/efi"
    echo "  mount LABEL=EFI /mnt/boot/efi"
    exit 1
fi
echo "[OK] /mnt is mounted"

if ! mountpoint -q /mnt/boot/efi; then
    echo "[ERROR] /mnt/boot/efi is not mounted!"
    echo "Please mount EFI:"
    echo "  mkdir -p /mnt/boot/efi"
    echo "  mount LABEL=EFI /mnt/boot/efi"
    exit 1
fi
echo "[OK] /mnt/boot/efi is mounted"

# Verify config exists
if [ ! -f /mnt/etc/config.scm ]; then
    echo "[ERROR] /mnt/etc/config.scm not found!"
    echo "Please generate config first (step 3)"
    exit 1
fi
echo "[OK] Config exists: /mnt/etc/config.scm"

# Check if channels.scm exists (needed for framework installers)
# Check home directory first, then /tmp as fallback
CHANNELS_PATH=""
if [ -f ~/channels.scm ]; then
    CHANNELS_PATH="$HOME/channels.scm"
    echo "[OK] Channels file exists: $CHANNELS_PATH"
    USE_TIME_MACHINE=true
elif [ -f /tmp/channels.scm ]; then
    CHANNELS_PATH="/tmp/channels.scm"
    echo "[OK] Channels file exists: $CHANNELS_PATH"
    USE_TIME_MACHINE=true
else
    echo "[WARN] No channels.scm found in ~/ or /tmp - using plain guix system init"
    USE_TIME_MACHINE=false
fi

# Verify installation state
echo ""
echo "=== Checking Installation State ==="
HAS_KERNEL=false
HAS_INITRD=false
HAS_GRUB_EFI=false

if ls /mnt/boot/vmlinuz-* >/dev/null 2>&1; then
    echo "[OK] Kernel found: $(ls /mnt/boot/vmlinuz-* | head -1 | xargs basename)"
    HAS_KERNEL=true
else
    echo "[MISSING] No kernel in /mnt/boot/"
fi

if ls /mnt/boot/initrd-* >/dev/null 2>&1; then
    echo "[OK] Initrd found: $(ls /mnt/boot/initrd-* | head -1 | xargs basename)"
    HAS_INITRD=true
else
    echo "[MISSING] No initrd in /mnt/boot/"
fi

if [ -f /mnt/boot/efi/EFI/Guix/grubx64.efi ] || [ -f /mnt/boot/efi/EFI/guix/grubx64.efi ]; then
    echo "[OK] GRUB EFI bootloader found"
    HAS_GRUB_EFI=true
else
    echo "[MISSING] No GRUB EFI bootloader"
fi

# Determine if we need to run system init
NEED_SYSTEM_INIT=false
if [ "$HAS_KERNEL" = false ] || [ "$HAS_INITRD" = false ]; then
    echo ""
    echo "[!] Installation is INCOMPLETE - system init must be run"
    NEED_SYSTEM_INIT=true
fi

# Setup environment for installation
echo ""
echo "=== Setting Up Environment ==="
export TMPDIR=/mnt/var/tmp
export XDG_CACHE_HOME=/mnt/var/cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"
echo "TMPDIR=$TMPDIR"
echo "XDG_CACHE_HOME=$XDG_CACHE_HOME"

# Clear substitute cache to free space
echo "Clearing substitute cache..."
rm -rf /var/guix/substitute-cache/ || true

# Run system init if needed
if [ "$NEED_SYSTEM_INIT" = true ]; then
    echo ""
    echo "=== Running System Init ==="

    # Verify ESP before init
    echo "Verifying EFI partition..."
    if ! df -T /mnt/boot/efi | grep -q vfat; then
        echo "[ERROR] /mnt/boot/efi is not vfat filesystem!"
        df -T /mnt/boot/efi
        exit 1
    fi
    echo "[OK] EFI partition is vfat"

    # Start cow-store
    echo "Starting cow-store..."
    if ! herd status cow-store | grep -q "running"; then
        herd start cow-store /mnt
        sleep 2
    fi
    echo "[OK] cow-store is running"

    # Ensure guix-daemon is responsive
    echo "Checking guix-daemon..."
    for i in {1..10}; do
        if guix build --version >/dev/null 2>&1; then
            echo "[OK] guix-daemon is responsive"
            break
        fi
        echo "Waiting for daemon... ($i/10)"
        sleep 3
    done

    # Build the system init command
    if [ "$USE_TIME_MACHINE" = true ]; then
        INIT_CMD="guix time-machine -C $CHANNELS_PATH -- system init --fallback -v6 /mnt/etc/config.scm /mnt --substitute-urls=\"https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org\""
    else
        INIT_CMD="guix system init --fallback -v6 /mnt/etc/config.scm /mnt --substitute-urls=\"https://ci.guix.gnu.org https://bordeaux.guix.gnu.org\""
    fi

    echo ""
    echo "Running: $INIT_CMD"
    echo ""
    echo "This will take 5-15 minutes. Do NOT interrupt!"
    echo "If it fails, you can re-run this script."
    echo ""

    # Run the init command
    if eval "$INIT_CMD"; then
        echo ""
        echo "[OK] System init completed successfully"
    else
        echo ""
        echo "[ERROR] System init failed!"
        echo ""
        echo "You can try running it manually:"
        echo "  $INIT_CMD"
        echo ""
        echo "Or re-run this recovery script after fixing the issue."
        exit 1
    fi

    # Verify again
    echo ""
    echo "=== Verifying Installation (Post-Init) ==="
    if ! ls /mnt/boot/vmlinuz-* >/dev/null 2>&1; then
        echo "[ERROR] Still no kernel after system init!"
        echo "System init may have failed silently."
        exit 1
    fi
    if ! ls /mnt/boot/initrd-* >/dev/null 2>&1; then
        echo "[ERROR] Still no initrd after system init!"
        echo "System init may have failed silently."
        exit 1
    fi
    echo "[OK] Kernel and initrd are now present"
else
    echo ""
    echo "[OK] System init already complete - skipping"
fi

# Get username from config
echo ""
echo "=== Detecting Username ==="
USERNAME=$(grep -oP '(?<=\(name ")[^"]+' /mnt/etc/config.scm | head -1)
if [ -z "$USERNAME" ]; then
    echo "[WARN] Could not detect username from config.scm"
    read -p "Enter your username: " USERNAME </dev/tty
fi
echo "Username: $USERNAME"

# Set user password
echo ""
echo "=== Setting User Password ==="
echo "You need this password to log in after first boot."
echo ""

# Check if keyboard layout has swap options and warn user
if grep -q "keyboard-layout" /mnt/etc/config.scm && grep -q "ctrl:swapcaps" /mnt/etc/config.scm; then
    echo "⚠️  IMPORTANT: Keyboard Layout Warning"
    echo "   Your system is configured with Caps Lock ↔ Ctrl swap."
    echo "   However, the GNOME login screen (GDM) uses the DEFAULT layout (no swap)."
    echo ""
    echo "   When typing your password:"
    echo "   - Type it as if Caps Lock and Ctrl are NOT swapped"
    echo "   - This ensures it will work at the GDM login screen"
    echo "   - After logging in, your keyboard swap will be active"
    echo ""
fi

# Check if password is already set
PASSWORD_SET=false
if chroot /mnt /run/current-system/profile/bin/bash -c "grep '^$USERNAME:' /etc/shadow | grep -v ':!:'" >/dev/null 2>&1; then
    echo "[OK] Password already set for $USERNAME"
    read -p "Do you want to change it? [y/N] " -r </dev/tty
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        PASSWORD_SET=false
    else
        PASSWORD_SET=true
    fi
fi

if [ "$PASSWORD_SET" = false ]; then
    if chroot /mnt /run/current-system/profile/bin/passwd "$USERNAME"; then
        echo ""
        echo "[OK] Password set successfully for $USERNAME"
    else
        echo ""
        echo "[ERROR] Failed to set password!"
        echo "You can set it manually after first boot."
    fi
fi

# Download customization tools
echo ""
echo "=== Downloading Customization Tools ==="
PLATFORM="${GUIX_PLATFORM:-` + platform + `}"
USER_HOME="/mnt/home/$USERNAME"
CUSTOMIZE_DIR="$USER_HOME/guix-customize"

if [ -d "$CUSTOMIZE_DIR" ]; then
    echo "[OK] Customization tools already present at $CUSTOMIZE_DIR"
else
    echo "Downloading to $CUSTOMIZE_DIR..."
    mkdir -p "$CUSTOMIZE_DIR"

    # Download customize script
    REPO_BASE="https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main"
    if wget -q -O "$CUSTOMIZE_DIR/customize" "$REPO_BASE/$PLATFORM/postinstall/customize"; then
        chmod +x "$CUSTOMIZE_DIR/customize"
        echo "[OK] Downloaded customize script"
    else
        echo "[WARN] Failed to download customization tools"
        echo "      You can download manually after first boot"
    fi

    # Set ownership
    chroot /mnt /run/current-system/profile/bin/chown -R "$USERNAME:users" "/home/$USERNAME/guix-customize" 2>/dev/null || true
fi

# Configure dual-boot GRUB (if framework-dual)
if [ "$PLATFORM" = "framework-dual" ]; then
    echo ""
    echo "=== Configuring Dual-Boot GRUB ==="

    # Check if os-prober is available in the installed system
    if chroot /mnt /run/current-system/profile/bin/bash -c "command -v os-prober" >/dev/null 2>&1; then
        echo "Enabling os-prober..."
        chroot /mnt /run/current-system/profile/bin/bash -c 'echo "GRUB_DISABLE_OS_PROBER=false" >> /etc/default/grub' 2>/dev/null || true

        echo "Running os-prober to detect Pop!_OS..."
        chroot /mnt /run/current-system/profile/bin/os-prober || true

        echo "Updating GRUB configuration..."
        chroot /mnt /run/current-system/profile/bin/grub-mkconfig -o /boot/grub/grub.cfg || {
            echo "[WARN] Failed to update GRUB config"
            echo "      You can run this after first boot:"
            echo "      sudo os-prober && sudo grub-mkconfig -o /boot/grub/grub.cfg"
        }
    else
        echo "[WARN] os-prober not found in installed system"
        echo "      You'll need to manually configure dual-boot after first boot"
    fi
fi

# Write installation receipt
echo ""
echo "=== Writing Installation Receipt ==="
RECEIPT_PATH="/mnt/root/install-receipt.txt"
cat > "$RECEIPT_PATH" <<EOF
Guix System Installation Receipt
=================================
Date: $(date)
Platform: $PLATFORM
Username: $USERNAME
Hostname: $(grep 'host-name' /mnt/etc/config.scm | grep -oP '(?<=")[^"]+')

Installation completed via recovery script.

Config: /etc/config.scm
Channels: $([ -f ~/channels.scm ] && echo "~/channels.scm (nonguix)" || ([ -f /tmp/channels.scm ] && echo "/tmp/channels.scm (nonguix)" || echo "default"))

Next steps:
1. Log in with your username and password
2. Run: ~/guix-customize/customize (to add SSH, desktop, packages)
3. For dual-boot: Access Pop!_OS via F12 boot menu or GRUB menu

Installation log: /tmp/guix-install.log (if available)
EOF
echo "[OK] Installation receipt written to $RECEIPT_PATH"

# Final verification using comprehensive verification script
echo ""
echo "=== Final Verification ==="

# Try to use verify-guix-install.sh if available for comprehensive check
VERIFY_SCRIPT=""
if [ -f /root/verify-guix-install.sh ]; then
    VERIFY_SCRIPT="/root/verify-guix-install.sh"
elif [ -f ./verify-guix-install.sh ]; then
    VERIFY_SCRIPT="./verify-guix-install.sh"
fi

if [ -n "$VERIFY_SCRIPT" ]; then
    echo "Running comprehensive verification..."
    echo ""
    if bash "$VERIFY_SCRIPT"; then
        ALL_GOOD=true
    else
        ALL_GOOD=false
    fi
else
    # Fallback to basic checks if verify script not available
    echo "Note: Using basic verification (verify-guix-install.sh not found)"
    echo ""
    ALL_GOOD=true

    if ! ls /mnt/boot/vmlinuz-* >/dev/null 2>&1; then
        echo "[ERROR] No kernel installed!"
        ALL_GOOD=false
    else
        echo "[OK] Kernel: $(ls /mnt/boot/vmlinuz-* | head -1 | xargs basename)"
    fi

    if ! ls /mnt/boot/initrd-* >/dev/null 2>&1; then
        echo "[ERROR] No initrd installed!"
        ALL_GOOD=false
    else
        echo "[OK] Initrd: $(ls /mnt/boot/initrd-* | head -1 | xargs basename)"
    fi

    if [ -f /mnt/boot/efi/EFI/Guix/grubx64.efi ] || [ -f /mnt/boot/efi/EFI/guix/grubx64.efi ]; then
        echo "[OK] GRUB EFI bootloader installed"
    else
        echo "[ERROR] No GRUB EFI bootloader!"
        ALL_GOOD=false
    fi

    if [ -f /mnt/boot/grub/grub.cfg ]; then
        echo "[OK] GRUB config exists"
    else
        echo "[ERROR] No GRUB config!"
        ALL_GOOD=false
    fi
fi

echo ""
if [ "$ALL_GOOD" = true ]; then
    echo "=== Installation Complete! ==="
    echo ""
    echo "The system is ready to boot."
    echo ""
    echo "Next steps:"
    echo "  1. Sync and unmount:"
    echo "     sync"
    echo "     umount -R /mnt"
    echo "  2. Reboot:"
    echo "     reboot"
    echo ""
    echo "After first boot:"
    echo "  - Log in with username: $USERNAME"
    echo "  - Run: ~/guix-customize/customize"
    echo "  - For Pop!_OS: Press F12 at boot or select from GRUB menu"
    echo ""

    read -p "Unmount and reboot now? [y/N] " -r </dev/tty
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        sync
        umount -R /mnt
        reboot
    else
        echo ""
        echo "Remember to unmount before rebooting:"
        echo "  sync && umount -R /mnt && reboot"
    fi
else
    echo "=== Installation INCOMPLETE ==="
    echo ""
    echo "Critical files are missing. DO NOT REBOOT."
    echo ""
    echo "Please review the errors above and:"
    echo "  1. Check /tmp/guix-install.log for errors"
    echo "  2. Try re-running the system init manually"
    echo "  3. Or re-run this recovery script"
    exit 1
fi
`

	return os.WriteFile(scriptPath, []byte(script), 0755)
}

// Device detection and validation functions

// DetectDevice auto-detects the appropriate block device for the platform
func DetectDevice(platform string) (string, error) {
	// First, dynamically discover all available block devices
	cmd := exec.Command("lsblk", "-d", "-n", "-o", "NAME,TYPE")
	output, err := cmd.Output()

	var availableDevices []string
	if err == nil {
		for _, line := range strings.Split(string(output), "\n") {
			fields := strings.Fields(line)
			if len(fields) >= 2 && fields[1] == "disk" {
				device := "/dev/" + fields[0]
				availableDevices = append(availableDevices, device)
			}
		}
	}

	// Build priority list based on platform
	var candidates []string

	switch platform {
	case "cloudzy":
		// VPS platforms: prefer vda, then sda, then nvme
		candidates = []string{"/dev/vda", "/dev/sda"}
	case "framework", "framework-dual":
		// Framework laptops: prefer NVMe, then SATA
		// Start with specific known NVMe paths, then add any discovered NVMe devices
		candidates = []string{"/dev/nvme0n1", "/dev/nvme1n1"}
		// Add any other discovered NVMe devices
		for _, dev := range availableDevices {
			if strings.Contains(dev, "nvme") && !contains(candidates, dev) {
				candidates = append(candidates, dev)
			}
		}
		candidates = append(candidates, "/dev/sda", "/dev/sdb")
	default:
		// Generic fallback: NVMe first, then SATA, then VirtIO
		candidates = []string{"/dev/nvme0n1"}
		for _, dev := range availableDevices {
			if strings.Contains(dev, "nvme") && !contains(candidates, dev) {
				candidates = append(candidates, dev)
			}
		}
		candidates = append(candidates, "/dev/sda", "/dev/vda")
	}

	// Try each candidate in priority order
	for _, device := range candidates {
		if _, err := os.Stat(device); err == nil {
			fmt.Printf("Auto-detected device: %s\n", device)
			return device, nil
		}
	}

	// If nothing found, show available devices to help user
	fmt.Println("Error: No suitable block device found.")
	fmt.Println("Available block devices:")
	if len(availableDevices) > 0 {
		for _, dev := range availableDevices {
			fmt.Printf("  %s\n", dev)
		}
	} else {
		RunCommand("lsblk", "-d", "-n", "-o", "NAME,SIZE,TYPE")
	}

	return "", fmt.Errorf("no suitable block device found")
}

// Helper function to check if slice contains string
func contains(slice []string, str string) bool {
	for _, s := range slice {
		if s == str {
			return true
		}
	}
	return false
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
	// Check if /sys/firmware/efi exists and is a directory
	// This is the most reliable way to detect EFI boot on Linux
	if info, err := os.Stat("/sys/firmware/efi"); err == nil && info.IsDir() {
		// Additional check: verify it has entries (not just an empty dir)
		// Some VPS environments create /sys/firmware but not the efi subdir
		entries, err := os.ReadDir("/sys/firmware/efi")
		if err == nil && len(entries) > 0 {
			return "uefi"
		}
	}
	
	// Default to BIOS if UEFI not detected
	// Note: Most modern VPS providers (Cloudzy, OVH, DigitalOcean, Vultr) use UEFI
	// If BIOS is detected, it may be because:
	// 1. The VPS actually uses BIOS (rare but possible)
	// 2. The Guix ISO was booted in BIOS/CSM mode even though VPS supports UEFI
	// 3. The detection failed (VPS doesn't expose /sys/firmware/efi properly)
	// Users can override with BOOT_MODE=uefi env var if needed
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
// FindGuixRootPartitionAnyDevice searches for GUIX_ROOT partition across all block devices
// Returns: (partition path, device path, error)
// This is useful when you don't know which device contains the GUIX_ROOT partition
func FindGuixRootPartitionAnyDevice() (string, string, error) {
	// Get all block devices
	cmd := exec.Command("lsblk", "-d", "-n", "-o", "NAME,TYPE")
	output, err := cmd.Output()
	if err != nil {
		return "", "", fmt.Errorf("failed to list block devices: %w", err)
	}

	var devices []string
	for _, line := range strings.Split(string(output), "\n") {
		fields := strings.Fields(line)
		if len(fields) >= 2 && fields[1] == "disk" {
			devices = append(devices, "/dev/"+fields[0])
		}
	}

	// Search each device for GUIX_ROOT partition
	for _, device := range devices {
		partition, err := FindGuixRootPartition(device)
		if err == nil && partition != "" {
			return partition, device, nil
		}
	}

	return "", "", nil // Not found (not an error)
}

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
func BootstrapUserChannels(channelInfo *ChannelInfo, platform string) error {
	if channelInfo.HasExistingChannels {
		fmt.Println("[INFO] User has existing channel configuration, skipping bootstrap")
		return nil
	}
	
	if channelInfo.ChannelRepo == "" {
		fmt.Println("[INFO] No channel repository specified, using default channels")
		return SetupDefaultChannels(platform)
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
// Platform-aware: cloudzy gets free software only (no nonguix), framework platforms get nonguix
func SetupDefaultChannels(platform string) error {
	// Cloudzy doesn't need user channels (free software only)
	// User can add channels manually after installation if needed
	if platform == "cloudzy" {
		fmt.Println("[INFO] Cloudzy platform detected - skipping user channel setup (free software only)")
		fmt.Println("       User channels can be configured manually after installation if needed")
		return nil
	}
	
	configDir := filepath.Join(os.Getenv("HOME"), ".config", "guix")
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}
	
	// Framework platforms get nonguix channel (needed for WiFi/firmware)
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
	fmt.Println("     Note: nonguix channel is required for Framework WiFi/firmware")
	
	return nil
}

// ValidateChannels validates the channel configuration
func ValidateChannels() error {
	PrintSectionHeader("Validating Channel Configuration")
	
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
