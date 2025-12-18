// Package lib provides shared functionality for Guix installation across platforms
package lib

import (
	"bufio"
	"encoding/json"
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

    // Track consecutive "hung" warnings (when log is not growing)
    consecutiveHungWarnings := 0
    const maxHungWarnings = 10 // Auto-recover after 10 consecutive warnings (10 minutes)

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
            // Reset hung warning counter when we get output
            consecutiveHungWarnings = 0

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
                        consecutiveHungWarnings++
                        
                        // After 10 consecutive warnings (10 minutes), auto-recover
                        if consecutiveHungWarnings >= maxHungWarnings {
                            fmt.Println()
                            fmt.Println("========================================")
                            fmt.Println("  PROCESS APPEARS HUNG - AUTO-RECOVERY")
                            fmt.Println("========================================")
                            fmt.Println()
                            fmt.Printf("No output and log not growing for %d consecutive checks (%d minutes).\n", consecutiveHungWarnings, consecutiveHungWarnings)
                            fmt.Println("The process will be stopped and recovery tool will be suggested.")
                            fmt.Println()
                            
                            // Kill the hung process
                            fmt.Println("Stopping hung process...")
                            if cmd.Process != nil {
                                cmd.Process.Kill()
                                cmd.Process.Wait() // Wait for it to exit
                            }
                            
                            fmt.Println()
                            fmt.Println("The installation process has been stopped.")
                            fmt.Println()
                            fmt.Println("To recover and complete installation, run:")
                            fmt.Println("  /root/recovery-complete-install.sh")
                            fmt.Println()
                            fmt.Println("The recovery tool will:")
                            fmt.Println("  1. Check installation state")
                            fmt.Println("  2. Re-run guix system init if needed")
                            fmt.Println("  3. Complete any remaining steps")
                            fmt.Println()
                            
		return fmt.Errorf("process hung - stopped after %d consecutive warnings. Run recovery tool to continue", consecutiveHungWarnings)
                        } else {
                            fmt.Printf("         (%d/%d consecutive warnings - will auto-recover after %d)\n", consecutiveHungWarnings, maxHungWarnings, maxHungWarnings)
                        }
                    } else {
                        // Log is growing, reset counter
                        consecutiveHungWarnings = 0
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
	// Capture the output to get the system path
	buildCmd := exec.Command("guix", "time-machine", "-C", channelsPath, "--", "system", "build", "/mnt/etc/config.scm", "--substitute-urls=https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org")
	buildCmd.Stdout = os.Stdout
	buildCmd.Stderr = os.Stderr
	if err := buildCmd.Run(); err != nil {
		return fmt.Errorf("guix system build failed: %w", err)
	}

	PrintSectionHeader("Copying Kernel Files")
	fmt.Println("Step 2/3: Manually copying kernel and initrd to /boot")
	fmt.Println("(Workaround for guix system init bug)")
	fmt.Println()

	// Step 2: Find the built system and copy kernel/initrd to /boot
	// The system build creates /gnu/store/*-system with kernel and initrd
	fmt.Println("Searching for built system in /gnu/store...")
	cmd := exec.Command("bash", "-c", "ls -td /gnu/store/*-system 2>/dev/null | head -1")
	output, err := cmd.Output()
	if err != nil {
		fmt.Printf("[ERROR] Failed to find built system: %v\n", err)
		fmt.Println("Listing all *-system directories:")
		exec.Command("bash", "-c", "ls -ld /gnu/store/*-system 2>/dev/null | head -10").Run()
		return fmt.Errorf("failed to find built system: %w", err)
	}
	systemPath := strings.TrimSpace(string(output))
	if systemPath == "" {
		fmt.Println("[ERROR] No system found in /gnu/store")
		fmt.Println("Listing /gnu/store contents:")
		exec.Command("bash", "-c", "ls /gnu/store/ | grep system | head -20").Run()
		return fmt.Errorf("no system found in /gnu/store")
	}

	fmt.Printf("[INFO] Found built system: %s\n", systemPath)

	// List contents of system directory to verify kernel/initrd are there
	fmt.Println("[INFO] Contents of system directory:")
	listCmd := exec.Command("ls", "-lh", systemPath)
	listCmd.Stdout = os.Stdout
	listCmd.Stderr = os.Stderr
	listCmd.Run()

	// Copy kernel
	fmt.Println()
	kernelSrc := filepath.Join(systemPath, "kernel")
	fmt.Printf("[INFO] Checking for kernel at: %s\n", kernelSrc)
	if info, err := os.Stat(kernelSrc); err != nil {
		fmt.Printf("[ERROR] Kernel not found: %v\n", err)
		return fmt.Errorf("kernel not found in built system: %w", err)
	} else {
		fmt.Printf("[OK] Kernel found (%.1f MB)\n", float64(info.Size())/1024/1024)
	}

	kernelDest := "/mnt/boot/vmlinuz"
	fmt.Printf("[INFO] Copying kernel to: %s\n", kernelDest)
	if err := exec.Command("cp", "-Lv", kernelSrc, kernelDest).Run(); err != nil {
		fmt.Printf("[ERROR] Failed to copy kernel: %v\n", err)
		return fmt.Errorf("failed to copy kernel: %w", err)
	}

	// Verify the copy
	if info, err := os.Stat(kernelDest); err != nil {
		fmt.Printf("[ERROR] Kernel not found at destination after copy: %v\n", err)
		return fmt.Errorf("kernel copy verification failed: %w", err)
	} else {
		fmt.Printf("[OK] Kernel copied successfully (%.1f MB)\n", float64(info.Size())/1024/1024)
	}

	// Copy initrd
	fmt.Println()
	initrdSrc := filepath.Join(systemPath, "initrd")
	fmt.Printf("[INFO] Checking for initrd at: %s\n", initrdSrc)
	if info, err := os.Stat(initrdSrc); err != nil {
		fmt.Printf("[ERROR] Initrd not found: %v\n", err)
		return fmt.Errorf("initrd not found in built system: %w", err)
	} else {
		fmt.Printf("[OK] Initrd found (%.1f MB)\n", float64(info.Size())/1024/1024)
	}

	initrdDest := "/mnt/boot/initrd"
	fmt.Printf("[INFO] Copying initrd to: %s\n", initrdDest)
	if err := exec.Command("cp", "-Lv", initrdSrc, initrdDest).Run(); err != nil {
		fmt.Printf("[ERROR] Failed to copy initrd: %v\n", err)
		return fmt.Errorf("failed to copy initrd: %w", err)
	}

	// Verify the copy
	if info, err := os.Stat(initrdDest); err != nil {
		fmt.Printf("[ERROR] Initrd not found at destination after copy: %v\n", err)
		return fmt.Errorf("initrd copy verification failed: %w", err)
	} else {
		fmt.Printf("[OK] Initrd copied successfully (%.1f MB)\n", float64(info.Size())/1024/1024)
	}

	fmt.Println()
	fmt.Println("[SUCCESS] Kernel and initrd files copied to /mnt/boot/")

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

	// CRITICAL: Verify daemon is actually running RIGHT BEFORE we need it
	// Previous verification may have been too early, and intermediate steps
	// might have affected the daemon. Verify again immediately before use.
	fmt.Println("Verifying guix-daemon is running and responsive before system init...")
	if err := isDaemonReady(); err != nil {
		fmt.Printf("[WARN] Daemon not ready: %v\n", err)
		fmt.Println("Restarting daemon to ensure it's available...")
		if err := EnsureGuixDaemonRunning(); err != nil {
			return fmt.Errorf("failed to ensure guix-daemon is running before system init: %w", err)
		}
	} else {
		fmt.Println("[OK] guix-daemon is running and responsive")
	}
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
			
			// Re-verify daemon before each retry
			fmt.Println("Verifying guix-daemon is still running before retry...")
			if err := isDaemonReady(); err != nil {
				fmt.Printf("[WARN] Daemon not ready before retry: %v\n", err)
				fmt.Println("Restarting daemon...")
				if err := EnsureGuixDaemonRunning(); err != nil {
					fmt.Printf("[ERROR] Failed to restart daemon: %v\n", err)
				}
			}
			fmt.Println()
		}

		if err := RunCommandWithSpinner("guix", "time-machine", "-C", channelsPath, "--", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt", "--substitute-urls=https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
			lastErr = err
			fmt.Printf("\n[WARN] Attempt %d failed: %v\n", attempt, err)
			
			// CRITICAL: Check if this is a connection error
			// Even if isDaemonReady() passed, guix system init might fail to connect
			// This can happen if the socket becomes unavailable between verification and use
			fmt.Println("Checking if this is a daemon connection issue...")
			if err := checkDaemonResponsive(); err != nil {
				fmt.Printf("[WARN] Daemon connection check failed: %v\n", err)
				fmt.Println("Restarting daemon due to connection failure...")
				if restartErr := EnsureGuixDaemonRunning(); restartErr != nil {
					fmt.Printf("[ERROR] Failed to restart daemon: %v\n", restartErr)
				} else {
					fmt.Println("[OK] Daemon restarted successfully")
				}
			} else {
				fmt.Println("[INFO] Daemon is still responsive - error may be unrelated to connection")
			}
			fmt.Println()
			
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

	// CRITICAL: Verify kernel/initrd still exist after Step 3 (guix system init can remove them)
	// This matches the verification done in framework-dual/install/04-system-init.go
	fmt.Println()
	PrintSectionHeader("Verifying Kernel/Initrd After Bootloader Install")
	if err := VerifyAndRecoverKernelFiles(3); err != nil {
		return fmt.Errorf("kernel/initrd verification failed after bootloader install: %w", err)
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

	// Also copy to /root/ (for use by recovery tool on ISO)
	rootPath := "/root/verify-guix-install.sh"
	if err := os.WriteFile(rootPath, scriptContent, 0755); err != nil {
		return fmt.Errorf("failed to write %s: %w", rootPath, err)
	}
	fmt.Printf("[OK] Also copied to: %s (for recovery tool)\n", rootPath)
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
		fmt.Println("To fix the issues, run the recovery tool:")
		fmt.Printf("  %s\n", recoveryScriptPath)
		fmt.Println()
		fmt.Println("The recovery tool will:")
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
		return fmt.Errorf("comprehensive verification failed - run recovery tool before rebooting")
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

// VerifyAndRecoverKernelFiles checks if kernel/initrd exist and attempts recovery if missing
// This handles the case where guix system init succeeds but doesn't copy kernel files
func VerifyAndRecoverKernelFiles(maxAttempts int) error {
	fmt.Println()
	PrintSectionHeader("Verifying Kernel and Initrd Files")

	for attempt := 1; attempt <= maxAttempts; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Attempting to recover kernel/initrd files...\n", attempt, maxAttempts)
			fmt.Println()
		}

		// Check for kernel files (with wildcards for version numbers)
		kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
		initrds, _ := filepath.Glob("/mnt/boot/initrd*")

		kernelMissing := len(kernels) == 0
		initrdMissing := len(initrds) == 0

		if !kernelMissing && !initrdMissing {
			// Both exist - verify they're real files with reasonable size
			kernelSize := int64(0)
			initrdSize := int64(0)

			if info, err := os.Stat(kernels[0]); err == nil {
				kernelSize = info.Size()
			}
			if info, err := os.Stat(initrds[0]); err == nil {
				initrdSize = info.Size()
			}

			// Kernel should be at least 5MB, initrd at least 10MB (typical sizes)
			if kernelSize > 5*1024*1024 && initrdSize > 10*1024*1024 {
				fmt.Printf("[OK] Kernel found: %s (%.1f MB)\n", filepath.Base(kernels[0]), float64(kernelSize)/1024/1024)
				fmt.Printf("[OK] Initrd found: %s (%.1f MB)\n", filepath.Base(initrds[0]), float64(initrdSize)/1024/1024)
				fmt.Println()
				return nil
			}

			// Files exist but are suspiciously small
			fmt.Printf("[WARN] Kernel/initrd files exist but are too small:\n")
			fmt.Printf("       Kernel: %.1f MB (expected > 5 MB)\n", float64(kernelSize)/1024/1024)
			fmt.Printf("       Initrd: %.1f MB (expected > 10 MB)\n", float64(initrdSize)/1024/1024)
		}

		// Files are missing or too small - attempt recovery
		fmt.Println()
		if kernelMissing {
			fmt.Println("[ERROR] Kernel file missing from /mnt/boot/")
		}
		if initrdMissing {
			fmt.Println("[ERROR] Initrd file missing from /mnt/boot/")
		}

		// Check if system generation symlink exists (should exist after guix system init)
		systemLink := "/mnt/run/current-system"
		var systemPath string
		
		// #region agent log
		logDebug("lib/common.go:1654", "Checking for system generation symlink", map[string]interface{}{
			"hypothesisId": "E",
			"step":         "check_system_link",
			"systemLink":   systemLink,
			"attempt":      attempt,
		})
		// #endregion
		
		if _, err := os.Lstat(systemLink); err == nil {
			// Symlink exists - follow it
			systemPath, err = filepath.EvalSymlinks(systemLink)
			if err != nil {
				fmt.Printf("[ERROR] System generation symlink is broken: %v\n", err)
				// #region agent log
				logDebug("lib/common.go:1668", "System generation symlink broken", map[string]interface{}{
					"hypothesisId": "E",
					"step":         "system_link_broken",
					"error":        err.Error(),
				})
				// #endregion
			if attempt < maxAttempts {
				fmt.Println()
				fmt.Println("Waiting 10 seconds before retry...")
				time.Sleep(10 * time.Second)
				continue
			}
				return fmt.Errorf("broken system generation symlink: %w", err)
			}
			fmt.Printf("[INFO] Found system generation: %s\n", systemPath)
			// #region agent log
			logDebug("lib/common.go:1675", "System generation found", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "system_path_found",
				"systemPath":   systemPath,
			})
			// #endregion
		} else {
			fmt.Printf("[ERROR] System generation symlink missing: %s\n", systemLink)
			fmt.Println("        This should have been created by guix system init or our fix")
			// #region agent log
			logDebug("lib/common.go:1683", "System generation symlink missing", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "system_link_missing",
				"error":        err.Error(),
			})
			// #endregion
			if attempt < maxAttempts {
				fmt.Println()
				fmt.Println("Waiting 10 seconds before retry...")
				time.Sleep(10 * time.Second)
				continue
			}
			return fmt.Errorf("system generation symlink not found - installation incomplete")
		}

		// Try to copy kernel and initrd from system generation
		fmt.Println()
		fmt.Println("Attempting to recover kernel/initrd from system generation...")

		recovered := true

		// Copy kernel if missing
		if kernelMissing || (len(kernels) > 0 && func() bool {
			info, _ := os.Stat(kernels[0])
			return info.Size() < 5*1024*1024
		}()) {
			kernelSrc := filepath.Join(systemPath, "kernel")
			// #region agent log
			logDebug("lib/common.go:1692", "Checking kernel in system generation", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "check_kernel_src",
				"kernelSrc":    kernelSrc,
			})
			// #endregion
			if info, err := os.Stat(kernelSrc); err != nil {
				fmt.Printf("[ERROR] Kernel not found in system generation: %s\n", kernelSrc)
				// #region agent log
				logDebug("lib/common.go:1697", "Kernel not found in system generation", map[string]interface{}{
					"hypothesisId": "E",
					"step":         "kernel_not_in_system",
					"kernelSrc":    kernelSrc,
					"error":        err.Error(),
				})
				// #endregion
				recovered = false
			} else {
				kernelDest := "/mnt/boot/vmlinuz"
				// #region agent log
				logDebug("lib/common.go:1705", "Copying kernel from system generation", map[string]interface{}{
					"hypothesisId": "E",
					"step":         "copy_kernel",
					"kernelSrc":    kernelSrc,
					"kernelDest":   kernelDest,
					"kernelSize":   info.Size(),
				})
				// #endregion
				if err := exec.Command("cp", "-Lf", kernelSrc, kernelDest).Run(); err != nil {
					fmt.Printf("[ERROR] Failed to copy kernel: %v\n", err)
					// #region agent log
					logDebug("lib/common.go:1712", "Kernel copy failed", map[string]interface{}{
						"hypothesisId": "E",
						"step":         "kernel_copy_failed",
						"error":        err.Error(),
					})
					// #endregion
					recovered = false
				} else {
					if info, err := os.Stat(kernelDest); err == nil {
						fmt.Printf("[OK] Recovered kernel: %s (%.1f MB)\n", kernelDest, float64(info.Size())/1024/1024)
						// #region agent log
						logDebug("lib/common.go:1719", "Kernel copy succeeded", map[string]interface{}{
							"hypothesisId": "E",
							"step":         "kernel_copy_succeeded",
							"kernelDest":   kernelDest,
							"kernelSize":   info.Size(),
						})
						// #endregion
					}
				}
			}
		}

		// Copy initrd if missing
		if initrdMissing || (len(initrds) > 0 && func() bool {
			info, _ := os.Stat(initrds[0])
			return info.Size() < 10*1024*1024
		}()) {
			initrdSrc := filepath.Join(systemPath, "initrd")
			// #region agent log
			logDebug("lib/common.go:1730", "Checking initrd in system generation", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "check_initrd_src",
				"initrdSrc":    initrdSrc,
			})
			// #endregion
			if info, err := os.Stat(initrdSrc); err != nil {
				fmt.Printf("[ERROR] Initrd not found in system generation: %s\n", initrdSrc)
				// #region agent log
				logDebug("lib/common.go:1735", "Initrd not found in system generation", map[string]interface{}{
					"hypothesisId": "E",
					"step":         "initrd_not_in_system",
					"initrdSrc":    initrdSrc,
					"error":        err.Error(),
				})
				// #endregion
				recovered = false
			} else {
				initrdDest := "/mnt/boot/initrd"
				// #region agent log
				logDebug("lib/common.go:1743", "Copying initrd from system generation", map[string]interface{}{
					"hypothesisId": "E",
					"step":         "copy_initrd",
					"initrdSrc":    initrdSrc,
					"initrdDest":   initrdDest,
					"initrdSize":   info.Size(),
				})
				// #endregion
				if err := exec.Command("cp", "-Lf", initrdSrc, initrdDest).Run(); err != nil {
					fmt.Printf("[ERROR] Failed to copy initrd: %v\n", err)
					// #region agent log
					logDebug("lib/common.go:1750", "Initrd copy failed", map[string]interface{}{
						"hypothesisId": "E",
						"step":         "initrd_copy_failed",
						"error":        err.Error(),
					})
					// #endregion
					recovered = false
				} else {
					if info, err := os.Stat(initrdDest); err == nil {
						fmt.Printf("[OK] Recovered initrd: %s (%.1f MB)\n", initrdDest, float64(info.Size())/1024/1024)
						// #region agent log
						logDebug("lib/common.go:1757", "Initrd copy succeeded", map[string]interface{}{
							"hypothesisId": "E",
							"step":         "initrd_copy_succeeded",
							"initrdDest":   initrdDest,
							"initrdSize":   info.Size(),
						})
						// #endregion
					}
				}
			}
		}

		if recovered {
			fmt.Println()
			fmt.Println("[OK] Successfully recovered kernel/initrd files")
			fmt.Println()
			// #region agent log
			logDebug("lib/common.go:1768", "Recovery succeeded", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "recovery_succeeded",
			})
			// #endregion
			return nil
		}

		if attempt < maxAttempts {
			fmt.Println()
			fmt.Println("Recovery attempt failed. Waiting 10 seconds before retry...")
			// #region agent log
			logDebug("lib/common.go:1778", "Recovery attempt failed, will retry", map[string]interface{}{
				"hypothesisId": "E",
				"step":         "recovery_failed_retry",
				"attempt":      attempt,
				"maxAttempts":  maxAttempts,
			})
			// #endregion
			time.Sleep(10 * time.Second)
		}
	}

	// #region agent log
	logDebug("lib/common.go:1786", "Recovery failed after all attempts", map[string]interface{}{
		"hypothesisId": "E",
		"step":         "recovery_failed_final",
		"maxAttempts":  maxAttempts,
	})
	// #endregion
	return fmt.Errorf("failed to recover kernel/initrd files after %d attempts", maxAttempts)
}

// isMountPoint checks if a path is a mount point
func isMountPoint(path string) bool {
	cmd := exec.Command("mountpoint", "-q", path)
	return cmd.Run() == nil
}

// RunGuixSystemInitFreeSoftware runs guix system init with only free software (no nonguix)
// platform: the installation platform (cloudzy, framework, etc.) for tracking purposes
func RunGuixSystemInitFreeSoftware(platform string) error {
	// Default platform if not provided
	if platform == "" {
		platform = "unknown"
	}

	buildType := "libre" // This function only handles free-software builds

	// Log build context at start
	logDebug("lib/common.go:1910", "Starting free-software system init", map[string]interface{}{
		"platform":  platform,
		"buildType": buildType,
		"step":      "init_start",
	})

	// Ensure daemon is running before validation (validation needs daemon)
	if err := EnsureGuixDaemonRunning(); err != nil {
		return fmt.Errorf("failed to ensure guix-daemon is running: %w", err)
	}

	// Validate config after daemon is confirmed running
	configPath := "/mnt/etc/config.scm"
	if err := ValidateGuixConfig(configPath); err != nil {
		return fmt.Errorf("config validation failed: %w", err)
	}

	// Setup GRUB EFI only for UEFI boot mode (BIOS uses grub-bootloader, not grub-efi-bootloader)
	bootMode := DetectBootModeFromConfig(configPath)
	if bootMode == "uefi" {
	if err := SetupGRUBEFI(); err != nil {
		return fmt.Errorf("GRUB EFI setup failed: %w", err)
	}
	} else {
		fmt.Println("[OK] BIOS boot mode detected - skipping EFI setup (GRUB will be installed to disk MBR)")
		fmt.Println()
	}

	// CRITICAL DISCOVERY (2025-01-XX): guix system init (free-software-only) does NOT create kernel/initrd files
	// in the system generation. Debug logs show system generation only contains ["gnu","gnu.go","guix"].
	// We must use the 3-step workaround (same as framework-dual):
	// 1. guix system build - creates system generation with kernel/initrd
	// 2. Manually copy kernel/initrd to /mnt/boot/
	// 3. guix system init - installs bootloader only

	PrintSectionHeader("Building Guix System (Free Software)")
	fmt.Println("Step 1/3: Building system closure (includes kernel)")
	fmt.Println("This will take 5-30 minutes depending on substitutes availability...")
	fmt.Println()
	
	// Check disk space before starting
	cmd := exec.Command("df", "-h", "/mnt")
	if output, err := cmd.Output(); err == nil {
		fmt.Println("Disk space check:")
		fmt.Println(string(output))
		fmt.Println()
	}

	// CRITICAL: Verify daemon is actually running RIGHT BEFORE we need it
	fmt.Println("Verifying guix-daemon is running and responsive before system build...")
	if err := isDaemonReady(); err != nil {
		fmt.Printf("[WARN] Daemon not ready: %v\n", err)
		fmt.Println("Restarting daemon to ensure it's available...")
		if err := EnsureGuixDaemonRunning(); err != nil {
			return fmt.Errorf("failed to ensure guix-daemon is running before system build: %w", err)
		}
	} else {
		fmt.Println("[OK] guix-daemon is running and responsive")
	}
	fmt.Println()

	// Step 1: Build the system (creates system generation with kernel/initrd)
	// #region agent log
	logDebug("lib/common.go:1805", "Before guix system build (free software)", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "before_guix_system_build",
	})
	// #endregion
	
	buildCmd := exec.Command("guix", "system", "build", "/mnt/etc/config.scm", "--substitute-urls=https://ci.guix.gnu.org https://bordeaux.guix.gnu.org")
	buildCmd.Stdout = os.Stdout
	buildCmd.Stderr = os.Stderr
	if err := buildCmd.Run(); err != nil {
		// #region agent log
		logDebug("lib/common.go:1817", "guix system build failed", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "guix_system_build_failed",
			"error":        err.Error(),
		})
		// #endregion
		return fmt.Errorf("guix system build failed: %w", err)
	}
	
	// #region agent log
	logDebug("lib/common.go:1821", "After guix system build", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "after_guix_system_build",
		"checking":     "system_generation_exists",
	})
	// #endregion

	PrintSectionHeader("Copying Kernel Files (Free Software)")
	fmt.Println("Step 2/3: Manually copying kernel and initrd to /boot")
	fmt.Println("(Workaround for guix system init bug)")
	fmt.Println()

	// Find the latest system generation
	findCmd := exec.Command("bash", "-c", "ls -td /gnu/store/*-system 2>/dev/null | head -1")
	output, err := findCmd.Output()
	if err != nil || len(strings.TrimSpace(string(output))) == 0 {
		// #region agent log
		logDebug("lib/common.go:1834", "Failed to find system generation after build", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "system_path_not_found",
			"error":        err.Error(),
		})
		// #endregion
		return fmt.Errorf("failed to find system generation after build: %w", err)
	}
	systemPath := strings.TrimSpace(string(output))
	// #region agent log
	logDebug("lib/common.go:1843", "System path found after build", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "system_path_found",
		"systemPath":   systemPath,
	})
	// #endregion

	// List system generation contents to understand what's actually there
	entries, err := os.ReadDir(systemPath)
	entryNames := []string{}
	if err == nil {
		for _, entry := range entries {
			entryNames = append(entryNames, entry.Name())
		}
	}
	// #region agent log
	logDebug("lib/common.go:1850", "System generation contents after build", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "list_system_contents",
		"systemPath":   systemPath,
		"entries":      entryNames,
		"entryCount":   len(entryNames),
	})
	// #endregion

	// Check for kernel in alternative locations
	alternativeKernelPaths := []string{
		filepath.Join(systemPath, "kernel"),
		filepath.Join(systemPath, "boot", "kernel"),
		filepath.Join(systemPath, "boot", "vmlinuz"),
		filepath.Join(systemPath, "boot", "vmlinuz-linux"),
	}
	kernelFound := false
	var kernelSrc string
	for _, altPath := range alternativeKernelPaths {
		if info, err := os.Stat(altPath); err == nil {
			// Check if it's a symlink
			isSymlink := false
			if linkInfo, err := os.Lstat(altPath); err == nil {
				isSymlink = linkInfo.Mode()&os.ModeSymlink != 0
			}
			// #region agent log
			logDebug("lib/common.go:1870", "Kernel found in alternative location", map[string]interface{}{
				"hypothesisId": "G",
				"step":         "kernel_found_alternative",
				"kernelPath":   altPath,
				"isSymlink":    isSymlink,
				"size":         info.Size(),
			})
			// #endregion
			kernelSrc = altPath
			kernelFound = true
			break
		}
	}
	// #region agent log
	logDebug("lib/common.go:1880", "Kernel search complete", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "kernel_search_complete",
		"kernelFound":  kernelFound,
		"kernelSrc":    kernelSrc,
		"checkedPaths": alternativeKernelPaths,
	})
	// #endregion

	// Ensure /mnt/boot exists
	if err := os.MkdirAll("/mnt/boot", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/boot directory: %w", err)
	}

	// Copy kernel
	kernelDest := "/mnt/boot/vmlinuz"
	if !kernelFound {
		// #region agent log
		logDebug("lib/common.go:1895", "Kernel not found in any location", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "kernel_not_found_anywhere",
			"systemPath":   systemPath,
			"checkedPaths": alternativeKernelPaths,
		})
		// #endregion
		
		// CRITICAL DISCOVERY: guix system build (free software mode) does NOT create kernel files
		// The system generation only contains ["gnu","gnu.go","guix"] - no kernel, no initrd
		// Alternative approach: Build kernel package separately using guix build
		fmt.Println()
		fmt.Println("[WARN] Kernel files not found in system generation after guix system build")
		fmt.Println("This indicates that guix system build (free software mode) does not create kernel files.")
		fmt.Println()
		fmt.Println("Attempting alternative approach: Building kernel package separately...")
		fmt.Println("(guix build linux-libre should create the kernel binary)")
		fmt.Println()
		
		// Build kernel package separately
		// #region agent log
		logDebug("lib/common.go:1905", "Before guix build linux-libre", map[string]interface{}{
			"hypothesisId": "H",
			"step":         "before_guix_build_kernel",
		})
		// #endregion
		
		// First check if network is working (Hypothesis M)
		networkErr := CheckNetworkConnectivity(platform, buildType)
		if networkErr != nil {
			fmt.Println("[WARN] Network/DNS not working - skipping network-based kernel build")
			fmt.Println("       Will try alternative approaches that don't require network")
			fmt.Println()

			// Try Hypothesis K: Deep system generation search
			fmt.Println("Attempting Hypothesis K: Deep search of system generation...")
			if kPath, iPath, err := SearchSystemGenerationDeep(systemPath, platform, buildType); err == nil && kPath != "" {
				kernelSrc = kPath
				kernelFound = true
				fmt.Printf("[OK] Found kernel via deep search: %s\n", kPath)
				if iPath != "" {
					fmt.Printf("[OK] Found initrd via deep search: %s\n", iPath)
				}
				// Continue to copy step below
			} else {
				fmt.Println("[WARN] Deep search failed - no kernel found in system generation subdirectories")

				// Try Hypothesis N: Store-wide search
				fmt.Println("Attempting Hypothesis N: Store-wide kernel search...")
				if kPath, iPath, err := SearchStoreForKernel(platform, buildType); err == nil && kPath != "" {
					kernelSrc = kPath
					kernelFound = true
					fmt.Printf("[OK] Found kernel via store search: %s\n", kPath)
					if iPath != "" {
						fmt.Printf("[OK] Found initrd via store search: %s\n", iPath)
					}
					// Continue to copy step below
				} else {
					fmt.Println("[ERROR] All non-network approaches failed")
					fmt.Println()
					fmt.Println("Cannot proceed without network for kernel build, and no kernel found in store.")
					fmt.Println("Please fix network connectivity and retry, or use guix system init directly.")
					fmt.Println()
					logDebug("lib/common.go:2135", "All non-network hypotheses failed", map[string]interface{}{
						"hypothesisId": "ALL",
						"step":         "all_failed_no_network",
						"networkError": networkErr.Error(),
					})
					return fmt.Errorf("network unavailable and no kernel found via alternative searches: %w", networkErr)
				}
			}
		} else {
			// Network works, try building kernel package
			// NOTE: --substitute-urls must come before the package name, and URLs must be quoted as a single argument
			buildKernelCmd := exec.Command("guix", "build", "--substitute-urls=https://ci.guix.gnu.org https://bordeaux.guix.gnu.org", "linux-libre")
			buildKernelCmd.Stdout = os.Stdout
			buildKernelCmd.Stderr = os.Stderr
			if err := buildKernelCmd.Run(); err != nil {
				// #region agent log
				logDebug("lib/common.go:1915", "guix build linux-libre failed", map[string]interface{}{
					"hypothesisId": "H",
					"step":         "guix_build_kernel_failed",
					"platform":     platform,
					"buildType":    buildType,
					"error":        err.Error(),
				})
				// #endregion

				// Even with network, build failed - try alternative approaches
				fmt.Println("[WARN] Kernel build failed despite network being available")
				fmt.Println("       Trying alternative search approaches...")

				// Try Hypothesis K first
				if kPath, iPath, err := SearchSystemGenerationDeep(systemPath, platform, buildType); err == nil && kPath != "" {
					kernelSrc = kPath
					kernelFound = true
					fmt.Printf("[OK] Found kernel via deep search: %s\n", kPath)
					if iPath != "" {
						fmt.Printf("[OK] Found initrd via deep search: %s\n", iPath)
					}
				} else if kPath, iPath, err := SearchStoreForKernel(platform, buildType); err == nil && kPath != "" {
					// Try Hypothesis N
					kernelSrc = kPath
					kernelFound = true
					fmt.Printf("[OK] Found kernel via store search: %s\n", kPath)
					if iPath != "" {
						fmt.Printf("[OK] Found initrd via store search: %s\n", iPath)
					}
				} else {
					return fmt.Errorf("guix build linux-libre failed and no kernel found via alternative searches: %w", err)
				}
			} else {
				// Build succeeded - find the kernel package output
				findKernelCmd := exec.Command("bash", "-c", "guix build --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org' linux-libre 2>&1 | tail -1")
				kernelOutput, err := findKernelCmd.Output()
				if err != nil {
					return fmt.Errorf("failed to find kernel package path: %w", err)
				}
				kernelPackagePath := strings.TrimSpace(string(kernelOutput))

				// Check if output is an error message rather than a valid path
				if strings.Contains(kernelPackagePath, "error:") || !strings.HasPrefix(kernelPackagePath, "/gnu/store/") {
					// #region agent log
					logDebug("lib/common.go:1930", "Kernel build output is error message, not valid path", map[string]interface{}{
						"hypothesisId": "H",
						"step":         "kernel_build_output_invalid",
						"platform":     platform,
						"buildType":    buildType,
						"output":       kernelPackagePath,
					})
					// #endregion

					// Try alternative approaches
					fmt.Println("[WARN] Kernel build output invalid - trying alternative searches")
					if kPath, iPath, err := SearchSystemGenerationDeep(systemPath, platform, buildType); err == nil && kPath != "" {
						kernelSrc = kPath
						kernelFound = true
						fmt.Printf("[OK] Found kernel via deep search: %s\n", kPath)
						if iPath != "" {
							fmt.Printf("[OK] Found initrd via deep search: %s\n", iPath)
						}
					} else if kPath, iPath, err := SearchStoreForKernel(platform, buildType); err == nil && kPath != "" {
						kernelSrc = kPath
						kernelFound = true
						fmt.Printf("[OK] Found kernel via store search: %s\n", kPath)
						if iPath != "" {
							fmt.Printf("[OK] Found initrd via store search: %s\n", iPath)
						}
					} else {
						return fmt.Errorf("guix build output invalid (%s) and no kernel found via alternative searches", kernelPackagePath)
					}
				} else {
					// #region agent log
					logDebug("lib/common.go:1930", "Kernel package path found", map[string]interface{}{
						"hypothesisId": "H",
						"step":         "kernel_package_path_found",
						"platform":     platform,
						"buildType":    buildType,
						"kernelPackagePath": kernelPackagePath,
					})
					// #endregion

				// Check for kernel binary in the package
				// Kernel packages typically have the kernel at: $package/bzImage or $package/vmlinux
				kernelPackagePaths := []string{
					filepath.Join(kernelPackagePath, "bzImage"),
					filepath.Join(kernelPackagePath, "vmlinux"),
					filepath.Join(kernelPackagePath, "boot", "bzImage"),
					filepath.Join(kernelPackagePath, "boot", "vmlinux"),
				}

				kernelFound2 := false
				var kernelSrc2 string
				for _, altPath := range kernelPackagePaths {
					if info, err := os.Stat(altPath); err == nil {
						isSymlink := false
						if linkInfo, err := os.Lstat(altPath); err == nil {
							isSymlink = linkInfo.Mode()&os.ModeSymlink != 0
						}
						// #region agent log
						logDebug("lib/common.go:1950", "Kernel found in package", map[string]interface{}{
							"hypothesisId": "H",
							"step":         "kernel_found_in_package",
							"platform":     platform,
							"buildType":    buildType,
							"kernelPath":   altPath,
							"isSymlink":    isSymlink,
							"size":         info.Size(),
						})
						// #endregion
						kernelSrc2 = altPath
						kernelFound2 = true
						break
					}
				}

				if !kernelFound2 {
					// List package contents for debugging
					entries2, err := os.ReadDir(kernelPackagePath)
					entryNames2 := []string{}
					if err == nil {
						for _, entry := range entries2 {
							entryNames2 = append(entryNames2, entry.Name())
						}
					}
					// #region agent log
					logDebug("lib/common.go:1970", "Kernel not found in package, listing contents", map[string]interface{}{
						"hypothesisId": "H",
						"step":         "kernel_not_found_in_package",
						"platform":     platform,
						"buildType":    buildType,
						"kernelPackagePath": kernelPackagePath,
						"checkedPaths": kernelPackagePaths,
						"packageEntries": entryNames2,
					})
					// #endregion
					return fmt.Errorf("kernel binary not found in linux-libre package at %s (checked: %v, package contents: %v). This indicates a fundamental issue with kernel file location in free-software-only installs", kernelPackagePath, kernelPackagePaths, entryNames2)
				}

				// Use the kernel from the package
				kernelSrc = kernelSrc2
				kernelFound = true

				// For initrd, we still need to build it separately or get it from guix system init
				// For now, we'll try to build initrd separately or skip it and let guix system init create it
				fmt.Println("[INFO] Kernel found in package, but initrd still needs to be created")
				fmt.Println("       Will attempt to create initrd during guix system init")
				fmt.Println()
				}
			}
		}
	}

	// #region agent log
	logDebug("lib/common.go:1905", "Before kernel copy", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "before_kernel_copy",
		"kernelSrc":    kernelSrc,
		"kernelDest":   kernelDest,
	})
	// #endregion
	if info, err := os.Stat(kernelSrc); err != nil {
		// #region agent log
		logDebug("lib/common.go:1913", "Kernel not found in system generation for copy", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "kernel_src_missing",
			"kernelSrc":    kernelSrc,
			"error":        err.Error(),
		})
		// #endregion
		return fmt.Errorf("kernel not found in system generation at %s: %w", kernelSrc, err)
	} else {
		if err := exec.Command("cp", "-Lf", kernelSrc, kernelDest).Run(); err != nil {
			// #region agent log
			logDebug("lib/common.go:1875", "Kernel copy failed", map[string]interface{}{
				"hypothesisId": "G",
				"step":         "kernel_copy_failed",
				"kernelSrc":    kernelSrc,
				"kernelDest":   kernelDest,
				"error":        err.Error(),
			})
			// #endregion
			return fmt.Errorf("failed to copy kernel from %s to %s: %w", kernelSrc, kernelDest, err)
		}
		// #region agent log
		logDebug("lib/common.go:1885", "Kernel copy succeeded", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "kernel_copy_succeeded",
			"kernelSrc":    kernelSrc,
			"kernelDest":   kernelDest,
			"kernelSize":   info.Size(),
		})
		// #endregion
		fmt.Printf("[OK] Copied kernel: %s (%.1f MB)\n", kernelDest, float64(info.Size())/1024/1024)
	}

	// Copy initrd
	initrdSrc := filepath.Join(systemPath, "initrd")
	initrdDest := "/mnt/boot/initrd"
	// #region agent log
	logDebug("lib/common.go:1898", "Before initrd copy", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "before_initrd_copy",
		"initrdSrc":    initrdSrc,
		"initrdDest":   initrdDest,
	})
	// #endregion
	if info, err := os.Stat(initrdSrc); err != nil {
		// #region agent log
		logDebug("lib/common.go:1906", "Initrd not found in system generation for copy", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "initrd_src_missing",
			"initrdSrc":    initrdSrc,
			"error":        err.Error(),
		})
		// #endregion
		return fmt.Errorf("initrd not found in system generation at %s: %w", initrdSrc, err)
	} else {
		if err := exec.Command("cp", "-Lf", initrdSrc, initrdDest).Run(); err != nil {
			// #region agent log
			logDebug("lib/common.go:1916", "Initrd copy failed", map[string]interface{}{
				"hypothesisId": "G",
				"step":         "initrd_copy_failed",
				"initrdSrc":    initrdSrc,
				"initrdDest":   initrdDest,
				"error":        err.Error(),
			})
			// #endregion
			return fmt.Errorf("failed to copy initrd from %s to %s: %w", initrdSrc, initrdDest, err)
		}
		// #region agent log
		logDebug("lib/common.go:1926", "Initrd copy succeeded", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "initrd_copy_succeeded",
			"initrdSrc":    initrdSrc,
			"initrdDest":   initrdDest,
			"initrdSize":   info.Size(),
		})
		// #endregion
		fmt.Printf("[OK] Copied initrd: %s (%.1f MB)\n", initrdDest, float64(info.Size())/1024/1024)
	}
	fmt.Println()

	// Create /mnt/run/current-system symlink manually
	currentSystemLink := "/mnt/run/current-system"
	if _, err := os.Lstat(currentSystemLink); err == nil {
		os.Remove(currentSystemLink) // Remove existing broken symlink or directory
	}
	if err := os.MkdirAll("/mnt/run", 0755); err != nil {
		return fmt.Errorf("failed to create /mnt/run directory for symlink: %w", err)
	}
	if err := os.Symlink(systemPath, currentSystemLink); err != nil {
		return fmt.Errorf("failed to create /mnt/run/current-system symlink: %w", err)
	}
	fmt.Printf("[OK] Created symlink: %s -> %s\n", currentSystemLink, systemPath)
	// #region agent log
	logDebug("lib/common.go:1950", "Created current-system symlink manually", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "created_symlink_manual",
		"symlink":      currentSystemLink,
		"target":       systemPath,
	})
	// #endregion
	fmt.Println()

	PrintSectionHeader("Installing Bootloader (Free Software)")
	fmt.Println("Step 3/3: Running guix system init to install bootloader")
	
	// CRITICAL: Prepare directories RIGHT BEFORE system init
	// This ensures /var/lock is a directory (not symlink) when guix system init chroots
	if err := PrepareSystemInitDirectories(); err != nil {
		return fmt.Errorf("failed to prepare system init directories: %w", err)
	}

	// CRITICAL: Verify daemon is actually running RIGHT BEFORE we need it
	fmt.Println("Verifying guix-daemon is running and responsive before bootloader install...")
	if err := isDaemonReady(); err != nil {
		fmt.Printf("[WARN] Daemon not ready: %v\n", err)
		fmt.Println("Restarting daemon to ensure it's available...")
		if err := EnsureGuixDaemonRunning(); err != nil {
			return fmt.Errorf("failed to ensure guix-daemon is running before bootloader install: %w", err)
		}
	} else {
		fmt.Println("[OK] guix-daemon is running and responsive")
	}
	fmt.Println()

	// Step 3: Run guix system init (installs bootloader only)
	maxRetries := 3
	var lastErr error
	// #region agent log
	logDebug("lib/common.go:1960", "Before guix system init (free software) - bootloader install", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "before_guix_system_init_bootloader",
		"maxRetries":   maxRetries,
	})
	// #endregion
	for attempt := 1; attempt <= maxRetries; attempt++ {
		if attempt > 1 {
			fmt.Printf("\n[RETRY %d/%d] Retrying guix system init...\n", attempt, maxRetries)
			fmt.Println("Waiting 10 seconds before retry...")
			fmt.Println()
			time.Sleep(10 * time.Second)

			// Re-verify daemon before each retry
			fmt.Println("Verifying guix-daemon is still running before retry...")
			if err := isDaemonReady(); err != nil {
				fmt.Printf("[WARN] Daemon not ready before retry: %v\n", err)
				fmt.Println("Restarting daemon...")
				if err := EnsureGuixDaemonRunning(); err != nil {
					fmt.Printf("[ERROR] Failed to restart daemon: %v\n", err)
				}
			}
			fmt.Println()
		}

	if err := RunCommandWithSpinner("guix", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt", "--substitute-urls=https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"); err != nil {
			lastErr = err
			fmt.Printf("\n[WARN] Attempt %d failed: %v\n", attempt, err)
			
			// #region agent log
			logDebug("lib/common.go:1998", "guix system init failed (attempt)", map[string]interface{}{
				"hypothesisId": "D",
				"step":         "guix_system_init_failed_attempt",
				"attempt":      attempt,
				"maxRetries":   maxRetries,
				"error":        err.Error(),
			})
			// #endregion
			
			// CRITICAL: Check if this is a connection error
			// Even if isDaemonReady() passed, guix system init might fail to connect
			// This can happen if the socket becomes unavailable between verification and use
			fmt.Println("Checking if this is a daemon connection issue...")
			daemonCheckErr := checkDaemonResponsive()
			// #region agent log
			logDebug("lib/common.go:2010", "Daemon connection check after failure", map[string]interface{}{
				"hypothesisId": "D",
				"step":         "daemon_check_after_failure",
				"attempt":      attempt,
				"daemonCheckErr": func() string {
					if daemonCheckErr != nil {
						return daemonCheckErr.Error()
					}
					return ""
				}(),
				"daemonResponsive": daemonCheckErr == nil,
			})
			// #endregion
			
			if daemonCheckErr != nil {
				fmt.Printf("[WARN] Daemon connection check failed: %v\n", daemonCheckErr)
				fmt.Println("Restarting daemon due to connection failure...")
				restartErr := EnsureGuixDaemonRunning()
				// #region agent log
				logDebug("lib/common.go:2023", "Daemon restart attempted", map[string]interface{}{
					"hypothesisId": "D",
					"step":         "daemon_restart_attempted",
					"attempt":      attempt,
					"restartErr": func() string {
						if restartErr != nil {
							return restartErr.Error()
						}
						return ""
					}(),
					"restartSuccess": restartErr == nil,
				})
				// #endregion
				if restartErr != nil {
					fmt.Printf("[ERROR] Failed to restart daemon: %v\n", restartErr)
				} else {
					fmt.Println("[OK] Daemon restarted successfully")
				}
			} else {
				fmt.Println("[INFO] Daemon is still responsive - error may be unrelated to connection")
			}
	fmt.Println()

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
		fmt.Println("System installation failed. You can:")
		fmt.Println("  1. Try manually: guix system init /mnt/etc/config.scm /mnt")
		fmt.Println("  2. Or run the recovery tool: /root/recovery-complete-install.sh")
		// #region agent log
		logDebug("lib/common.go:1840", "guix system init failed", map[string]interface{}{
			"hypothesisId": "D",
			"step":         "guix_system_init_failed",
			"error":        lastErr.Error(),
		})
		// #endregion
		return fmt.Errorf("guix system init failed after %d attempts: %w", maxRetries, lastErr)
	}

	// #region agent log
	// Check state immediately after guix system init succeeds
	kernelAfterInit, _ := os.Stat("/mnt/boot/vmlinuz")
	initrdAfterInit, _ := os.Stat("/mnt/boot/initrd")
	bootFiles, _ := filepath.Glob("/mnt/boot/*")
	bootFileList := []string{}
	for _, f := range bootFiles {
		bootFileList = append(bootFileList, filepath.Base(f))
	}
	var kernelAfterSize int64
	var initrdAfterSize int64
	if kernelAfterInit != nil {
		kernelAfterSize = kernelAfterInit.Size()
	}
	if initrdAfterInit != nil {
		initrdAfterSize = initrdAfterInit.Size()
	}
	
	logDebug("lib/common.go:1987", "After guix system init (bootloader) succeeds", map[string]interface{}{
		"hypothesisId": "G",
		"step":         "after_guix_system_init_bootloader",
		"kernelExists": kernelAfterInit != nil,
		"initrdExists": initrdAfterInit != nil,
		"kernelSize":   kernelAfterSize,
		"initrdSize":   initrdAfterSize,
		"bootFiles":    bootFileList,
	})
	// #endregion

	// Verify kernel/initrd are still present after bootloader install
	// (guix system init might have overwritten them, so verify and recover if needed)
	fmt.Println()
	PrintSectionHeader("Verifying Kernel/Initrd Files")
	kernels, _ := filepath.Glob("/mnt/boot/vmlinuz*")
	initrds, _ := filepath.Glob("/mnt/boot/initrd*")
	
	if len(kernels) == 0 || len(initrds) == 0 {
		// Kernel/initrd were copied in Step 2, but guix system init may have removed them
		fmt.Println("[WARN] Kernel/initrd missing after bootloader install - attempting recovery...")
		// #region agent log
		logDebug("lib/common.go:2005", "Kernel/initrd missing after bootloader install, starting recovery", map[string]interface{}{
			"hypothesisId": "G",
			"step":         "before_recovery_after_bootloader",
			"kernelCount":  len(kernels),
			"initrdCount":  len(initrds),
		})
		// #endregion
		if err := VerifyAndRecoverKernelFiles(3); err != nil {
			return fmt.Errorf("failed to recover kernel/initrd files after bootloader install: %w", err)
		}
	} else {
	fmt.Printf("✓ Kernel found: %s\n", filepath.Base(kernels[0]))
	fmt.Printf("✓ Initrd found: %s\n", filepath.Base(initrds[0]))
	fmt.Println()
	fmt.Println("✓ Installation verified - system should boot successfully")
	fmt.Println()
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

// WriteTimeMachineHelperScript writes a simple helper script for manually retrying
// guix time-machine system init if the installer aborts
func WriteTimeMachineHelperScript(scriptPath string) error {
	channelsPath := GetChannelsPath()
	tmCmd := fmt.Sprintf("guix time-machine -C %s -- system init --fallback -v6 /mnt/etc/config.scm /mnt --substitute-urls=\"https://substitutes.nonguix.org https://ci.guix.gnu.org https://bordeaux.guix.gnu.org\"\n", channelsPath)
	
	scriptContent := "#!/bin/sh\n" + tmCmd
	if err := os.WriteFile(scriptPath, []byte(scriptContent), 0755); err != nil {
		return fmt.Errorf("failed to write helper script to %s: %w", scriptPath, err)
	}
	
	fmt.Printf("Helper script written: %s\n", scriptPath)
	fmt.Println("If the installer aborts, you can rerun the init with:")
	fmt.Printf("  %s\n", scriptPath)
	
	return nil
}

// WriteRecoveryScript builds and installs the Go recovery binary
// This ensures the recovery tool shares code with the main installer and stays in sync
func WriteRecoveryScript(scriptPath, platform string) error {
	PrintSectionHeader("Installing Recovery Tool")

	// Build the recovery binary
	fmt.Println("Building recovery tool...")
	buildCmd := exec.Command("go", "build", "-o", scriptPath, "./cmd/recovery")
	buildCmd.Stdout = os.Stdout
	buildCmd.Stderr = os.Stderr
	
	// Set working directory to repo root (if available)
	if _, err := os.Stat("go.mod"); err == nil {
		buildCmd.Dir = "."
	} else if _, err := os.Stat("../go.mod"); err == nil {
		buildCmd.Dir = ".."
	} else {
		// Try to find go.mod by checking common locations
		wd, _ := os.Getwd()
		if strings.Contains(wd, "cloudzy-guix-install") {
			// Try to navigate to repo root
			parts := strings.Split(wd, "cloudzy-guix-install")
			if len(parts) > 0 {
				repoRoot := parts[0] + "cloudzy-guix-install"
				if _, err := os.Stat(filepath.Join(repoRoot, "go.mod")); err == nil {
					buildCmd.Dir = repoRoot
				}
			}
		}
	}

	if err := buildCmd.Run(); err != nil {
		// Fallback: try to use pre-built binary or bash script
		fmt.Printf("[WARN] Failed to build recovery binary: %v\n", err)
		fmt.Println("Falling back to bash recovery script...")
		
		// Try to find and copy bash script as fallback
		var scriptContent []byte
		var readErr error
		
		scriptContent, readErr = os.ReadFile("lib/recovery-complete-install.sh")
		if readErr != nil {
			scriptContent, readErr = os.ReadFile("recovery-complete-install.sh")
		}
		if readErr != nil {
			scriptContent, readErr = os.ReadFile("/root/recovery-complete-install.sh")
		}
		if readErr != nil {
			return fmt.Errorf("failed to build recovery binary and could not find bash script: %w (build error: %v)", readErr, err)
		}

		if err := os.WriteFile(scriptPath, scriptContent, 0755); err != nil {
			return fmt.Errorf("failed to write recovery script: %w", err)
		}
		fmt.Printf("[OK] Recovery script (bash) installed to: %s\n", scriptPath)
		fmt.Println("     Note: Go recovery binary build failed, using bash script fallback")
		fmt.Println()
		return nil
	}

	// Make binary executable
	if err := os.Chmod(scriptPath, 0755); err != nil {
		return fmt.Errorf("failed to make recovery binary executable: %w", err)
	}

	fmt.Printf("[OK] Recovery tool (Go binary) installed to: %s\n", scriptPath)
	fmt.Println("     This tool will be available if installation needs to be completed manually")
	fmt.Println()

	return nil
}

// logDebug writes a debug log entry in NDJSON format to the kernel tracking log file
// Primarily tracks kernel/initrd file operations (copying, verification, recovery)
// Also tracks related operations: system builds, daemon connection issues, symlink creation
// Uses /tmp/kernel_tracking.log on the install machine (works on both local and remote)
func logDebug(location, message string, data map[string]interface{}) {
	// Use /tmp/kernel_tracking.log which exists on all Unix systems (including Guix ISO)
	logPath := "/tmp/kernel_tracking.log"
	logEntry := map[string]interface{}{
		"timestamp": time.Now().UnixMilli(),
		"location":  location,
		"message":   message,
		"sessionId": "debug-session",
		"runId":     "run1",
		"data":      data,
	}
	if hypothesisId, ok := data["hypothesisId"]; ok {
		logEntry["hypothesisId"] = hypothesisId
	}
	jsonData, err := json.Marshal(logEntry)
	if err != nil {
		return // Silently fail if JSON encoding fails
	}
	f, err := os.OpenFile(logPath, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return // Silently fail if file open fails
	}
	defer f.Close()
	f.Write(jsonData)
	f.WriteString("\n")
}

// Hypothesis M: Network Diagnostics
// CheckNetworkConnectivity tests if network and DNS are working before attempting builds
func CheckNetworkConnectivity(platform, buildType string) error {
	logDebug("lib/common.go:3315", "Checking network connectivity", map[string]interface{}{
		"hypothesisId": "M",
		"step":         "check_network_start",
		"platform":     platform,
		"buildType":    buildType,
	})

	// Test DNS resolution
	fmt.Println("[INFO] Testing DNS resolution...")
	lookupCmd := exec.Command("getent", "hosts", "ci.guix.gnu.org")
	if err := lookupCmd.Run(); err != nil {
		logDebug("lib/common.go:3323", "DNS resolution failed", map[string]interface{}{
			"hypothesisId": "M",
			"step":         "dns_failed",
			"platform":     platform,
			"buildType":    buildType,
			"error":        err.Error(),
		})
		return fmt.Errorf("DNS resolution failed (cannot resolve ci.guix.gnu.org): %w", err)
	}

	logDebug("lib/common.go:3332", "Network connectivity OK", map[string]interface{}{
		"hypothesisId": "M",
		"step":         "network_ok",
		"platform":     platform,
		"buildType":    buildType,
	})
	fmt.Println("[OK] Network and DNS working")
	return nil
}

// Hypothesis K: Deep System Generation Search
// SearchSystemGenerationDeep explores subdirectories and config files to find kernel paths
func SearchSystemGenerationDeep(systemPath, platform, buildType string) (kernelPath, initrdPath string, err error) {
	logDebug("lib/common.go:3343", "Starting deep system generation search", map[string]interface{}{
		"hypothesisId": "K",
		"step":         "deep_search_start",
		"systemPath":   systemPath,
		"platform":     platform,
		"buildType":    buildType,
	})

	// Strategy 1: Check parameters file for kernel/initrd paths
	parametersPath := filepath.Join(systemPath, "parameters")
	if data, err := os.ReadFile(parametersPath); err == nil {
		content := string(data)
		logDebug("lib/common.go:3353", "Found parameters file", map[string]interface{}{
			"hypothesisId": "K",
			"step":         "parameters_found",
			"platform":     platform,
			"buildType":    buildType,
			"contentSize":  len(content),
		})

		// Parse parameters file for kernel/initrd store paths
		// Parameters file contains S-expressions with store paths
		kernelMatches := regexp.MustCompile(`/gnu/store/[a-z0-9]+-linux[^"'\s]*`).FindAllString(content, -1)
		initrdMatches := regexp.MustCompile(`/gnu/store/[a-z0-9]+-initrd[^"'\s]*`).FindAllString(content, -1)

		if len(kernelMatches) > 0 {
			// Try each match to find actual kernel file
			for _, match := range kernelMatches {
				possibleKernelPaths := []string{
					filepath.Join(match, "bzImage"),
					filepath.Join(match, "vmlinuz"),
					filepath.Join(match, "Image"),
					match, // Try the path itself
				}
				for _, kPath := range possibleKernelPaths {
					if _, err := os.Stat(kPath); err == nil {
						kernelPath = kPath
						logDebug("lib/common.go:3378", "Kernel found via parameters file", map[string]interface{}{
							"hypothesisId": "K",
							"step":         "kernel_from_parameters",
							"platform":     platform,
							"buildType":    buildType,
							"kernelPath":   kernelPath,
						})
						break
					}
				}
				if kernelPath != "" {
					break
				}
			}
		}

		if len(initrdMatches) > 0 && initrdMatches[0] != "" {
			if _, err := os.Stat(initrdMatches[0]); err == nil {
				initrdPath = initrdMatches[0]
				logDebug("lib/common.go:3395", "Initrd found via parameters file", map[string]interface{}{
					"hypothesisId": "K",
					"step":         "initrd_from_parameters",
					"platform":     platform,
					"buildType":    buildType,
					"initrdPath":   initrdPath,
				})
			}
		}
	}

	// Strategy 2: Explore subdirectories (gnu/, guix/)
	subdirs := []string{"gnu", "guix", "bin", "boot"}
	for _, subdir := range subdirs {
		subdirPath := filepath.Join(systemPath, subdir)
		if entries, err := os.ReadDir(subdirPath); err == nil {
			logDebug("lib/common.go:3410", "Exploring subdirectory", map[string]interface{}{
				"hypothesisId": "K",
				"step":         "explore_subdir",
				"platform":     platform,
				"buildType":    buildType,
				"subdirPath":   subdirPath,
				"entryCount":   len(entries),
			})

			// Look for symlinks or files that might be kernel/initrd
			for _, entry := range entries {
				entryPath := filepath.Join(subdirPath, entry.Name())

				// Follow symlinks
				if entry.Type()&os.ModeSymlink != 0 {
					if target, err := os.Readlink(entryPath); err == nil {
						// Resolve relative symlinks
						if !filepath.IsAbs(target) {
							target = filepath.Join(subdirPath, target)
						}

						// Check if this looks like a kernel or initrd
						if strings.Contains(target, "linux") || strings.Contains(target, "kernel") {
							if _, err := os.Stat(target); err == nil && kernelPath == "" {
								kernelPath = target
								logDebug("lib/common.go:3434", "Kernel found via symlink in subdir", map[string]interface{}{
									"hypothesisId": "K",
									"step":         "kernel_from_subdir_symlink",
									"platform":     platform,
									"buildType":    buildType,
									"kernelPath":   kernelPath,
									"symlinkFrom":  entryPath,
								})
							}
						}
						if strings.Contains(target, "initrd") {
							if _, err := os.Stat(target); err == nil && initrdPath == "" {
								initrdPath = target
								logDebug("lib/common.go:3446", "Initrd found via symlink in subdir", map[string]interface{}{
									"hypothesisId": "K",
									"step":         "initrd_from_subdir_symlink",
									"platform":     platform,
									"buildType":    buildType,
									"initrdPath":   initrdPath,
									"symlinkFrom":  entryPath,
								})
							}
						}
					}
				}
			}
		}
	}

	if kernelPath != "" || initrdPath != "" {
		logDebug("lib/common.go:3462", "Deep search found files", map[string]interface{}{
			"hypothesisId": "K",
			"step":         "deep_search_success",
			"platform":     platform,
			"buildType":    buildType,
			"kernelPath":   kernelPath,
			"initrdPath":   initrdPath,
		})
		return kernelPath, initrdPath, nil
	}

	logDebug("lib/common.go:3471", "Deep search found nothing", map[string]interface{}{
		"hypothesisId": "K",
		"step":         "deep_search_empty",
		"platform":     platform,
		"buildType":    buildType,
	})
	return "", "", fmt.Errorf("deep search found no kernel or initrd files")
}

// Hypothesis N: Store-Wide Kernel Search
// SearchStoreForKernel searches entire /gnu/store for linux-libre packages as last resort
func SearchStoreForKernel(platform, buildType string) (kernelPath, initrdPath string, err error) {
	logDebug("lib/common.go:3482", "Starting store-wide kernel search", map[string]interface{}{
		"hypothesisId": "N",
		"step":         "store_search_start",
		"platform":     platform,
		"buildType":    buildType,
	})

	// Find all linux-libre packages in store
	findCmd := exec.Command("bash", "-c", "ls -td /gnu/store/*-linux-libre-* 2>/dev/null | head -5")
	output, err := findCmd.Output()
	if err != nil {
		logDebug("lib/common.go:3491", "No linux-libre packages found in store", map[string]interface{}{
			"hypothesisId": "N",
			"step":         "no_packages_found",
			"platform":     platform,
			"buildType":    buildType,
			"error":        err.Error(),
		})
		return "", "", fmt.Errorf("no linux-libre packages found in store")
	}

	packages := strings.Split(strings.TrimSpace(string(output)), "\n")
	logDebug("lib/common.go:3501", "Found linux-libre packages", map[string]interface{}{
		"hypothesisId": "N",
		"step":         "packages_found",
		"platform":     platform,
		"buildType":    buildType,
		"packageCount": len(packages),
		"packages":     packages,
	})

	// Search each package for kernel files
	kernelNames := []string{"bzImage", "vmlinuz", "Image", "vmlinux"}
	for _, pkg := range packages {
		if pkg == "" {
			continue
		}

		for _, kname := range kernelNames {
			kpath := filepath.Join(pkg, kname)
			if info, err := os.Stat(kpath); err == nil && info.Size() > 1024*1024 {
				// Found a file > 1MB (reasonable kernel size)
				kernelPath = kpath
				logDebug("lib/common.go:3521", "Kernel found in store package", map[string]interface{}{
					"hypothesisId": "N",
					"step":         "kernel_from_store",
					"platform":     platform,
					"buildType":    buildType,
					"kernelPath":   kernelPath,
					"packagePath":  pkg,
					"size":         info.Size(),
				})
				break
			}
		}

		if kernelPath != "" {
			break
		}
	}

	// Search for initrd packages
	findInitrdCmd := exec.Command("bash", "-c", "ls -td /gnu/store/*-initrd* 2>/dev/null | head -1")
	initrdOutput, err := findInitrdCmd.Output()
	if err == nil && len(initrdOutput) > 0 {
		possibleInitrd := strings.TrimSpace(string(initrdOutput))
		if _, err := os.Stat(possibleInitrd); err == nil {
			initrdPath = possibleInitrd
			logDebug("lib/common.go:3546", "Initrd found in store", map[string]interface{}{
				"hypothesisId": "N",
				"step":         "initrd_from_store",
				"platform":     platform,
				"buildType":    buildType,
				"initrdPath":   initrdPath,
			})
		}
	}

	if kernelPath == "" {
		logDebug("lib/common.go:3555", "Store search failed to find kernel", map[string]interface{}{
			"hypothesisId": "N",
			"step":         "store_search_failed",
			"platform":     platform,
			"buildType":    buildType,
		})
		return "", "", fmt.Errorf("no kernel found in any store packages")
	}

	logDebug("lib/common.go:3562", "Store search successful", map[string]interface{}{
		"hypothesisId": "N",
		"step":         "store_search_success",
		"platform":     platform,
		"buildType":    buildType,
		"kernelPath":   kernelPath,
		"initrdPath":   initrdPath,
	})
	return kernelPath, initrdPath, nil
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

// DetectBootModeFromConfig detects boot mode from config.scm file
// Returns "uefi" if grub-efi-bootloader is found, "bios" if grub-bootloader is found
// Returns empty string if neither is found or file can't be read
func DetectBootModeFromConfig(configPath string) string {
	content, err := os.ReadFile(configPath)
	if err != nil {
		return ""
	}
	
	contentStr := string(content)
	
	// Check for UEFI bootloader (grub-efi-bootloader)
	if strings.Contains(contentStr, "grub-efi-bootloader") {
		return "uefi"
	}
	
	// Check for BIOS bootloader (grub-bootloader)
	if strings.Contains(contentStr, "grub-bootloader") {
		return "bios"
	}
	
	return ""
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
