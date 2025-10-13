// Package lib provides shared functionality for Guix installation across platforms
package lib

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

	// Check if daemon is running
	for attempts := 0; attempts < 5; attempts++ {
		cmd := exec.Command("herd", "status", "guix-daemon")
		output, err := cmd.Output()

		if err == nil && (strings.Contains(string(output), "It is started") || strings.Contains(string(output), "It is enabled")) {
			fmt.Println("[OK] guix-daemon process is running")

			// Additional check: verify daemon is actually responsive
			fmt.Println("Verifying daemon is responsive...")
			testCmd := exec.Command("guix", "build", "--version")
			if err := testCmd.Run(); err == nil {
				fmt.Println("[OK] guix-daemon is responsive and ready")
				return nil
			}
			fmt.Println("Daemon process is running but not responsive yet, waiting...")
		}

		if attempts == 0 {
			fmt.Println("guix-daemon is not running, starting daemon...")
			fmt.Println()
		} else {
			fmt.Printf("Attempt %d/%d: Daemon not running or not responsive, retrying...\n", attempts+1, 5)
		}

		// Stop daemon if it exists but not running properly
		fmt.Println("Stopping guix-daemon (if running)...")
		exec.Command("herd", "stop", "guix-daemon").Run()
		exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
		exec.Command("pkill", "-KILL", "-x", "guix-daemon").Run() // Force kill if needed
		time.Sleep(3 * time.Second)

		// Start the daemon (cow-store handles store redirection)
		fmt.Println("Starting guix-daemon...")
		if err := RunCommand("herd", "start", "guix-daemon"); err != nil {
			fmt.Printf("Warning: herd start failed: %v\n", err)
			// Try alternative startup method
			fmt.Println("Trying alternative daemon startup...")
			exec.Command("guix-daemon", "--build-users-group=guixbuild").Start()
			time.Sleep(2 * time.Second)
		}

		// Wait longer for daemon to be ready
		fmt.Println("Waiting 8 seconds for daemon to initialize...")
		time.Sleep(8 * time.Second)
	}

	// Final check
	cmd := exec.Command("herd", "status", "guix-daemon")
	output, err := cmd.Output()
	if err != nil || (!strings.Contains(string(output), "It is started") && !strings.Contains(string(output), "It is enabled")) {
		fmt.Println()
		fmt.Println("[ERROR] Failed to start guix-daemon after multiple attempts")
		fmt.Println()
		fmt.Println("Please manually run these commands:")
		fmt.Println("  herd start guix-daemon")
		fmt.Println("  herd status guix-daemon")
		fmt.Println()
		fmt.Println("Once the daemon is running, continue with:")
		fmt.Println("  guix system init /mnt/etc/config.scm /mnt")
		fmt.Println("  # OR if substitutes fail:")
		fmt.Println("  guix system init --fallback /mnt/etc/config.scm /mnt")
		fmt.Println()
		fmt.Println("NOTE: cow-store should already be running. Do NOT bind mount /mnt/gnu!")
		return fmt.Errorf("guix-daemon is not running - please start manually and run guix system init")
	}

	fmt.Println()
	return nil
}

// VerifyESP verifies the EFI System Partition is properly mounted as vfat
func VerifyESP() error {
	fmt.Println()
	fmt.Println("=== Verifying EFI System Partition ===")
	
	// First check partition information with lsblk
	fmt.Println("Checking partition information:")
	RunCommand("lsblk", "-f")
	
	// Check if /mnt/boot/efi exists and is mounted
	fmt.Println("Checking if /mnt/boot/efi is mounted:")
	cmd := exec.Command("df", "-T", "/mnt/boot/efi")
	output, err := cmd.Output()
	if err != nil {
		fmt.Println("ERROR: Cannot check /mnt/boot/efi - may not be mounted")
		fmt.Println("Checking all EFI-related mounts:")
		RunCommand("mount | grep -i efi")
		RunCommand("ls -la /mnt/boot/")
		
		// Try to find EFI partition and check its type directly
		fmt.Println("Checking EFI partition filesystem type directly:")
		RunCommand("blkid", "-t", "TYPE=vfat")
		RunCommand("blkid", "-t", "LABEL=EFI")
		
		return fmt.Errorf("failed to check EFI partition: %w", err)
	}
	
	outputStr := string(output)
	fmt.Printf("EFI mount info: %s", outputStr)
	
	// Check for various vfat filesystem type names
	isVfat := strings.Contains(outputStr, "vfat") || 
			  strings.Contains(outputStr, "VFAT") ||
			  strings.Contains(outputStr, "fat32") ||
			  strings.Contains(outputStr, "FAT32")
	
	if !isVfat {
		fmt.Println("ERROR: /mnt/boot/efi is not mounted as vfat filesystem")
		fmt.Println("The EFI partition (p1) should be mounted at /mnt/boot/efi")
		fmt.Println("Current mount info:")
		RunCommand("df", "-T", "/mnt/boot/efi")
		RunCommand("mount | grep -i efi")
		RunCommand("ls -la /mnt/boot/")
		
		// Additional check: try to read the filesystem type directly
		fmt.Println("Checking filesystem type directly:")
		RunCommand("blkid", "/mnt/boot/efi")
		RunCommand("blkid", "-t", "TYPE=vfat")
		RunCommand("blkid", "-t", "LABEL=EFI")
		
		return fmt.Errorf("EFI verification failed: not a vfat filesystem")
	}
	fmt.Println("[OK] EFI partition (p1) is correctly mounted as vfat at /mnt/boot/efi")
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

// ValidateGuixConfig validates the config file for common issues
func ValidateGuixConfig(configPath string) error {
	fmt.Println("=== Validating Guix Configuration ===")
	fmt.Printf("Checking config file: %s\n", configPath)
	
	// Check if config file exists
	if _, err := os.Stat(configPath); err != nil {
		return fmt.Errorf("config file does not exist: %s", configPath)
	}
	
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
	// Validate config first to catch issues early
	configPath := "/mnt/etc/config.scm"
	if err := ValidateGuixConfig(configPath); err != nil {
		return fmt.Errorf("config validation failed: %w", err)
	}
	
	// Setup GRUB EFI if needed
	if err := SetupGRUBEFI(); err != nil {
		return fmt.Errorf("GRUB EFI setup failed: %w", err)
	}
	
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
			time.Sleep(10 * time.Second)
		}

		if err := RunCommand("guix", "system", "init", "--fallback", "-v6", "/mnt/etc/config.scm", "/mnt"); err != nil {
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

// VerifyInstallation verifies that all critical files were installed
func VerifyInstallation() error {
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
    avail := fields[3]
    if strings.HasSuffix(avail, "G") {
        avail = strings.TrimSuffix(avail, "G")
    }
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
    return RunCommand("mount", filepath.Join("/dev/disk/by-label", label), mountPoint)
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
