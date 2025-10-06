package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// Step02MountExisting mounts partitions and sets up store
type Step02MountExisting struct{}

func (s *Step02MountExisting) RunWarnings(state *State) error {
	// Auto-detect missing variables if Step01 was skipped
	if state.Device == "" {
		if err := s.detectDevice(state); err != nil {
			return err
		}
	}

	if state.EFI == "" {
		if err := s.findEFIPartition(state); err != nil {
			return err
		}
	}

	if state.Root == "" {
		if err := s.findGuixRootPartition(state); err != nil {
			return err
		}
	}

	// Verify required variables
	if state.Root == "" || state.EFI == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI). Please run Step01 first or set DEVICE, ROOT, and EFI environment variables")
	}

	fmt.Println("=== Step 2: Mount and Store Setup ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Mount Guix root partition to /mnt (if not already mounted)")
	fmt.Println("  2. Stop guix-daemon temporarily")
	fmt.Println("  3. Copy Guix store from ISO to /mnt/gnu/store (if not already done)")
	fmt.Println("  4. Copy /var/guix database to /mnt/var/guix")
	fmt.Println("  5. Mount EFI partition to /mnt/boot/efi")
	fmt.Println("  6. Mount home partition to /mnt/home (if HOME_PARTITION is set)")
	fmt.Println()
	fmt.Println("Environment variables used by this step:")
	fmt.Printf("  ROOT          - %s (from Step01)\n", state.Root)
	fmt.Printf("  EFI           - %s (from Step01)\n", state.EFI)
	fmt.Printf("  DEVICE        - %s (from Step01)\n", state.Device)
	if state.HomePartition != "" {
		fmt.Printf("  HOME_PARTITION - %s (from Step01)\n", state.HomePartition)
	} else {
		fmt.Println("  HOME_PARTITION - (not set, home will be in root)")
	}
	fmt.Println()
	fmt.Println("Idempotency: Skips store copy if /mnt/gnu/store already populated")
	fmt.Println()

	return nil
}

func (s *Step02MountExisting) RunClean(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
	}

	// Check if already mounted and populated (idempotency)
	if isMounted("/mnt") && s.isStorePopulated() {
		fmt.Println("/mnt is already mounted and /mnt/gnu/store is populated")
		fmt.Println("Skipping mount and store sync (idempotent - safe for reruns)")
		return nil
	}

	// Mount root partition if not mounted
	if !isMounted("/mnt") {
		fmt.Printf("Mounting %s to /mnt\n", state.Root)
		if err := runCommand("mount", state.Root, "/mnt"); err != nil {
			return err
		}
	} else {
		fmt.Println("/mnt is already mounted")
	}

	// Create directories
	dirs := []string{"/mnt/gnu/store", "/mnt/var/guix"}
	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create %s: %w", dir, err)
		}
	}

	// Stop guix-daemon
	fmt.Println("Stopping guix-daemon...")
	if commandExists("herd") {
		runCommand("herd", "stop", "guix-daemon")
	} else {
		exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
	}

	// Copy store
	start := time.Now()
	fmt.Println("Syncing /gnu/store to /mnt/gnu/store...")

	if commandExists("rsync") {
		fmt.Println("Using rsync...")
		if err := runCommand("rsync", "-aHAX", "--info=progress2,stats", "/gnu/store/.", "/mnt/gnu/store/."); err != nil {
			return fmt.Errorf("rsync failed: %w", err)
		}
		fmt.Println("rsync completed successfully")
	} else {
		fmt.Println("rsync not available, using cp instead...")
		if err := runCommand("cp", "-a", "/gnu/store/.", "/mnt/gnu/store/"); err != nil {
			return fmt.Errorf("cp failed: %w", err)
		}
		fmt.Println("cp completed successfully")
	}

	elapsed := time.Since(start)
	fmt.Printf("Time taken: %.0f seconds\n", elapsed.Seconds())

	// Copy /var/guix for database consistency
	fmt.Println("Copying /var/guix to /mnt/var/guix...")
	if err := runCommand("cp", "-a", "/var/guix", "/mnt/var/"); err != nil {
		return fmt.Errorf("failed to copy /var/guix: %w", err)
	}

	// Mount ESP
	if err := os.MkdirAll("/mnt/boot/efi", 0755); err != nil {
		return err
	}
	fmt.Printf("Mounting existing ESP: %s\n", state.EFI)
	if err := runCommand("mount", state.EFI, "/mnt/boot/efi"); err != nil {
		return err
	}

	// Verify ESP contents
	fmt.Println("Checking ESP contents...")
	runCommand("ls", "-la", "/mnt/boot/efi/")

	// Mount home partition if it exists
	if state.HomePartition != "" {
		fmt.Printf("Mounting home partition: %s\n", state.HomePartition)
		if err := os.MkdirAll("/mnt/home", 0755); err != nil {
			return err
		}
		if err := runCommand("mount", state.HomePartition, "/mnt/home"); err != nil {
			return err
		}
		fmt.Println("Home partition mounted successfully")
		runCommand("df", "-h", "/mnt/home")
	} else {
		fmt.Println("No separate home partition - home directories will be in root partition")
	}

	fmt.Println()
	fmt.Println("Mount setup complete. Ready for system initialization.")
	return nil
}

// Helper functions

func (s *Step02MountExisting) detectDevice(state *State) error {
	// Auto-detect
	candidates := []string{"/dev/nvme0n1", "/dev/nvme1n1", "/dev/sda"}
	for _, d := range candidates {
		if _, err := os.Stat(d); err == nil {
			state.Device = d
			fmt.Printf("Auto-detected device: %s\n", d)
			return nil
		}
	}
	return fmt.Errorf("no suitable block device found. Please set DEVICE environment variable")
}

func (s *Step02MountExisting) findEFIPartition(state *State) error {
	if state.Device == "" {
		return fmt.Errorf("DEVICE not set")
	}

	// Find EFI partition by GPT type GUID
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,PARTTYPE", state.Device)
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to list partitions: %w", err)
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
		return fmt.Errorf("no EFI System Partition found")
	}

	// Convert partition name to device path
	if strings.Contains(state.Device, "nvme") {
		// Extract partition number from nvme0n1p1
		parts := strings.Split(efiPartName, "p")
		if len(parts) >= 2 {
			state.EFI = state.Device + "p" + parts[len(parts)-1]
		}
	} else {
		// SATA: extract number from sda1
		for i := len(efiPartName) - 1; i >= 0; i-- {
			if efiPartName[i] < '0' || efiPartName[i] > '9' {
				state.EFI = state.Device + efiPartName[i+1:]
				break
			}
		}
	}

	fmt.Printf("Found EFI partition: %s\n", state.EFI)
	return nil
}

func (s *Step02MountExisting) findGuixRootPartition(state *State) error {
	if state.Device == "" {
		return fmt.Errorf("DEVICE not set")
	}

	cmd := exec.Command("parted", state.Device, "print")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to read partition table: %w", err)
	}

	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(line, "guix-root") {
			fields := strings.Fields(line)
			if len(fields) > 0 {
				partNum := fields[0]
				state.Root = s.makePartitionPath(state.Device, partNum)
				fmt.Printf("Found guix-root partition: %s\n", state.Root)
				return nil
			}
		}
	}

	return fmt.Errorf("no partition labeled 'guix-root' found")
}

func (s *Step02MountExisting) makePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}

func isMounted(path string) bool {
	cmd := exec.Command("mount")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(output), " on "+path+" ")
}

func commandExists(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}

func (s *Step02MountExisting) isStorePopulated() bool {
	// Check if /mnt/gnu/store has contents
	entries, err := os.ReadDir("/mnt/gnu/store")
	if err != nil {
		return false
	}
	// Store should have many entries if populated
	return len(entries) > 10
}
