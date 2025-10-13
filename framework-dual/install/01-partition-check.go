package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step01PartitionCheck performs partition detection and setup
type Step01PartitionCheck struct{}

func (s *Step01PartitionCheck) RunWarnings(state *State) error {
	fmt.Println("=== Step 1: Partition Check ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Detect your disk device (or use DEVICE env var if set)")
	fmt.Println("  2. Find your existing EFI partition (sets EFI env var)")
	fmt.Println("  3. Check for existing 'GUIX_ROOT' partition")
	fmt.Println("  4. Format GUIX_ROOT OR create new partition in free space")
	fmt.Println("  5. Detect separate home partition if it exists (sets HOME_PARTITION)")
	fmt.Println()
	fmt.Println("Environment variables set by this step:")
	fmt.Println("  DEVICE        - Block device (e.g., /dev/nvme0n1)")
	fmt.Println("  EFI           - EFI System Partition (e.g., /dev/nvme0n1p1)")
	fmt.Println("  ROOT          - Guix root partition (e.g., /dev/nvme0n1p4)")
	fmt.Println("  HOME_PARTITION - Separate home partition if found (optional)")
	fmt.Println()

	// Check if running from Guix live ISO
	if !lib.IsGuixLiveISO() {
		fmt.Println("WARNING: This doesn't appear to be a Guix live ISO environment!")
		fmt.Println("   This script is designed to run from a Guix live ISO.")
		fmt.Println("   If you're not sure you're in the right environment, STOP NOW!")
		fmt.Println()
		fmt.Println("   Press Ctrl+C to abort, or wait 10 seconds to continue...")
		time.Sleep(10 * time.Second)
		fmt.Println()
	}

	// Detect device
	if err := s.detectDevice(state); err != nil {
		return err
	}

	// Show current partition layout
	fmt.Println()
	fmt.Println("=== Current Partition Layout ===")
	lib.RunCommand("lsblk", state.Device, "-o", "NAME,SIZE,TYPE,FSTYPE,MOUNTPOINT,LABEL")
	fmt.Println()

	// Find EFI partition
	if err := s.findEFIPartition(state); err != nil {
		return err
	}

	// Check for Pop!_OS
	fmt.Println()
	fmt.Println("=== Checking for Pop!_OS installation ===")
	s.checkPopOS(state)

	// Check for existing GUIX_ROOT partition
	fmt.Println()
	fmt.Println("=== Checking for existing GUIX_ROOT partition ===")
	guixRootPart, err := s.findGuixRootPartition(state)
	if err != nil {
		return err
	}

	if guixRootPart != "" {
		// Existing GUIX_ROOT partition
		partSize := lib.GetPartitionSizeGiB(guixRootPart)

		fmt.Printf("Found existing partition labeled 'GUIX_ROOT': %s\n", guixRootPart)
		fmt.Printf("Partition size: %.1fGiB\n", partSize)
		fmt.Println()
		fmt.Println("=== Partition Plan ===")
		fmt.Println("This script will:")
		fmt.Printf("  1. Keep existing EFI partition: %s\n", state.EFI)
		fmt.Println("  2. Keep all existing Pop!_OS partitions (untouched)")
		fmt.Printf("  3. Format existing GUIX_ROOT partition: %s (%.1fGiB)\n", guixRootPart, partSize)
		fmt.Println("  4. Install Guix bootloader to ESP (will chain to Pop!_OS)")
	} else {
		// Need to create new partition
		fmt.Println("No partition labeled 'GUIX_ROOT' found.")

		// Show free space
		fmt.Println()
		fmt.Println("=== Available Free Space ===")
		lib.RunCommand("parted", state.Device, "unit", "GiB", "print", "free")
		fmt.Println()

		// Check free space
		freeSpaceGB := lib.GetFreeSpaceGiB(state.Device)
		if freeSpaceGB < 40 {
			fmt.Printf("WARNING: Less than 40GB of free space available (found: %.1fGiB)\n", freeSpaceGB)
			fmt.Println("   Recommended: At least 40-60GB for Guix root partition")
			fmt.Println("   You may need to shrink your Pop!_OS partition first.")
			fmt.Println("   OR: Create a partition labeled 'GUIX_ROOT' using GParted/parted")
			fmt.Println()
			fmt.Println("   Press Ctrl+C to abort, or wait 10 seconds to continue anyway...")
			time.Sleep(10 * time.Second)
			fmt.Println()
		}

		fmt.Println("=== Partition Plan ===")
		fmt.Println("This script will:")
		fmt.Printf("  1. Keep existing EFI partition: %s\n", state.EFI)
		fmt.Println("  2. Keep all existing Pop!_OS partitions (untouched)")
		fmt.Println("  3. Create a new partition for Guix in available free space")
		fmt.Println("  4. Install Guix bootloader to ESP (will chain to Pop!_OS)")
	}

	fmt.Println()
	fmt.Println("IMPORTANT: Make sure you have backed up your data!")
	fmt.Println("   While this script tries to be safe, disk operations are always risky.")
	fmt.Println()

	// Check for home partition
	fmt.Println()
	fmt.Println("=== Checking for separate home partition ===")
	s.findHomePartition(state)

	return nil
}

func (s *Step01PartitionCheck) RunClean(state *State) error {
	// Verify required variables
	if state.Device == "" || state.EFI == "" {
		return fmt.Errorf("required variables not set (DEVICE, EFI)")
	}

	fmt.Println("Looking for existing partition labeled 'GUIX_ROOT'...")
	root, err := s.findGuixRootPartition(state)
	if err != nil {
		return err
	}

	if root != "" {
		// Found existing partition
		state.Root = root

		fmt.Printf("Found existing partition labeled 'GUIX_ROOT': %s\n", root)

		// Check if partition is already formatted
		if s.isPartitionFormatted(root) {
			fmt.Println("Partition is already formatted as ext4")
			fmt.Println("Skipping format step (idempotent - safe for reruns)")
    fmt.Printf("ROOT is %s and EFI is %s\n", state.Root, state.EFI)
    fmt.Println()
    fmt.Println("Verifying partition labels...")
    if state.Device != "" {
        if strings.Contains(state.Device, "nvme") {
            lib.RunCommand("fatlabel", state.Device+"p1")
            lib.RunCommand("e2label", state.Device+"p2")
        } else {
            lib.RunCommand("fatlabel", state.Device+"1")
            lib.RunCommand("e2label", state.Device+"2")
        }
    }
    return nil
		}

		fmt.Println("This partition will be formatted (all data will be lost)")
		fmt.Println()

		if !lib.AskYesNo(fmt.Sprintf("Type 'YES' to format %s: ", root), "YES") {
			return fmt.Errorf("aborted by user")
		}

		fmt.Printf("Formatting %s as ext4 with label GUIX_ROOT...\n", root)
		if err := lib.RunCommand("mkfs.ext4", "-F", "-L", "GUIX_ROOT", root); err != nil {
			return err
		}
	} else {
		// Create new partition
		fmt.Println("No partition labeled 'GUIX_ROOT' found.")
		fmt.Printf("Creating new Guix partition on %s\n", state.Device)
		fmt.Printf("Existing EFI partition: %s (will be reused)\n", state.EFI)

		// Find free space
		freeStartMiB, err := lib.FindFreeSpaceStart(state.Device)
		if err != nil {
			return err
		}

		fmt.Printf("Creating Guix root partition starting at %dMiB\n", freeStartMiB)
		if err := lib.RunCommand("parted", "--script", state.Device,
			"mkpart", "GUIX_ROOT", "ext4", fmt.Sprintf("%dMiB", freeStartMiB), "100%"); err != nil {
			return err
		}

		// Find the partition that was just created
		root, err := lib.GetLastPartition(state.Device)
		if err != nil {
			return err
		}
		state.Root = root

		fmt.Printf("Created Guix root partition: %s\n", root)

		// Format it with label
		fmt.Printf("Formatting %s as ext4 with label GUIX_ROOT...\n", root)
		if err := lib.RunCommand("mkfs.ext4", "-F", "-L", "GUIX_ROOT", root); err != nil {
			return err
		}
	}

	fmt.Printf("ROOT is %s and EFI is %s\n", state.Root, state.EFI)
	return nil
}

// Helper functions

func (s *Step01PartitionCheck) detectDevice(state *State) error {
	if state.Device != "" {
		// User-specified device
		if _, err := os.Stat(state.Device); err != nil {
			return fmt.Errorf("specified device %s is not a block device", state.Device)
		}
		fmt.Printf("Using user-specified device: %s\n", state.Device)
		return nil
	}

	// Auto-detect
	candidates := []string{"/dev/nvme0n1", "/dev/nvme1n1", "/dev/sda"}
	for _, d := range candidates {
		if _, err := os.Stat(d); err == nil {
			state.Device = d
			fmt.Printf("Auto-detected device: %s\n", d)
			return nil
		}
	}

	fmt.Println("Error: No suitable block device found. Expected one of: /dev/nvme0n1, /dev/nvme1n1, /dev/sda")
	fmt.Println("Available block devices:")
	lib.RunCommand("lsblk", "-d", "-n", "-o", "NAME,SIZE,TYPE")
	return fmt.Errorf("no suitable block device found")
}

func (s *Step01PartitionCheck) findEFIPartition(state *State) error {
	fmt.Println("=== Checking for existing EFI partition ===")

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

	fmt.Printf("Found existing EFI partition: %s\n", state.EFI)
	return nil
}

func (s *Step01PartitionCheck) checkPopOS(state *State) {
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,FSTYPE,LABEL", state.Device)
	output, _ := cmd.Output()

	found := false
	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(line, "Pop_OS") || strings.Contains(line, "PopOS") {
			found = true
			fields := strings.Fields(line)
			if len(fields) > 0 {
				fmt.Printf("Found Pop!_OS partition: /dev/%s\n", fields[0])
			}
		}
	}
	if !found {
		fmt.Println("Note: No Pop!_OS partitions detected")
	}
}

func (s *Step01PartitionCheck) findGuixRootPartition(state *State) (string, error) {
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,LABEL", state.Device)
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to read partition table: %w", err)
	}

	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(line, "GUIX_ROOT") {
			fields := strings.Fields(line)
			if len(fields) >= 2 && fields[1] == "GUIX_ROOT" {
				// Extract partition name (strip any tree characters)
				partName := fields[0]
				// Remove tree branch characters like ├─ └─ │
				partName = strings.TrimLeft(partName, "├─└─│ ")
				return "/dev/" + partName, nil
			}
		}
	}

	return "", nil
}

func (s *Step01PartitionCheck) findHomePartition(state *State) {
	// Search for partition with filesystem label "DATA"
	cmd := exec.Command("lsblk", "-n", "-o", "NAME,LABEL", state.Device)
	output, _ := cmd.Output()

	for _, line := range strings.Split(string(output), "\n") {
		if strings.Contains(line, "DATA") {
			fields := strings.Fields(line)
			if len(fields) >= 2 && fields[1] == "DATA" {
				// Extract full device path from partition name (strip tree characters)
				partName := fields[0]
				partName = strings.TrimLeft(partName, "├─└─│ ")
				state.HomePartition = "/dev/" + partName
				homeSize := lib.GetPartitionSizeGiB(state.HomePartition)
				fmt.Printf("Found DATA partition: %s\n", state.HomePartition)
				fmt.Printf("DATA partition size: %.1fGiB\n", homeSize)
				fmt.Println("This partition will be mounted at /home and shared between Pop!_OS and Guix")
				return
			}
		}
	}

	fmt.Println("No separate DATA partition found - home directories will be in root partition")
}

func (s *Step01PartitionCheck) makePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}


func (s *Step01PartitionCheck) isPartitionFormatted(partition string) bool {
	cmd := exec.Command("blkid", "-s", "TYPE", "-o", "value", partition)
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	fsType := strings.TrimSpace(string(output))
	return fsType == "ext4"
}
