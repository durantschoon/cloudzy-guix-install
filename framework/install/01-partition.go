package install

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step01Partition handles partitioning for Framework laptop
type Step01Partition struct{}

func (s *Step01Partition) RunWarnings(state *State) error {
	lib.PrintStepHeader(1, "Partition Setup")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Detect your disk device (or use DEVICE env var if set)")
	fmt.Println("  2. Display current partition table")
	fmt.Println("  3. Create GPT partition table")
	fmt.Println("  4. Create EFI System Partition (512MB, FAT32)")
	fmt.Println("  5. Create root partition (remaining space, ext4)")
	fmt.Println("  6. Format both partitions")
	fmt.Println()
	fmt.Println("Environment variables set by this step:")
	fmt.Println("  DEVICE - Block device (e.g., /dev/nvme0n1, /dev/sda)")
	fmt.Println("  EFI    - EFI partition (e.g., /dev/nvme0n1p1)")
	fmt.Println("  ROOT   - Root partition (e.g., /dev/nvme0n1p2)")
	fmt.Println()
	fmt.Println("WARNING: This will DESTROY ALL DATA on the selected disk!")
	fmt.Println()
	fmt.Println("Idempotency: Skips formatting if partitions are already formatted")
	fmt.Println()

	return nil
}

func (s *Step01Partition) RunClean(state *State) error {
	// Auto-detect device if not set
	if state.Device == "" {
		device, err := lib.DetectDevice("framework")
		if err != nil {
			return err
		}
		state.Device = device
	}

	// Check available space on device
	if err := lib.CheckDeviceSpace(state.Device, 40.0); err != nil {
		return err
	}

	// Show current partition table
	fmt.Printf("Current partition table for %s:\n", state.Device)
	lib.RunCommand("parted", state.Device, "print")
	fmt.Println()

	// Confirm before proceeding
	fmt.Printf("WARNING: About to partition %s. This will DESTROY ALL DATA!\n", state.Device)
	fmt.Print("Type 'yes' to continue: ")

	tty, err := os.Open("/dev/tty")
	if err != nil {
		return fmt.Errorf("failed to open /dev/tty: %w", err)
	}
	defer tty.Close()

	reader := bufio.NewReader(tty)
	answer, err := reader.ReadString('\n')
	if err != nil {
		return err
	}

	if strings.TrimSpace(answer) != "yes" {
		return fmt.Errorf("aborted by user")
	}

	// Create partitions
	fmt.Println("Creating GPT partition table...")
	if err := lib.RunCommand("parted", "-s", state.Device, "mklabel", "gpt"); err != nil {
		return err
	}

	fmt.Println("Creating EFI partition (512MB)...")
	if err := lib.RunCommand("parted", "-s", state.Device, "mkpart", "EFI", "fat32", "1MiB", "513MiB"); err != nil {
		return err
	}
	if err := lib.RunCommand("parted", "-s", state.Device, "set", "1", "esp", "on"); err != nil {
		return err
	}

	fmt.Println("Creating root partition (remaining space)...")
	if err := lib.RunCommand("parted", "-s", state.Device, "mkpart", "primary", "ext4", "513MiB", "100%"); err != nil {
		return err
	}

	// Set partition names
	if err := lib.RunCommand("parted", "-s", state.Device, "name", "2", "GUIX_ROOT"); err != nil {
		return err
	}

	// Determine partition paths
	if strings.Contains(state.Device, "nvme") || strings.Contains(state.Device, "mmcblk") {
		state.EFI = state.Device + "p1"
		state.Root = state.Device + "p2"
	} else {
		state.EFI = state.Device + "1"
		state.Root = state.Device + "2"
	}

	// Wait for partitions to appear
	fmt.Println("Waiting for kernel to recognize partitions...")
	lib.RunCommand("partprobe", state.Device)
	lib.RunCommand("sleep", "2")

	// Check if already formatted (idempotency)
	if lib.IsPartitionFormatted(state.Root, "ext4") {
		fmt.Println("Partitions are already formatted")
		fmt.Println("Skipping format step (idempotent - safe for reruns)")
		return nil
	}

    // Format partitions
	fmt.Printf("Formatting EFI partition: %s\n", state.EFI)
	if err := lib.RunCommand("mkfs.vfat", "-F", "32", "-n", "EFI", state.EFI); err != nil {
		return err
	}

	fmt.Printf("Formatting root partition: %s\n", state.Root)
	if err := lib.RunCommand("mkfs.ext4", "-L", "GUIX_ROOT", state.Root); err != nil {
		return err
	}

    fmt.Println()
    fmt.Println("Verifying partition labels...")
    lib.RunCommand("fatlabel", state.EFI)
    lib.RunCommand("e2label", state.Root)

	fmt.Println()
	fmt.Println("Partition setup complete:")
	fmt.Printf("  DEVICE=%s\n", state.Device)
	fmt.Printf("  EFI=%s\n", state.EFI)
	fmt.Printf("  ROOT=%s\n", state.Root)

	return nil
}

