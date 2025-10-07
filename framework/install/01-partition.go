package install

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

func runCommand(name string, args ...string) error {
	cmd := exec.Command(name, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// Step01Partition handles partitioning for Framework laptop
type Step01Partition struct{}

func (s *Step01Partition) RunWarnings(state *State) error {
	fmt.Println("=== Step 1: Partition Setup ===")
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
		if err := s.detectDevice(state); err != nil {
			return err
		}
	}

	// Show current partition table
	fmt.Printf("Current partition table for %s:\n", state.Device)
	runCommand("parted", state.Device, "print")
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
	if err := runCommand("parted", "-s", state.Device, "mklabel", "gpt"); err != nil {
		return err
	}

	fmt.Println("Creating EFI partition (512MB)...")
	if err := runCommand("parted", "-s", state.Device, "mkpart", "ESP", "fat32", "1MiB", "513MiB"); err != nil {
		return err
	}
	if err := runCommand("parted", "-s", state.Device, "set", "1", "esp", "on"); err != nil {
		return err
	}

	fmt.Println("Creating root partition (remaining space)...")
	if err := runCommand("parted", "-s", state.Device, "mkpart", "primary", "ext4", "513MiB", "100%"); err != nil {
		return err
	}

	// Set partition names
	if err := runCommand("parted", "-s", state.Device, "name", "2", "guix-root"); err != nil {
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
	runCommand("partprobe", state.Device)
	runCommand("sleep", "2")

	// Check if already formatted (idempotency)
	if s.isPartitionFormatted(state.Root) {
		fmt.Println("Partitions are already formatted")
		fmt.Println("Skipping format step (idempotent - safe for reruns)")
		return nil
	}

	// Format partitions
	fmt.Printf("Formatting EFI partition: %s\n", state.EFI)
	if err := runCommand("mkfs.vfat", "-F", "32", "-n", "EFI", state.EFI); err != nil {
		return err
	}

	fmt.Printf("Formatting root partition: %s\n", state.Root)
	if err := runCommand("mkfs.ext4", "-L", "guix-root", state.Root); err != nil {
		return err
	}

	fmt.Println()
	fmt.Println("Partition setup complete:")
	fmt.Printf("  DEVICE=%s\n", state.Device)
	fmt.Printf("  EFI=%s\n", state.EFI)
	fmt.Printf("  ROOT=%s\n", state.Root)

	return nil
}

func (s *Step01Partition) detectDevice(state *State) error {
	// Framework laptops typically use NVMe or SATA
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

func (s *Step01Partition) isPartitionFormatted(partition string) bool {
	cmd := exec.Command("blkid", partition)
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	// If blkid returns output, the partition has a filesystem
	return len(output) > 0
}
