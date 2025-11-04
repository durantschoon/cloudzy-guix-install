package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step01Partition performs disk partitioning for clean VPS install
type Step01Partition struct{}

func (s *Step01Partition) RunWarnings(state *State) error {
	fmt.Println("=== Step 1: Partition Setup ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Detect your disk device (or use DEVICE env var if set)")
	fmt.Println("  2. DESTROY ALL DATA on the device")
	fmt.Println("  3. Create GPT partition table")
	fmt.Println("  4. Create EFI System Partition (512MB, FAT32)")
	fmt.Println("  5. Create root partition (remaining space, ext4)")
	fmt.Println()
	fmt.Println("Environment variables set by this step:")
	fmt.Println("  DEVICE - Block device (e.g., /dev/vda or /dev/sda)")
	fmt.Println("  EFI    - EFI System Partition (e.g., /dev/vda1)")
	fmt.Println("  ROOT   - Root partition (e.g., /dev/vda2)")
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

	// Show current state
	fmt.Println()
	fmt.Println("=== Current Disk State ===")
	lib.RunCommand("lsblk", state.Device, "-o", "NAME,SIZE,TYPE,FSTYPE,MOUNTPOINT,LABEL")
	fmt.Println()

	// Check for existing data
	fmt.Println("=== Checking for Existing Data ===")
	if s.hasExistingData(state.Device) {
		fmt.Println("WARNING: Existing data detected on", state.Device)
		fmt.Println("   This script will DESTROY ALL DATA on this device!")
		fmt.Println("   If you have important data, STOP NOW!")
		fmt.Println()
		fmt.Println("   Press Ctrl+C to abort, or wait 10 seconds to continue...")
		time.Sleep(10 * time.Second)
		fmt.Println()
	} else {
		fmt.Println("No existing data detected - safe to proceed")
		fmt.Println()
	}

	// Check disk size
	fmt.Println("=== Checking Disk Size ===")
	diskSizeGB := lib.GetDiskSizeGiB(state.Device)
	fmt.Printf("Disk size: %.1fGiB\n", diskSizeGB)
	
	if diskSizeGB < 20 {
		fmt.Printf("WARNING: Disk is very small (%.1fGiB) - may not have enough space for Guix\n", diskSizeGB)
		fmt.Println("   Recommended minimum: 20GB")
		fmt.Println("   Press Ctrl+C to abort, or wait 5 seconds to continue...")
		time.Sleep(5 * time.Second)
		fmt.Println()
	} else if diskSizeGB < 40 {
		fmt.Printf("WARNING: Disk is small (%.1fGiB) - may have limited space for packages\n", diskSizeGB)
		fmt.Println("   Recommended: 40GB+ for comfortable usage")
		fmt.Println("   Press Ctrl+C to abort, or wait 5 seconds to continue...")
		time.Sleep(5 * time.Second)
		fmt.Println()
	} else {
		fmt.Println("Disk size is adequate for Guix installation")
		fmt.Println()
	}

	fmt.Println("=== Final Warning ===")
	fmt.Println("This script will DESTROY ALL DATA on", state.Device)
	fmt.Println("   This is designed for fresh VPS instances.")
	fmt.Println("   If you have important data, STOP NOW!")
	fmt.Println()

	return nil
}

func (s *Step01Partition) RunClean(state *State) error {
	// Verify required variables
	if state.Device == "" {
		return fmt.Errorf("DEVICE not set")
	}

	// Check if device is mounted and unmount for idempotency
	if s.isDeviceMounted(state.Device) {
		fmt.Printf("Device %s is currently mounted. Unmounting for idempotency...\n", state.Device)
		if err := lib.UnmountDevice(state.Device); err != nil {
			return fmt.Errorf("failed to unmount device %s: %w", state.Device, err)
		}
		fmt.Println("Device unmounted successfully")
	}

	// Check available space on device
	if err := lib.CheckDeviceSpace(state.Device, 40.0); err != nil {
		return err
	}

	// Check if partitions already exist and are formatted (idempotency)
	if lib.HasExistingPartitions(state.Device) {
		fmt.Println("Partitions already exist, detecting layout...")

		// Detect partition layout: check if partition 1 is BIOS boot or EFI
		// BIOS layout: partition 1 = BIOS boot, 2 = EFI, 3 = root
		// UEFI layout: partition 1 = EFI, 2 = root
		part1 := lib.MakePartitionPath(state.Device, "1")
		isBIOSBoot := lib.IsPartitionType(part1, "bios_grub")

		if isBIOSBoot {
			fmt.Println("Detected BIOS partition layout (BIOS boot + ESP + root)")
			state.EFI = lib.MakePartitionPath(state.Device, "2")
			state.Root = lib.MakePartitionPath(state.Device, "3")
			// Set boot mode to match partition layout
			state.BootMode = "bios"
			fmt.Println("Setting BootMode=bios to match partition layout")
		} else {
			fmt.Println("Detected UEFI partition layout (ESP + root)")
			state.EFI = lib.MakePartitionPath(state.Device, "1")
			state.Root = lib.MakePartitionPath(state.Device, "2")
			// Set boot mode to match partition layout
			state.BootMode = "uefi"
			fmt.Println("Setting BootMode=uefi to match partition layout")
		}

		// Check if partitions are already formatted
		if lib.IsPartitionFormatted(state.EFI, "vfat") && lib.IsPartitionFormatted(state.Root, "ext4") {
			fmt.Println("Partitions are already formatted")
			fmt.Println("Skipping partitioning and formatting (idempotent - safe for reruns)")
			fmt.Printf("EFI is %s and ROOT is %s\n", state.EFI, state.Root)
			fmt.Println()
			fmt.Println("Verifying partition labels...")
			lib.RunCommand("fatlabel", state.EFI)
			lib.RunCommand("e2label", state.Root)
			return nil
		}

		fmt.Println("Partitions exist but not formatted, proceeding with formatting...")
	} else {
		fmt.Println("Creating partitions...")
		fmt.Println()

		// Detect boot mode to create appropriate partition layout
		// Use state.BootMode if set (from env var), otherwise detect from environment
		bootMode := state.BootMode
		if bootMode == "" {
			bootMode = lib.DetectBootMode()
		}
		fmt.Printf("Boot mode: %s\n", bootMode)
		if state.BootMode != "" {
			fmt.Println("  (using BOOT_MODE env var)")
		} else {
			fmt.Println("  (auto-detected from live ISO environment)")
		}
		fmt.Println()

		// Store boot mode in state for later steps
		state.BootMode = bootMode

		if bootMode == "uefi" {
			// UEFI: Create EFI System Partition + root
			fmt.Println("Creating UEFI partition layout (ESP + root)...")
			if err := lib.RunCommand("parted", "--script", state.Device,
				"mklabel", "gpt",
				"mkpart", "EFI", "fat32", "1MiB", "513MiB",
				"set", "1", "esp", "on",
				"mkpart", "root", "ext4", "513MiB", "100%"); err != nil {
				return fmt.Errorf("partitioning failed: %w", err)
			}

			// Set partition paths
			state.EFI = lib.MakePartitionPath(state.Device, "1")
			state.Root = lib.MakePartitionPath(state.Device, "2")
		} else {
			// BIOS: Create BIOS boot partition + EFI (for flexibility) + root
			fmt.Println("Creating BIOS partition layout (BIOS boot + ESP + root)...")
			fmt.Println("  Note: Including ESP for potential future UEFI boot")
			if err := lib.RunCommand("parted", "--script", state.Device,
				"mklabel", "gpt",
				"mkpart", "BIOSBOOT", "1MiB", "2MiB",
				"set", "1", "bios_grub", "on",
				"mkpart", "EFI", "fat32", "2MiB", "514MiB",
				"set", "2", "esp", "on",
				"mkpart", "root", "ext4", "514MiB", "100%"); err != nil {
				return fmt.Errorf("partitioning failed: %w", err)
			}

			// Set partition paths (note: different numbering for BIOS)
			state.EFI = lib.MakePartitionPath(state.Device, "2")
			state.Root = lib.MakePartitionPath(state.Device, "3")
		}
	}

	fmt.Printf("EFI is %s and ROOT is %s\n", state.EFI, state.Root)
	fmt.Println()

	// Format partitions with labels
    fmt.Printf("Formatting %s as FAT32 with label EFI...\n", state.EFI)
	if err := lib.RunCommand("mkfs.vfat", "-F32", "-n", "EFI", state.EFI); err != nil {
		return err
	}

	fmt.Printf("Formatting %s as ext4 with label GUIX_ROOT...\n", state.Root)
	if err := lib.RunCommand("mkfs.ext4", "-F", "-L", "GUIX_ROOT", state.Root); err != nil {
		return err
	}

    fmt.Println()
    fmt.Println("Verifying partition labels...")
    lib.RunCommand("fatlabel", state.EFI)
    lib.RunCommand("e2label", state.Root)

	fmt.Println()
	fmt.Println("Partitioning complete!")
	return nil
}

// Helper functions

func (s *Step01Partition) detectDevice(state *State) error {
	if state.Device != "" {
		// User-specified device
		if _, err := os.Stat(state.Device); err != nil {
			return fmt.Errorf("specified device %s not found", state.Device)
		}
		fmt.Printf("Using specified device: %s\n", state.Device)
		return nil
	}

	// Auto-detect - Cloudzy VPS typically uses /dev/vda or /dev/sda
	candidates := []string{"/dev/vda", "/dev/sda", "/dev/nvme0n1"}
	for _, d := range candidates {
		if _, err := os.Stat(d); err == nil {
			state.Device = d
			fmt.Printf("Auto-detected device: %s\n", d)
			return nil
		}
	}

	fmt.Println("Error: No suitable block device found.")
	fmt.Println("Available block devices:")
	lib.RunCommand("lsblk", "-d", "-n", "-o", "NAME,SIZE,TYPE")
	return fmt.Errorf("no suitable block device found")
}

func (s *Step01Partition) isDeviceMounted(device string) bool {
	cmd := exec.Command("mount")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(output), device)
}


func (s *Step01Partition) hasExistingData(device string) bool {
	// Check if device has any partitions
	cmd := exec.Command("lsblk", "-n", "-o", "NAME", device)
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	// If we have more than 1 line (device + partitions), there's existing data
	return len(lines) > 1
}
