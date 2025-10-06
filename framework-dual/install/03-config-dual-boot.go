package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// Step03ConfigDualBoot generates the Guix system configuration
type Step03ConfigDualBoot struct{}

func (s *Step03ConfigDualBoot) RunWarnings(state *State) error {
	// Auto-detect missing variables if previous steps were skipped
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
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE). Please run previous steps or set environment variables")
	}

	fmt.Println("=== Configuration Generation ===")
	fmt.Println("This script will generate a minimal Guix system configuration")
	fmt.Println("The configuration will be written to /mnt/etc/config.scm")
	fmt.Println()

	return nil
}

func (s *Step03ConfigDualBoot) RunClean(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
	}

	// Get UUID of root partition
	uuid, err := getRootUUID(state.Root)
	if err != nil {
		return fmt.Errorf("failed to get root UUID: %w", err)
	}
	fmt.Printf("UUID: %s\n", uuid)

	// Framework 13 uses UEFI - force it for dual-boot
	if state.BootMode == "" {
		state.BootMode = "uefi"
	}

	bootloader := ""
	targets := ""
	if state.BootMode == "uefi" {
		bootloader = "grub-efi-bootloader"
		targets = `("/boot/efi")`
		fmt.Println("UEFI boot mode - using grub-efi-bootloader")
	} else {
		return fmt.Errorf("dual-boot configuration requires UEFI mode")
	}

	fmt.Println()
	fmt.Println("=== Generating Minimal Config ===")
	fmt.Println("This creates a bare-bones bootable system with:")
	fmt.Println("  - Base system packages only")
	fmt.Println("  - No desktop environment")
	fmt.Println("  - No SSH (add after installation)")
	fmt.Println()
	fmt.Println("To customize after installation, use the guix-customize script")
	fmt.Println()

	// Set defaults for user info
	if state.UserName == "" {
		state.UserName = "guix"
	}
	if state.FullName == "" {
		state.FullName = "Guix User"
	}
	if state.Timezone == "" {
		state.Timezone = "America/New_York"
	}
	if state.HostName == "" {
		state.HostName = "guix-system"
	}

	// Generate config
	config := s.generateMinimalConfig(state, uuid, bootloader, targets)

	// Write to file
	if err := os.MkdirAll("/mnt/etc", 0755); err != nil {
		return err
	}

	if err := os.WriteFile("/mnt/etc/config.scm", []byte(config), 0644); err != nil {
		return fmt.Errorf("failed to write config: %w", err)
	}

	fmt.Println()
	fmt.Println("=== Generated config.scm ===")
	fmt.Println(config)
	fmt.Println()
	fmt.Println("Configuration written to /mnt/etc/config.scm")
	fmt.Println("  This will install GRUB to the existing ESP alongside Pop!_OS")

	return nil
}

func (s *Step03ConfigDualBoot) generateMinimalConfig(state *State, uuid, bootloader, targets string) string {
	homeFS := ""
	if state.HomePartition != "" {
		homeUUID, err := getUUID(state.HomePartition)
		if err == nil && homeUUID != "" {
			homeFS = fmt.Sprintf(`         (file-system
          (mount-point "/home")
          (device (uuid "%s" 'ext4))
          (type "ext4"))
`, homeUUID)
		}
	}

	config := fmt.Sprintf(`;; Minimal Guix System Configuration
;; This is the bare minimum to get a bootable system.
;; Customize after installation using: guix-customize

(use-modules (gnu)
             (gnu system nss))

(operating-system
 (host-name "%s")
 (timezone "%s")
 (locale "en_US.utf8")

 (bootloader
  (bootloader-configuration
   (bootloader %s)
   (targets '%s)
   (keyboard-layout (keyboard-layout "us"))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (uuid "%s" 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device "%s")
          (type "vfat"))
%s         %%base-file-systems))

 (users (cons* (user-account
                (name "%s")
                (comment "%s")
                (group "users")
                (home-directory "/home/%s")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %%base-user-accounts))

 ;; Minimal packages - add more after installation
 (packages %%base-packages)

 ;; Minimal services - add SSH, desktop, etc. after installation
 (services %%base-services))
`,
		state.HostName,
		state.Timezone,
		bootloader,
		targets,
		uuid,
		state.EFI,
		homeFS,
		state.UserName,
		state.FullName,
		state.UserName,
	)

	return config
}

func getRootUUID(device string) (string, error) {
	cmd := exec.Command("blkid", "-s", "UUID", "-o", "value", device)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

func getUUID(device string) (string, error) {
	cmd := exec.Command("blkid", "-s", "UUID", "-o", "value", device)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

func (s *Step03ConfigDualBoot) detectDevice(state *State) error {
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

func (s *Step03ConfigDualBoot) findEFIPartition(state *State) error {
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

func (s *Step03ConfigDualBoot) findGuixRootPartition(state *State) error {
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

func (s *Step03ConfigDualBoot) makePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}
