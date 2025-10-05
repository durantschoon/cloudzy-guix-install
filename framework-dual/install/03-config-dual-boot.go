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
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
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
