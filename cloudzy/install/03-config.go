package install

import (
	"fmt"
	"os"
	"strings"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step03Config generates the Guix system configuration
type Step03Config struct{}

func (s *Step03Config) RunWarnings(state *State) error {
	// Auto-detect missing variables if previous steps were skipped
	if state.Device == "" {
		if err := s.detectDevice(state); err != nil {
			return err
		}
	}

	if state.EFI == "" || state.Root == "" {
		if err := s.detectPartitions(state); err != nil {
			return err
		}
	}

	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE). Please run previous steps or set environment variables")
	}

	fmt.Println("=== Step 3: Configuration Generation ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Get UUID of root partition")
	fmt.Println("  2. Detect boot mode (UEFI or BIOS)")
	fmt.Println("  3. Generate minimal Guix system configuration")
	fmt.Println("  4. Write configuration to /mnt/etc/config.scm")
	fmt.Println()
	fmt.Println("Environment variables used by this step:")
	fmt.Printf("  ROOT     - %s (from Step01)\n", state.Root)
	fmt.Printf("  EFI      - %s (from Step01)\n", state.EFI)
	fmt.Println()
	fmt.Println("Optional environment variables (with defaults):")
	fmt.Printf("  USER_NAME - %s (default: guix)\n", lib.GetEnvOrDefault(state.UserName, "guix"))
	fmt.Printf("  FULL_NAME - %s (default: Guix User)\n", lib.GetEnvOrDefault(state.FullName, "Guix User"))
	fmt.Printf("  TIMEZONE  - %s (default: America/New_York)\n", lib.GetEnvOrDefault(state.Timezone, "America/New_York"))
	fmt.Printf("  HOST_NAME - %s (default: guix-system)\n", lib.GetEnvOrDefault(state.HostName, "guix-system"))
	fmt.Printf("  BOOT_MODE - %s (default: auto-detect)\n", lib.GetEnvOrDefault(state.BootMode, "auto-detect"))
	fmt.Println()
	fmt.Println("The generated config is minimal - customize after installation.")
	fmt.Println("Idempotency: Skips generation if /mnt/etc/config.scm already exists")
	fmt.Println()

	return nil
}

func (s *Step03Config) RunClean(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
	}

	// Check if config already exists (idempotency)
	configPath := "/mnt/etc/config.scm"
	if _, err := os.Stat(configPath); err == nil {
		fmt.Printf("Configuration file %s already exists\n", configPath)
		fmt.Println("Skipping config generation (idempotent - safe for reruns)")
		fmt.Println()
		fmt.Println("To regenerate config, remove the file first:")
		fmt.Printf("  rm %s\n", configPath)
		fmt.Println()
		fmt.Println("If you need to pause the installer to do this:")
		fmt.Println("  1. Press Ctrl+Z to suspend this process")
		fmt.Println("  2. Run: rm /mnt/etc/config.scm")
		fmt.Println("  3. Run: fg to resume the installer")
		return nil
	}


	// Detect boot mode if not set
	if state.BootMode == "" {
		state.BootMode = s.detectBootMode()
	}

	bootloader := ""
	targets := ""
	if state.BootMode == "uefi" {
		bootloader = "grub-efi-bootloader"
		targets = `'("/boot/efi")`
		fmt.Println("UEFI boot mode - using grub-efi-bootloader")
	} else {
		bootloader = "grub-bootloader"
		targets = fmt.Sprintf(`'("%s")`, state.Device)
		fmt.Printf("BIOS boot mode - using grub-bootloader on %s\n", state.Device)
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
	config := s.generateMinimalConfig(state, bootloader, targets)

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

	return nil
}

func (s *Step03Config) generateMinimalConfig(state *State, bootloader, targets string) string {
	config := fmt.Sprintf(`;; Minimal Guix System Configuration (Free Software Only)
;; This is the bare minimum to get a bootable system using only free software.
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
   (targets %s)
   (timeout 5)))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (file-system-label "GUIX_ROOT"))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device (file-system-label "EFI"))
          (type "vfat"))
         %%base-file-systems))

 (users (cons* (user-account
                (name "%s")
                (comment "%s")
                (group "users")
                (home-directory "/home/%s")
                (supplementary-groups '("wheel" "netdev")))
               %%base-user-accounts))

 ;; Minimal packages - add more after installation
 (packages %%base-packages)

 ;; Minimal services - add SSH, desktop, etc. after installation
 (services %%base-services))
`,
		state.HostName,    // host-name
		state.Timezone,    // timezone
		bootloader,        // bootloader
		targets,           // targets
		state.UserName,    // name
		state.FullName,    // comment
		state.UserName,    // home-directory
	)

	return config
}

func (s *Step03Config) detectBootMode() string {
	// Check if /sys/firmware/efi exists
	if _, err := os.Stat("/sys/firmware/efi"); err == nil {
		return "uefi"
	}
	return "bios"
}

// Helper functions

func (s *Step03Config) detectDevice(state *State) error {
	candidates := []string{"/dev/vda", "/dev/sda", "/dev/nvme0n1"}
	for _, d := range candidates {
		if _, err := os.Stat(d); err == nil {
			state.Device = d
			fmt.Printf("Auto-detected device: %s\n", d)
			return nil
		}
	}
	return fmt.Errorf("no suitable block device found")
}

func (s *Step03Config) detectPartitions(state *State) error {
	if state.Device == "" {
		return fmt.Errorf("DEVICE not set")
	}

	state.EFI = s.makePartitionPath(state.Device, "1")
	state.Root = s.makePartitionPath(state.Device, "2")

	fmt.Printf("Detected EFI: %s\n", state.EFI)
	fmt.Printf("Detected ROOT: %s\n", state.Root)

	return nil
}

func (s *Step03Config) makePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}

