package install

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step03Config generates the Guix system configuration
type Step03Config struct{}

func (s *Step03Config) RunWarnings(state *State) error {
	// Auto-detect missing variables if previous steps were skipped
	if state.Device == "" {
		device, err := lib.DetectDevice("framework")
		if err != nil {
			return err
		}
		state.Device = device
	}

	if state.EFI == "" || state.Root == "" {
		efi, root, err := lib.DetectPartitions(state.Device)
		if err != nil {
			return err
		}
		state.EFI = efi
		state.Root = root
		fmt.Printf("Detected EFI: %s\n", state.EFI)
		fmt.Printf("Detected ROOT: %s\n", state.Root)
	}

	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE). Please run previous steps or set environment variables")
	}

	lib.PrintStepHeader(3, "Configuration Generation")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Setup nonguix channel for proprietary firmware (prompts for consent)")
	fmt.Println("  2. Prompt for keyboard layout selection")
	fmt.Println("  3. Detect boot mode (UEFI or BIOS)")
	fmt.Println("  4. Generate minimal Guix system configuration")
	fmt.Println("  5. Write configuration to /mnt/etc/config.scm")
	fmt.Println("  6. Configuration uses partition labels (GUIX_ROOT, EFI)")
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
		state.BootMode = lib.DetectBootMode()
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

	// Setup nonguix channel for proprietary firmware and kernel
	fmt.Println()
	fmt.Println("=== Configuring Nonguix Channel ===")
	homeDir := os.Getenv("HOME")
	if homeDir == "" {
		homeDir = "/root"
	}
	homeChannelsPath := filepath.Join(homeDir, "channels.scm")
	if _, err := os.Stat(homeChannelsPath); err == nil {
		fmt.Printf("Nonguix channel already configured (using existing %s)\n", homeChannelsPath)
	} else if _, err := os.Stat("/tmp/channels.scm"); err == nil {
		fmt.Println("Nonguix channel already configured (using existing /tmp/channels.scm)")
	} else {
		fmt.Println("Setting up nonguix channel for proprietary firmware...")
	}

	if err := lib.SetupNonguixChannel(state.GuixPlatform); err != nil {
		return fmt.Errorf("failed to setup nonguix channel: %w", err)
	}
	fmt.Println()

	// Prompt for keyboard layout if not already set
	if state.KeyboardLayout == "" {
		layout, err := lib.PromptKeyboardLayout()
		if err != nil {
			return fmt.Errorf("failed to prompt for keyboard layout: %w", err)
		}
		state.KeyboardLayout = layout
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
	// Generate keyboard layout configuration if set
	keyboardLayoutConfig := ""
	if state.KeyboardLayout != "" {
		// Parse the layout string - format is "layout" or "layout:option"
		parts := strings.Split(state.KeyboardLayout, ":")
		layout := parts[0]
		if len(parts) > 1 {
			// Has options (e.g., "us:ctrl:swapcaps")
			options := strings.Join(parts[1:], ":")
			keyboardLayoutConfig = fmt.Sprintf(`
 (keyboard-layout
  (keyboard-layout "%s"
                   #:options '("%s")))
`, layout, options)
		} else {
			// No options, just layout
			keyboardLayoutConfig = fmt.Sprintf(`
 (keyboard-layout (keyboard-layout "%s"))
`, layout)
		}
	}

	config := fmt.Sprintf(`;; Framework 13 AMD - Hardware-Aware Minimal Configuration
;; Includes kernel, firmware, and initrd modules for Framework 13 AMD hardware
;; Customize after installation using: guix-customize

(use-modules (gnu)
             (gnu packages linux)
             (gnu system nss)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1))

(operating-system
 (host-name "%s")
 (timezone "%s")
 (locale "en_US.utf8")
%s
 ;; Linux kernel with proprietary firmware support (from nonguix)
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 ;; Framework 13 AMD specific initrd modules
 ;; Note: "nvme" and "xhci_pci" are built-in to kernel 6.6.16, not loadable modules
 ;; Including them in initrd-modules causes "kernel module not found" errors
 ;; We filter them out from base-initrd-modules as a safeguard
 (initrd-modules
  (append '("amdgpu"      ; AMD GPU driver (critical for display)
            "usbhid"      ; USB keyboard/mouse
            "i2c_piix4")  ; SMBus/I2C for sensors
          (remove (lambda (module) (or (string=? module "nvme")
                                       (string=? module "xhci_pci"))) %%base-initrd-modules)))

 ;; Kernel arguments - minimal for compatibility
 (kernel-arguments '("quiet"))

 (bootloader
  (bootloader-configuration
   (bootloader %s)
   (targets %s)
   (timeout 5)))

 (file-systems
  (cons*          (file-system
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
		state.HostName,           // host-name
		state.Timezone,           // timezone
		keyboardLayoutConfig,     // keyboard-layout (or empty string)
		bootloader,               // bootloader
		targets,                  // targets
		state.UserName,           // name
		state.FullName,           // comment
		state.UserName,           // home-directory
	)

	return config
}

