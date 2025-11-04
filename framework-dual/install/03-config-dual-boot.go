package install

import (
	"fmt"
	"os"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step03ConfigDualBoot generates the Guix system configuration
type Step03ConfigDualBoot struct{}

func (s *Step03ConfigDualBoot) RunWarnings(state *State) error {
  // Auto-detect missing variables if previous steps were skipped
  if state.Device == "" {
    device, err := lib.DetectDeviceFromState(state.Device, "framework-dual")
    if err != nil {
      return err
    }
    state.Device = device
  }

  if state.EFI == "" {
    efiPart, err := lib.FindEFIPartition(state.Device)
    if err != nil {
      return err
    }
    state.EFI = efiPart
  }

  if state.Root == "" {
    rootPart, err := lib.FindGuixRootPartition(state.Device)
    if err != nil {
      return err
    }
    state.Root = rootPart
  }

  // Verify required variables
  if state.Root == "" || state.EFI == "" || state.Device == "" {
    return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE). Please run previous steps or set environment variables")
  }

  fmt.Println("=== Step 3: Configuration Generation ===")
  fmt.Println()
  fmt.Println("This step will:")
  fmt.Println("  1. Setup nonguix channel (prompts for user consent)")
  fmt.Println("  2. Generate minimal Guix system configuration")
  fmt.Println("  3. Write configuration to /mnt/etc/config.scm")
  fmt.Println("  4. Configuration uses partition labels (GUIX_ROOT, EFI, DATA)")
  fmt.Println()
  fmt.Println("Environment variables used by this step:")
  fmt.Printf("  ROOT          - %s (from Step01)\n", state.Root)
  fmt.Printf("  EFI           - %s (from Step01)\n", state.EFI)
  if state.HomePartition != "" {
    fmt.Printf("  HOME_PARTITION - %s (from Step01)\n", state.HomePartition)
  }
  fmt.Println()
  fmt.Println("Optional environment variables (with defaults):")
  fmt.Printf("  USER_NAME     - %s (default: guix)\n", lib.GetEnvOrDefault(state.UserName, "guix"))
  fmt.Printf("  FULL_NAME     - %s (default: Guix User)\n", lib.GetEnvOrDefault(state.FullName, "Guix User"))
  fmt.Printf("  TIMEZONE      - %s (default: America/New_York)\n", lib.GetEnvOrDefault(state.Timezone, "America/New_York"))
  fmt.Printf("  HOST_NAME     - %s (default: guix-system)\n", lib.GetEnvOrDefault(state.HostName, "guix-system"))
  fmt.Println()
  fmt.Println("The generated config is minimal - customize after installation.")
  fmt.Println("Idempotency: Skips generation if /mnt/etc/config.scm already exists")
  fmt.Println()

  return nil
}

func (s *Step03ConfigDualBoot) RunClean(state *State) error {
  // Verify required variables
  if state.Root == "" || state.EFI == "" || state.Device == "" {
    return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
  }

  // Check if config already exists (idempotency)
  configPath := "/mnt/etc/config.scm"
  channelsPath := "/tmp/channels.scm"
  configExists := false
  if _, err := os.Stat(configPath); err == nil {
    configExists = true
    fmt.Printf("Configuration file %s already exists\n", configPath)

    // Check if channels.scm also exists
    if _, err := os.Stat(channelsPath); err == nil {
      fmt.Println("Channels file also exists - skipping config generation")
      fmt.Println("(idempotent - safe for reruns)")
      fmt.Println()
      fmt.Println("To regenerate config and channels, remove both files:")
      fmt.Printf("  rm %s %s\n", configPath, channelsPath)
      return nil
    }

    // Config exists but channels.scm doesn't - need to setup nonguix
    fmt.Println("But channels.scm is missing - will setup nonguix channel")
    fmt.Println()
  }


  // Framework 13 uses UEFI - force it for dual-boot
  if state.BootMode == "" {
    state.BootMode = "uefi"
  }

  bootloader := ""
  targets := ""
  if state.BootMode == "uefi" {
    bootloader = "grub-efi-bootloader"
    targets = `'("/boot/efi")`
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

  // Setup nonguix channel for proprietary firmware and kernel
  if err := lib.SetupNonguixChannel(); err != nil {
    return fmt.Errorf("failed to setup nonguix channel: %w", err)
  }

  // Generate and write config if it doesn't exist
  if !configExists {
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
    fmt.Println("  This will install GRUB to the existing EFI alongside Pop!_OS")
  } else {
    fmt.Println()
    fmt.Println("[OK] Config file already exists, channels.scm has been created")
    fmt.Println("     Ready to proceed with system init")
  }

  return nil
}

func (s *Step03ConfigDualBoot) generateMinimalConfig(state *State, bootloader, targets string) string {
  dataFS := ""
  if state.HomePartition != "" {
    dataFS = `         (file-system
          (mount-point "/data")
          (device (file-system-label "DATA"))
          (type "ext4")
          (options "defaults,noatime"))
`
  }

  config := fmt.Sprintf(`;; Framework 13 AMD Dual-Boot - Hardware-Aware Minimal Configuration
;; Includes kernel, firmware, and initrd modules for Framework 13 AMD hardware
;; Configured for dual-boot with Pop!_OS (shared EFI partition)
;; Customize after installation using: guix-customize

(use-modules (gnu)
             (gnu packages linux)
             (gnu system nss)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(operating-system
 (host-name "%s")
 (timezone "%s")
 (locale "en_US.utf8")

 ;; Linux kernel with proprietary firmware support (from nonguix)
 (kernel linux)
 (firmware (list linux-firmware))

 ;; Framework 13 AMD specific initrd modules
 (initrd-modules
  (append '("amdgpu"      ; AMD GPU driver (critical for display)
            "nvme"        ; NVMe SSD driver
            "xhci_pci"    ; USB 3.0 host controller
            "usbhid"      ; USB keyboard/mouse
            "i2c_piix4")  ; SMBus/I2C for sensors
          %%base-initrd-modules))

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
%s         %%base-file-systems))

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
    dataFS,            // data filesystem conditional
    state.UserName,    // name
    state.FullName,    // comment
    state.UserName,    // for home-directory
  )

  return config
}


