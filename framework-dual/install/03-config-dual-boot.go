package install

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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

  lib.PrintStepHeader(3, "Configuration Generation")
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
    fmt.Printf("  DATA           - %s (from Step01)\n", state.HomePartition)
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

  // Generate and write config if it doesn't exist
  if !configExists {
    // Generate config
    // Note: Module filtering happens in generateMinimalConfig using known built-in modules
    // If kernel package is available, we could check it, but for now we use a static list
    // based on kernel 6.6.16 configuration
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
    // no-atime goes in 'flags', NOT in 'options'.
    //
    // 'options' is passed verbatim as mount(2)'s data argument, i.e. it may
    // only contain filesystem-specific parameters. A VFS-level token there
    // makes the kernel reject the mount with "ext4: Unknown parameter
    // 'noatime'". On a non-root filesystem that is unrecoverable in place:
    // file-system-/data fails, so the file-systems target fails, so
    // user-processes never starts, and the machine boots with no login ttys.
    // 'flags' takes symbols and is converted to mount flag bits instead.
    dataFS = `         (file-system
          (mount-point "/data")
          (device (file-system-label "DATA"))
          (type "ext4")
          (flags '(no-atime)))
`
  }

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
  
  // Build the initrd-modules expression.
  //
  // Normally this is just %base-initrd-modules. If GetBuiltInModulesToFilter
  // ever becomes non-empty (because a kernel really does build one of these in
  // and Guix would fail to find it), wrap it in a 'remove' instead of emitting
  // a no-op filter -- an always-false predicate in the generated config reads
  // like something is being filtered when nothing is.
  initrdModules := lib.BuildInitrdModulesExpr(lib.GetBuiltInModulesToFilter())

  // Kernel arguments removed from the template below, and why. These are named
  // here rather than in the generated config so that grepping a deployed
  // config.scm for them stays a reliable "this machine is misconfigured" check.
  //
  //   nomodeset  Disables kernel modesetting. Contradicts loading amdgpu and
  //              linux-firmware at all, and leaves an unaccelerated console.
  //   noapic     Disables the I/O APIC.
  //   nolapic    Disables the local APIC. Together these force legacy 8259
  //              interrupt routing, which modern AMD platforms do not reliably
  //              provide. The internal keyboard is an i8042 "AT Translated Set
  //              2 keyboard" on IRQ 1, so it receives no interrupts: the
  //              greeter renders and then ignores every keystroke. nolapic
  //              also drops the machine to a single core.
  //   acpi=off   Already removed earlier; broke xhci_hcd USB init.
  //
  // All four were workarounds for a boot hang whose actual cause was amdgpu
  // failing on firmware older than the GPU. That is fixed by the channel pin
  // in lib/common.go, not by disabling the interrupt controller.

  config := fmt.Sprintf(`;; Framework 13 AMD Dual-Boot - Hardware-Aware Minimal Configuration
;; Includes kernel, firmware, and initrd modules for Framework 13 AMD hardware
;; Configured for dual-boot with Pop!_OS (shared EFI partition)
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
 (initrd base-initrd)
 (firmware (list linux-firmware))

 ;; The initrd only has to get us far enough to mount the root filesystem;
 ;; everything else is udev's job once the real system is up.
 ;;
 ;; %%base-initrd-modules already covers this machine: ahci, usb-storage/uas,
 ;; and -- importantly for a laptop -- usbhid and hid-generic for keyboards
 ;; during early boot.  We previously prepended three modules here; all three
 ;; are removed on purpose:
 ;;   - amdgpu:    not needed to mount root.  Loading the GPU driver from the
 ;;                initrd also means its firmware must be in the initrd, which
 ;;                is a second way to fail before there is any console to see
 ;;                it on.  udev loads it fine later.
 ;;   - usbhid:    already in %%base-initrd-modules; listing it twice is noise.
 ;;   - i2c_piix4: SMBus for sensors.  Nothing to do with booting.
 ;;
 ;; NVMe note: "nvme" is deliberately absent, matching %%base-initrd-modules.
 ;; NVMe root works because the driver is built into the kernel rather than
 ;; loadable.  If a future kernel makes it modular, root will fail to mount and
 ;; "nvme" must be added here -- see docs/NVME_MODULE_FIX.md.
 (initrd-modules %s)

 ;; Kernel arguments.  Append to %%default-kernel-arguments rather than
 ;; replacing it: the default carries modprobe.blacklist=usbmouse,usbkbd, and
 ;; upstream blacklists usbkbd because it races usbhid (bugs.gnu.org/35574).
 ;; On a laptop, losing that blacklist is a way to lose your keyboard.
 ;;
 ;; Several display/interrupt workarounds that used to live here have been
 ;; removed -- do not add them back without reading
 ;; docs/FRAMEWORK_STARTUP_HANG_FIX.md first.
 (kernel-arguments (append '("loglevel=3") %%default-kernel-arguments))

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
    state.HostName,          // host-name
    state.Timezone,          // timezone
    keyboardLayoutConfig,    // keyboard-layout (conditional)
    initrdModules,           // initrd-modules expression
    bootloader,              // bootloader
    targets,                 // targets
    dataFS,                  // data filesystem conditional
    state.UserName,          // name
    state.FullName,          // comment
    state.UserName,          // for home-directory
  )

  return config
}


