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
  //              2 keyboard" on IRQ 1, so it would receive no interrupts.
  //              nolapic also drops the machine to a single core.
  //   acpi=off   Already removed earlier; broke xhci_hcd USB init.
  //
  // All four were workarounds for a boot hang whose actual cause was a channel
  // pin older than the hardware. That is fixed in lib/common.go, not by
  // disabling the interrupt controller.
  //
  // Do NOT over-attribute to these arguments. When this laptop was actually
  // observed with a dead console keyboard, its deployed GRUB entry carried only
  // "quiet" -- none of them had ever reached the machine. Repinning forward to
  // linux-7.1.5 fixed the keyboard, WiFi, Bluetooth and amdgpu together
  // (verified on hardware 2026-08-02). Several unrelated-looking hardware
  // failures on one boot means one cause underneath, not several.

  config := fmt.Sprintf(`;; Framework 13 AMD Dual-Boot - Hardware-Aware Minimal Configuration
;; Includes kernel, firmware, and initrd modules for Framework 13 AMD hardware
;; Configured for dual-boot with Pop!_OS (shared EFI partition)
;; Customize after installation using: guix-customize

(use-modules (gnu)
             (gnu packages linux)
             (gnu services dbus)       ;dbus-root-service-type, polkit-service-type
             (gnu services networking) ;network-manager, wpa-supplicant, ntp
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
 ;;
 ;; microcode-initrd, not base-initrd: it prepends the CPU microcode blob so
 ;; the AMD update is applied before the kernel proper starts.  It comes from
 ;; (nongnu system linux-initrd), which this config already imports.  This is
 ;; also what the previously deployed system on this laptop used, so keeping it
 ;; means the repo reproduces the configuration that was verified on hardware.
 (kernel linux)
 (initrd microcode-initrd)
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
   (timeout 5)
   ;; Chainload Pop!_OS's systemd-boot from Guix's GRUB, so switching back does
   ;; not require the firmware boot menu.  Guix and Pop!_OS occupy separate
   ;; directories on the one shared ESP (\EFI\Guix\ and \EFI\systemd\); this
   ;; hands control to the other bootloader without modifying it.
   ;;
   ;; The device is the ESP itself, matched by LABEL rather than device path --
   ;; partition numbering on a dual-boot disk is whatever the other OS's
   ;; installer left behind.  GRUB renders this as a "search --label" followed
   ;; by "chainloader".
   ;;
   ;; If Pop!_OS is absent, this entry is harmless: selecting it fails, and the
   ;; rest of the menu is unaffected.
   (menu-entries
    (list (menu-entry
           (label "Pop!_OS")
           (device (file-system-label "EFI"))
           (chain-loader "/EFI/systemd/systemd-bootx64.efi"))))))
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

 ;; Minimal services PLUS networking.  Add SSH, desktop, etc. after install.
 ;;
 ;; Networking is NOT optional here, even though everything else is minimal.
 ;; This machine's only network interface is the MediaTek MT7925 wireless card
 ;; (14c3:0717, driver mt7925e); Framework 13 has no built-in ethernet, it uses
 ;; expansion cards.  %%base-services provides loopback and nothing else, so a
 ;; system installed without these boots with no way to reach the network --
 ;; and therefore no way to guix pull and repair itself.  That is a very
 ;; expensive state to land in on a laptop whose keyboard may also be suspect.
 ;;
 ;; dbus and polkit are prerequisites, not extras: network-manager-service-type
 ;; declares service-extensions onto dbus-root-service-type and
 ;; polkit-service-type, and Guix aborts the build with "no target of type ..."
 ;; if either is missing.  %%base-services instantiates neither.  Guix's own
 ;; %%desktop-services pairs wpa-supplicant with NetworkManager, annotated in
 ;; gnu/services/desktop.scm as ";needed by NetworkManager".
 ;;
 ;; ntp is here because clock skew breaks TLS, which breaks guix pull -- an
 ;; unpleasant thing to debug on a fresh install with no network.
 ;;
 ;; nmcli/nmtui reach PATH automatically; the service extends
 ;; profile-service-type.  After first boot, run nmtui on the console.
 (services
  (append
   (list (service network-manager-service-type)
         (service wpa-supplicant-service-type)
         (service dbus-root-service-type)
         (service polkit-service-type)
         (service ntp-service-type))
   %%base-services)))
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


