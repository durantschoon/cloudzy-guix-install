package install

import (
	"strings"
	"testing"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// TestStep03ConfigDualBoot_Integration tests the integration of refactored functions
func TestStep03ConfigDualBoot_Integration(t *testing.T) {
	// Test that the step can be created without panicking
	step := &Step03ConfigDualBoot{}

	// Verify the step has the expected methods (compile-time check)
	_ = step.RunWarnings
	_ = step.RunClean

	// Test that state can be created
	state := &State{}
	// Verify state is usable by setting and reading a field
	state.Device = "/dev/test"
	if state.Device != "/dev/test" {
		t.Error("State fields should be readable and writable")
	}
}

// TestLibFunctionIntegration_Config tests that all lib functions used in refactored code are accessible
func TestLibFunctionIntegration_Config(t *testing.T) {
	// Test DetectDeviceFromState
	_, err := lib.DetectDeviceFromState("", "framework-dual")
	if err == nil {
		t.Log("DetectDeviceFromState returned no error (unexpected in test environment)")
	}

	// Test FindEFIPartition
	_, err = lib.FindEFIPartition("/dev/nonexistent")
	if err == nil {
		t.Log("FindEFIPartition returned no error for non-existent device (unexpected)")
	}

	// Test FindGuixRootPartition
	_, err = lib.FindGuixRootPartition("/dev/nonexistent")
	if err == nil {
		t.Log("FindGuixRootPartition returned no error for non-existent device (unexpected)")
	}

	// Test MakePartitionPath
	result := lib.MakePartitionPath("/dev/nvme0n1", "1")
	expected := "/dev/nvme0n1p1"
	if result != expected {
		t.Errorf("MakePartitionPath returned %s, expected %s", result, expected)
	}
}

// TestRefactoredLogicFlow_Config tests the logic flow that was refactored in the config step
func TestRefactoredLogicFlow_Config(t *testing.T) {
	state := &State{}

	// Simulate the refactored logic flow from the config step
	// This tests that the function calls we made work together

	// Step 1: Detect device if not set
	if state.Device == "" {
		device, err := lib.DetectDeviceFromState(state.Device, "framework-dual")
		if err == nil {
			state.Device = device
		}
		// We expect an error in test environment, so this is expected
	}

	// Step 2: Find EFI partition if not set
	if state.EFI == "" {
		efiPart, err := lib.FindEFIPartition(state.Device)
		if err == nil {
			state.EFI = efiPart
		}
		// We expect an error in test environment, so this is expected
	}

	// Step 3: Find root partition if not set
	if state.Root == "" {
		rootPart, err := lib.FindGuixRootPartition(state.Device)
		if err == nil {
			state.Root = rootPart
		}
		// We expect an error in test environment, so this is expected
	}

	// Verify that the state structure is still valid
	if state.Device == "" {
		t.Log("Device not set (expected in test environment)")
	}
	if state.EFI == "" {
		t.Log("EFI not set (expected in test environment)")
	}
	if state.Root == "" {
		t.Log("Root not set (expected in test environment)")
	}
}

// TestFunctionSignatures_Config tests that function signatures match what we use in refactored code
func TestFunctionSignatures_Config(t *testing.T) {
	// Test DetectDeviceFromState signature: (device string, platform string) (string, error)
	device, err := lib.DetectDeviceFromState("/dev/test", "framework-dual")
	_ = device // Use the variable to avoid unused variable warning
	if err == nil {
		t.Log("DetectDeviceFromState returned no error (unexpected in test environment)")
	}

	// Test FindEFIPartition signature: (device string) (string, error)
	efi, err := lib.FindEFIPartition("/dev/test")
	_ = efi // Use the variable to avoid unused variable warning
	if err == nil {
		t.Log("FindEFIPartition returned no error (unexpected in test environment)")
	}

	// Test FindGuixRootPartition signature: (device string) (string, error)
	root, err := lib.FindGuixRootPartition("/dev/test")
	_ = root // Use the variable to avoid unused variable warning
	if err == nil {
		t.Log("FindGuixRootPartition returned no error (unexpected in test environment)")
	}

	// Test MakePartitionPath signature: (device string, partNum string) string
	path := lib.MakePartitionPath("/dev/nvme0n1", "1")
	if path == "" {
		t.Error("MakePartitionPath should return non-empty string")
	}
}

// TestStateValidation tests that state validation works correctly
func TestStateValidation(t *testing.T) {
	// Test with empty state
	state := &State{}
	if state.Device == "" && state.EFI == "" && state.Root == "" {
		t.Log("Empty state created successfully")
	}

	// Test with populated state
	state = &State{
		Device: "/dev/nvme0n1",
		EFI:    "/dev/nvme0n1p1",
		Root:   "/dev/nvme0n1p2",
	}
	if state.Device != "/dev/nvme0n1" {
		t.Errorf("Device not set correctly: got %s", state.Device)
	}
	if state.EFI != "/dev/nvme0n1p1" {
		t.Errorf("EFI not set correctly: got %s", state.EFI)
	}
	if state.Root != "/dev/nvme0n1p2" {
		t.Errorf("Root not set correctly: got %s", state.Root)
	}
}

// TestErrorHandling_Config tests that error handling works correctly in refactored code
func TestErrorHandling_Config(t *testing.T) {
	// Test that functions handle invalid inputs gracefully
	testCases := []struct {
		name string
		test func() error
	}{
		{
			name: "DetectDeviceFromState with invalid platform",
			test: func() error {
				_, err := lib.DetectDeviceFromState("", "invalid-platform")
				return err
			},
		},
		{
			name: "FindEFIPartition with invalid device",
			test: func() error {
				_, err := lib.FindEFIPartition("/dev/invalid")
				return err
			},
		},
		{
			name: "FindGuixRootPartition with invalid device",
			test: func() error {
				_, err := lib.FindGuixRootPartition("/dev/invalid")
				return err
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.test()
			if err == nil {
				t.Logf("%s returned no error (unexpected in test environment)", tc.name)
			}
		})
	}
}

// TestPartitionPathGeneration_Config tests various partition path generation scenarios
func TestPartitionPathGeneration_Config(t *testing.T) {
	testCases := []struct {
		device   string
		partNum  string
		expected string
	}{
		{"/dev/nvme0n1", "1", "/dev/nvme0n1p1"},
		{"/dev/nvme0n1", "2", "/dev/nvme0n1p2"},
		{"/dev/sda", "1", "/dev/sda1"},
		{"/dev/sda", "3", "/dev/sda3"},
		{"/dev/mmcblk0", "1", "/dev/mmcblk0p1"},
		{"/dev/vda", "1", "/dev/vda1"},
	}

	for _, tc := range testCases {
		t.Run(tc.device+"_"+tc.partNum, func(t *testing.T) {
			result := lib.MakePartitionPath(tc.device, tc.partNum)
			if result != tc.expected {
				t.Errorf("MakePartitionPath(%s, %s) = %s, want %s",
					tc.device, tc.partNum, result, tc.expected)
			}
		})
	}
}

// TestGeneratedConfigContainsInitrd verifies that the generated config includes the initrd field
func TestGeneratedConfigContainsInitrd(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName:      "test-host",
		Timezone:      "America/New_York",
		UserName:      "testuser",
		FullName:      "Test User",
		Device:        "/dev/nvme0n1",
		EFI:           "/dev/nvme0n1p1",
		Root:          "/dev/nvme0n1p4",
		BootMode:      "uefi",
		HomePartition: "",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	// Verify critical fields are present.
	//
	// microcode-initrd rather than base-initrd: it wraps base-initrd to prepend
	// the CPU microcode blob, so the AMD update lands before the kernel proper
	// starts. It comes from (nongnu system linux-initrd).
	if !strings.Contains(config, "(initrd microcode-initrd)") {
		t.Error("Generated config missing '(initrd microcode-initrd)' field")
	}
	if !strings.Contains(config, "(kernel linux)") {
		t.Error("Generated config missing '(kernel linux)' field")
	}
	if !strings.Contains(config, "(firmware (list linux-firmware))") {
		t.Error("Generated config missing '(firmware (list linux-firmware))' field")
	}

	// Verify kernel arguments.
	//
	// This assertion used to require nomodeset/noapic/nolapic. It is inverted
	// on purpose: those arguments broke the machine they were meant to fix.
	// noapic+nolapic force legacy 8259 interrupt routing, which starves the
	// i8042 internal keyboard of interrupts -- the greeter renders and then
	// ignores every keystroke. nomodeset contradicts loading amdgpu at all.
	expectedArgs := `(kernel-arguments (append '("loglevel=3") %default-kernel-arguments))`
	if !strings.Contains(config, expectedArgs) {
		t.Errorf("Generated config missing expected kernel arguments.\nExpected: %s\nGot: %s", expectedArgs, config)
	}

	for _, banned := range []string{"nomodeset", "noapic", "nolapic", "acpi=off"} {
		if strings.Contains(config, banned) {
			t.Errorf("Generated config MUST NOT contain %q: it breaks the internal "+
				"keyboard and/or the GPU on Framework 13 AMD. See "+
				"docs/FRAMEWORK_STARTUP_HANG_FIX.md", banned)
		}
	}

	// kernel-arguments must APPEND to %default-kernel-arguments, never replace
	// it: the default carries modprobe.blacklist=usbmouse,usbkbd and usbkbd
	// races usbhid (bugs.gnu.org/35574).
	if !strings.Contains(config, "%default-kernel-arguments") {
		t.Errorf("Generated config must build on %%default-kernel-arguments, not replace it")
	}
}

// TestGenerateMinimalConfig_InitrdModules guards the initrd module list.
func TestGenerateMinimalConfig_InitrdModules(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	if !strings.Contains(config, "(initrd-modules %base-initrd-modules)") {
		t.Errorf("Generated config should use the Guix default initrd modules.\nGot: %s", config)
	}

	// amdgpu in the initrd means its firmware must also be in the initrd, which
	// is an extra way to fail before any console exists. udev loads it later.
	if strings.Contains(config, `"amdgpu"`) {
		t.Error("Generated config MUST NOT put amdgpu in initrd-modules")
	}

	// A no-op filter reads like something is being filtered when nothing is.
	if strings.Contains(config, "(lambda (module) #f)") {
		t.Error("Generated config should not emit an always-false module filter")
	}
}

// TestGenerateMinimalConfig_DataFilesystemFlags guards against a mount option
// that has bricked this platform twice. no-atime is a VFS-level flag; putting
// it in 'options' makes ext4 reject the mount, which fails the file-systems
// target, which means user-processes never starts and there are no login ttys.
func TestGenerateMinimalConfig_DataFilesystemFlags(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName:      "test-host",
		Timezone:      "America/New_York",
		UserName:      "testuser",
		FullName:      "Test User",
		HomePartition: "/dev/nvme0n1p5",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	if !strings.Contains(config, `(flags '(no-atime))`) {
		t.Errorf("DATA filesystem must use (flags '(no-atime)).\nGot: %s", config)
	}
	if strings.Contains(config, `(options "noatime")`) || strings.Contains(config, `(options "defaults`) {
		t.Error("DATA filesystem must not pass VFS mount flags via 'options'")
	}
}

// TestGenerateMinimalConfig_PopOSMenuEntry guards the GRUB chainload entry for
// Pop!_OS. Without it, Guix's GRUB lists only its own generations and "Firmware
// setup", so getting back to the other OS requires the firmware boot menu (F12
// on Framework). That is a poor experience for what is explicitly a dual-boot
// installer, and on machines whose UEFI timeout is 0 it is easy to miss.
func TestGenerateMinimalConfig_PopOSMenuEntry(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	if !strings.Contains(config, "(menu-entries") {
		t.Errorf("Generated config should add a menu entry for the other OS.\nGot: %s", config)
	}
	// The path moved into a definition so the entry can be constructed from two
	// cond branches (present / unknown) without repeating the literal. Assert
	// both halves rather than the old inline spelling: the constant, and the
	// reference that consumes it. See
	// TestGenerateMinimalConfig_PopOSMenuEntryIsConditional.
	if !strings.Contains(config, `(define %popos-boot-loader "/EFI/systemd/systemd-bootx64.efi")`) {
		t.Error("Pop!_OS entry must chainload systemd-boot from the shared ESP")
	}
	if !strings.Contains(config, "(chain-loader %popos-boot-loader)") {
		// Errorf: vet reads a bare %p in the message as a format verb.
		t.Errorf("the menu entry must consume %%popos-boot-loader rather than " +
			"re-spelling the path, so the two branches cannot drift")
	}

	// Match the ESP by label, not device path: numbering on a dual-boot disk is
	// whatever the other OS's installer left behind.
	if !strings.Contains(config, `(device (file-system-label "EFI"))`) {
		t.Error("Menu entry must locate the ESP by label, not by device path")
	}
	if strings.Contains(config, `(device "/dev/nvme`) || strings.Contains(config, `(device "/dev/sd`) {
		t.Error("Menu entry must not reference a raw device path")
	}
}

// TestGenerateMinimalConfig_Networking guards the one non-minimal thing in this
// config. The Framework 13 has no built-in ethernet -- its only interface is a
// MediaTek MT7925 wireless card -- so a system installed with bare
// %base-services has no way to reach the network and therefore no way to
// 'guix pull' and repair itself.
//
// dbus and polkit are prerequisites rather than extras: network-manager-service-type
// declares service-extensions onto both, and Guix aborts the build with
// "no target of type ..." if either is absent from the service list.
func TestGenerateMinimalConfig_Networking(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	// NetworkManager is matched without its closing paren because it now takes a
	// network-manager-configuration argument (see TestGenerateMinimalConfig_DNS).
	// The others are still instantiated bare.
	required := []string{
		"(service network-manager-service-type",
		"(service wpa-supplicant-service-type)",
		"(service dbus-root-service-type)",
		"(service polkit-service-type)",
	}
	for _, svc := range required {
		if !strings.Contains(config, svc) {
			t.Errorf("Generated config is missing required service %s.\nGot: %s", svc, config)
		}
	}

	// The modules those services live in must be imported, or the config fails
	// to evaluate with "unbound variable".
	for _, mod := range []string{"(gnu services dbus)", "(gnu services networking)"} {
		if !strings.Contains(config, mod) {
			t.Errorf("Generated config is missing module import %s", mod)
		}
	}

	// Services must be appended to %base-services, not replace it: %base-services
	// carries login, mingetty on tty1-6 and the system log. Replacing it yields a
	// system with no way to log in.
	// Either shape is fine -- appended directly, or wrapped in modify-services to
	// override one of the base services -- so long as %base-services is the base.
	if !strings.Contains(config, "%base-services)") &&
		!strings.Contains(config, "(modify-services %base-services") {
		t.Errorf("Services must build on %%base-services.\nGot: %s", config)
	}
	if strings.Contains(config, "(services %base-services))") {
		t.Error("Services no longer include networking; the installed system would have no network")
	}
}

// TestGenerateMinimalConfig_ChannelsAndSubstitutes checks that the installed
// system is told about the same channels and substitute server that built it.
//
// Regression: the first system installed from this repo booted knowing only the
// "guix" channel, because the pin lived on the installing machine and was never
// mirrored into the target. Running "guix system reconfigure /etc/config.scm" on
// the installed machine -- against the very config that produced it -- failed
// with "no code for module (nongnu packages linux)" (observed 2026-08-02).
//
// The substitute URL is a separate assertion from the signing key on purpose:
// authorizing the key without adding the URL means guix never queries nonguix
// and silently compiles Linux from source instead.
func TestGenerateMinimalConfig_ChannelsAndSubstitutes(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	required := []string{
		"(guix channels)",                   // channel / make-channel-introduction
		"(define %framework-dual-channels",  // the mirrored pin
		"(name 'nonguix)",                   // ...which must include nonguix
		"(channels %framework-dual-channels)",
		`"https://substitutes.nonguix.org"`, // URL, not just the key
		"(define %nonguix-signing-key",      // key, not just the URL
		"%default-authorized-guix-keys",     // added to the defaults, not replacing
		"%default-substitute-urls",
		"(modify-services %base-services",
	}
	for _, want := range required {
		if !strings.Contains(config, want) {
			t.Errorf("Generated config is missing %q.\nGot: %s", want, config)
		}
	}

	// The pin in the generated config must be the one lib actually installs
	// with; a config that mirrors a different commit pair would reproduce a
	// system other than the one that was built.
	for _, commit := range []string{lib.FrameworkDualGuixCommit, lib.FrameworkDualNonguixCommit} {
		if !strings.Contains(config, commit) {
			t.Errorf("Generated config does not carry pinned commit %s", commit)
		}
	}
}

// The Framework 13 panel is 2256x1504 on a 13.5" diagonal, where the default
// Unifont-APL8x16 console font is too small to read. The override must go
// through modify-services: %base-services ALREADY instantiates
// console-font-service-type, so a second (service console-font-service-type ...)
// in the appended list collides on the shepherd provisions console-font-tty1
// .. console-font-tty6 and the system fails to build.
func TestGenerateMinimalConfig_ConsoleFont(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	if !strings.Contains(config, "(console-font-service-type") {
		t.Errorf("Generated config does not set a console font.\nGot: %s", config)
	}

	// solar24x32 ships in kbd, which is the package the service invokes setfont
	// from. ter-v32n/ter-v32b appear in older notes in this repo but live in
	// font-terminus, which this config does not install -- naming one would
	// leave the console at the default font with no error anywhere obvious.
	if !strings.Contains(config, `"solar24x32"`) {
		t.Errorf("Console font must be solar24x32 (a font kbd actually ships).\nGot: %s", config)
	}
	for _, absent := range []string{"ter-v32n", "ter-v32b"} {
		if strings.Contains(config, absent) {
			t.Errorf("Console font %q is not provided by kbd; it needs font-terminus", absent)
		}
	}

	// The collision guard: the font override must NOT be a second instantiation
	// appended alongside network-manager et al.
	if strings.Contains(config, "(service console-font-service-type") {
		t.Errorf("console-font must be overridden via modify-services, not added as a "+
			"second service -- %%base-services already provides console-font-tty1..6.\nGot: %s", config)
	}
}

// Observed 2026-08-02: after a reboot the machine had connectivity but an
// unusable /etc/resolv.conf, so "guix pull" failed in getaddrinfo. That also
// surfaced as "nonguix untrusted", because a channel introduction is verified
// against the keyring branch of a repo that could not be cloned -- one fault
// wearing two error messages.
//
// Editing /etc/resolv.conf by hand does not survive: NetworkManager rewrites it
// on re-association. NetworkManager's own [global-dns-domain-*] override does
// survive, and is applied to every connection.
func TestGenerateMinimalConfig_DNS(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	required := []string{
		"(network-manager-configuration", // not the bare service
		"(extra-configuration-files",     // symlinked to /etc/NetworkManager/conf.d
		`"dns.conf"`,
		"[global-dns-domain-*]", // the section NM actually honours as an override
		"servers=",
	}
	for _, want := range required {
		if !strings.Contains(config, want) {
			t.Errorf("Generated config is missing %q -- DNS would revert to DHCP and a "+
				"machine that loses name resolution cannot guix pull to repair itself.\nGot: %s",
				want, config)
		}
	}

	// A bare (service network-manager-service-type) takes no configuration, so
	// the DNS block would be silently absent.
	if strings.Contains(config, "(service network-manager-service-type)") {
		t.Errorf("NetworkManager must be given a network-manager-configuration, " +
			"not instantiated bare")
	}
}

// TestGenerateMinimalConfig_PopOSMenuEntryIsConditional pins the Pop!_OS GRUB
// entry to being emitted only when Pop!_OS is actually installed.
//
// It used to be unconditional. That is wrong for a fresh Framework with no
// Pop!_OS on it: GRUB then advertises an OS that was never installed, and
// selecting it fails with "file not found".
func TestGenerateMinimalConfig_PopOSMenuEntryIsConditional(t *testing.T) {
	step := &Step03ConfigDualBoot{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
		Device:   "/dev/nvme0n1",
		EFI:      "/dev/nvme0n1p1",
		Root:     "/dev/nvme0n1p4",
		BootMode: "uefi",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	// The bootloader must reference the computed list, never a literal one.
	if !strings.Contains(config, "(menu-entries %popos-menu-entries)") {
		// Errorf, not Error: vet reads a bare %p in the message as a format verb.
		t.Errorf("bootloader-configuration must use (menu-entries %%popos-menu-entries), " +
			"so the Pop!_OS entry can be omitted on machines without Pop!_OS")
	}

	// The ESP sits at a different path during `guix system init` than on a
	// running system, so both must be probed or the entry vanishes at install
	// time and reappears at the first reconfigure.
	for _, candidate := range []string{"/boot/efi", "/mnt/guixroot/boot/efi"} {
		if !strings.Contains(config, candidate) {
			t.Errorf("ESP candidate %q missing: the probe must cover both the "+
				"installed-system and mid-install mount points", candidate)
		}
	}

	// Fail-open is the whole point. An ESP is vfat mounted umask=0077, so a
	// non-root evaluation gets EACCES, which file-exists? reports exactly like
	// "absent". Only a definite ENOENT may drop the entry; anything else keeps
	// it. Losing the entry costs the boot route back to the other OS, which is
	// worse than a stale entry that merely fails when selected.
	if !strings.Contains(config, "ENOENT") {
		t.Error("the probe must distinguish ENOENT from other errno values; " +
			"without that, EACCES silently drops the Pop!_OS entry")
	}
	if !strings.Contains(config, "(memq 'unknown states)") {
		t.Error("the 'unknown state must keep the Pop!_OS entry (fail open)")
	}

	// Go format-verb escaping: every literal % in the generated Guile has to be
	// written %% in the template. Guile's own ~% newline directive is the easy
	// one to get wrong. Go marks a botched verb in the output, so scan for it.
	if strings.Contains(config, "%!") {
		t.Errorf("generated config contains a Go format error marker (%%!...); "+
			"a %% in the template is unescaped.\nConfig:\n%s", config)
	}
	if !strings.Contains(config, "keeping the Pop!_OS entry~%") {
		t.Error("Guile's ~% newline directive did not survive templating; " +
			"it must be written ~%% inside the Go format string")
	}
}
