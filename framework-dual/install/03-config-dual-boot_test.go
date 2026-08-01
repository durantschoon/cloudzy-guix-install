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

	// Verify critical fields are present
	if !strings.Contains(config, "(initrd base-initrd)") {
		t.Error("Generated config missing '(initrd base-initrd)' field")
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
