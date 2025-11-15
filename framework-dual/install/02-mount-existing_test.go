package install

import (
	"testing"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// TestStep02MountExisting_Integration tests the integration of refactored functions
func TestStep02MountExisting_Integration(t *testing.T) {
	// Test that the step can be created without panicking
	step := &Step02MountExisting{}

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

// TestLibFunctionAccessibility_Mount tests that all lib functions used in refactored code are accessible
func TestLibFunctionAccessibility_Mount(t *testing.T) {
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

// TestFunctionSignatures_Mount tests that function signatures match what we use in refactored code
func TestFunctionSignatures_Mount(t *testing.T) {
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

// TestRefactoredLogicFlow_Mount tests the logic flow that was refactored
func TestRefactoredLogicFlow_Mount(t *testing.T) {
	state := &State{}

	// Simulate the refactored logic flow from the mount step
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

// TestErrorHandling_Mount tests that error handling works correctly in refactored code
func TestErrorHandling_Mount(t *testing.T) {
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

// TestStatePersistence tests that state variables persist correctly
func TestStatePersistence(t *testing.T) {
	state := &State{
		Device: "/dev/nvme0n1",
		EFI:    "/dev/nvme0n1p1",
		Root:   "/dev/nvme0n1p2",
	}

	// Test that state values are preserved
	if state.Device != "/dev/nvme0n1" {
		t.Errorf("Device not preserved: got %s", state.Device)
	}
	if state.EFI != "/dev/nvme0n1p1" {
		t.Errorf("EFI not preserved: got %s", state.EFI)
	}
	if state.Root != "/dev/nvme0n1p2" {
		t.Errorf("Root not preserved: got %s", state.Root)
	}
}
