package lib

import (
	"os"
	"strings"
	"testing"
)

// TestMakePartitionPath tests the MakePartitionPath function with different device types
func TestMakePartitionPath(t *testing.T) {
	tests := []struct {
		name     string
		device   string
		partNum  string
		expected string
	}{
		{
			name:     "NVMe device with partition",
			device:   "/dev/nvme0n1",
			partNum:  "1",
			expected: "/dev/nvme0n1p1",
		},
		{
			name:     "NVMe device with partition 2",
			device:   "/dev/nvme0n1",
			partNum:  "2",
			expected: "/dev/nvme0n1p2",
		},
		{
			name:     "SATA device with partition",
			device:   "/dev/sda",
			partNum:  "1",
			expected: "/dev/sda1",
		},
		{
			name:     "SATA device with partition 3",
			device:   "/dev/sda",
			partNum:  "3",
			expected: "/dev/sda3",
		},
		{
			name:     "eMMC device with partition",
			device:   "/dev/mmcblk0",
			partNum:  "1",
			expected: "/dev/mmcblk0p1",
		},
		{
			name:     "VDA device (VPS) with partition",
			device:   "/dev/vda",
			partNum:  "1",
			expected: "/dev/vda1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := MakePartitionPath(tt.device, tt.partNum)
			if result != tt.expected {
				t.Errorf("MakePartitionPath(%s, %s) = %s, want %s", tt.device, tt.partNum, result, tt.expected)
			}
		})
	}
}

// TestDetectDeviceFromState tests the DetectDeviceFromState function
func TestDetectDeviceFromState(t *testing.T) {
	// Test with user-specified device
	t.Run("UserSpecifiedDevice", func(t *testing.T) {
		// This test would require mocking os.Stat, but for now we'll test the logic
		// In a real test environment, we'd use a test filesystem or mock
		device := "/dev/testdevice"
		platform := "test"
		
		// Since we can't easily mock os.Stat in this simple test,
		// we'll test the auto-detection path by using a non-existent device
		// and verifying it falls back to DetectDevice
		result, err := DetectDeviceFromState("", platform)
		
		// DetectDevice should return an error for unknown platform
		if err == nil {
			t.Error("Expected error for unknown platform, got nil")
		}
		
		// Test that empty device string triggers auto-detection
		if result != "" {
			t.Errorf("Expected empty result for unknown platform, got %s", result)
		}
		
		// Use the variables to avoid unused variable warnings
		_ = device
		_ = result
	})
	
	// Test with known platforms
	t.Run("KnownPlatforms", func(t *testing.T) {
		platforms := []string{"cloudzy", "framework", "framework-dual"}
		
		for _, platform := range platforms {
			result, err := DetectDeviceFromState("", platform)
			
			// For known platforms, we expect either a result or a specific error
			// (since we don't have actual devices in test environment)
			if err != nil && !strings.Contains(err.Error(), "no suitable block device found") {
				t.Errorf("Unexpected error for platform %s: %v", platform, err)
			}
			
			// Use the result variable to avoid unused variable warnings
			_ = result
		}
	})
}

// TestIsPartitionFormatted tests the IsPartitionFormatted function
func TestIsPartitionFormatted(t *testing.T) {
	tests := []struct {
		name     string
		partition string
		fsTypes  []string
		// Note: We can't easily test the actual blkid output without mocking
		// This test structure shows what we would test in a real environment
	}{
		{
			name:      "ext4 partition",
			partition: "/dev/sda1",
			fsTypes:   []string{"ext4"},
		},
		{
			name:      "vfat partition",
			partition: "/dev/sda1",
			fsTypes:   []string{"vfat"},
		},
		{
			name:      "any filesystem",
			partition: "/dev/sda1",
			fsTypes:   []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// In a real test, we would mock the blkid command output
			// For now, we just verify the function doesn't panic
			result := IsPartitionFormatted(tt.partition, tt.fsTypes...)
			
			// We expect false for non-existent partitions in test environment
			if result {
				t.Logf("IsPartitionFormatted(%s, %v) returned true (expected in test environment)", tt.partition, tt.fsTypes)
			}
		})
	}
}

// TestGetEnv tests the GetEnv function
func TestGetEnv(t *testing.T) {
	tests := []struct {
		name         string
		key          string
		defaultValue string
		setValue     string
		expected     string
	}{
		{
			name:         "Environment variable set",
			key:          "TEST_VAR",
			defaultValue: "default",
			setValue:     "set_value",
			expected:     "set_value",
		},
		{
			name:         "Environment variable not set",
			key:          "NONEXISTENT_VAR",
			defaultValue: "default",
			setValue:     "",
			expected:     "default",
		},
		{
			name:         "Empty environment variable",
			key:          "EMPTY_VAR",
			defaultValue: "default",
			setValue:     "",
			expected:     "default",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Set environment variable if needed
			if tt.setValue != "" {
				os.Setenv(tt.key, tt.setValue)
				defer os.Unsetenv(tt.key)
			} else {
				// Ensure it's not set
				os.Unsetenv(tt.key)
			}

			result := GetEnv(tt.key, tt.defaultValue)
			if result != tt.expected {
				t.Errorf("GetEnv(%s, %s) = %s, want %s", tt.key, tt.defaultValue, result, tt.expected)
			}
		})
	}
}

// TestGetEnvOrDefault tests the GetEnvOrDefault function
func TestGetEnvOrDefault(t *testing.T) {
	tests := []struct {
		name         string
		value        string
		defaultValue string
		expected     string
	}{
		{
			name:         "Non-empty value",
			value:        "test_value",
			defaultValue: "default",
			expected:     "test_value",
		},
		{
			name:         "Empty value",
			value:        "",
			defaultValue: "default",
			expected:     "default",
		},
		{
			name:         "Both empty",
			value:        "",
			defaultValue: "",
			expected:     "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := GetEnvOrDefault(tt.value, tt.defaultValue)
			if result != tt.expected {
				t.Errorf("GetEnvOrDefault(%s, %s) = %s, want %s", tt.value, tt.defaultValue, result, tt.expected)
			}
		})
	}
}

// TestDetectDevice tests the DetectDevice function with different platforms
func TestDetectDevice(t *testing.T) {
	tests := []struct {
		name     string
		platform string
		// We expect errors since we don't have actual devices in test environment
		expectError bool
	}{
		{
			name:        "Cloudzy platform",
			platform:    "cloudzy",
			expectError: true, // No actual devices in test environment
		},
		{
			name:        "Framework platform",
			platform:    "framework",
			expectError: true, // No actual devices in test environment
		},
		{
			name:        "Framework-dual platform",
			platform:    "framework-dual",
			expectError: true, // No actual devices in test environment
		},
		{
			name:        "Unknown platform",
			platform:    "unknown",
			expectError: true, // Unknown platform should error
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := DetectDevice(tt.platform)
			
			if tt.expectError {
				if err == nil {
					t.Errorf("DetectDevice(%s) expected error, got nil", tt.platform)
				}
			} else {
				if err != nil {
					t.Errorf("DetectDevice(%s) unexpected error: %v", tt.platform, err)
				}
				if result == "" {
					t.Errorf("DetectDevice(%s) returned empty result", tt.platform)
				}
			}
		})
	}
}

// TestCommandExists tests the CommandExists function
func TestCommandExists(t *testing.T) {
	tests := []struct {
		name     string
		command  string
		expected bool
	}{
		{
			name:     "Existing command (ls)",
			command:  "ls",
			expected: true,
		},
		{
			name:     "Non-existing command",
			command:  "nonexistentcommand12345",
			expected: false,
		},
		{
			name:     "Empty command",
			command:  "",
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := CommandExists(tt.command)
			if result != tt.expected {
				t.Errorf("CommandExists(%s) = %v, want %v", tt.command, result, tt.expected)
			}
		})
	}
}

// TestIsGuixLiveISO tests the IsGuixLiveISO function
func TestIsGuixLiveISO(t *testing.T) {
	// This test will likely return false in a normal test environment
	// but we can verify it doesn't panic and returns a boolean
	result := IsGuixLiveISO()

	// We expect false in test environment, but the important thing is it doesn't panic
	if result {
		t.Log("IsGuixLiveISO() returned true (unexpected in test environment)")
	}
}

func TestDetectBootMode(t *testing.T) {
	// Test boot mode detection
	// This will return actual boot mode of test system, but shouldn't panic
	mode := DetectBootMode()

	// Should return either "uefi" or "bios"
	if mode != "uefi" && mode != "bios" {
		t.Errorf("DetectBootMode() returned unexpected value: %s (expected 'uefi' or 'bios')", mode)
	}

	// Log what we detected for debugging
	t.Logf("Detected boot mode: %s", mode)

	// Additional verification: check if detection matches reality
	_, efiErr := os.Stat("/sys/firmware/efi")
	if mode == "uefi" && efiErr != nil {
		t.Error("DetectBootMode() returned 'uefi' but /sys/firmware/efi doesn't exist")
	}

	// Verify that if /sys/firmware/efi exists as dir with entries, we get uefi
	if efiErr == nil {
		info, err := os.Stat("/sys/firmware/efi")
		if err == nil && info.IsDir() {
			entries, err := os.ReadDir("/sys/firmware/efi")
			if err == nil && len(entries) > 0 {
				if mode != "uefi" {
					t.Error("DetectBootMode() should return 'uefi' when /sys/firmware/efi exists with entries")
				}
			}
		}
	}
}

// Benchmark tests for performance-critical functions
func BenchmarkMakePartitionPath(b *testing.B) {
	for i := 0; i < b.N; i++ {
		MakePartitionPath("/dev/nvme0n1", "1")
	}
}

func BenchmarkGetEnv(b *testing.B) {
	os.Setenv("BENCHMARK_VAR", "test_value")
	defer os.Unsetenv("BENCHMARK_VAR")
	
	for i := 0; i < b.N; i++ {
		GetEnv("BENCHMARK_VAR", "default")
	}
}

func BenchmarkCommandExists(b *testing.B) {
	for i := 0; i < b.N; i++ {
		CommandExists("ls")
	}
}
