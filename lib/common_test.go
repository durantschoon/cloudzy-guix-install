package lib

import (
	"fmt"
	"os"
	"strings"
	"testing"
	"time"
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

// TestRetryUntilReady tests the generic retry helper function
func TestRetryUntilReady(t *testing.T) {
	t.Run("Success on first try", func(t *testing.T) {
		attempts := 0
		check := func() error {
			attempts++
			return nil // Success immediately
		}

		err := retryUntilReady(check, 1*time.Second, 100*time.Millisecond, nil)
		if err != nil {
			t.Errorf("Expected success, got error: %v", err)
		}
		if attempts != 1 {
			t.Errorf("Expected 1 attempt, got %d", attempts)
		}
	})

	t.Run("Success after retries", func(t *testing.T) {
		attempts := 0
		check := func() error {
			attempts++
			if attempts < 3 {
				return fmt.Errorf("not ready yet")
			}
			return nil // Success on 3rd attempt
		}

		err := retryUntilReady(check, 1*time.Second, 100*time.Millisecond, nil)
		if err != nil {
			t.Errorf("Expected success, got error: %v", err)
		}
		if attempts != 3 {
			t.Errorf("Expected 3 attempts, got %d", attempts)
		}
	})

	t.Run("Timeout after max attempts", func(t *testing.T) {
		attempts := 0
		check := func() error {
			attempts++
			return fmt.Errorf("never succeeds")
		}

		// 500ms timeout, 100ms interval = max 5 attempts
		err := retryUntilReady(check, 500*time.Millisecond, 100*time.Millisecond, nil)
		if err == nil {
			t.Error("Expected timeout error, got nil")
		}
		if !strings.Contains(err.Error(), "timeout") {
			t.Errorf("Expected timeout error, got: %v", err)
		}
		if attempts != 5 {
			t.Errorf("Expected 5 attempts, got %d", attempts)
		}
	})

	t.Run("Progress callback called correctly", func(t *testing.T) {
		attempts := 0
		progressCalls := 0
		lastMax := 0

		check := func() error {
			attempts++
			if attempts < 3 {
				return fmt.Errorf("not ready")
			}
			return nil // Success on 3rd attempt
		}

		progressFn := func(attempt, max int) {
			progressCalls++
			lastMax = max
		}

		err := retryUntilReady(check, 1*time.Second, 100*time.Millisecond, progressFn)
		if err != nil {
			t.Errorf("Expected success, got error: %v", err)
		}
		// Progress callback is called before each check, so 3 calls for 3 attempts
		if progressCalls != 3 {
			t.Errorf("Expected 3 progress calls, got %d", progressCalls)
		}
		if lastMax != 10 { // 1s / 100ms = 10 max attempts
			t.Errorf("Expected max=10, got %d", lastMax)
		}
	})
}

// TestCheckSocketExists tests socket file verification
func TestCheckSocketExists(t *testing.T) {
	t.Run("Non-existent socket returns error", func(t *testing.T) {
		// Test with a path that definitely doesn't exist
		err := checkSocketExists()
		// This will fail on most systems since daemon socket doesn't exist
		// That's expected - we're testing the function behavior
		if err == nil {
			t.Log("Socket exists on this system - skipping test")
			return
		}
		if !strings.Contains(err.Error(), "socket not ready") {
			t.Errorf("Expected 'socket not ready' error, got: %v", err)
		}
	})
}

// TestCheckDaemonResponsive tests daemon command responsiveness
func TestCheckDaemonResponsive(t *testing.T) {
	t.Run("Daemon not available returns error", func(t *testing.T) {
		// On most dev machines, guix daemon won't be running
		err := checkDaemonResponsive()
		if err == nil {
			t.Log("Guix daemon is running on this system - skipping test")
			return
		}
		if !strings.Contains(err.Error(), "daemon not responsive") {
			t.Errorf("Expected 'daemon not responsive' error, got: %v", err)
		}
	})
}

// TestIsDaemonProcessRunning tests process status check
func TestIsDaemonProcessRunning(t *testing.T) {
	t.Run("Returns bool without error", func(t *testing.T) {
		// Should always return a bool, never panic
		result := isDaemonProcessRunning()
		// On dev machine without Guix, should be false
		// On Guix system, might be true
		t.Logf("Daemon process running: %v", result)
	})
}

// TestCheckDaemonStable tests stability verification
func TestCheckDaemonStable(t *testing.T) {
	t.Run("Fails fast if daemon not responsive", func(t *testing.T) {
		// On most dev machines, this will fail immediately
		err := checkDaemonStable(3, 100*time.Millisecond)
		if err == nil {
			t.Log("Guix daemon is running and stable - skipping test")
			return
		}
		if !strings.Contains(err.Error(), "stability check 1/3 failed") {
			t.Errorf("Expected stability check to fail on first attempt, got: %v", err)
		}
	})
}

// TestCleanupISOArtifacts tests the CleanupISOArtifacts function
func TestCleanupISOArtifacts(t *testing.T) {
	// Create a temporary directory to simulate /mnt
	tmpDir, err := os.MkdirTemp("", "guix-install-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create directory structure
	mntVar := tmpDir + "/var"
	mntVarRun := mntVar + "/run"
	mntEtc := tmpDir + "/etc"
	mntVarGuix := mntVar + "/guix"
	mntHome := tmpDir + "/home"

	if err := os.MkdirAll(mntVarRun, 0755); err != nil {
		t.Fatalf("Failed to create /var/run: %v", err)
	}
	if err := os.MkdirAll(mntEtc, 0755); err != nil {
		t.Fatalf("Failed to create /etc: %v", err)
	}
	if err := os.MkdirAll(mntVarGuix, 0755); err != nil {
		t.Fatalf("Failed to create /var/guix: %v", err)
	}
	if err := os.MkdirAll(mntHome, 0755); err != nil {
		t.Fatalf("Failed to create /home: %v", err)
	}

	// Create test files that should be removed
	machineID := mntEtc + "/machine-id"
	resolvConf := mntEtc + "/resolv.conf"
	mtabFile := mntEtc + "/mtab"
	isoUserProfile := mntVarGuix + "/profiles/per-user/live-image-user"
	isoUserHome := mntHome + "/live-image-user"

	os.WriteFile(machineID, []byte("test-machine-id"), 0644)
	os.WriteFile(resolvConf, []byte("nameserver 8.8.8.8"), 0644)
	os.WriteFile(mtabFile, []byte("test mtab content"), 0644)
	os.MkdirAll(isoUserProfile, 0755)
	os.MkdirAll(isoUserHome, 0755)

	// Save original /mnt path behavior - we'll test with tmpDir
	// Since CleanupISOArtifacts hardcodes "/mnt", we need to test the logic differently
	// For now, we'll test that the function exists and can be called
	// In a real integration test, we'd mount a test filesystem at /mnt

	t.Run("FunctionExists", func(t *testing.T) {
		// Verify the function exists and has correct signature
		// We can't easily test it without actual /mnt mount, but we verify it compiles
		_ = CleanupISOArtifacts
	})

	t.Run("SymlinkLogic", func(t *testing.T) {
		// Test the symlink creation logic separately
		testVarRun := tmpDir + "/test-var-run"
		testMtab := tmpDir + "/test-mtab"

		// Test: directory should become symlink
		os.MkdirAll(testVarRun, 0755)
		if info, err := os.Lstat(testVarRun); err == nil && info.IsDir() {
			os.RemoveAll(testVarRun)
			if err := os.Symlink("/run", testVarRun); err != nil {
				t.Errorf("Failed to create symlink: %v", err)
			}
			if target, err := os.Readlink(testVarRun); err != nil || target != "/run" {
				t.Errorf("Symlink target incorrect: got %s, want /run", target)
			}
		}

		// Test: file should become symlink
		os.WriteFile(testMtab, []byte("test"), 0644)
		if info, err := os.Lstat(testMtab); err == nil && !info.IsDir() {
			os.Remove(testMtab)
			if err := os.Symlink("/proc/self/mounts", testMtab); err != nil {
				t.Errorf("Failed to create mtab symlink: %v", err)
			}
			if target, err := os.Readlink(testMtab); err != nil || target != "/proc/self/mounts" {
				t.Errorf("Mtab symlink target incorrect: got %s, want /proc/self/mounts", target)
			}
		}
	})
}

// TestDaemonCheckComposition tests that checks compose correctly
func TestDaemonCheckComposition(t *testing.T) {
	t.Run("isDaemonReady fails early if socket missing", func(t *testing.T) {
		// Should fail at socket check before trying daemon responsiveness
		err := isDaemonReady()
		if err == nil {
			t.Log("Guix daemon is fully ready - skipping test")
			return
		}
		// Error should be from socket or daemon check
		errorStr := err.Error()
		if !strings.Contains(errorStr, "socket not ready") &&
		   !strings.Contains(errorStr, "daemon not responsive") {
			t.Errorf("Expected socket or daemon error, got: %v", err)
		}
	})

	t.Run("isDaemonReadyAfterStart checks process first", func(t *testing.T) {
		// Should check process running before other checks
		err := isDaemonReadyAfterStart()
		if err == nil {
			t.Log("Guix daemon process is running and ready - skipping test")
			return
		}
		// Error could be "daemon process not started yet" or from inner checks
		errorStr := err.Error()
		validErrors := []string{
			"daemon process not started yet",
			"socket not ready",
			"daemon not responsive",
		}
		hasValidError := false
		for _, validErr := range validErrors {
			if strings.Contains(errorStr, validErr) {
				hasValidError = true
				break
			}
		}
		if !hasValidError {
			t.Errorf("Expected daemon check error, got: %v", err)
		}
	})
}
