package install

import (
	"strings"
	"testing"
)

// TestGeneratedConfigContainsInitrd verifies that the generated config includes the initrd field
func TestGeneratedConfigContainsInitrd(t *testing.T) {
	step := &Step03Config{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
		Device:   "/dev/nvme0n1",
		EFI:      "/dev/nvme0n1p1",
		Root:     "/dev/nvme0n1p2",
		BootMode: "uefi",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	// Verify critical fields are present
	if !strings.Contains(config, "(initrd microcode-initrd)") {
		t.Error("Generated config missing '(initrd microcode-initrd)' field")
	}
	if !strings.Contains(config, "(kernel linux)") {
		t.Error("Generated config missing '(kernel linux)' field")
	}
	if !strings.Contains(config, "(firmware (list linux-firmware))") {
		t.Error("Generated config missing '(firmware (list linux-firmware))' field")
	}
	if !strings.Contains(config, "(nongnu system linux-initrd)") {
		t.Error("Generated config missing nonguix linux-initrd module import")
	}
}

// TestConfigStructure verifies the basic structure of generated config
func TestConfigStructure(t *testing.T) {
	step := &Step03Config{}
	state := &State{
		HostName: "test-host",
		Timezone: "America/New_York",
		UserName: "testuser",
		FullName: "Test User",
		Device:   "/dev/nvme0n1",
		EFI:      "/dev/nvme0n1p1",
		Root:     "/dev/nvme0n1p2",
		BootMode: "uefi",
	}

	config := step.generateMinimalConfig(state, "grub-efi-bootloader", `'("/boot/efi")`)

	// Verify all critical sections are present
	criticalSections := []string{
		"(use-modules",
		"(operating-system",
		"(host-name",
		"(timezone",
		"(locale",
		"(kernel linux)",
		"(initrd microcode-initrd)",
		"(firmware",
		"(initrd-modules",
		"(kernel-arguments",
		"(bootloader",
		"(file-systems",
		"(users",
		"(packages",
		"(services",
	}

	for _, section := range criticalSections {
		if !strings.Contains(config, section) {
			t.Errorf("Generated config missing critical section: %s", section)
		}
	}
}

// TestStateIntegrity verifies State struct can be created and populated
func TestStateIntegrity(t *testing.T) {
	state := &State{
		Device:   "/dev/nvme0n1",
		EFI:      "/dev/nvme0n1p1",
		Root:     "/dev/nvme0n1p2",
		UserName: "testuser",
		FullName: "Test User",
		Timezone: "America/New_York",
		HostName: "test-host",
		BootMode: "uefi",
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
	if state.UserName != "testuser" {
		t.Errorf("UserName not set correctly: got %s", state.UserName)
	}
	if state.FullName != "Test User" {
		t.Errorf("FullName not set correctly: got %s", state.FullName)
	}
	if state.Timezone != "America/New_York" {
		t.Errorf("Timezone not set correctly: got %s", state.Timezone)
	}
	if state.HostName != "test-host" {
		t.Errorf("HostName not set correctly: got %s", state.HostName)
	}
	if state.BootMode != "uefi" {
		t.Errorf("BootMode not set correctly: got %s", state.BootMode)
	}
}
