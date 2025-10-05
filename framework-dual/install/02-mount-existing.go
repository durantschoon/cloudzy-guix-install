package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// Step02MountExisting mounts partitions and sets up store
type Step02MountExisting struct{}

func (s *Step02MountExisting) RunWarnings(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI)")
	}

	fmt.Println("=== Mount and Store Setup ===")
	fmt.Printf("ROOT partition: %s\n", state.Root)
	fmt.Printf("EFI partition: %s\n", state.EFI)
	fmt.Println()
	fmt.Println("This script will:")
	fmt.Println("  1. Mount the new Guix root partition to /mnt")
	fmt.Println("  2. Mount the existing ESP to /mnt/boot/efi")
	fmt.Println("  3. Copy Guix store from ISO to target")
	fmt.Println("  4. Set up bind mounts for /gnu and /var/guix")
	fmt.Println()

	return nil
}

func (s *Step02MountExisting) RunClean(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
	}

	// Mount root partition
	if !isMounted("/mnt") {
		fmt.Printf("Mounting %s to /mnt\n", state.Root)
		if err := runCommand("mount", state.Root, "/mnt"); err != nil {
			return err
		}
	}

	// Create directories
	dirs := []string{"/mnt/gnu/store", "/mnt/var/guix"}
	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create %s: %w", dir, err)
		}
	}

	// Stop guix-daemon
	fmt.Println("Stopping guix-daemon...")
	if commandExists("herd") {
		runCommand("herd", "stop", "guix-daemon")
	} else {
		exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
	}

	// Copy store
	start := time.Now()
	fmt.Println("Syncing /gnu/store to /mnt/gnu/store...")

	if commandExists("rsync") {
		fmt.Println("Using rsync...")
		if err := runCommand("rsync", "-aHAX", "--info=progress2,stats", "/gnu/store/.", "/mnt/gnu/store/."); err != nil {
			return fmt.Errorf("rsync failed: %w", err)
		}
		fmt.Println("rsync completed successfully")
	} else {
		fmt.Println("rsync not available, using cp instead...")
		if err := runCommand("cp", "-a", "/gnu/store/.", "/mnt/gnu/store/"); err != nil {
			return fmt.Errorf("cp failed: %w", err)
		}
		fmt.Println("cp completed successfully")
	}

	elapsed := time.Since(start)
	fmt.Printf("Time taken: %.0f seconds\n", elapsed.Seconds())

	// Mount ESP
	if err := os.MkdirAll("/mnt/boot/efi", 0755); err != nil {
		return err
	}
	fmt.Printf("Mounting existing ESP: %s\n", state.EFI)
	if err := runCommand("mount", state.EFI, "/mnt/boot/efi"); err != nil {
		return err
	}

	// Verify ESP contents
	fmt.Println("Checking ESP contents...")
	runCommand("ls", "-la", "/mnt/boot/efi/")

	// Mount home partition if it exists
	if state.HomePartition != "" {
		fmt.Printf("Mounting home partition: %s\n", state.HomePartition)
		if err := os.MkdirAll("/mnt/home", 0755); err != nil {
			return err
		}
		if err := runCommand("mount", state.HomePartition, "/mnt/home"); err != nil {
			return err
		}
		fmt.Println("Home partition mounted successfully")
		runCommand("df", "-h", "/mnt/home")
	} else {
		fmt.Println("No separate home partition - home directories will be in root partition")
	}

	// Copy store and var/guix for bind mounts
	fmt.Println("Copying /gnu/store to /mnt/gnu/store for bind mount...")
	if err := runCommand("cp", "-a", "/gnu/store", "/mnt/gnu/"); err != nil {
		return err
	}

	fmt.Println("Copying /var/guix to /mnt/var/guix for bind mount...")
	if err := runCommand("cp", "-a", "/var/guix", "/mnt/var/"); err != nil {
		return err
	}

	if commandExists("herd") {
		runCommand("herd", "stop", "guix-daemon")
	}

	// Set up bind mounts
	fmt.Println("Setting up bind mounts...")
	if err := runCommand("mount", "--bind", "/mnt/gnu", "/gnu"); err != nil {
		return err
	}
	if err := runCommand("mount", "--bind", "/mnt/var/guix", "/var/guix"); err != nil {
		return err
	}

	runCommand("df", "-h", "/gnu", "/var/guix")

	// Restart guix-daemon
	if commandExists("herd") {
		if err := runCommand("herd", "start", "guix-daemon"); err != nil {
			return err
		}
	}

	return nil
}

// Helper functions

func isMounted(path string) bool {
	cmd := exec.Command("mount")
	output, err := cmd.Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(output), " on "+path+" ")
}

func commandExists(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}
