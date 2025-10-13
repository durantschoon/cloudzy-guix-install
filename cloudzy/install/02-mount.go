package install

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step02Mount mounts partitions and sets up store
type Step02Mount struct{}

func (s *Step02Mount) RunWarnings(state *State) error {
	// Auto-detect missing variables if Step01 was skipped
	if state.Device == "" {
		if err := s.detectDevice(state); err != nil {
			return err
		}
	}

	if state.EFI == "" || state.Root == "" {
		if err := s.detectPartitions(state); err != nil {
			return err
		}
	}

	// Verify required variables
	if state.Root == "" || state.EFI == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI). Please run Step01 first or set environment variables")
	}

	fmt.Println("=== Step 2: Mount and Store Setup ===")
	fmt.Println()
	fmt.Println("This step will:")
	fmt.Println("  1. Mount root partition to /mnt (if not already mounted)")
	fmt.Println("  2. Stop guix-daemon temporarily")
	fmt.Println("  3. Copy Guix store from ISO to /mnt/gnu/store (if not already done)")
	fmt.Println("  4. Copy /var/guix database to /mnt/var/guix")
    fmt.Println("  5. Mount EFI partition to /mnt/boot/efi")
    fmt.Println("  6. Verify labels exist before mounting and warn on low free space")
	fmt.Println()
	fmt.Println("Note: Store is copied to disk to avoid 'no space left' errors.")
	fmt.Println("      The ISO's RAM filesystem is tiny - we use your disk instead!")
	fmt.Println("      Later, cow-store will redirect writes properly (NOT bind mounts!)")
	fmt.Println()
	fmt.Println("Environment variables used by this step:")
	fmt.Printf("  ROOT   - %s (from Step01)\n", state.Root)
	fmt.Printf("  EFI    - %s (from Step01)\n", state.EFI)
	fmt.Printf("  DEVICE - %s (from Step01)\n", state.Device)
	fmt.Println()
	fmt.Println("Idempotency: Skips store copy if /mnt/gnu/store already populated")
	fmt.Println()

	return nil
}

func (s *Step02Mount) RunClean(state *State) error {
	// Verify required variables
	if state.Root == "" || state.EFI == "" || state.Device == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI, DEVICE)")
	}

	// Check if already mounted and populated (idempotency)
	if lib.IsMounted("/mnt") && s.isStorePopulated() {
		fmt.Println("/mnt is already mounted and /mnt/gnu/store is populated")
		fmt.Println("Skipping mount and store sync (idempotent - safe for reruns)")
		return nil
	}

    // Basic label existence checks before mount
    if _, err := os.Stat("/dev/disk/by-label/GUIX_ROOT"); err != nil {
        return fmt.Errorf("label GUIX_ROOT not found under /dev/disk/by-label")
    }
    if _, err := os.Stat("/dev/disk/by-label/EFI"); err != nil {
        return fmt.Errorf("label EFI not found under /dev/disk/by-label")
    }

    // Mount root partition by label if not mounted
	if !lib.IsMounted("/mnt") {
		fmt.Println("Mounting GUIX_ROOT to /mnt")
		if err := lib.RunCommand("mount", "/dev/disk/by-label/GUIX_ROOT", "/mnt"); err != nil {
			return err
		}
	} else {
		fmt.Println("/mnt is already mounted")
	}

    // Free space check (warn if < 40 GiB available on root after mount)
    avail := lib.GetMountFreeSpaceGiB("/mnt")
    if avail > 0 && avail < 40 {
        fmt.Printf("WARNING: Only %.1fGiB free on /mnt. Installation may fail due to low space.\n", avail)
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
	if lib.CommandExists("herd") {
		lib.RunCommand("herd", "stop", "guix-daemon")
	} else {
		exec.Command("pkill", "-TERM", "-x", "guix-daemon").Run()
	}

	// Copy store
	start := time.Now()
	fmt.Println("Syncing /gnu/store to /mnt/gnu/store...")

	if lib.CommandExists("rsync") {
		fmt.Println("Using rsync...")
		if err := lib.RunCommand("rsync", "-aHAX", "--info=progress2,stats", "/gnu/store/.", "/mnt/gnu/store/."); err != nil {
			return fmt.Errorf("rsync failed: %w", err)
		}
		fmt.Println("rsync completed successfully")
	} else {
		fmt.Println("rsync not available, using cp instead...")
		if err := lib.RunCommand("cp", "-a", "/gnu/store/.", "/mnt/gnu/store/"); err != nil {
			return fmt.Errorf("cp failed: %w", err)
		}
		fmt.Println("cp completed successfully")
	}

	elapsed := time.Since(start)
	fmt.Printf("Time taken: %.0f seconds\n", elapsed.Seconds())

	// Copy /var/guix for database consistency
	fmt.Println("Copying /var/guix to /mnt/var/guix...")
	if err := lib.RunCommand("cp", "-a", "/var/guix", "/mnt/var/"); err != nil {
		return fmt.Errorf("failed to copy /var/guix: %w", err)
	}

	// Ensure critical directories exist in /mnt/var/guix
	// guix system init will try to create these and will fail if they can't be created
	fmt.Println()
	fmt.Println("Pre-creating critical /var/guix directories...")
	criticalDirs := []string{
		"/mnt/var/guix/profiles",
		"/mnt/var/guix/gcroots",
		"/mnt/var/guix/userpool",
	}
	for _, dir := range criticalDirs {
		fmt.Printf("  Creating %s...", dir)
		if err := os.MkdirAll(dir, 0755); err != nil {
			fmt.Printf(" [FAILED]: %v\n", err)
			return fmt.Errorf("failed to create critical directory %s: %w", dir, err)
		}
		// Verify it was created and is accessible
		if stat, err := os.Stat(dir); err != nil {
			fmt.Printf(" [VERIFY FAILED]: %v\n", err)
			return fmt.Errorf("created directory %s cannot be verified: %w", dir, err)
		} else if !stat.IsDir() {
			fmt.Printf(" [NOT A DIRECTORY]\n")
			return fmt.Errorf("%s exists but is not a directory", dir)
		}
		fmt.Printf(" [OK]\n")
	}
	fmt.Println("All critical directories created successfully")

    // Mount ESP by label
	if err := os.MkdirAll("/mnt/boot/efi", 0755); err != nil {
		return err
	}
	fmt.Println("Mounting EFI to /mnt/boot/efi")
	if err := lib.RunCommand("mount", "/dev/disk/by-label/EFI", "/mnt/boot/efi"); err != nil {
		return err
	}

	// Verify ESP contents
	fmt.Println("Checking ESP contents...")
	lib.RunCommand("ls", "-la", "/mnt/boot/efi/")

	fmt.Println()
	fmt.Println("Mount setup complete. Ready for system initialization.")
	return nil
}

// Helper functions

func (s *Step02Mount) detectDevice(state *State) error {
	// Auto-detect
	candidates := []string{"/dev/vda", "/dev/sda", "/dev/nvme0n1"}
	for _, d := range candidates {
		if _, err := os.Stat(d); err == nil {
			state.Device = d
			fmt.Printf("Auto-detected device: %s\n", d)
			return nil
		}
	}
	return fmt.Errorf("no suitable block device found. Please set DEVICE environment variable")
}

func (s *Step02Mount) detectPartitions(state *State) error {
	if state.Device == "" {
		return fmt.Errorf("DEVICE not set")
	}

	// For cloudzy, partitions are straightforward: partition 1 is EFI, partition 2 is root
	state.EFI = s.makePartitionPath(state.Device, "1")
	state.Root = s.makePartitionPath(state.Device, "2")

	fmt.Printf("Detected EFI: %s\n", state.EFI)
	fmt.Printf("Detected ROOT: %s\n", state.Root)

	return nil
}

func (s *Step02Mount) makePartitionPath(device, partNum string) string {
	if strings.Contains(device, "nvme") || strings.Contains(device, "mmcblk") {
		return fmt.Sprintf("%sp%s", device, partNum)
	}
	return device + partNum
}

func (s *Step02Mount) isStorePopulated() bool {
	// Check if /mnt/gnu/store has contents
	entries, err := os.ReadDir("/mnt/gnu/store")
	if err != nil {
		return false
	}
	// Store should have many entries if populated
	return len(entries) > 10
}

