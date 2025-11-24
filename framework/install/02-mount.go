package install

import (
	"fmt"
	"os"
	"os/exec"
	"time"

	"github.com/durantschoon/cloudzy-guix-install/lib"
)

// Step02Mount mounts partitions and sets up store
type Step02Mount struct{}

func (s *Step02Mount) RunWarnings(state *State) error {
	// Auto-detect missing variables if Step01 was skipped
	if state.Device == "" {
		device, err := lib.DetectDevice("framework")
		if err != nil {
			return err
		}
		state.Device = device
	}

	if state.EFI == "" || state.Root == "" {
		efi, root, err := lib.DetectPartitions(state.Device)
		if err != nil {
			return err
		}
		state.EFI = efi
		state.Root = root
		fmt.Printf("Detected EFI: %s\n", state.EFI)
		fmt.Printf("Detected ROOT: %s\n", state.Root)
	}

	// Verify required variables
	if state.Root == "" || state.EFI == "" {
		return fmt.Errorf("required variables not set (ROOT, EFI). Please run Step01 first or set environment variables")
	}

	lib.PrintStepHeader(2, "Mount and Store Setup")
	fmt.Println()
    fmt.Println("This step will:")
	fmt.Println("  1. Mount root partition to /mnt (if not already mounted)")
	fmt.Println("  2. Stop guix-daemon temporarily")
	fmt.Println("  3. Copy Guix store from ISO to /mnt/gnu/store (if not already done)")
	fmt.Println("  4. Copy /var/guix database to /mnt/var/guix")
	fmt.Println("  5. Clean up ISO artifacts after rsync:")
	fmt.Println("     - Fix /var/run (directory -> symlink to /run)")
	fmt.Println("     - Fix /var/lock (directory -> symlink to /run/lock)")
	fmt.Println("     - Fix /etc/mtab (file -> symlink to /proc/self/mounts)")
	fmt.Println("     - Empty /run directory (remove ISO runtime state)")
	fmt.Println("     - Set /var/tmp permissions (sticky bit 1777)")
	fmt.Println("     - Remove ISO-specific files (machine-id, resolv.conf, etc.)")
	fmt.Println("  6. Mount EFI partition to /mnt/boot/efi")
	fmt.Println("  7. Verify labels exist before mounting and warn on low free space")
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
	storeAlreadyPopulated := lib.IsMounted("/mnt") && lib.IsStorePopulated()
	if storeAlreadyPopulated {
		fmt.Println("/mnt is already mounted and /mnt/gnu/store is populated")
		fmt.Println("Skipping mount and store sync (idempotent - safe for reruns)")
		fmt.Println("NOTE: Will still run ISO cleanup to ensure filesystem invariants are correct")
		fmt.Println()
	}

    // Basic label existence checks before mount
    if err := lib.VerifyLabelsExist("GUIX_ROOT", "EFI"); err != nil {
        return err
    }

    // Mount root partition by label if not mounted
    if !lib.IsMounted("/mnt") {
        fmt.Println("Mounting GUIX_ROOT to /mnt")
        if err := lib.MountByLabel("GUIX_ROOT", "/mnt"); err != nil {
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

	// Only sync store if not already populated
	if !storeAlreadyPopulated {
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
	} else {
		fmt.Println("Store already populated - proceeding directly to ISO cleanup")
	}

	// Clean up ISO artifacts and fix filesystem invariants
	// This fixes /var/run symlink, /etc/mtab symlink, and removes ISO-specific files
	if err := lib.CleanupISOArtifacts(); err != nil {
		return fmt.Errorf("failed to cleanup ISO artifacts: %w", err)
	}

    // Mount EFI - try different possible labels
	fmt.Println("Mounting EFI to /mnt/boot/efi")
    efiLabels := []string{"EFI", "ESP", "BOOT"}
    var mountErr error
    
    for _, label := range efiLabels {
        fmt.Printf("Trying to mount EFI with label: %s\n", label)
        if err := lib.MountByLabel(label, "/mnt/boot/efi"); err != nil {
            mountErr = err
            fmt.Printf("Failed to mount with label '%s': %v\n", label, err)
            continue
        }
        fmt.Printf("Successfully mounted EFI with label: %s\n", label)
        mountErr = nil
        break
    }
    
    if mountErr != nil {
        return fmt.Errorf("failed to mount EFI partition with any known label: %w", mountErr)
    }

	// Verify EFI contents
	fmt.Println("Checking EFI contents...")
	lib.RunCommand("ls", "-la", "/mnt/boot/efi/")

	fmt.Println()
	fmt.Println("Mount setup complete. Ready for system initialization.")
	return nil
}

