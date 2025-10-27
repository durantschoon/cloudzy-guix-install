package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	cloudzyi "github.com/durantschoon/cloudzy-guix-install/cloudzy/install"
	frameworkduali "github.com/durantschoon/cloudzy-guix-install/framework-dual/install"
	frameworki "github.com/durantschoon/cloudzy-guix-install/framework/install"
	"github.com/durantschoon/cloudzy-guix-install/lib"
)

func main() {
	platform := getEnv("GUIX_PLATFORM", "cloudzy")

	// Detect and bootstrap user channels
	fmt.Println("=== Channel Detection ===")
	channelInfo, err := lib.DetectUserChannels()
	if err != nil {
		fatal("Failed to detect user channels: %v", err)
	}

	if err := lib.BootstrapUserChannels(channelInfo); err != nil {
		fatal("Failed to bootstrap user channels: %v", err)
	}

	if err := lib.ValidateChannels(); err != nil {
		fatal("Failed to validate channels: %v", err)
	}

	fmt.Println()

	switch platform {
	case "framework-dual":
		runFrameworkDual()
	case "framework":
		runFramework()
	case "cloudzy":
		runCloudzy()
	default:
		fatal("Unsupported platform: %s\nSupported: cloudzy, framework, framework-dual", platform)
	}
}

func runCloudzy() {
	fmt.Println("=== Guix Installation for Cloudzy VPS ===")
	fmt.Println()

	// Create state to hold all installation variables
	state := cloudzyi.NewState()

	// Define installation steps
	steps := []struct {
		name    string
		warning func(*cloudzyi.State) error
		clean   func(*cloudzyi.State) error
	}{
		{
			name:    "Partition",
			warning: (&cloudzyi.Step01Partition{}).RunWarnings,
			clean:   (&cloudzyi.Step01Partition{}).RunClean,
		},
		{
			name:    "Mount",
			warning: (&cloudzyi.Step02Mount{}).RunWarnings,
			clean:   (&cloudzyi.Step02Mount{}).RunClean,
		},
		{
			name:    "Config",
			warning: (&cloudzyi.Step03Config{}).RunWarnings,
			clean:   (&cloudzyi.Step03Config{}).RunClean,
		},
		{
			name:    "System Init",
			warning: (&cloudzyi.Step04SystemInit{}).RunWarnings,
			clean:   (&cloudzyi.Step04SystemInit{}).RunClean,
		},
	}

	// Run each step
	for i, step := range steps {
		fmt.Printf("\n=== Step %d/%d: %s ===\n\n", i+1, len(steps), step.name)

		// Run warnings
		if err := step.warning(state); err != nil {
			fatal("Warnings failed for %s: %v", step.name, err)
		}

		// Ask user if they want to run this step
		if !askYesNo(fmt.Sprintf("\nRun %s step now? [Y/n] ", step.name), "Y") {
			fmt.Printf("Skipping %s per user request\n", step.name)
			continue
		}

		// Run clean script
		fmt.Printf("\nRunning %s...\n", step.name)
		if err := step.clean(state); err != nil {
			warn("Step %s returned error: %v", step.name, err)
			if !askYesNo("\nContinue to next step anyway? [y/N] ", "y") {
				fatal("Installation aborted by user")
			}
		}

		fmt.Printf("\n[OK] %s completed\n", step.name)

		// Don't ask to continue after the last step (system-init will reboot)
		if i < len(steps)-1 {
			if !askYesNo("\nContinue to next step? [Y/n] ", "Y") {
				fmt.Println("Installation paused. Run again to continue.")
				os.Exit(0)
			}
		}
	}

	fmt.Println("\n=== All installation steps completed ===")
}

func runFramework() {
	fmt.Println("=== Guix Installation for Framework 13 ===")
	fmt.Println()

	// Create state to hold all installation variables
	state := frameworki.NewState()

	// Define installation steps
	steps := []struct {
		name    string
		warning func(*frameworki.State) error
		clean   func(*frameworki.State) error
	}{
		{
			name:    "Partition",
			warning: (&frameworki.Step01Partition{}).RunWarnings,
			clean:   (&frameworki.Step01Partition{}).RunClean,
		},
		{
			name:    "Mount",
			warning: (&frameworki.Step02Mount{}).RunWarnings,
			clean:   (&frameworki.Step02Mount{}).RunClean,
		},
		{
			name:    "Config",
			warning: (&frameworki.Step03Config{}).RunWarnings,
			clean:   (&frameworki.Step03Config{}).RunClean,
		},
		{
			name:    "System Init",
			warning: (&frameworki.Step04SystemInit{}).RunWarnings,
			clean:   (&frameworki.Step04SystemInit{}).RunClean,
		},
	}

	// Run each step
	for i, step := range steps {
		fmt.Printf("\n=== Step %d/%d: %s ===\n\n", i+1, len(steps), step.name)

		// Run warnings
		if err := step.warning(state); err != nil {
			fatal("Warnings failed for %s: %v", step.name, err)
		}

		// Ask user if they want to run this step
		if !askYesNo(fmt.Sprintf("\nRun %s step now? [Y/n] ", step.name), "Y") {
			fmt.Printf("Skipping %s per user request\n", step.name)
			continue
		}

		// Run clean script
		fmt.Printf("\nRunning %s...\n", step.name)
		if err := step.clean(state); err != nil {
			warn("Step %s returned error: %v", step.name, err)
			if !askYesNo("\nContinue to next step anyway? [y/N] ", "y") {
				fatal("Installation aborted by user")
			}
		}

		fmt.Printf("\n[OK] %s completed\n", step.name)

		// Don't ask to continue after the last step (system-init will reboot)
		if i < len(steps)-1 {
			if !askYesNo("\nContinue to next step? [Y/n] ", "Y") {
				fmt.Println("Installation paused. Run again to continue.")
				os.Exit(0)
			}
		}
	}

	fmt.Println("\n=== All installation steps completed ===")
}

func runFrameworkDual() {
	fmt.Println("=== Guix Installation for Framework 13 (Dual-Boot) ===")
	fmt.Println()

	// Create state to hold all installation variables
	state := frameworkduali.NewState()

	// Define installation steps
	steps := []struct {
		name    string
		warning func(*frameworkduali.State) error
		clean   func(*frameworkduali.State) error
	}{
		{
			name:    "Partition Check",
			warning: (&frameworkduali.Step01PartitionCheck{}).RunWarnings,
			clean:   (&frameworkduali.Step01PartitionCheck{}).RunClean,
		},
		{
			name:    "Mount Existing",
			warning: (&frameworkduali.Step02MountExisting{}).RunWarnings,
			clean:   (&frameworkduali.Step02MountExisting{}).RunClean,
		},
		{
			name:    "Config Dual-Boot",
			warning: (&frameworkduali.Step03ConfigDualBoot{}).RunWarnings,
			clean:   (&frameworkduali.Step03ConfigDualBoot{}).RunClean,
		},
		{
			name:    "System Init",
			warning: (&frameworkduali.Step04SystemInit{}).RunWarnings,
			clean:   (&frameworkduali.Step04SystemInit{}).RunClean,
		},
	}

	// Run each step
	for i, step := range steps {
		fmt.Printf("\n=== Step %d/%d: %s ===\n\n", i+1, len(steps), step.name)

		// Run warnings
		if err := step.warning(state); err != nil {
			fatal("Warnings failed for %s: %v", step.name, err)
		}

		// Ask user if they want to run this step
		if !askYesNo(fmt.Sprintf("\nRun %s step now? [Y/n] ", step.name), "Y") {
			fmt.Printf("Skipping %s per user request\n", step.name)
			continue
		}

		// Run clean script
		fmt.Printf("\nRunning %s...\n", step.name)
		if err := step.clean(state); err != nil {
			warn("Step %s returned error: %v", step.name, err)
			if !askYesNo("\nContinue to next step anyway? [y/N] ", "y") {
				fatal("Installation aborted by user")
			}
		}

		fmt.Printf("\n[OK] %s completed\n", step.name)

		// Don't ask to continue after the last step (system-init will reboot)
		if i < len(steps)-1 {
			if !askYesNo("\nContinue to next step? [Y/n] ", "Y") {
				fmt.Println("Installation paused. Run again to continue.")
				os.Exit(0)
			}
		}
	}

	fmt.Println("\n=== All installation steps completed ===")
}

func askYesNo(prompt string, defaultYes string) bool {
	fmt.Fprint(os.Stderr, prompt)
	os.Stderr.Sync() // Ensure prompt is displayed before reading

	// Open /dev/tty to read directly from terminal (not stdin which may be redirected)
	tty, err := os.Open("/dev/tty")
	if err != nil {
		fmt.Fprintf(os.Stderr, "\n[ERROR] Failed to open /dev/tty: %v\n", err)
		fmt.Fprintf(os.Stderr, "[ERROR] Cannot proceed safely without user confirmation\n")
		os.Exit(1)
	}
	defer tty.Close()

	reader := bufio.NewReader(tty)
	answer, err := reader.ReadString('\n')

	if err != nil {
		fmt.Fprintf(os.Stderr, "\n[ERROR] Failed to read from terminal: %v\n", err)
		fmt.Fprintf(os.Stderr, "[ERROR] Cannot proceed safely without user confirmation\n")
		os.Exit(1)
	}

	answer = strings.TrimSpace(strings.ToLower(answer))

	if answer == "" {
		answer = strings.ToLower(defaultYes)
	}

	return answer == "y" || answer == "yes"
}

func warn(format string, args ...any) {
	fmt.Printf("\n\033[1;33m[warn]\033[0m "+format+"\n", args...)
}

func fatal(format string, args ...any) {
	fmt.Printf("\n\033[1;31m[err]\033[0m  "+format+"\n", args...)
	os.Exit(1)
}

func getEnv(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}
