package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/durantschoon/cloudzy-guix-install/framework-dual/install"
)

func main() {
	platform := getEnv("GUIX_PLATFORM", "cloudzy")

	if platform == "framework-dual" {
		runFrameworkDual()
	} else {
		fatal("Only framework-dual platform is currently supported in Go version.\nFor other platforms, use the bash version.")
	}
}

func runFrameworkDual() {
	fmt.Println("=== Guix Installation for Framework 13 (Dual-Boot) ===")
	fmt.Println()

	// Create state to hold all installation variables
	state := install.NewState()

	// Define installation steps
	steps := []struct {
		name    string
		warning func(*install.State) error
		clean   func(*install.State) error
	}{
		{
			name:    "Partition Check",
			warning: (&install.Step01PartitionCheck{}).RunWarnings,
			clean:   (&install.Step01PartitionCheck{}).RunClean,
		},
		{
			name:    "Mount Existing",
			warning: (&install.Step02MountExisting{}).RunWarnings,
			clean:   (&install.Step02MountExisting{}).RunClean,
		},
		{
			name:    "Config Dual-Boot",
			warning: (&install.Step03ConfigDualBoot{}).RunWarnings,
			clean:   (&install.Step03ConfigDualBoot{}).RunClean,
		},
		{
			name:    "System Init",
			warning: (&install.Step04SystemInit{}).RunWarnings,
			clean:   (&install.Step04SystemInit{}).RunClean,
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

		fmt.Printf("\n✓ %s completed\n", step.name)

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
