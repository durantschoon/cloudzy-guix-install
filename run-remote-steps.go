package main

import (
	"bufio"
	"crypto/sha256"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

const (
	defaultOwnerRepo = "durantschoon/cloudzy-guix-install"
	defaultRef       = "v0.1.5"
)

// Script checksums (generated from update-sha256.sh)
var checksums = map[string]string{
	// cloudzy platform
	"cloudzy/install/01-partition-warnings.sh": "67212d93cc487ddb0f99d4e2295031c89203e4ef99d7fc928a1c288a264a4b63",
	"cloudzy/install/02-mount-bind-warnings.sh": "bbe6b045f4ce8a66378825c2668b0a3de9c442fc1cc6e0708d4bb325c26797de",
	"cloudzy/install/03-config-write-warnings.sh": "c15c7748692bc2455e22db8e04e196b7f83fc41800140bd33311dc69c06d2cf8",
	"cloudzy/install/04-system-init-warnings.sh": "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"cloudzy/install/01-partition-clean.sh": "77bd7057c16b90d0a9174ad285f64590e1c68cf7d6f7d76be91c272c99a259a9",
	"cloudzy/install/02-mount-bind-clean.sh": "4bf7267947e1dea213fae437d96b05cc497bfcc250e2afc3deb00ad7b1b9234d",
	"cloudzy/install/03-config-write-clean.sh": "38b278a5b066f21b9c8dc71adc42f1d37c020330a1ea4b65bcf19463125f0f3a",
	"cloudzy/install/04-system-init-clean.sh": "dbb6273402eea33040d7766317f8c4bb4fa8833e58cccaf8c80364507595db91",

	// framework platform
	"framework/install/01-partition-warnings.sh": "67212d93cc487ddb0f99d4e2295031c89203e4ef99d7fc928a1c288a264a4b63",
	"framework/install/02-mount-bind-warnings.sh": "bbe6b045f4ce8a66378825c2668b0a3de9c442fc1cc6e0708d4bb325c26797de",
	"framework/install/03-config-write-warnings.sh": "c15c7748692bc2455e22db8e04e196b7f83fc41800140bd33311dc69c06d2cf8",
	"framework/install/04-system-init-warnings.sh": "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"framework/install/01-partition-clean.sh": "77bd7057c16b90d0a9174ad285f64590e1c68cf7d6f7d76be91c272c99a259a9",
	"framework/install/02-mount-bind-clean.sh": "4bf7267947e1dea213fae437d96b05cc497bfcc250e2afc3deb00ad7b1b9234d",
	"framework/install/03-config-write-clean.sh": "38b278a5b066f21b9c8dc71adc42f1d37c020330a1ea4b65bcf19463125f0f3a",
	"framework/install/04-system-init-clean.sh": "dbb6273402eea33040d7766317f8c4bb4fa8833e58cccaf8c80364507595db91",

	// framework-dual platform
	"framework-dual/install/01-partition-check-warnings.sh": "f372c4bbf736e21efd1442071a1864797e7af1667efd4fc440262017b55651e5",
	"framework-dual/install/02-mount-existing-warnings.sh": "05d1d21c317e22e5d241cfadbc02b78c465e4cdd06abb6195974fe16689ae841",
	"framework-dual/install/03-config-dual-boot-warnings.sh": "656b5c2722eae551c576c1086077f98a28762c8024e2d36dfe0b53f786be48e0",
	"framework-dual/install/04-system-init-warnings.sh": "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"framework-dual/install/01-partition-check-clean.sh": "d1fac6aa5bed38e54cf408e75f2e4d92e26384f41c4fe444802d3fc87813c779",
	"framework-dual/install/02-mount-existing-clean.sh": "ef36ce4c00c93cb0008f879d91b4e62d3cd58d7ae03827d955912edd76983149",
	"framework-dual/install/03-config-dual-boot-clean.sh": "190890a38b1c109619f202001bf8ac168fbdc61ce6a82140c428cdbddda7c74f",
	"framework-dual/install/04-system-init-clean.sh": "dbb6273402eea33040d7766317f8c4bb4fa8833e58cccaf8c80364507595db91",

}

// Platform script sequences
var platformScripts = map[string][]string{
	"cloudzy": {
		"cloudzy/install/01-partition",
		"cloudzy/install/02-mount-bind",
		"cloudzy/install/03-config-write",
		"cloudzy/install/04-system-init",
	},
	"framework": {
		"framework/install/01-partition",
		"framework/install/02-mount-bind",
		"framework/install/03-config-write",
		"framework/install/04-system-init",
	},
	"framework-dual": {
		"framework-dual/install/01-partition-check",
		"framework-dual/install/02-mount-existing",
		"framework-dual/install/03-config-dual-boot",
		"framework-dual/install/04-system-init",
	},
}

type Config struct {
	OwnerRepo string
	Ref       string
	Platform  string
	WorkDir   string
	LogDir    string
}

func main() {
	// Get configuration from environment
	cfg := Config{
		OwnerRepo: getEnv("GUIX_INSTALL_REPO", defaultOwnerRepo),
		Ref:       getEnv("GUIX_INSTALL_REF", defaultRef),
		Platform:  getEnv("GUIX_PLATFORM", "cloudzy"),
	}

	// Create working directories in current directory
	cwd, err := os.Getwd()
	if err != nil {
		fatal("Failed to get current directory: %v", err)
	}
	cfg.WorkDir = filepath.Join(cwd, "guix-install-scripts")
	cfg.LogDir = filepath.Join(cfg.WorkDir, "logs")

	if err := os.MkdirAll(cfg.LogDir, 0755); err != nil {
		fatal("Failed to create log directory: %v", err)
	}

	msg("Workdir: %s", cfg.WorkDir)
	msg("Fetching from: https://raw.githubusercontent.com/%s/%s", cfg.OwnerRepo, cfg.Ref)

	// Validate platform
	scripts, ok := platformScripts[cfg.Platform]
	if !ok {
		fatal("Unsupported platform: %s\nSupported platforms: cloudzy, framework, framework-dual", cfg.Platform)
	}

	msg("Selected platform: %s", cfg.Platform)
	msg("Script sequence: %v", scripts)

	// Download shared library files first
	msg("Fetching shared library files")
	libFiles := []string{"lib/common.sh", "lib/mirrors.sh", "lib/runner-common.sh"}
	for _, lib := range libFiles {
		if err := fetchFile(cfg, lib); err != nil {
			fatal("Failed to fetch %s: %v", lib, err)
		}
	}

	// Process each script
	for _, base := range scripts {
		warningScript := base + "-warnings.sh"
		cleanScript := base + "-clean.sh"

		// Fetch warning script
		msg("Fetch %s", warningScript)
		if err := fetchFile(cfg, warningScript); err != nil {
			fatal("Failed to fetch %s: %v", warningScript, err)
		}

		// Fetch clean script
		msg("Fetch %s", cleanScript)
		if err := fetchFile(cfg, cleanScript); err != nil {
			fatal("Failed to fetch %s: %v", cleanScript, err)
		}

		// Preview clean script
		previewScript(cfg, cleanScript)

		// Ask user if they want to run this step
		if !askYes("Run %s step now?", base) {
			warn("Skipping %s per user request", base)
			continue
		}

		// Run warning and clean scripts in same bash session to preserve exports
		msg("Running warnings and clean scripts for %s", base)
		if err := runScriptPair(cfg, warningScript, cleanScript); err != nil {
			warn("Script pair returned non-zero: %v", err)
		}
	}

	msg("All done. Logs in: %s", cfg.LogDir)
}

func fetchFile(cfg Config, relPath string) error {
	url := fmt.Sprintf("https://raw.githubusercontent.com/%s/%s/%s?%d",
		cfg.OwnerRepo, cfg.Ref, relPath, time.Now().Unix())

	destPath := filepath.Join(cfg.WorkDir, relPath)

	// Create parent directories
	if err := os.MkdirAll(filepath.Dir(destPath), 0755); err != nil {
		return fmt.Errorf("mkdir failed: %w", err)
	}

	// Download file
	resp, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("HTTP GET failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return fmt.Errorf("HTTP %d: %s", resp.StatusCode, resp.Status)
	}

	// Write to file
	out, err := os.Create(destPath)
	if err != nil {
		return fmt.Errorf("create file failed: %w", err)
	}
	defer out.Close()

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("read body failed: %w", err)
	}

	if _, err := out.Write(data); err != nil {
		return fmt.Errorf("write file failed: %w", err)
	}

	// Make executable
	if err := os.Chmod(destPath, 0755); err != nil {
		return fmt.Errorf("chmod failed: %w", err)
	}

	// Verify checksum
	if expected, ok := checksums[relPath]; ok {
		actual := fmt.Sprintf("%x", sha256.Sum256(data))
		if actual != expected {
			return fmt.Errorf("SHA256 mismatch: expected %s, got %s", expected, actual)
		}
	}

	return nil
}

func previewScript(cfg Config, relPath string) {
	scriptPath := filepath.Join(cfg.WorkDir, relPath)

	fmt.Printf("---- %s (head) ----\n", relPath)

	file, err := os.Open(scriptPath)
	if err != nil {
		warn("Failed to open for preview: %v", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineCount := 0
	for scanner.Scan() && lineCount < 30 {
		fmt.Println(scanner.Text())
		lineCount++
	}

	fmt.Println("-----------------------")
}

func runScriptPair(cfg Config, warningScript, cleanScript string) error {
	warningPath := filepath.Join(cfg.WorkDir, warningScript)
	cleanPath := filepath.Join(cfg.WorkDir, cleanScript)
	logPath := filepath.Join(cfg.LogDir, filepath.Base(cleanScript)+".combined.log")

	// Check if scripts exist
	if _, err := os.Stat(warningPath); err != nil {
		return fmt.Errorf("warning script not found: %s (error: %w)", warningPath, err)
	}
	if _, err := os.Stat(cleanPath); err != nil {
		return fmt.Errorf("clean script not found: %s (error: %w)", cleanPath, err)
	}

	// Create log file
	logFile, err := os.Create(logPath)
	if err != nil {
		return fmt.Errorf("create log failed: %w", err)
	}
	defer logFile.Close()

	fmt.Printf("Executing script pair in same bash session...\n")

	// Capture output to parse variables
	var outputBuf strings.Builder
	multiWriter := io.MultiWriter(os.Stdout, logFile, &outputBuf)

	// Run both scripts in same bash session using source
	// Prepend any captured variables from previous script pair
	bashCmd := fmt.Sprintf("source %s && source %s", warningPath, cleanPath)
	if capturedVars != "" {
		bashCmd = capturedVars + " " + bashCmd
		fmt.Printf("Running with captured vars: %s\n", capturedVars)
	}
	cmd := exec.Command("bash", "-c", bashCmd)
	cmd.Stdin = os.Stdin
	cmd.Stdout = multiWriter
	cmd.Stderr = io.MultiWriter(os.Stderr, logFile)

	// Inherit environment variables
	cmd.Env = os.Environ()

	err = cmd.Run()

	// Parse output for exported variables
	parseAndSetVars(outputBuf.String())

	msg("Exit status for script pair: %v", cmd.ProcessState.ExitCode())
	fmt.Printf("Log saved to: %s\n", logPath)

	// Show last 40 lines of log
	showLogTail(logPath, 40)

	if !askYes("Continue to next step?") {
		os.Exit(cmd.ProcessState.ExitCode())
	}

	return err
}

var capturedVars string

func parseAndSetVars(output string) {
	// Look for the marker line followed by variable assignments
	lines := strings.Split(output, "\n")

	for i, line := range lines {
		if strings.TrimSpace(line) == "###GUIX_INSTALL_VARS###" {
			// Next line contains the variable assignments
			if i+1 < len(lines) {
				capturedVars = strings.TrimSpace(lines[i+1])
				fmt.Printf("Captured variables: %s\n", capturedVars)
			}
			return
		}
	}
}

func runScript(cfg Config, relPath string, isWarning bool) error {
	scriptPath := filepath.Join(cfg.WorkDir, relPath)
	logPath := filepath.Join(cfg.LogDir, filepath.Base(relPath)+".log")

	// Check if script exists
	if _, err := os.Stat(scriptPath); err != nil {
		return fmt.Errorf("script not found: %s (error: %w)", scriptPath, err)
	}

	// Create log file
	logFile, err := os.Create(logPath)
	if err != nil {
		return fmt.Errorf("create log failed: %w", err)
	}
	defer logFile.Close()

	fmt.Printf("Executing: bash %s\n", scriptPath)

	// Run script with bash (use PATH to find bash, not hardcoded /bin/bash)
	cmd := exec.Command("bash", scriptPath)
	cmd.Stdin = os.Stdin
	cmd.Stdout = io.MultiWriter(os.Stdout, logFile)
	cmd.Stderr = io.MultiWriter(os.Stderr, logFile)

	// Inherit environment variables
	cmd.Env = os.Environ()

	err = cmd.Run()

	msg("Exit status for %s: %v", filepath.Base(relPath), cmd.ProcessState.ExitCode())
	fmt.Printf("Log saved to: %s\n", logPath)

	// Show last 40 lines of log
	showLogTail(logPath, 40)

	if !askYes("Continue to next step?") {
		os.Exit(cmd.ProcessState.ExitCode())
	}

	return err
}

func showLogTail(logPath string, lines int) {
	fmt.Println("---- last 40 lines ----")

	cmd := exec.Command("tail", "-n", fmt.Sprintf("%d", lines), logPath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Run()

	fmt.Println("-----------------------")
}

func askYes(format string, args ...interface{}) bool {
	fmt.Printf("\n"+format+" [Y/n] ", args...)

	reader := bufio.NewReader(os.Stdin)
	answer, _ := reader.ReadString('\n')
	answer = strings.TrimSpace(answer)

	return answer == "" || strings.ToLower(answer) == "y"
}

func msg(format string, args ...interface{}) {
	fmt.Printf("\n\033[1;34m==> "+format+"\033[0m\n", args...)
}

func warn(format string, args ...interface{}) {
	fmt.Printf("\n\033[1;33m[warn]\033[0m "+format+"\n", args...)
}

func fatal(format string, args ...interface{}) {
	fmt.Printf("\n\033[1;31m[err]\033[0m  "+format+"\n", args...)
	os.Exit(1)
}

func getEnv(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}
