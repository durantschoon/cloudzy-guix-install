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
	"cloudzy/install/01-partition-warnings.sh":     "32b81097e203d0e9f6c011a8ea9b02a9d988107b3c8356b788a611a3d009e952",
	"cloudzy/install/02-mount-bind-warnings.sh":    "bbe6b045f4ce8a66378825c2668b0a3de9c442fc1cc6e0708d4bb325c26797de",
	"cloudzy/install/03-config-write-warnings.sh":  "c15c7748692bc2455e22db8e04e196b7f83fc41800140bd33311dc69c06d2cf8",
	"cloudzy/install/04-system-init-warnings.sh":   "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"cloudzy/install/01-partition-clean.sh":        "f99bdab57a8ef7aa3f6ef83f563524a0fade6b22e07c4914ca58c3381bc05ec1",
	"cloudzy/install/02-mount-bind-clean.sh":       "6289b50678e9b80fe26181da1400966642f6b12b94523bc3e53b0a7aba80f632",
	"cloudzy/install/03-config-write-clean.sh":     "149c03c78650835bbddda5d47c6e6afa125bf5c0980278e0537789aaa969cff2",
	"cloudzy/install/04-system-init-clean.sh":      "45f56ee0002df99f03670f6e59bfde33c2b2ed96ff167c833c02809f17f277f8",

	// framework platform
	"framework/install/01-partition-warnings.sh":   "32b81097e203d0e9f6c011a8ea9b02a9d988107b3c8356b788a611a3d009e952",
	"framework/install/02-mount-bind-warnings.sh":  "bbe6b045f4ce8a66378825c2668b0a3de9c442fc1cc6e0708d4bb325c26797de",
	"framework/install/03-config-write-warnings.sh": "c15c7748692bc2455e22db8e04e196b7f83fc41800140bd33311dc69c06d2cf8",
	"framework/install/04-system-init-warnings.sh": "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"framework/install/01-partition-clean.sh":      "f99bdab57a8ef7aa3f6ef83f563524a0fade6b22e07c4914ca58c3381bc05ec1",
	"framework/install/02-mount-bind-clean.sh":     "6289b50678e9b80fe26181da1400966642f6b12b94523bc3e53b0a7aba80f632",
	"framework/install/03-config-write-clean.sh":   "149c03c78650835bbddda5d47c6e6afa125bf5c0980278e0537789aaa969cff2",
	"framework/install/04-system-init-clean.sh":    "45f56ee0002df99f03670f6e59bfde33c2b2ed96ff167c833c02809f17f277f8",

	// framework-dual platform
	"framework-dual/install/01-partition-check-warnings.sh": "d139a00815eb00637c808d8fa243c92b7b922639255b99c50ecbc40e12d4bff1",
	"framework-dual/install/02-mount-existing-warnings.sh":  "3a67c43528660ced1e27040c31a16669e56ef6676811f53ac1ef71c534555fda",
	"framework-dual/install/03-config-dual-boot-warnings.sh": "689a61109a9da4a4dab494d201ff359c25dc7b61cdbf6a3bf1b7fc3156d872eb",
	"framework-dual/install/04-system-init-warnings.sh":     "f9ce166f0b36201cc366ad224860f26f951bb228583704c2b9effdf7eb00a4ea",
	"framework-dual/install/01-partition-check-clean.sh":    "5a72ac5478b3e83a9cd4a3acce160f78efabedb40eea6543760fda66e3ddb84b",
	"framework-dual/install/02-mount-existing-clean.sh":     "88b350588411e7a2239c5849bb7b963037e8347e040efd0a6efaf1d21b1f339d",
	"framework-dual/install/03-config-dual-boot-clean.sh":   "280831ca3d56e207391b6b9a33bde7990d7393586891a7cfdb723339c1d5c151",
	"framework-dual/install/04-system-init-clean.sh":        "45f56ee0002df99f03670f6e59bfde33c2b2ed96ff167c833c02809f17f277f8",
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

	// Verify checksum (skip for main branch)
	if cfg.Ref != "main" {
		if expected, ok := checksums[relPath]; ok {
			actual := fmt.Sprintf("%x", sha256.Sum256(data))
			if actual != expected {
				return fmt.Errorf("SHA256 mismatch: expected %s, got %s", expected, actual)
			}
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

	// Run both scripts in same bash session using source
	// This preserves exports from warnings script for clean script
	bashCmd := fmt.Sprintf("source %s && source %s", warningPath, cleanPath)
	cmd := exec.Command("bash", "-c", bashCmd)
	cmd.Stdin = os.Stdin
	cmd.Stdout = io.MultiWriter(os.Stdout, logFile)
	cmd.Stderr = io.MultiWriter(os.Stderr, logFile)

	// Inherit environment variables
	cmd.Env = os.Environ()

	err = cmd.Run()

	msg("Exit status for script pair: %v", cmd.ProcessState.ExitCode())
	fmt.Printf("Log saved to: %s\n", logPath)

	// Show last 40 lines of log
	showLogTail(logPath, 40)

	if !askYes("Continue to next step?") {
		os.Exit(cmd.ProcessState.ExitCode())
	}

	return err
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
