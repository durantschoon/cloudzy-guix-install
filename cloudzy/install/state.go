package install

import (
	"fmt"
	"os"
)

// State holds all installation variables shared between steps
type State struct {
	// Disk/partition info
	Device    string
	EFI       string
	Root      string
	SwapSize  string

	// User configuration
	UserName       string
	FullName       string
	Timezone       string
	HostName       string
	BootMode       string
	KeyboardLayout string // Layout and options, e.g., "us:ctrl:swapcaps"

	// Environment
	GuixPlatform string
}

// NewState creates a new State from environment variables
func NewState() *State {
	platform := getEnv("GUIX_PLATFORM", "cloudzy")
	
	// Validate platform matches this installer
	if platform != "cloudzy" && platform != "" {
		fmt.Printf("[WARN] GUIX_PLATFORM=%s but running cloudzy installer. Using 'cloudzy' instead.\n", platform)
		platform = "cloudzy"
	}
	
	return &State{
		Device:         os.Getenv("DEVICE"),
		EFI:            os.Getenv("EFI"),
		Root:           os.Getenv("ROOT"),
		UserName:       os.Getenv("USER_NAME"),
		FullName:       os.Getenv("FULL_NAME"),
		Timezone:       os.Getenv("TIMEZONE"),
		HostName:       os.Getenv("HOST_NAME"),
		BootMode:       os.Getenv("BOOT_MODE"),
		SwapSize:       os.Getenv("SWAP_SIZE"),
		KeyboardLayout: os.Getenv("KEYBOARD_LAYOUT"),
		GuixPlatform:   platform,
	}
}

func getEnv(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}
