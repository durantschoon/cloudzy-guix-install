package lib

import "fmt"

// ANSI color codes for terminal output
const (
	// Reset
	ColorReset = "\033[0m"

	// Subtle background colors for step distinction
	BgLightBlue   = "\033[48;5;153m\033[38;5;0m" // Light blue background, black text
	BgLightGreen  = "\033[48;5;158m\033[38;5;0m" // Light green background, black text
	BgLightYellow = "\033[48;5;229m\033[38;5;0m" // Light yellow background, black text
	BgLightCyan   = "\033[48;5;195m\033[38;5;0m" // Light cyan background, black text

	// Foreground colors for status messages
	FgGreen  = "\033[32m"
	FgYellow = "\033[33m"
	FgRed    = "\033[31m"
	FgCyan   = "\033[36m"
	FgBold   = "\033[1m"
)

// StepColors maps step numbers to background colors
var StepColors = map[int]string{
	1: BgLightBlue,
	2: BgLightGreen,
	3: BgLightYellow,
	4: BgLightCyan,
}

// PrintStepHeader prints a colored step header
func PrintStepHeader(stepNum int, title string) {
	color := StepColors[stepNum]
	if color == "" {
		color = ColorReset
	}
	fmt.Printf("%s=== Step %d: %s ===%s\n", color, stepNum, title, ColorReset)
}

// PrintSuccess prints a success message
func PrintSuccess(msg string) {
	fmt.Printf("%s[OK]%s %s\n", FgGreen, ColorReset, msg)
}

// PrintWarning prints a warning message
func PrintWarning(msg string) {
	fmt.Printf("%s[WARN]%s %s\n", FgYellow, ColorReset, msg)
}

// PrintError prints an error message
func PrintError(msg string) {
	fmt.Printf("%s[ERROR]%s %s\n", FgRed, ColorReset, msg)
}

// PrintInfo prints an info message
func PrintInfo(msg string) {
	fmt.Printf("%s[INFO]%s %s\n", FgCyan, ColorReset, msg)
}

// SubsectionColors are the colors to cycle through for subsection headers
var SubsectionColors = []string{
	BgLightBlue,
	BgLightGreen,
	BgLightYellow,
	BgLightCyan,
}

var subsectionColorIndex = 0

// PrintSectionHeader prints a colored section header (cycles through colors)
func PrintSectionHeader(title string) {
	color := SubsectionColors[subsectionColorIndex%len(SubsectionColors)]
	subsectionColorIndex++
	fmt.Printf("%s=== %s ===%s\n", color, title, ColorReset)
}
