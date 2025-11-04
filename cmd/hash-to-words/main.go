package main

import (
	"bufio"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

//go:embed words.json
var wordsJSON []byte

// Word mapping for hex bytes (00-ff) to words
// Word list embedded from upstream: https://github.com/anton-bot/guid-in-words
// License: Unlicense/Public Domain
var hexToWord map[string]string

func init() {
	// Parse the embedded JSON file
	var data struct {
		Data map[string]string `json:"data"`
	}
	
	if err := json.Unmarshal(wordsJSON, &data); err != nil {
		// If JSON parsing fails, we can't continue
		// This should never happen with embedded file, but handle it gracefully
		fmt.Fprintf(os.Stderr, "Fatal error: failed to parse embedded words.json: %v\n", err)
		os.Exit(1)
	}
	
	hexToWord = data.Data
}

func hashToWords(hash string) (string, error) {
	// Remove any whitespace or newlines
	hash = strings.TrimSpace(hash)

	// Validate hash length (should be even number of hex chars)
	if len(hash)%2 != 0 {
		return "", fmt.Errorf("invalid hash: length must be even number of hex characters")
	}

	// Convert to lowercase for consistent lookup
	hash = strings.ToLower(hash)

	// Split into byte pairs and convert to words
	var words []string
	for i := 0; i < len(hash); i += 2 {
		byteStr := hash[i : i+2]
		word, ok := hexToWord[byteStr]
		if !ok {
			return "", fmt.Errorf("invalid hex byte: %s", byteStr)
		}
		words = append(words, word)
	}

	return strings.Join(words, " "), nil
}

func main() {
	var hash string

	// Check if hash is provided as argument
	if len(os.Args) > 1 {
		hash = os.Args[1]
	} else {
		// Read from stdin
		scanner := bufio.NewScanner(os.Stdin)
		if scanner.Scan() {
			hash = scanner.Text()
		}
		if err := scanner.Err(); err != nil {
			fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
			os.Exit(1)
		}
	}

	if hash == "" {
		fmt.Fprintf(os.Stderr, "Usage: hash-to-words <hash>\n")
		fmt.Fprintf(os.Stderr, "   or: echo <hash> | hash-to-words\n")
		os.Exit(1)
	}

	words, err := hashToWords(hash)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(words)
}
