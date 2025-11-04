package main

import (
	"fmt"
	"strings"
	"testing"
)

func TestHashToWords(t *testing.T) {
	tests := []struct {
		name        string
		hash        string
		expectError bool
		expectedLen int // Expected number of words (hash length / 2)
		firstWord   string
		lastWord    string
	}{
		{
			name:        "Valid SHA256 hash (64 chars)",
			hash:        "2c3fb0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
			expectError: false,
			expectedLen: 31, // 62 hex chars = 31 bytes
			firstWord:   "rule",
			lastWord:    "bring",
		},
		{
			name:        "Valid short hash (4 chars = 2 bytes)",
			hash:        "2c3f",
			expectError: false,
			expectedLen: 2,
			firstWord:   "rule",
			lastWord:    "address",
		},
		{
			name:        "Valid hash with uppercase",
			hash:        "2C3FB0A1",
			expectError: false,
			expectedLen: 4,
			firstWord:   "rule",
			lastWord:    "source",
		},
		{
			name:        "Valid hash with whitespace",
			hash:        "  2c3f  \n",
			expectError: false,
			expectedLen: 2,
			firstWord:   "rule",
			lastWord:    "address",
		},
		{
			name:        "Empty hash",
			hash:        "",
			expectError: false,
			expectedLen: 0,
			firstWord:   "",
			lastWord:    "",
		},
		{
			name:        "Odd length hash",
			hash:        "2c3",
			expectError: true,
		},
		{
			name:        "Invalid hex character",
			hash:        "2c3g",
			expectError: true,
		},
		{
			name:        "Invalid hex byte",
			hash:        "2c3z",
			expectError: true,
		},
		{
			name:        "All zeros",
			hash:        "00000000",
			expectError: false,
			expectedLen: 4,
			firstWord:   "site",
			lastWord:    "site",
		},
		{
			name:        "All Fs",
			hash:        "ffffffff",
			expectError: false,
			expectedLen: 4,
			firstWord:   "culture",
			lastWord:    "culture",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := hashToWords(tt.hash)

			if tt.expectError {
				if err == nil {
					t.Errorf("hashToWords(%q) expected error but got none", tt.hash)
				}
				return
			}

			if err != nil {
				t.Errorf("hashToWords(%q) unexpected error: %v", tt.hash, err)
				return
			}

			words := strings.Fields(result)
			if len(words) != tt.expectedLen {
				t.Errorf("hashToWords(%q) expected %d words, got %d: %q", tt.hash, tt.expectedLen, len(words), result)
			}

			if tt.expectedLen > 0 {
				if tt.firstWord != "" && words[0] != tt.firstWord {
					t.Errorf("hashToWords(%q) first word: got %q, want %q", tt.hash, words[0], tt.firstWord)
				}
				if tt.lastWord != "" && words[len(words)-1] != tt.lastWord {
					t.Errorf("hashToWords(%q) last word: got %q, want %q", tt.hash, words[len(words)-1], tt.lastWord)
				}
			}
		})
	}
}

func TestHashToWords_AllHexBytes(t *testing.T) {
	// Verify all 256 hex bytes (00-ff) map to words
	// This ensures the embedded JSON was parsed correctly
	for i := 0; i < 256; i++ {
		hexByte := strings.ToLower(fmt.Sprintf("%02x", i))
		hash := hexByte + hexByte // Create a 4-char hash for testing

		result, err := hashToWords(hash)
		if err != nil {
			t.Errorf("hashToWords(%q) failed for hex byte %02x: %v", hash, i, err)
			continue
		}

		words := strings.Fields(result)
		if len(words) != 2 {
			t.Errorf("hashToWords(%q) expected 2 words, got %d", hash, len(words))
			continue
		}

		// Both words should be the same (same byte repeated)
		if words[0] != words[1] {
			t.Errorf("hashToWords(%q) expected same word for repeated byte, got %q and %q", hash, words[0], words[1])
		}

		// Word should not be empty
		if words[0] == "" {
			t.Errorf("hashToWords(%q) returned empty word for hex byte %02x", hash, i)
		}
	}
}

func TestHashToWords_Consistency(t *testing.T) {
	// Test that same hash produces same output
	hash := "2c3fb0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8"

	result1, err1 := hashToWords(hash)
	if err1 != nil {
		t.Fatalf("hashToWords(%q) failed: %v", hash, err1)
	}

	result2, err2 := hashToWords(hash)
	if err2 != nil {
		t.Fatalf("hashToWords(%q) failed on second call: %v", hash, err2)
	}

	if result1 != result2 {
		t.Errorf("hashToWords(%q) inconsistent results:\n  First:  %q\n  Second: %q", hash, result1, result2)
	}
}

func TestHashToWords_CaseInsensitive(t *testing.T) {
	// Test that uppercase and lowercase produce same result
	lowerHash := "2c3fb0a1"
	upperHash := "2C3FB0A1"

	lowerResult, err1 := hashToWords(lowerHash)
	if err1 != nil {
		t.Fatalf("hashToWords(%q) failed: %v", lowerHash, err1)
	}

	upperResult, err2 := hashToWords(upperHash)
	if err2 != nil {
		t.Fatalf("hashToWords(%q) failed: %v", upperHash, err2)
	}

	if lowerResult != upperResult {
		t.Errorf("hashToWords case insensitive mismatch:\n  Lower: %q\n  Upper: %q", lowerResult, upperResult)
	}
}

func TestInit_EmbeddedJSON(t *testing.T) {
	// Test that the embedded JSON was parsed correctly
	// This is implicitly tested by other tests, but we can verify the map is populated
	if hexToWord == nil {
		t.Fatal("hexToWord map is nil - init() may have failed")
	}

	if len(hexToWord) != 256 {
		t.Errorf("hexToWord map should have 256 entries (00-ff), got %d", len(hexToWord))
	}

	// Test a few specific mappings to ensure JSON parsing worked
	expectedMappings := map[string]string{
		"00": "site",
		"01": "certain",
		"ff": "culture",
		"2c": "rule",
		"3f": "address",
	}

	for hex, expectedWord := range expectedMappings {
		actualWord, ok := hexToWord[hex]
		if !ok {
			t.Errorf("hexToWord missing mapping for %q", hex)
			continue
		}
		if actualWord != expectedWord {
			t.Errorf("hexToWord[%q] = %q, want %q", hex, actualWord, expectedWord)
		}
	}
}

