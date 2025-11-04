package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Word mapping for hex bytes (00-ff) to words
// Based on guid-in-words library by anton-bot (Unlicense/Public Domain)
var hexToWord = map[string]string{
	"00": "site", "01": "certain", "02": "friend", "03": "specific",
	"04": "section", "05": "risk", "06": "conference", "07": "individual",
	"08": "writer", "09": "represent", "0a": "moment", "0b": "owner",
	"0c": "report", "0d": "every", "0e": "life", "0f": "option",
	"10": "year", "11": "movement", "12": "beat", "13": "plan",
	"14": "reach", "15": "same", "16": "newspaper", "17": "debate",
	"18": "national", "19": "policy", "1a": "voice", "1b": "star",
	"1c": "hear", "1d": "use", "1e": "team", "1f": "possible",
	"20": "stand", "21": "provide", "22": "pay", "23": "develop",
	"24": "discussion", "25": "pick", "26": "mother", "27": "nature",
	"28": "argue", "29": "plant", "2a": "vote", "2b": "finally",
	"2c": "rule", "2d": "sister", "2e": "capital", "2f": "meet",
	"30": "have", "31": "security", "32": "economic", "33": "almost",
	"34": "rather", "35": "after", "36": "world", "37": "beyond",
	"38": "feel", "39": "machine", "3a": "author", "3b": "where",
	"3c": "light", "3d": "always", "3e": "dark", "3f": "address",
	"40": "house", "41": "expert", "42": "value", "43": "tough",
	"44": "table", "45": "indicate", "46": "necessary", "47": "sound",
	"48": "opportunity", "49": "budget", "4a": "range", "4b": "director",
	"4c": "sport", "4d": "mention", "4e": "idea", "4f": "certainly",
	"50": "kitchen", "51": "strategy", "52": "process", "53": "rock",
	"54": "service", "55": "fast", "56": "fail", "57": "big",
	"58": "woman", "59": "movie", "5a": "paper", "5b": "save",
	"5c": "board", "5d": "note", "5e": "pressure", "5f": "order",
	"60": "style", "61": "teach", "62": "maintain", "63": "buy",
	"64": "fire", "65": "determine", "66": "would", "67": "account",
	"68": "worry", "69": "money", "6a": "head", "6b": "watch",
	"6c": "parent", "6d": "interest", "6e": "young", "6f": "follow",
	"70": "none", "71": "call", "72": "sea", "73": "mind",
	"74": "example", "75": "south", "76": "difference", "77": "create",
	"78": "agent", "79": "full", "7a": "memory", "7b": "fill",
	"7c": "listen", "7d": "avoid", "7e": "serious", "7f": "economy",
	"80": "perform", "81": "decade", "82": "method", "83": "occur",
	"84": "society", "85": "say", "86": "increase", "87": "window",
	"88": "city", "89": "public", "8a": "write", "8b": "agency",
	"8c": "huge", "8d": "speech", "8e": "third", "8f": "commercial",
	"90": "tonight", "91": "already", "92": "pass", "93": "different",
	"94": "right", "95": "sometimes", "96": "alone", "97": "support",
	"98": "require", "99": "push", "9a": "maybe", "9b": "hospital",
	"9c": "clear", "9d": "try", "9e": "describe", "9f": "building",
	"a0": "free", "a1": "source", "a2": "apply", "a3": "finger",
	"a4": "customer", "a5": "manage", "a6": "artist", "a7": "form",
	"a8": "door", "a9": "likely", "aa": "open", "ab": "become",
	"ac": "surface", "ad": "inside", "ae": "prove", "af": "sign",
	"b0": "future", "b1": "glass", "b2": "career", "b3": "build",
	"b4": "leader", "b5": "story", "b6": "especially", "b7": "employee",
	"b8": "easy", "b9": "wonder", "ba": "college", "bb": "purpose",
	"bc": "science", "bd": "happen", "be": "something", "bf": "produce",
	"c0": "character", "c1": "property", "c2": "imagine", "c3": "entire",
	"c4": "smile", "c5": "mission", "c6": "such", "c7": "station",
	"c8": "yourself", "c9": "others", "ca": "trade", "cb": "period",
	"cc": "still", "cd": "product", "ce": "card", "cf": "past",
	"d0": "people", "d1": "indeed", "d2": "everybody", "d3": "final",
	"d4": "begin", "d5": "program", "d6": "suggest", "d7": "class",
	"d8": "bring", "d9": "campaign", "da": "cost", "db": "enjoy",
	"dc": "about", "dd": "situation", "de": "identify", "df": "name",
	"e0": "time", "e1": "key", "e2": "detail", "e3": "only",
	"e4": "from", "e5": "discuss", "e6": "green", "e7": "strong",
	"e8": "anything", "e9": "however", "ea": "part", "eb": "break",
	"ec": "truth", "ed": "party", "ee": "kind", "ef": "price",
	"f0": "popular", "f1": "prepare", "f2": "travel", "f3": "meeting",
	"f4": "admit", "f5": "best", "f6": "agreement", "f7": "win",
	"f8": "see", "f9": "current", "fa": "reality", "fb": "significant",
	"fc": "nice", "fd": "quickly", "fe": "professional", "ff": "culture",
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
