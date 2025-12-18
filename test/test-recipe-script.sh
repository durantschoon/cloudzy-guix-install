#!/usr/bin/env bash
# Test runner for postinstall recipe scripts
# Tests a Guile recipe script against sample config.scm files
# Note: This uses /usr/bin/env for development testing on non-Guix systems

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FIXTURES_DIR="$SCRIPT_DIR/fixtures/postinstall-recipes"
TEMP_DIR="$SCRIPT_DIR/tmp"

# Usage
if [ $# -lt 1 ]; then
    echo "Usage: $0 <recipe-script.scm> [fixture-name]"
    echo ""
    echo "Available fixtures:"
    echo "  minimal          - Minimal config with %base-packages"
    echo "  existing         - Config with some packages already"
    echo "  has-fonts        - Config with some fonts already"
    echo "  all              - Test against all fixtures (default)"
    echo ""
    echo "Example:"
    echo "  $0 ../postinstall/recipes/add-fonts.scm minimal"
    echo "  $0 ../postinstall/recipes/add-development.scm all"
    exit 1
fi

RECIPE_SCRIPT="$1"
FIXTURE="${2:-all}"

if [ ! -f "$RECIPE_SCRIPT" ]; then
    echo -e "${RED}[ERROR]${NC} Recipe script not found: $RECIPE_SCRIPT"
    exit 1
fi

SCRIPT_NAME=$(basename "$RECIPE_SCRIPT" .scm)

# Create temp directory
mkdir -p "$TEMP_DIR"

# Function to test against a fixture
test_fixture() {
    local fixture_name="$1"
    local fixture_file="$FIXTURES_DIR/${fixture_name}-config.scm"

    if [ ! -f "$fixture_file" ]; then
        echo -e "${RED}[ERROR]${NC} Fixture not found: $fixture_file"
        return 1
    fi

    echo ""
    echo "========================================="
    echo "Testing: $SCRIPT_NAME"
    echo "Fixture: $fixture_name"
    echo "========================================="

    # Copy fixture to temp location
    local temp_config="$TEMP_DIR/${fixture_name}-config.scm"
    cp "$fixture_file" "$temp_config"

    # Show original config
    echo ""
    echo "--- Original Config ---"
    cat "$temp_config"

    # Run the recipe script
    echo ""
    echo "--- Running Recipe Script ---"
    if CONFIG_FILE="$temp_config" guile --no-auto-compile "$RECIPE_SCRIPT"; then
        echo -e "${GREEN}[OK]${NC} Script executed successfully"
    else
        echo -e "${RED}[FAIL]${NC} Script execution failed"
        return 1
    fi

    # Show modified config
    echo ""
    echo "--- Modified Config ---"
    cat "$temp_config"

    # Validate the output is valid Scheme
    echo ""
    echo "--- Validating Scheme Syntax ---"
    if guile --no-auto-compile -c "(use-modules (ice-9 pretty-print)) (call-with-input-file \"$temp_config\" (lambda (port) (let loop () (let ((expr (read port))) (if (eof-object? expr) (display \"Valid Scheme\\n\") (loop))))))" 2>&1; then
        echo -e "${GREEN}[OK]${NC} Output is valid Scheme"
    else
        echo -e "${RED}[FAIL]${NC} Output is NOT valid Scheme"
        echo "This means the script corrupted the config file!"
        return 1
    fi

    # Show diff
    echo ""
    echo "--- Diff (Original vs Modified) ---"
    diff -u "$fixture_file" "$temp_config" || true

    echo ""
    echo -e "${GREEN}[PASS]${NC} Test passed for fixture: $fixture_name"
    return 0
}

# Run tests
if [ "$FIXTURE" = "all" ]; then
    # Test against all fixtures
    passed=0
    failed=0

    for fixture_file in "$FIXTURES_DIR"/*-config.scm; do
        fixture_name=$(basename "$fixture_file" -config.scm)

        if test_fixture "$fixture_name"; then
            ((passed++))
        else
            ((failed++))
        fi
    done

    echo ""
    echo "========================================="
    echo "Test Summary"
    echo "========================================="
    echo -e "Passed: ${GREEN}$passed${NC}"
    echo -e "Failed: ${RED}$failed${NC}"
    echo "========================================="

    if [ $failed -gt 0 ]; then
        exit 1
    fi
else
    # Test against single fixture
    test_fixture "$FIXTURE"
fi

echo ""
echo -e "${GREEN}[SUCCESS]${NC} All tests passed!"
