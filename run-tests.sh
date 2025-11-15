#!/bin/bash

# Test runner script for cloudzy-guix-install
# This script runs all tests to ensure the refactored code works correctly

set -e

echo "=== Running Tests for cloudzy-guix-install ==="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to run tests for a package
run_tests() {
    local package=$1
    local description=$2
    
    echo -e "${YELLOW}Testing $description...${NC}"
    echo "Package: $package"
    echo "----------------------------------------"
    
    if go test -v "$package"; then
        echo -e "${GREEN}✓ $description tests passed${NC}"
    else
        echo -e "${RED}✗ $description tests failed${NC}"
        return 1
    fi
    echo
}

# Test the common library
run_tests "./lib" "Common Library Functions"

# Test framework-dual install functions
run_tests "./framework-dual/install" "Framework Dual-Boot Install Functions"

# Test Guile config helper
if command -v guile &> /dev/null; then
    echo -e "${YELLOW}Testing Guile Config Helper...${NC}"
    echo "----------------------------------------"
    if framework-dual/postinstall/tests/run-guile-tests.sh; then
        echo -e "${GREEN}✓ Guile Config Helper tests passed${NC}"
    else
        echo -e "${RED}✗ Guile Config Helper tests failed${NC}"
        return 1
    fi
    echo
else
    echo -e "${YELLOW}⊘ Skipping Guile tests (guile not installed)${NC}"
    echo
fi

echo -e "${GREEN}=== All Tests Completed Successfully! ===${NC}"
echo
echo "Test Summary:"
echo "✓ Common library functions (MakePartitionPath, DetectDeviceFromState, etc.)"
echo "✓ Framework-dual integration tests"
echo "✓ String operations and error handling"
echo "✓ Function signatures and accessibility"
echo "✓ State management and persistence"
echo
echo "The refactored code is working correctly!"
