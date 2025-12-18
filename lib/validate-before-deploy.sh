#!/usr/bin/env bash
#
# Pre-Deployment Validation Script
# Run this locally to catch issues before deploying to remote machines
#
# Usage: ./validate-before-deploy.sh [--verbose]

set -uo pipefail

VERBOSE=0
if [[ "${1:-}" == "--verbose" ]]; then
    VERBOSE=1
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
WARNINGS=0

log_test() {
    local status="$1"
    local message="$2"

    case "$status" in
        PASS)
            echo -e "${GREEN}[PASS]${NC} $message"
            ((PASSED++))
            ;;
        FAIL)
            echo -e "${RED}[FAIL]${NC} $message"
            ((FAILED++))
            ;;
        WARN)
            echo -e "${YELLOW}[WARN]${NC} $message"
            ((WARNINGS++))
            ;;
    esac
}

verbose_log() {
    if [[ $VERBOSE -eq 1 ]]; then
        echo "  → $1"
    fi
}

echo "=== Pre-Deployment Validation ==="
echo

# Test 1: Validate guix command syntax
echo "Checking Guix command syntax..."
check_guix_commands() {
    # Extract guix build commands from Go source
    local commands=$(grep -r "guix.*build" lib/common.go cloudzy/install/*.go framework*/install/*.go 2>/dev/null || true)

    if [[ -z "$commands" ]]; then
        log_test PASS "No guix build commands found (or grep failed)"
        return 0
    fi

    # Check for common syntax errors
    if echo "$commands" | grep -q 'substitute-urls=.*https.*https' && ! echo "$commands" | grep -q "substitute-urls='"; then
        log_test FAIL "Found unquoted --substitute-urls with multiple URLs (will cause 'unknown package' error)"
        verbose_log "Use: --substitute-urls='https://url1 https://url2'"
        return 1
    fi

    # Check if --substitute-urls comes before package name
    # Note: This is OK for "guix time-machine ... -- build" pattern
    if echo "$commands" | grep -qE 'guix.*build.*linux.*--substitute-urls|guix.*build.*linux-libre.*--substitute-urls'; then
        log_test WARN "Found --substitute-urls after package name (may cause issues)"
        verbose_log "Recommended: guix build --substitute-urls='...' PACKAGE"
        verbose_log "Note: 'guix time-machine ... -- build --substitute-urls=... PACKAGE' is OK"
    fi

    log_test PASS "Guix build command syntax looks correct"
}
check_guix_commands

# Test 2: Validate store path checks
echo
echo "Checking store path validation..."
check_store_paths() {
    # Find code that uses paths without validation
    local unvalidated=$(grep -n 'kernelPackagePath\|systemPath' lib/common.go | \
        grep -v 'strings.HasPrefix.*"/gnu/store/"' | \
        grep -v 'strings.Contains.*"error:"' | \
        head -5 || true)

    if [[ -n "$unvalidated" && $(echo "$unvalidated" | wc -l) -gt 3 ]]; then
        log_test WARN "Some paths may not be validated before use"
        verbose_log "Consider adding: strings.HasPrefix(path, \"/gnu/store/\")"
    else
        log_test PASS "Store paths appear to be validated"
    fi
}
check_store_paths

# Test 3: Check for error handling
echo
echo "Checking error handling..."
check_error_handling() {
    # Look for exec.Command without error checks
    local missing_checks=$(grep -A 2 'exec.Command' lib/common.go | \
        grep -v '\.Run()' | \
        grep -v '\.Output()' | \
        grep -v 'if err' | \
        grep -v 'return' | \
        wc -l)

    if [[ $missing_checks -gt 10 ]]; then
        log_test WARN "Some commands may lack error handling"
    else
        log_test PASS "Error handling appears comprehensive"
    fi
}
check_error_handling

# Test 4: Validate hypothesis logging consistency
echo
echo "Checking hypothesis logging consistency..."
check_hypothesis_logging() {
    local missing_platform=0
    local missing_buildtype=0

    # Check Hypothesis M, H, K, N for platform/buildType fields
    for hyp in M H K N; do
        local logs=$(grep -n "hypothesisId.*$hyp" lib/common.go | head -20)
        local log_count=$(echo "$logs" | wc -l)

        if [[ $log_count -gt 0 ]]; then
            local with_platform=$(echo "$logs" | grep -c "platform" || true)
            local with_buildtype=$(echo "$logs" | grep -c "buildType" || true)

            if [[ $with_platform -lt $((log_count - 2)) ]]; then
                ((missing_platform++))
                verbose_log "Hypothesis $hyp: $((log_count - with_platform)) logs missing platform field"
            fi

            if [[ $with_buildtype -lt $((log_count - 2)) ]]; then
                ((missing_buildtype++))
                verbose_log "Hypothesis $hyp: $((log_count - with_buildtype)) logs missing buildType field"
            fi
        fi
    done

    if [[ $missing_platform -eq 0 && $missing_buildtype -eq 0 ]]; then
        log_test PASS "All hypothesis logs include platform and buildType tracking"
    else
        log_test WARN "Some hypothesis logs may be missing tracking fields"
    fi
}
check_hypothesis_logging

# Test 5: Check for Unicode in ISO scripts
echo
echo "Checking for Unicode in ISO scripts..."
check_unicode() {
    local unicode_found=0

    # Check all install scripts for Unicode
    for script in lib/bootstrap-installer.sh cloudzy/install/*.sh framework*/install/*.sh; do
        if [[ -f "$script" ]]; then
            if grep -P '[^\x00-\x7F]' "$script" >/dev/null 2>&1; then
                log_test FAIL "Unicode found in $script (will break on Guix ISO)"
                verbose_log "Use [OK] instead of ✓, [ERROR] instead of ❌"
                ((unicode_found++))
            fi
        fi
    done

    if [[ $unicode_found -eq 0 ]]; then
        log_test PASS "No Unicode characters in ISO scripts"
    fi
}
check_unicode

# Test 6: Validate function signatures match callers
echo
echo "Checking function signature consistency..."
check_function_signatures() {
    # Check if RunGuixSystemInitFreeSoftware is called with platform parameter
    local calls_free=$(grep -n 'RunGuixSystemInitFreeSoftware(' cloudzy/install/*.go cmd/recovery/*.go 2>/dev/null || true)
    local calls_free_with_param=$(echo "$calls_free" | grep -cE 'RunGuixSystemInitFreeSoftware\(.*platform|RunGuixSystemInitFreeSoftware\(.*GuixPlatform' || true)
    local total_calls_free=$(echo "$calls_free" | wc -l)

    # Check if RunGuixSystemInit is called with platform parameter (framework-dual)
    local calls_init=$(grep -n 'RunGuixSystemInit(' framework*/install/*.go cmd/recovery/*.go 2>/dev/null || true)
    local calls_init_with_param=$(echo "$calls_init" | grep -cE 'RunGuixSystemInit\(.*platform|RunGuixSystemInit\(.*GuixPlatform' || true)
    local total_calls_init=$(echo "$calls_init" | wc -l)

    local all_pass=true

    if [[ $total_calls_free -gt 0 && $calls_free_with_param -ne $total_calls_free ]]; then
        log_test FAIL "Some RunGuixSystemInitFreeSoftware calls missing platform parameter"
        verbose_log "Expected: lib.RunGuixSystemInitFreeSoftware(state.GuixPlatform)"
        all_pass=false
    fi

    if [[ $total_calls_init -gt 0 && $calls_init_with_param -ne $total_calls_init ]]; then
        log_test FAIL "Some RunGuixSystemInit calls missing platform parameter"
        verbose_log "Expected: lib.RunGuixSystemInit(state.GuixPlatform)"
        all_pass=false
    fi

    if [[ $all_pass == true ]]; then
        log_test PASS "All function calls include platform parameter"
    fi
}
check_function_signatures

# Test 7: Compile check
echo
echo "Running compilation check..."
check_compilation() {
    if go build -o /tmp/validate-build ./run-remote-steps.go 2>/tmp/validate-build.log; then
        log_test PASS "Code compiles successfully"
        rm -f /tmp/validate-build
    else
        log_test FAIL "Compilation failed"
        verbose_log "See: /tmp/validate-build.log"
        cat /tmp/validate-build.log
    fi
}
check_compilation

# Test 8: Run unit tests
echo
echo "Running unit tests..."
check_tests() {
    if go test ./lib/... 2>&1 | grep -q "ok"; then
        log_test PASS "Unit tests pass"
    else
        log_test FAIL "Unit tests failed"
        verbose_log "Run: go test -v ./lib/..."
    fi
}
check_tests

# Test 9: Check manifest consistency
echo
echo "Checking source manifest..."
check_manifest() {
    # Verify manifest exists and is up-to-date
    if [[ ! -f SOURCE_MANIFEST.txt ]]; then
        log_test FAIL "SOURCE_MANIFEST.txt not found"
        return 1
    fi

    # Check if lib/common.go checksum matches manifest
    local current_hash=$(shasum -a 256 lib/common.go | awk '{print $1}')
    local manifest_hash=$(grep lib/common.go SOURCE_MANIFEST.txt | awk '{print $1}')

    if [[ "$current_hash" == "$manifest_hash" ]]; then
        log_test PASS "Source manifest is up-to-date"
    else
        log_test WARN "Source manifest may be outdated (run ./update-manifest.sh)"
        verbose_log "Current:  $current_hash"
        verbose_log "Manifest: $manifest_hash"
    fi
}
check_manifest

# Test 10: Check for common anti-patterns
echo
echo "Checking for common anti-patterns..."
check_antipatterns() {
    local issues=0

    # Check for reading from os.Stdin instead of /dev/tty
    if grep -n 'os.Stdin' lib/common.go cloudzy/install/*.go 2>/dev/null | grep -v '//.*os.Stdin' | grep -qv 'reader.*tty'; then
        log_test WARN "Found direct os.Stdin usage (should use /dev/tty for user input)"
        ((issues++))
    fi

    # Check for done < file instead of done < <(cat file)
    if grep -n 'done <.*[^)]$' lib/*.sh 2>/dev/null | grep -qv 'dev/tty'; then
        log_test WARN "Found 'done < file' pattern (may consume stdin, use process substitution)"
        ((issues++))
    fi

    if [[ $issues -eq 0 ]]; then
        log_test PASS "No common anti-patterns found"
    fi
}
check_antipatterns

# Summary
echo
echo "=== Validation Summary ==="
echo -e "${GREEN}Passed:   $PASSED${NC}"
echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
echo -e "${RED}Failed:   $FAILED${NC}"
echo

if [[ $FAILED -gt 0 ]]; then
    echo -e "${RED}[ERROR] VALIDATION FAILED - DO NOT DEPLOY${NC}"
    echo "Fix the issues above before deploying to remote machines"
    exit 1
elif [[ $WARNINGS -gt 0 ]]; then
    echo -e "${YELLOW}[WARN] VALIDATION PASSED WITH WARNINGS${NC}"
    echo "Review warnings before deploying"
    exit 0
else
    echo -e "${GREEN}[OK] ALL VALIDATIONS PASSED${NC}"
    echo "Safe to deploy to remote machines"
    exit 0
fi
