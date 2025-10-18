# Testing Guide

This document describes the test suite for the cloudzy-guix-install project, created to ensure the refactored code works correctly after moving duplicate functions to the common library.

## Test Structure

### Common Library Tests (`lib/common_test.go`)

Tests the shared functions that were moved to `lib/common.go`:

- **`TestMakePartitionPath`** - Tests partition path generation for different device types (NVMe, SATA, eMMC, VDA)
- **`TestDetectDeviceFromState`** - Tests device detection logic with state management
- **`TestIsPartitionFormatted`** - Tests filesystem type detection
- **`TestGetEnv`** - Tests environment variable handling
- **`TestGetEnvOrDefault`** - Tests environment variable with defaults
- **`TestDetectDevice`** - Tests platform-specific device detection
- **`TestCommandExists`** - Tests command availability checking
- **`TestIsGuixLiveISO`** - Tests live ISO environment detection

### Framework-Dual Integration Tests

Tests the refactored framework-dual install functions:

#### `01-partition-check_test.go`

- **`TestStep01PartitionCheck_Integration`** - Tests step creation and state management
- **`TestStateStructure`** - Tests State struct field accessibility
- **`TestLibFunctionIntegration`** - Tests that lib functions are accessible
- **`TestRefactoredFunctionCalls`** - Tests the refactored function call signatures
- **`TestStringOperations`** - Tests string operations (fixes linter errors)
- **`TestPartitionPathGeneration`** - Tests partition path generation scenarios
- **`TestErrorHandling`** - Tests error handling with invalid inputs

#### `02-mount-existing_test.go`

- **`TestStep02MountExisting_Integration`** - Tests mount step integration
- **`TestLibFunctionAccessibility_Mount`** - Tests lib function accessibility
- **`TestFunctionSignatures_Mount`** - Tests function signatures
- **`TestRefactoredLogicFlow_Mount`** - Tests refactored logic flow
- **`TestErrorHandling_Mount`** - Tests error handling
- **`TestStatePersistence`** - Tests state variable persistence

#### `03-config-dual-boot_test.go`

- **`TestStep03ConfigDualBoot_Integration`** - Tests config step integration
- **`TestLibFunctionIntegration_Config`** - Tests lib function integration
- **`TestRefactoredLogicFlow_Config`** - Tests refactored logic flow
- **`TestFunctionSignatures_Config`** - Tests function signatures
- **`TestStateValidation`** - Tests state validation
- **`TestErrorHandling_Config`** - Tests error handling
- **`TestPartitionPathGeneration_Config`** - Tests partition path generation

## Running Tests

### Run All Tests

```bash
./run-tests.sh
```

### Run Individual Test Packages

```bash
# Test common library functions
go test -v ./lib

# Test framework-dual functions
go test -v ./framework-dual/install
```

### Run Specific Tests

```bash
# Run only MakePartitionPath tests
go test -v ./lib -run TestMakePartitionPath

# Run only integration tests
go test -v ./framework-dual/install -run Integration
```

## Test Coverage

The tests cover:

### ✅ **String Detection & Operations**

- Partition path generation for all device types
- String trimming operations (fixes linter errors)
- Environment variable handling

### ✅ **Function Signatures**

- All refactored function calls use correct signatures
- Return value handling
- Error propagation

### ✅ **Integration Testing**

- Step creation and state management
- Function accessibility across packages
- Logic flow validation

### ✅ **Error Handling**

- Invalid input handling
- Missing device scenarios
- Platform-specific error cases

### ✅ **State Management**

- State struct field accessibility
- Variable persistence
- State validation

## What the Tests Verify

1. **Refactoring Success** - All functions moved to `lib/common.go` are accessible and work correctly
2. **Function Signatures** - All refactored function calls use the correct signatures
3. **String Operations** - Fixed linter errors (duplicate characters in `strings.TrimLeft`)
4. **Error Handling** - Functions handle invalid inputs gracefully
5. **Integration** - Framework-dual steps can access and use lib functions
6. **State Management** - State variables persist correctly across function calls

## Expected Test Behavior

- **Device Detection Tests** - Will show "No suitable block device found" errors (expected in test environment)
- **Partition Tests** - Will return false for non-existent partitions (expected)
- **Integration Tests** - Will log expected messages about missing devices/partitions
- **All Tests Should Pass** - Despite expected errors, all tests should complete successfully

## Adding New Tests

When adding new functions to `lib/common.go`:

1. Add corresponding tests to `lib/common_test.go`
2. Add integration tests to relevant framework test files
3. Update this documentation
4. Run `./run-tests.sh` to verify

## Continuous Integration

These tests should be run:

- Before committing refactoring changes
- After adding new functions to the common library
- When modifying function signatures
- As part of CI/CD pipeline (if implemented)

The test suite ensures that the refactored code maintains the same functionality while eliminating code duplication and improving maintainability.
