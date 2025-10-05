#!/usr/bin/env bash
set -euo pipefail

# Bootstrap script to build and run the Guix installer from source
# Verification strategy:
#   1. Git verifies the commit/tag integrity when cloning
#   2. Go verifies go.mod and go.sum (if external deps exist)
#   3. Source code is compiled locally before execution

REPO_OWNER="${GUIX_INSTALL_REPO:-durantschoon/cloudzy-guix-install}"
REPO_REF="${GUIX_INSTALL_REF:-main}"

echo "=== Guix Installer Bootstrap ==="
echo "Repository: ${REPO_OWNER}"
echo "Reference: ${REPO_REF}"
echo ""

# Create temporary directory
WORK_DIR=$(mktemp -d)
trap "rm -rf $WORK_DIR" EXIT

cd "$WORK_DIR"

# Clone repository - Git verifies commit integrity
echo "Cloning repository..."
if ! git clone --depth 1 --branch "$REPO_REF" "https://github.com/${REPO_OWNER}.git" installer; then
    echo "Error: Failed to clone repository"
    exit 1
fi
cd installer

# Show the commit we're building from
COMMIT=$(git rev-parse HEAD)
echo ""
echo "Building from commit: $COMMIT"
echo ""

# Verify go.mod exists
if [[ ! -f go.mod ]]; then
    echo "Error: go.mod not found. This doesn't appear to be a Go module."
    exit 1
fi

# Build the installer
# Note: We have no external dependencies, so go.sum won't exist
# Go will still verify the build matches go.mod
echo "Building installer from source..."
if ! go build -o run-remote-steps .; then
    echo "Error: Build failed"
    exit 1
fi

# Verify the binary was created
if [[ ! -f run-remote-steps ]]; then
    echo "Error: Binary not created"
    exit 1
fi

# Run the installer
echo ""
echo "Starting installation..."
echo ""
exec ./run-remote-steps
