# Guix Container Testing (Future Exploration)

This document outlines ideas for full end-to-end installer testing using actual Guix containers.

## Current Status

**Implemented:** Docker-based developer testing (see `./test-docker.sh`)
- Tests Go code, unit tests, integration tests
- Fast, simple, works on any machine with Docker
- Does NOT test actual Guix installation flow

**Not Implemented:** Full installer testing in Guix environment
- Would test complete installation flow
- Requires actual Guix daemon and packages
- More complex setup requirements

---

## Concept: Full Installer Testing with Guix Container

### Approach Using cnelson31/guix Image

The `cnelson31/guix` Docker image provides a working Guix environment that could be used for more realistic testing.

**Example usage (tested on macOS with Colima):**

```bash
# Pull the Guix image (platform flag needed on Apple Silicon)
docker pull --platform linux/amd64 cnelson31/guix

# Run interactive Guix container
docker run --platform linux/amd64 -it --rm \
  -v $HOME/guix-config:/root/guix-config \
  cnelson31/guix bash
```

### What Could Be Tested

1. **Channel Operations**
   - `guix pull` with custom channels
   - Channel authentication and pinning
   - Time-machine operations

2. **Package Operations**
   - `guix package -i` installations
   - Profile generation management
   - Garbage collection

3. **System Configuration**
   - `guix system` operations (limited - needs privileges)
   - Config.scm syntax validation
   - Service configuration validation

4. **Script Behavior**
   - Bootstrap script download and verification
   - Manifest checksum validation
   - Recovery script functionality

### Challenges

1. **Privileged Operations**
   - Full `guix system init` requires root and real block devices
   - Partitioning/formatting not possible in container
   - Bootloader installation not possible

2. **Platform Compatibility**
   - Apple Silicon Macs need `--platform linux/amd64` flag
   - Requires Rosetta 2 or Colima with x86_64 emulation
   - Performance overhead from emulation

3. **Setup Complexity**
   - Users need Docker + Colima (on macOS) properly configured
   - Platform-specific Docker socket paths
   - Volume mounting for development workflow

4. **Guix Daemon Requirements**
   - Container needs Guix daemon running
   - Build processes require isolation
   - Substitute server connectivity

### Potential Implementation Path

**Phase 1: Read-only Testing** (Easier)
- Validate config.scm syntax
- Test channel operations
- Test package queries
- Test manifest operations

**Phase 2: Package Installation Testing** (Moderate)
- Test `guix package -i` in container
- Test profile management
- Test garbage collection
- Test store operations

**Phase 3: Mock System Testing** (Advanced)
- Mock partition detection
- Mock block device operations
- Test installation logic without actual disk writes
- Validate generated configs

### Example Test Structure

```bash
#!/bin/bash
# Full installer test in Guix container

# Start Guix container with test volume
docker run --platform linux/amd64 -it --rm \
  -v $(pwd):/workspace \
  -w /workspace \
  cnelson31/guix bash -c "
    # Start Guix daemon
    guix-daemon --build-users-group=guixbuild &

    # Run installer tests
    ./test-guix-container.sh
  "
```

### Why This is Deferred

1. **Current Docker Testing is Sufficient**
   - Covers 90% of code paths
   - Fast, simple, reliable
   - No platform-specific issues

2. **Diminishing Returns**
   - Actual installer must be tested on real hardware anyway
   - Container can't test bootloader, partitioning, etc.
   - Complex setup for marginal additional coverage

3. **Resource Investment**
   - Time better spent on other features
   - Requires Guix expertise to debug container issues
   - Platform-specific troubleshooting (Colima, Docker Desktop, etc.)

---

## Alternative: Targeted Integration Tests

Instead of full container testing, consider targeted integration tests:

### Config Generation Tests
Test that generated config.scm files are valid:

```bash
# In Guix container
guix system build /tmp/test-config.scm
```

### Channel Configuration Tests
Test channel setup and operations:

```bash
# In Guix container
export GUIX_CHANNEL_REPO="https://github.com/user/repo"
./lib/postinstall.sh  # Test channel generation
guix pull -C /tmp/test-channels.scm
```

### Package Availability Tests
Test that packages referenced in configs exist:

```bash
# In Guix container
guix show linux-firmware
guix show network-manager
```

---

## Documentation for Manual Testing

For developers who want to test manually with Guix containers:

### Prerequisites

**macOS (Colima):**
```bash
# Install Colima
brew install colima

# Start Colima with x86_64 emulation
colima start --arch x86_64

# Verify
docker info
```

**Linux:**
```bash
# Standard Docker installation
# No special setup needed
```

### Running Guix Container

```bash
# Pull image
docker pull --platform linux/amd64 cnelson31/guix

# Run with repository mounted
docker run --platform linux/amd64 -it --rm \
  -v $(pwd):/workspace \
  -w /workspace \
  cnelson31/guix bash

# Inside container:
# - Test bootstrap script
# - Test channel operations
# - Validate configs
# - Test package operations
```

### Example Test Session

```bash
# In Guix container
cd /workspace

# Test channel generation
./lib/postinstall.sh

# Test manifest verification
./lib/bootstrap-installer.sh --help

# Test config.scm syntax
guix system search network-manager

# Test package availability
guix show linux linux-firmware network-manager
```

---

## Future Considerations

If full container testing becomes necessary:

1. **Create guix-test-env image**
   - Based on cnelson31/guix
   - Pre-configured for testing
   - Includes test utilities

2. **Add test-guix-container.sh**
   - Automated test runner
   - Handles platform detection
   - Manages container lifecycle

3. **Document platform-specific setup**
   - Colima on macOS
   - Docker Desktop on Windows
   - Native Docker on Linux

4. **Add to CI/CD pipeline**
   - GitHub Actions with Guix container
   - Automated integration testing
   - Pre-release validation

---

## Related Documentation

- Current Docker testing: `./test-docker.sh`
- Developer tests: `./run-tests.sh`
- Guix container image: https://hub.docker.com/r/cnelson31/guix
- Guix manual: https://guix.gnu.org/manual/
