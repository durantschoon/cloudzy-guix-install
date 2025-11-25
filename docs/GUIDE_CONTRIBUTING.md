# Contributing Guide

**For developers contributing code, documentation, or maintaining this project.**

This guide helps you get started contributing to the cloudzy-guix-install project. It covers the codebase structure, development workflow, testing, and best practices.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Workflows](#development-workflows)
3. [Code Patterns](#code-patterns)
4. [Testing](#testing)
5. [Process Documentation](#process-documentation)
6. [Tools](#tools)

---

## Getting Started

### Repository Structure

- **[Repository Structure](STRUCTURE.md)** - Complete directory layout and organization
  - Platform-specific directories (cloudzy/, framework/, framework-dual/)
  - Shared libraries (lib/)
  - Installation vs postinstall separation
  - Testing structure

**Key Directories:**
```
cloudzy-guix-install/
├── lib/                    # Shared Go library functions
├── cloudzy/               # VPS platform
├── framework/             # Framework 13 single-boot
├── framework-dual/        # Framework 13 dual-boot
├── docs/                  # Documentation
├── tools/                 # Development tools
└── postinstall/           # Shared postinstall recipes
```

### Development Checklist

- **[Development Checklist](../CHECKLIST.md)** - Current development status
  - Platform support status
  - Testing status
  - Known issues
  - Roadmap items

### Testing Setup

- **[Testing Guide](TESTING.md)** - Complete test suite documentation
  - Test structure
  - Running tests locally
  - Docker testing
  - Adding new tests

**Quick Start:**
```bash
# Run all tests
./run-tests.sh

# Docker testing (no local Go needed)
./test-docker.sh
```

---

## Development Workflows

### Postinstall Script Development

- **[Postinstall Script Development](POSTINSTALL_DEV.md)** - Developing customization scripts
  - User installation workflow
  - Developer workflow
  - Guile helper functions
  - Testing postinstall scripts

**Key Principle:** Use Guile for all config.scm manipulation to ensure proper S-expression parsing.

**Example:**
```bash
# Add a service using Guile S-expression parser
guile_add_service "(gnu services desktop)" "(service gnome-desktop-service-type)"
```

### Guile Conversion Strategy

- **[Guile Conversion Strategy](GUILE_CONVERSION.md)** - Converting bash to Guile
  - Why convert to Guile
  - Conversion approach
  - Testing converted scripts
  - Common patterns

**Batch Conversion:**
- See [Batch Conversion Tools](../tools/README.md) for automated conversion workflow
- Uses Anthropic Batch API for cost-effective conversion
- See [Batch Conversion Best Practices](BATCH_CONVERSION_BEST_PRACTICES.md) for workflow

---

## Code Patterns

### Guile Best Practices

- **[Guile Best Practices](GUILE_BEST_PRACTICES.md)** - Scheme/Guile patterns
  - Naming conventions (?, !, ->)
  - Error handling
  - File operations
  - Command execution

### Common Pitfalls

- **[Guile Gotchas](GUILE_GOTCHAS.md)** - Common Guile pitfalls
  - Frequency-sorted list of mistakes
  - Solutions and workarounds
  - Examples from real conversions

**Top Gotchas:**
1. Missing SRFI imports
2. Wrong command names (shasum vs sha256sum)
3. Incorrect shebang format
4. String vs symbol confusion

### Quick Reference

- **[Guile Knowledge](GUILE_KNOWLEDGE.md)** - Quick reference and patterns
  - Common patterns
  - Module imports
  - File operations
  - Command execution

---

## Testing

### Test Structure

**Common Library Tests (`lib/common_test.go`):**
- Partition path generation
- Device detection
- Environment variable handling
- Command availability checking

**Platform-Specific Tests:**
- `framework-dual/install/*_test.go` - Dual-boot installation tests
- Integration tests for each installation step

### Running Tests

**Local Testing:**
```bash
# Run all tests
./run-tests.sh

# Run specific test file
go test ./lib/common_test.go
```

**Docker Testing:**
```bash
# Run all tests in Docker
./test-docker.sh

# Interactive shell for debugging
./test-docker.sh shell

# Clean up
./test-docker.sh clean
```

**Guile Script Testing:**
```bash
# Test converted Guile scripts
find tools/converted-scripts -name "test-*.scm" -exec guile --no-auto-compile -s {} \;
```

See [Testing Guide](TESTING.md) for complete details.

---

## Process Documentation

### Time Tracking

- **[Time Tracking Retrospective](TIME_TRACKING_RETROSPECTIVE.md)** - Process lessons
  - Time invested in each feature
  - What took longest and why
  - Best practices for future projects
  - Git history analysis

**Key Insights:**
- Document time investment as you go
- Use git commit messages with time estimates
- Track what took longest for future planning

### Batch Conversion

- **[Batch Conversion Best Practices](BATCH_CONVERSION_BEST_PRACTICES.md)** - Conversion workflow
  - Pre-conversion preparation
  - Structured comments
  - Validation
  - Review process

**Workflow:**
1. Add structured comments to bash scripts
2. Generate batch conversion requests
3. Submit to Anthropic Batch API
4. Retrieve and review results
5. Test converted scripts
6. Deploy to production

### AI Assistant Notes

- **[CLAUDE.md](../CLAUDE.md)** - Development notes for AI assistants
  - Code patterns and constraints
  - Guix ISO terminal limitations (no Unicode)
  - User input handling
  - Error handling patterns

**Important Constraints:**
- **DO NOT use Unicode** in scripts that run on Guix ISO
- Use `/dev/tty` for user input
- Use `/run/current-system/profile/bin/bash` shebang
- Follow error handling patterns

---

## Tools

### Batch Conversion Tools

- **[Batch Conversion Tools](../tools/README.md)** - Automated bash-to-Guile conversion
  - Generate batch requests
  - Submit to Anthropic API
  - Retrieve and extract results
  - Review and test conversions

**Quick Start:**
```bash
# Verify setup
./tools/verify-setup.sh

# Generate batch requests
./tools/generate-batch-conversion.sh

# Submit batch
./tools/submit-batch.sh

# Check status
./tools/check-batch-status.sh <batch-id>

# Retrieve results
./tools/retrieve-batch.sh <batch-id>
```

### Deployment Tools

- **[Deployment Checklist](../tools/DEPLOYMENT_CHECKLIST.md)** - Deployment process
- **[Generalization Plan](../tools/GENERALIZATION_PLAN.md)** - Generalization strategy
- **[Batch Conversion Plan](../tools/BATCH_CONVERSION_PLAN.md)** - Conversion planning

---

## Code Style Guidelines

### Go Code

- Use `lib/common.go` for shared functions
- Follow Go naming conventions
- Include error handling
- Add tests for new functions

### Bash Scripts

- Use `set -euo pipefail`
- Use `/run/current-system/profile/bin/bash` shebang for ISO scripts
- **NO Unicode** in scripts that run on Guix ISO
- Use `/dev/tty` for user input

### Guile Scripts

- Use `#!/run/current-system/profile/bin/guile --no-auto-compile -s` shebang
- Include all required `use-modules`
- Follow naming conventions (?, !, ->)
- Add comments explaining complex conversions

---

## Contributing Process

### 1. Find an Issue or Feature

- Check [CHECKLIST.md](../CHECKLIST.md) for known issues
- Review open GitHub issues
- Discuss major changes before implementing

### 2. Set Up Development Environment

```bash
# Clone repository
git clone https://github.com/durantschoon/cloudzy-guix-install.git
cd cloudzy-guix-install

# Install dependencies (if testing locally)
# Or use Docker: ./test-docker.sh
```

### 3. Make Changes

- Follow code style guidelines
- Add tests for new functionality
- Update documentation as needed
- Test on real Guix ISO when possible

### 4. Test Your Changes

```bash
# Run tests
./run-tests.sh

# Test specific platform (if applicable)
# Boot Guix ISO and test installation
```

### 5. Update Documentation

- Update relevant guides
- Add to CHECKLIST.md if fixing an issue
- Update CHANGELOG if significant change

### 6. Submit Changes

- Create pull request
- Reference related issues
- Include test results
- Describe changes clearly

---

## Getting Help

**Questions?**
- Check [Installation Knowledge](INSTALLATION_KNOWLEDGE.md) for technical details
- Review [Guile Gotchas](GUILE_GOTCHAS.md) for common issues
- Check [Testing Guide](TESTING.md) for test-related questions

**Found a Bug?**
- Check [CHECKLIST.md](../CHECKLIST.md) to see if it's known
- Open a GitHub issue with details
- Include logs and error messages

---

## Further Reading

- [Repository Structure](STRUCTURE.md) - Complete architecture overview
- [Testing Guide](TESTING.md) - Test suite details
- [Postinstall Development](POSTINSTALL_DEV.md) - Postinstall script workflow
- [Guile Best Practices](GUILE_BEST_PRACTICES.md) - Code patterns
- [CLAUDE.md](../CLAUDE.md) - AI assistant development notes

---

**Ready to contribute?** Start with [Testing Guide](TESTING.md) to understand the test suite, then pick an issue from [CHECKLIST.md](../CHECKLIST.md) or create a new feature!

