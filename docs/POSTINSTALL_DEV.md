# Postinstall Script Development Guide

This guide explains how to develop and test postinstall customization scripts.

## Overview

Postinstall scripts run **after** the user boots into their installed Guix system. They customize `/etc/config.scm` by adding services, packages, and hardware configurations.

**Key principle**: Use Guile for all config.scm manipulation to ensure proper S-expression parsing.

## User Installation (On Guix System)

Users install postinstall scripts with a single command:

```bash
# One-line installation (pure Guile)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-postinstall.scm | guile --no-auto-compile -s -
```

**What this does:**

1. Downloads the bootstrap script and runs it
2. Detects platform (framework, framework-dual, cloudzy, raspberry-pi)
3. Creates `~/guix-customize/` directory
4. Downloads platform-specific customize script
5. Downloads Guile library (lib/guile-config-helper.scm)
6. Downloads shared recipes (Spacemacs, development tools, fonts)
7. Verifies all checksums against SOURCE_MANIFEST.txt
8. Creates convenience symlink: `~/guix-customize/customize`

**After installation:**
```bash
cd ~/guix-customize
./customize
```

This opens the interactive customization menu for their platform.

## Developer Workflow

### 1. Making Changes to Customize Scripts

When editing a platform's customize script (e.g., `framework-dual/postinstall/customize`):

**For adding services**, use the `guile_add_service()` helper:

```bash
# Add a service using Guile S-expression parser
guile_add_service "(gnu services desktop)" "(service gnome-desktop-service-type)"
guile_add_service "(gnu services ssh)" "(service openssh-service-type)"
```

**For adding packages**, use the safe_edit_config() helper (temporary until converted to Guile):

```bash
safe_edit_config "s/(packages (append (list/(packages (append (list\n    $package_name/"
```

### 2. Testing Guile Changes Locally

After modifying Guile helper code or adding new service types:

**Quick test** (tests the Guile helper directly):
```bash
framework-dual/postinstall/tests/run-guile-tests.sh
```

**Docker test** (full environment):
```bash
./test-docker.sh test
```

**Full test suite** (all platforms):
```bash
./run-tests.sh
```

### 3. Updating the Manifest

**CRITICAL**: After any changes to postinstall scripts or Guile helpers, update the manifest:

```bash
# Step 1: Update manifest with new checksums
./update-manifest.sh

# Step 2: Verify the manifest looks correct
cat SOURCE_MANIFEST.txt

# Step 3: Commit both the script changes AND the manifest
git add .
git commit -m "Update customize script to add X service"
```

**Why this matters**: Users download postinstall scripts from GitHub and verify them against `SOURCE_MANIFEST.txt`. If you don't update the manifest, they'll get checksum mismatches.

### 4. Verifying Downloaded Scripts

Users can verify their downloaded postinstall scripts:

```bash
# On Guix system after downloading customize scripts
bash lib/verify-postinstall.sh
```

This compares local file checksums against `SOURCE_MANIFEST.txt` from GitHub.

## File Structure

```
cloudzy-guix-install/
├── lib/
│   ├── guile-config-helper.scm      # Core S-expression parser
│   └── verify-postinstall.sh        # Checksum verification tool
├── framework-dual/
│   └── postinstall/
│       ├── customize                # Main customization script
│       └── tests/
│           ├── run-guile-tests.sh   # Test runner
│           ├── test-work.scm        # Test config file
│           └── test-*.sh            # Individual test cases
├── SOURCE_MANIFEST.txt              # Checksums of all critical files
└── update-manifest.sh               # Regenerate manifest
```

## Common Tasks

### Adding a New Service Type

1. **Identify the service module and type**:
   ```scheme
   ;; In Guix documentation, find:
   (use-modules (gnu services desktop))
   (service gnome-desktop-service-type)
   ```

2. **Add to customize script**:
   ```bash
   add_my_service() {
     echo ""
     echo "Adding My Service..."

     if ! guile_add_service "(gnu services my-category)" "(service my-service-type)"; then
       echo "Error: Failed to add my-service"
       return 1
     fi

     echo "My Service added successfully"
   }
   ```

3. **Test locally**:
   ```bash
   framework-dual/postinstall/tests/run-guile-tests.sh
   ```

4. **Update manifest and commit**:
   ```bash
   ./update-manifest.sh
   git add framework-dual/postinstall/customize SOURCE_MANIFEST.txt
   git commit -m "Add my-service to customize script"
   ```

### Debugging S-expression Parsing

If the Guile helper fails to find or modify the services field:

1. **Check the config structure**:
   ```bash
   grep -A 10 "(services" /etc/config.scm
   ```

2. **Test the parser manually**:
   ```bash
   guile --no-auto-compile -s lib/guile-config-helper.scm \
     check-config /etc/config.scm
   ```

3. **Add debug output**:
   Edit `lib/guile-config-helper.scm` and add:
   ```scheme
   (format #t "DEBUG: services-expr = ~s\n" services-expr)
   ```

### Converting a .sh Script to .scm

**IMPORTANT**: Read [docs/GUILE_KNOWLEDGE.md](GUILE_KNOWLEDGE.md) before starting Guile development!

See [docs/GUILE_CONVERSION.md](GUILE_CONVERSION.md) for the comprehensive conversion plan.

**Quick reference**:

1. Start with the shebang:
   ```scheme
   #!/run/current-system/profile/bin/guile --no-auto-compile -s
   !#
   ```

2. Import common modules:
   ```scheme
   (use-modules (ice-9 pretty-print)
                (ice-9 match)
                (ice-9 rdelim)
                (srfi srfi-1))  ; Required for filter-map, fold, etc.
   ```

3. Convert bash patterns to Guile - see GUILE_KNOWLEDGE.md for examples:
   - File reading: `(call-with-input-file file read-string)`
   - Command execution: `(open-input-pipe cmd)` + `(close-pipe port)`
   - Pattern matching: `(match expr ...)`

4. Add tests in `framework-dual/postinstall/tests/test-*.sh`

5. Update manifest and commit

## Testing Strategy

**Current focus** (as of Guile conversion project):

- ✅ **Guile (.scm) scripts**: Fully tested in Docker + run-tests.sh
- ⏸️ **Shell (.sh) scripts**: Not actively testing, will migrate to Guile
- 🎯 **Goal**: Guile-based postinstall workflow from GNOME config onward

**Test coverage requirements**:

- All Guile helper functions must have tests
- Service addition operations must be tested
- Config structure variations must be handled (append, list, %base-services)
- Error cases must be tested (invalid config, missing files)

## Common Pitfalls

1. **Forgetting to update manifest** - Always run `./update-manifest.sh` after script changes
2. **Using sed on /etc/config.scm** - Use `guile_add_service()` instead for proper S-expression parsing
3. **Wrong shebang in Guile scripts** - Use `#!/run/current-system/profile/bin/guile --no-auto-compile -s`
4. **Missing SRFI imports** - Add `(srfi srfi-1)` for filter-map, fold, etc.
5. **Not reading all S-expressions** - Config files have multiple top-level forms (use-modules + operating-system)

## Questions?

- See [docs/GUILE_KNOWLEDGE.md](GUILE_KNOWLEDGE.md) for Guile best practices and lessons learned
- See [docs/GUILE_CONVERSION.md](GUILE_CONVERSION.md) for conversion strategy
- See [CLAUDE.md](../CLAUDE.md) for AI assistant development notes
- See [CHECKLIST.md](../CHECKLIST.md) for current project status
