# cloudzy-guix-install

## Preparation

### On local machine

Where repo is cloned, get the shasum256 of the main `run-remote-steps.sh`

**IMPORTANT**: Always get the checksum from the same source you'll download from!

- **For testing/development**: Use `main` branch
- **For stable releases**: Use a specific tag (e.g., `v0.1.3`)

Substitute your command for pbcopy if not on a mac

To check and record on the iso instance:

```bash
# For testing latest changes (main branch)
shasum -a 256 run-remote-steps.sh | pbcopy

# For stable version (specific tag)
git checkout v0.1.3
shasum -a 256 run-remote-steps.sh | pbcopy
```

or instead prepare to manually check

```bash
shasum -a 256 run-remote-steps.sh
```

### In guix iso environment

`perl` is for `shasum`

```bash
guix install curl
guix install perl
```

## Quick Start

### To leave copy of shasum in temp iso environment

Download and verify the script (do preparation above first)

**Choose the right URL based on what you're testing:**

```bash
# For testing latest changes (main branch)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh

# For stable version (specific tag)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/v0.1.3/run-remote-steps.sh -o run-remote-steps.sh
```

Then verify with your checksum:

```bash
USER_NAME="YOUR_USER_NAME"
FULL_NAME="YOUR_FULL_NAME"

echo " PASTE-YOUR-SHASUM-HERE-WITH-NO-SPACES-INSIDE-THESE-QUOTES-SINGLE-NEWLINE-IS-OK " | head -1 > rrs-checksum.txt
cat rrs-checksum.txt
shasum -a 256 -c rrs-checksum.txt

chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```

### Manual Verification Instead

If you prefer to verify manually:

```bash
USER_NAME="YOUR_USER_NAME"
FULL_NAME="YOUR_FULL_NAME"

# Download (choose appropriate URL)
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
shasum -a 256 run-remote-steps.sh

chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```

### Troubleshooting: CDN Cache Issues

If you're getting a different checksum than expected (especially across different regions), use:

```bash
# Add timestamp to bypass cache (most reliable method)
curl -fsSL "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh?$(date +%s)" -o run-remote-steps.sh
```

## Version Anticipation Workflow

**IMPORTANT: Only tag versions that are tested and working!**

When making changes that affect the scripts:

1. **Make your changes** on the `main` branch
2. **Test thoroughly** in the target environment (Guix ISO)
3. **Update SHA256 checksums** in `run-remote-steps.sh`:

   ```bash
   shasum -a 256 *.sh
   ```

4. **Only when you have a working version:**
   - Anticipate the next version in `run-remote-steps.sh`:
     - Change `REF="v0.1.3"` to `REF="v0.1.4"` (or next version)
   - Commit the changes:

     ```bash
     git add .
     git commit -m "Description of changes"
     ```

   - Create the anticipated tag:

     ```bash
     git tag -a v0.1.4 -m "Description of changes"
     ```

   - Push everything:

     ```bash
     git push origin main
     git push origin v0.1.4
     ```

**For development/testing:**

- Use `REF="main"` during development
- Only switch to a specific tag when you have a stable, tested version

### Why This Works

- **No chicken-and-egg problem**: The script points to a tag that includes the script itself
- **Stable releases**: Tags are immutable, so users get consistent behavior
- **Clean development**: Continue working on `main` without affecting stable versions
- **Self-referential**: Each tag contains the script pointing to itself

### Current Stable Version

- **Tag**: `v0.1.3`
- **Features**: Safety flags (`set -euo pipefail`) on all scripts
- **Status**: Production ready

### Latest Development Version (main branch)

- **Checksum**: `c5abb8ca797bdd6dcefa9acf03062d47a6e18555215f5eebe259e9581616a8c3`
- **Features**: Latest fixes including race condition improvements, variable sharing between scripts, user variable setup
- **Status**: Testing/development
