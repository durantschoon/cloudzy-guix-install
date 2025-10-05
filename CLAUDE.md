# Development Notes for Claude Code

This document contains important notes for AI assistants (like Claude Code) working on this repository.

## Important Constraints

### Guix ISO Terminal Limitations

**DO NOT use Unicode characters in scripts that run on the Guix ISO.**

The Guix ISO terminal has limited Unicode support and will display broken characters for:

- Checkmarks: ✓ ✅ ❌
- Emojis: 🚀 💻 🥧 ⚡ etc.
- Special symbols: ⚠️ 🛡️ etc.

**Affected files:**

- `bootstrap-installer.sh` - Runs on Guix ISO
- `run-remote-steps.go` - Runs on Guix ISO (compiled and executed during install)
- Any `**/install/*.sh` or `**/install/*.go` files

**Use instead:**

- `[OK]` instead of ✓
- `[ERROR]` instead of ❌
- `[WARN]` or `WARNING:` instead of ⚠️
- Plain text descriptions

**OK to use Unicode:**

- README.md and other documentation (viewed in browsers/editors)
- `**/postinstall/**` scripts (run after booting into installed Guix system)
- Scripts run on modern terminals outside the ISO

## Code Patterns

### Reading User Input

Always read from `/dev/tty` instead of stdin when prompting for user input:

**Bash:**

```bash
read -p "Continue? [Y/n] " -r </dev/tty
```

**Go:**

```go
tty, err := os.Open("/dev/tty")
if err != nil {
    // handle error
}
defer tty.Close()
reader := bufio.NewReader(tty)
answer, err := reader.ReadString('\n')
```

**Why:** stdin may be redirected by process substitution, pipes, or other operations. `/dev/tty` always points to the actual terminal.

### Process Substitution for File Reading

When reading files in loops, use process substitution to avoid consuming stdin:

**Correct:**

```bash
while IFS= read -r line; do
    # process line
done < <(cat SOURCE_MANIFEST.txt)
```

**Incorrect (breaks stdin for later reads):**

```bash
while IFS= read -r line; do
    # process line
done < SOURCE_MANIFEST.txt
```

## Architecture

### Installation Flow

1. User downloads `bootstrap-installer.sh` from GitHub
2. Bootstrap script:
   - Downloads repository tarball
   - Verifies checksums against `SOURCE_MANIFEST.txt`
   - Asks user to verify manifest hash
   - Builds `run-remote-steps` from Go source
   - Runs the compiled installer
3. Go installer (`run-remote-steps.go`):
   - Prompts before each step
   - Runs installation steps in sequence
   - Uses shared State struct for configuration

### State Management

All installation state is managed through a shared `State` struct (in `framework-dual/install/state.go`):

- No bash variable passing between scripts
- Type-safe, centralized configuration
- Passed as pointer to each step

### Verification Strategy

1. `SOURCE_MANIFEST.txt` contains SHA256 checksums of all source files
2. Bootstrap verifies downloaded files match manifest
3. User manually verifies manifest hash matches documentation
4. Go modules provide additional verification (go.mod/go.sum)

## Common Pitfalls

1. **Don't use Unicode in ISO scripts** - See above
2. **Don't read from os.Stdin directly** - Use /dev/tty
3. **Don't use `done < file.txt`** - Use process substitution
4. **Don't add timestamps to manifest** - Makes hash unstable
5. **Don't use `exec` in bootstrap** - Breaks stdin for Go installer
