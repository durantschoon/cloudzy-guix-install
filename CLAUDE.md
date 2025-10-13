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
6. **Don't commit without updating manifest** - See below

## Development Workflow

### Before Committing Changes

If you modified any scripts that run on the Guix ISO, you **MUST** update the manifest before committing:

```bash
./update-manifest.sh
git add SOURCE_MANIFEST.txt
git commit -m "Your commit message"
```

**Files that require manifest update:**

- `bootstrap-installer.sh`
- `run-remote-steps.go`
- Any file in `**/install/*.go`
- Any file in `**/install/*.sh`

**Why:** The manifest contains SHA256 checksums of all source files. Users verify this manifest hash to ensure GitHub's CDN has the latest version. If you don't update it, users will get checksum mismatches when they try to install.

**The update-manifest.sh script:**

- Generates checksums for all Go source files and bootstrap script
- Writes them to `SOURCE_MANIFEST.txt`
- Displays the manifest hash for documentation/verification
- Only changes when actual source files change (no timestamp)

## Repository Conventions

### Factor Common Code into lib/common.go

When adding features that are needed in multiple installers (cloudzy, framework, framework-dual, raspberry-pi), extract reusable logic into `lib/common.go` rather than duplicating it. Examples:

- Label checks: use `VerifyLabelsExist(...)`
- Mount-by-label: use `MountByLabel(label, mountPoint)`
- Free-space checks: use `GetMountFreeSpaceGiB(path)`
- Command logging: enable once per step with `EnableCommandLogging("/tmp/guix-install.log")`

This keeps installers small, readable, and consistent.

### Update Documentation After Adding Features

After implementing user-visible behavior or flow changes, update the docs in the same commit:

- `CHECKLIST.md` statuses and guidance
- `README.md` and `QUICKSTART.md` usage notes
- Platform readmes under `framework*/README.md` or `raspberry-pi/**` where applicable

Docs should reflect new safety checks, logging, and any changed commands or defaults.

### Checklist-Driven Workflow

We work from `CHECKLIST.md` and track progress explicitly:

- Before starting a feature, review the relevant checklist items
- Implement changes, then immediately update `CHECKLIST.md` statuses
- Keep commits aligned with checklist items so changes are auditable
- If items spawn new sub-tasks, add them to the checklist and complete them before closing the parent

### Resolve Design Conflicts Early

If you encounter a potential conflict between existing code, documentation, or stated design goals:

- Do not guess; add a brief note in the commit or PR description
- Alert the user promptly to discuss the trade-offs and pick a direction
- Once resolved, update code and docs together to reflect the agreed design

## Reviewer Personas (Offer These at Milestones)

After a developer completes a milestone, offer to review changes by adopting one of these personas:

1) Mid‑Level Unix User New to Guix
- Focus: onboarding clarity, data‑loss warnings, Secure Boot notes, first‑boot expectations, networking quick path
- Goal: ensure the Quickstart and README answer “what do I do next?” with minimal Guix background

2) Seasoned Guix User
- Focus: channel pinning, substitute trust, receipts/provenance, services (NetworkManager, TLP, time sync, fstrim), storage options (LUKS/btrfs), diagnostics
- Goal: robustness, reproducibility, and clean Guix idioms

3) Dual‑Boot Laptop Owner
- Focus: GRUB visibility, chainloading Pop!_OS, keeping existing partitions safe, clear rollback paths
- Goal: confidence that dual‑boot stays usable and recoverable

How to offer: “Do you want a review as a [Mid‑Level Unix User], [Seasoned Guix User], or [Dual‑Boot Laptop Owner]?”
