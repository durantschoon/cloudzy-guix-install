# Time Tracking Retrospective & Future Best Practices

## What We Can Reconstruct (Estimated Time Investment)

Based on git commit timestamps and conversation patterns:

### Major Problem-Solving Efforts

| Problem | Commits | Est. Time | Iterations | Notes |
|---------|---------|-----------|------------|-------|
| `/var/lock` ENOENT error | 3 | ~2 hours | 4-5 | Initial debugging, understanding chroot behavior, implementing two-phase fix |
| Kernel/initrd missing files | 5 | ~4 hours | 6-8 | Discovered after fixing /var/lock, required 3-step workaround, auto-retry logic |
| ISO artifact cleanup | 8 | ~3 hours | Multiple | Evolved over time, symlinks, /run cleanup, idempotency |
| resolv.conf for chroot | 2 | ~1 hour | 2-3 | Manual process → automation, added verification |
| Platform selection UX | 4 | ~1 hour | 3-4 | Simplified from flags to env var only |
| Label-based mounting | 6 | ~2 hours | 5-6 | EFI vs GUIX_ROOT priority, uppercase labels |
| Font selection UX | 3 | ~1 hour | 3 | Filter large fonts, skip if already set |

### Time Sinks (Where Too Much Time Was Spent)

1. **Platform selection mechanism** (~1 hour across 4 commits)
   - Started with `-s` flag
   - Realized env var was cleaner
   - Could have chosen env-var-only from start
   - **Lesson:** Design CLI interfaces upfront, consider env vars first for installers

2. **Label naming conventions** (~1 hour across 3 commits)
   - Inconsistent casing (efi vs EFI)
   - Framework-dual used DATA, others didn't
   - Multiple fixes to align
   - **Lesson:** Establish naming conventions in a `CONVENTIONS.md` file early

3. **Idempotency edge cases** (~2 hours across 5+ commits)
   - ISO cleanup not running when store populated
   - Parent directories not existing before symlinks
   - Multiple iteration cycles
   - **Lesson:** Write idempotency tests FIRST, then implement

## Best Practices for Future Projects

### 1. Real-Time Time Tracking

**Use git commit messages with time estimates:**

Example commit message format:

    [2h] Fix /var/lock ENOENT error during guix system init

    Spent ~2 hours debugging:
    - 30m: Reproduced error, read Guix source
    - 1h: Understood chroot behavior, tested solutions
    - 30m: Implemented two-phase fix, verified

    This estimates the ACTUAL time spent, not the commit timestamp delta.

**Why this works:**
- Captures effort at commit time (while fresh in memory)
- Shows iteration patterns (multiple commits on same issue = complexity)
- Helps identify time sinks for future improvement

### 2. Decision Log (ADR - Architecture Decision Records)

Create `docs/decisions/` directory with timestamped decisions.

Example file `001-use-environment-variables-for-platform-selection.md`:

    # 001-use-environment-variables-for-platform-selection.md

    Date: 2025-11-23
    Status: Accepted (replaces command-line flags)
    Time spent: ~1 hour (including 3 commits to change approach)

    ## Context
    Initially used `-s cloudzy` flag, but env vars are more standard for installers.

    ## Decision
    Use GUIX_PLATFORM environment variable only.

    ## Consequences
    - Simpler CLI interface
    - Consistent with installer conventions (DEVICE, USER_NAME, etc.)
    - **Time sink:** Should have used env vars from start

    ## Time Investment
    - Initial implementation with flags: 20m
    - Realization env vars better: 10m (conversation)
    - Refactor to env vars: 30m (2 commits)

    Total: 1 hour that could have been 20 minutes with upfront design.

### 3. Problem Discovery Log

Create `docs/problems/` directory tracking when issues were found.

Example file `kernel-initrd-missing-after-system-init.md`:

    ## Discovery Timeline

    **2025-11-25 14:20** - Issue first reported
    - User: "We have no kernel files under /mnt/boot"
    - Expected: vmlinuz*, initrd* files
    - Found: Only efi/ and grub/ directories

    **2025-11-25 14:25** - Initial investigation (30m)
    - Checked if guix system init completed (yes)
    - Checked /gnu/store for built system (yes, has kernel/initrd)
    - Hypothesis: Files not being copied from store to /boot/

    **2025-11-25 14:55** - Breakthrough (1h spent debugging)
    - Manual copy from store works!
    - Realized: guix time-machine + nonguix doesn't copy kernel files
    - Workaround: Build system → Copy kernel manually → Run init

    **2025-11-25 15:30** - Implementation (1h)
    - Added 3-step workaround to RunGuixSystemInit()
    - Added auto-retry verification
    - Added diagnostic logging

    **Total time: ~2.5 hours**

    ## What Slowed Us Down
    - Initially thought it was a permissions issue (30m wasted)
    - Didn't check store immediately (should have been first step)

    ## What Would Speed This Up Next Time
    1. Check store FIRST when boot files missing
    2. Compare store contents vs /boot/ immediately
    3. Document "kernel in store but not in /boot" as known pattern

### 4. Time-Complexity Matrix

Create `docs/TIME_MATRIX.md` to track over time.

Example content:

    # Feature Time-Complexity Matrix

    | Feature | Initial Est. | Actual Time | Complexity | Reason for Delta |
    |---------|-------------|-------------|------------|------------------|
    | Basic partition | 1h | 2h | Medium | Edge cases with NVMe detection |
    | Mount by label | 30m | 2h | High | Label priority issues, casing |
    | Config generation | 1h | 1h | Low | Straightforward |
    | System init | 2h | 6h | **Very High** | /var/lock bug + kernel workaround |
    | Recovery scripts | 1h | 3h | Medium | Iterative improvements |

    ## High-Complexity Indicators
    - **System init** was underestimated by 3x due to:
      - Undocumented Guix behaviors (/var/lock directive)
      - Nonguix + time-machine quirks (kernel copy)
      - Required reading Guix source code
      - Multiple iteration cycles

    **Learning:** When working with Guix internals (system init, profiles, generations),
    multiply time estimates by 2-3x for unknown bugs.

### 5. Daily/Session Logs

At end of each session, write `docs/dev-log/YYYY-MM-DD.md`.

Example session log format:

    # 2025-11-25 Development Log

    ## Session: 14:00 - 17:00 (3 hours)

    ### Accomplished
    - [x] Fixed /var/lock ENOENT error (2h)
    - [x] Added kernel/initrd verification (1h)

    ### Time Breakdown
    - 14:00-14:30: Reproduced /var/lock error
    - 14:30-15:30: Understood chroot directives, implemented fix
    - 15:30-15:45: Break
    - 15:45-16:45: Discovered kernel files missing, added verification

    ### Blockers Encountered
    1. `/var/lock` symlink → directory chroot issue (1.5h debugging)
    2. Kernel files not copied by system init (0.5h to discover)

    ### Tomorrow's Plan
    - Add diagnostic logging for kernel copy process
    - Test on VPS to ensure fix is platform-agnostic

    ### Time Sinks to Avoid
    - Spent 30m thinking permissions issue (was wrong hypothesis)
    - Should have checked store first → saved 30m

### 6. Automated Time Tracking

Use git hooks to prompt for time estimates.

Example `.git/hooks/prepare-commit-msg`:

    #!/run/current-system/profile/bin/bash

    # Prompt for time estimate
    echo ""
    echo "Estimated time spent on this change? (e.g., 30m, 2h, 15m)"
    read -p "Time: " time_est

    if [ -n "$time_est" ]; then
        # Prepend time to commit message
        echo "[$time_est] $(cat "$1")" > "$1"
    fi

### 7. Post-Mortem Analysis

After major features, write `docs/postmortems/FEATURE-NAME.md`.

Example post-mortem structure:

    # Post-Mortem: Guix System Init Implementation

    ## Timeline
    - Start: 2025-11-23 (basic impl)
    - Finish: 2025-11-25 (fully working with recovery)
    - Total: 2 days, ~10 hours invested

    ## What Went Well
    - Two-phase /var/lock approach works perfectly
    - Auto-retry for kernel files is robust
    - Good error messages guide recovery

    ## What Went Wrong
    1. **Underestimated Guix complexity** (6h vs 2h estimate)
    2. **Didn't check documentation first** (could have found /var/lock issue documented)
    3. **Assumed kernel copy was automatic** (wasted 1h debugging before checking store)

    ## Time Analysis
    | Phase | Estimated | Actual | Delta |
    |-------|-----------|--------|-------|
    | Basic impl | 2h | 2h | ✓ On target |
    | /var/lock fix | 1h | 2h | +1h (unknown issue) |
    | Kernel workaround | 1h | 4h | +3h (undocumented quirk) |
    | Recovery + verify | 1h | 2h | +1h (more robust than planned) |

    **Total: 5h estimated → 10h actual (2x factor)**

    ## Lessons for Next Time
    1. **Guix internals are complex** - multiply estimates by 2x
    2. **Check store first** when boot files missing
    3. **Read upstream issues** before debugging (might be known)
    4. **Write tests for edge cases** (idempotency, missing files)

    ## Process Improvements
    - [ ] Add "Check Guix bug tracker" to debugging checklist
    - [ ] Document common Guix quirks in KNOWN_ISSUES.md
    - [ ] Add time estimates to commit messages
    - [ ] Create TIME_MATRIX.md for future features

## Recommended Tools

### Time Tracking Tools

1. **git-hours** - Estimates time from commit history

        npm install -g git-hours
        git hours

2. **git-stats** - Visualizes commit patterns

        npm install -g git-stats
        git-stats

3. **Manual time log** - Simple markdown file

        echo "2025-11-25 14:00-17:00 (3h) - System init fixes" >> docs/TIME_LOG.md

### Process Tracking

1. **ADR Tools** - Architecture decision records

        npm install -g adr-tools
        adr init docs/decisions
        adr new "Use environment variables for platform"

2. **Daily Dev Logs** - Template generator script example:

        #!/bin/bash
        DATE=$(date +%Y-%m-%d)
        LOG_FILE="docs/dev-log/$DATE.md"
        mkdir -p docs/dev-log

        if [ ! -f "$LOG_FILE" ]; then
            cat > "$LOG_FILE" << TEMPLATE
        # $DATE Development Log

        ## Session: START-END (Xh)

        ### Accomplished
        - [ ]

        ### Time Breakdown
        - START-END:

        ### Blockers
        1.

        ### Tomorrow
        -
        TEMPLATE
        fi

        $EDITOR "$LOG_FILE"

## Summary: Minimum Viable Time Tracking

If you only do 3 things:

1. **Commit message time estimates**: `[2h] Fix critical bug`
2. **Daily session logs**: `docs/dev-log/YYYY-MM-DD.md`
3. **Post-mortems for major features**: `docs/postmortems/FEATURE.md`

These three practices will give you 80% of the value with minimal overhead.

## Example Time Tracking Workflow

    # Start of day
    ./log-today.sh  # Creates/opens today's dev log

    # During work
    git commit -m "[30m] Fix label priority in device detection"

    # End of day
    # Update dev log with accomplishments, time breakdown, blockers

    # End of feature
    # Write post-mortem: what took longest, why, lessons learned

This creates a trail that future you (or other developers) can follow to understand where time was spent and how to improve the process.
