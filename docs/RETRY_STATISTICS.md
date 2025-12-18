# Retry Statistics Tracking

## Overview

The `VerifyAndRecoverKernelFiles()` function in stage 4 (system init) uses a triple-retry mechanism to recover kernel/initrd files. This document explains the retry statistics tracking system that helps determine if retries are actually useful.

## The Question

**Observation:** Retries either succeed on the first attempt or fail all 3 times. This suggests retries might not be useful.

**Goal:** Track retry statistics to determine:
- How often retries succeed on attempt 2 or 3
- What failure reasons cause retries
- Whether the 10-second wait between retries helps
- If retries should be removed or modified

## Statistics Tracked

Each retry attempt logs:

```json
{
  "attempt": 1,
  "startTime": 1234567890,
  "kernelFound": false,
  "initrdFound": false,
  "failureReason": "kernel_missing, initrd_missing",
  "recovered": false,
  "duration": 2.5
}
```

Overall retry statistics:

```json
{
  "maxAttempts": 3,
  "platform": "framework-dual",
  "buildType": "non-libre",
  "success": true,
  "successAttempt": 1,
  "failureReason": "",
  "totalDuration": 2.5,
  "attempts": [...]
}
```

## Failure Reasons Tracked

1. **`kernel_missing`** - Kernel file not found in `/mnt/boot/`
2. **`initrd_missing`** - Initrd file not found in `/mnt/boot/`
3. **`system_link_broken`** - System generation symlink exists but is broken
4. **`system_link_missing`** - System generation symlink doesn't exist
5. **`recovery_strategies_failed`** - All recovery strategies (K, N, E) failed
6. **`all_recovery_attempts_failed`** - All retry attempts failed

## What We'll Learn

### Success Patterns
- **First-attempt success rate**: How often files are found immediately
- **Retry success rate**: How often retries succeed on attempt 2 or 3
- **Success attempt distribution**: Which attempt number succeeds most often

### Failure Patterns
- **Failure reason distribution**: Which failures are most common
- **Failure consistency**: Do the same failures repeat across retries?
- **Failure resolution**: Do failures resolve themselves after waiting?

### Timing Patterns
- **Duration per attempt**: How long each attempt takes
- **Total duration**: How long the entire retry process takes
- **Wait effectiveness**: Does the 10-second wait help?

## Analysis Questions

1. **Are retries useful?**
   - If success rate on attempt 1 = success rate overall → retries don't help
   - If success rate increases on attempts 2-3 → retries are useful

2. **What causes failures?**
   - If `system_link_missing` → `guix system init` didn't complete properly
   - If `recovery_strategies_failed` → kernel/initrd weren't created by build
   - If same failure repeats → retries won't help

3. **Should we remove retries?**
   - If retry success rate < 5% → consider removing retries
   - If retry success rate > 20% → keep retries
   - If failures are consistent → retries won't help

## Log Location

Retry statistics are logged to kernel tracking logs (`/tmp/kernel_tracking.log`) with:
- `hypothesisId: "E"`
- `step: "retry_stats_success"` or `"retry_stats_final_failure"`

## Example Analysis

After collecting data from multiple installations, we can analyze:

```bash
# Count success attempts
grep "retry_stats_success" /tmp/kernel_tracking.log | jq '.data.retryStats.successAttempt' | sort | uniq -c

# Count failure reasons
grep "retry_stats_final_failure" /tmp/kernel_tracking.log | jq '.data.retryStats.failureReason' | sort | uniq -c

# Calculate retry effectiveness
# (successes on attempt 2 or 3) / (total successes)
```

## Decision Criteria

Based on statistics, we'll decide:

- **Remove retries** if:
  - Retry success rate < 5%
  - Same failure repeats across all attempts
  - No time-dependent failures observed

- **Keep retries** if:
  - Retry success rate > 20%
  - Different failures occur across attempts
  - Time-dependent failures observed (e.g., symlink creation delay)

- **Modify retries** if:
  - Specific failure reasons benefit from retries
  - Different retry strategies needed for different failures
  - Wait time should be adjusted

## Next Steps

1. Collect statistics from multiple installations
2. Analyze retry success/failure patterns
3. Make data-driven decision about retry mechanism
4. Update code based on findings
