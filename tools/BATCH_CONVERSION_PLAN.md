# Batch Conversion Plan - Bash to Guile Migration

**Status**: Phase 1 Complete - Ready for Enhancement
**Parallel Project**: Can be worked on alongside framework-dual development

---

## Overview

Automated system for converting `.sh` scripts to `.scm` using Anthropic Batch API with comprehensive validation.

## Current Status (Phase 1 Complete ✅)

**Tools Built:**
- ✅ `generate-batch-conversion.sh` - Creates batch requests
- ✅ `submit-batch.sh` - Submits to Anthropic API
- ✅ `check-batch-status.sh` - Monitors progress
- ✅ `retrieve-batch.sh` - Downloads and extracts results
- ✅ `tools/README.md` - Complete workflow documentation

**Documentation Embedded:**
- ✅ GUILE_KNOWLEDGE.md
- ✅ GUILE_BEST_PRACTICES.md
- ✅ GUILE_GOTCHAS.md (frequency-sorted)
- ✅ GUILE_CONVERSION.md

**Cost**: ~$0.12 for 3 recipe scripts (50% savings vs interactive)

---

## Enhancement Plan (Phase 2 - IN PROGRESS)

### Priority 1: Validation & Safety (Must-Have)

**Goal**: Catch errors before they reach production

#### 1.1 Enhanced Batch Prompt ⏳
**File**: `tools/generate-batch-conversion.sh`

Add validation requirements to prompt:
```bash
VALIDATION_REQUIREMENTS="
CRITICAL REQUIREMENTS:

1. EXACT FUNCTIONAL EQUIVALENCE
   - Every bash function → Guile procedure
   - No logic changes, only syntax translation
   - Mark uncertainties: ;; TODO: VERIFY

2. ENVIRONMENT CHECKS
   - Commands: sha256sum (NOT shasum)
   - Paths: /run/current-system/profile/bin/guile
   - Guile 3.0+ features only

3. CONFIG.SCM SAFETY
   - Use ONLY guile-config-helper.scm functions
   - Never use sed/awk on config.scm
   - Comment all transformations

4. SUDO DOCUMENTATION
   - Mark: ;; REQUIRES-SUDO
   - List at top of file
   - Example: ;; SUDO: cp /etc/config.scm

5. SELF-VALIDATION
   Add to every converted script:
   ;; VALIDATION CHECKLIST:
   ;; [ ] All bash functions converted
   ;; [ ] No macOS commands (shasum, md5, etc.)
   ;; [ ] All sudo operations documented
   ;; [ ] Same behavior as original
   ;; [ ] Dry-run mode implemented
"
```

**Deliverable**: Updated `generate-batch-conversion.sh` with enhanced prompt

---

#### 1.2 Syntax Validation Script ⏳
**File**: `tools/validate-syntax.sh`

**Purpose**: Run automated checks on converted scripts

```bash
#!/usr/bin/env bash
# Validates converted .scm files for common issues

validate_script() {
  local script="$1"
  local errors=0

  # 1. Syntax check
  if ! guile --no-auto-compile -c "(load \"$script\")" 2>/dev/null; then
    echo "  ✗ Syntax error in $script"
    errors=$((errors + 1))
  fi

  # 2. Shebang check
  if ! head -1 "$script" | grep -q "guile"; then
    echo "  ✗ Missing Guile shebang in $script"
    errors=$((errors + 1))
  fi

  # 3. Gotcha scan (macOS commands)
  if grep -q "shasum\|\\bmd5\\b" "$script"; then
    echo "  ✗ macOS command found in $script (use sha256sum/md5sum)"
    errors=$((errors + 1))
  fi

  # 4. SRFI-1 import check
  if grep -q "filter-map\\|fold\\|take\\|drop" "$script"; then
    if ! grep -q "(srfi srfi-1)" "$script"; then
      echo "  ✗ Missing (srfi srfi-1) import in $script"
      errors=$((errors + 1))
    fi
  fi

  # 5. Sudo documentation check
  if grep -q "sudo" "$script"; then
    if ! grep -q "REQUIRES-SUDO" "$script"; then
      echo "  ⚠  Sudo found but not documented in $script"
    fi
  fi

  if [ $errors -eq 0 ]; then
    echo "  ✓ $script passes validation"
  fi

  return $errors
}
```

**Deliverable**: `tools/validate-syntax.sh` script

---

#### 1.3 Diff-Based Validation ⏳
**File**: `tools/validate-conversion.sh`

**Purpose**: Compare .sh vs .scm behavior using dry-run mode

**Requirements**:
1. Add `--dry-run` flag to all converted scripts
2. Run both versions, capture output
3. Diff outputs to verify equivalence

**Dry-run template for converted scripts**:
```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;; Dry-run mode support
(define dry-run #f)

(define (main args)
  ;; Check for --dry-run flag
  (when (member "--dry-run" args)
    (set! dry-run #t)
    (format #t "[DRY-RUN MODE] Simulating operations\n\n"))

  ;; Before each side-effect:
  (if dry-run
      (format #t "[DRY-RUN] Would execute: ~a\n" cmd)
      (system cmd)))
```

**Validation script**:
```bash
#!/usr/bin/env bash
# tools/validate-conversion.sh

ORIGINAL_SH="$1"
CONVERTED_SCM="$2"

echo "Comparing $ORIGINAL_SH vs $CONVERTED_SCM"

# Run both with dry-run
bash "$ORIGINAL_SH" --dry-run > /tmp/sh-output.txt 2>&1
guile "$CONVERTED_SCM" --dry-run > /tmp/scm-output.txt 2>&1

# Compare
if diff -u /tmp/sh-output.txt /tmp/scm-output.txt; then
  echo "✓ Outputs match - conversion is functionally equivalent"
  exit 0
else
  echo "✗ Outputs differ - review conversion"
  exit 1
fi
```

**Deliverable**:
- `tools/validate-conversion.sh`
- Dry-run mode template added to batch prompt

---

### Priority 2: Test Account Setup (Should-Have)

**Goal**: Test in real Guix environment with limited permissions

#### 2.1 Test Account Documentation ⏳
**File**: `tools/TEST_ACCOUNT_SETUP.md`

```markdown
# Test Account Setup for Conversion Testing

## Create Test User on Guix System

```bash
# On your Guix laptop
sudo useradd -m -s /run/current-system/profile/bin/bash guix-test
sudo passwd guix-test  # Set password: ConversionTest2024!

# Create test directory structure
sudo -u guix-test mkdir -p /home/guix-test/{test-scripts,test-logs,test-configs}

# Copy test config.scm
sudo cp /etc/config.scm /home/guix-test/test-configs/config.scm.test
sudo chown guix-test:guix-test /home/guix-test/test-configs/config.scm.test
```

## Configure Sudo Whitelist

```bash
sudo visudo
# Add these lines:

# Allow guix-test to read real config
guix-test ALL=(ALL) NOPASSWD: /usr/bin/cat /etc/config.scm

# Allow copying test config (for verification)
guix-test ALL=(ALL) NOPASSWD: /usr/bin/cp /home/guix-test/test-configs/*.test /etc/config.scm.backup

# Deny everything else (default)
```

## SSH Key Setup

```bash
# On development machine
ssh-keygen -t ed25519 -f ~/.ssh/guix-test -C "guix-conversion-testing"

# Copy to test account
ssh-copy-id -i ~/.ssh/guix-test.pub guix-test@framework-laptop
```

## Deploy Converted Scripts

```bash
# From development machine
scp tools/converted-scripts/postinstall/recipes/*.scm \
    guix-test@framework-laptop:~/test-scripts/

# Run remote validation
ssh guix-test@framework-laptop \
    'cd test-scripts && for f in *.scm; do guile --no-auto-compile "$f" --dry-run; done'
```

## Logging All Actions

```bash
# On test account, add to ~/.bashrc
PROMPT_COMMAND='history -a'
export HISTFILE=~/.bash_history_conversion
export HISTTIMEFORMAT="%F %T "
export HISTSIZE=10000
```
```

**Deliverable**: `tools/TEST_ACCOUNT_SETUP.md`

---

#### 2.2 Remote Deployment Script ⏳
**File**: `tools/deploy-to-test.sh`

```bash
#!/usr/bin/env bash
# Deploy converted scripts to test account for validation

TEST_HOST="${1:-guix-test@framework-laptop}"
CONVERTED_DIR="tools/converted-scripts"

echo "Deploying to $TEST_HOST..."

# Create directories
ssh "$TEST_HOST" 'mkdir -p ~/test-scripts ~/test-logs'

# Deploy converted scripts
rsync -av "$CONVERTED_DIR/" "$TEST_HOST:~/test-scripts/"

# Deploy validation helpers
scp tools/validate-syntax.sh "$TEST_HOST:~/test-scripts/"

echo "✓ Deployment complete"
echo ""
echo "Run tests:"
echo "  ssh $TEST_HOST './test-scripts/validate-syntax.sh test-scripts/*.scm'"
```

**Deliverable**: `tools/deploy-to-test.sh`

---

### Priority 3: Sudo Authorization Workflow (Nice-to-Have)

**Goal**: Review and approve sudo commands before production

#### 3.1 Sudo Command Extractor ⏳
**File**: `tools/extract-sudo-commands.sh`

```bash
#!/usr/bin/env bash
# Extract all REQUIRES-SUDO comments from converted scripts

CONVERTED_DIR="tools/converted-scripts"

echo "Scanning for sudo requirements..."
echo ""

grep -r "REQUIRES-SUDO" "$CONVERTED_DIR" | while IFS=: read -r file comment; do
  echo "File: $file"
  echo "  $comment"
  echo ""
done
```

**Deliverable**: `tools/extract-sudo-commands.sh`

---

#### 3.2 Final Exam Script (Future)
**File**: `tools/final-exam.sh`

Interactive approval of each sudo command before execution.

**Not implementing yet** - wait until we have real conversions to test.

---

## Phase 3: Execution Plan

### Step 1: Enhanced Validation (Week 1)
- [ ] Update `generate-batch-conversion.sh` with validation requirements
- [ ] Create `validate-syntax.sh`
- [ ] Create `validate-conversion.sh` with dry-run support
- [ ] Test validation scripts locally

### Step 2: Test Account Setup (Week 1-2)
- [ ] Create `TEST_ACCOUNT_SETUP.md`
- [ ] Set up test account on framework-dual laptop
- [ ] Configure sudo whitelist
- [ ] Create `deploy-to-test.sh`
- [ ] Test deployment workflow

### Step 3: First Batch Conversion (Week 2)
- [ ] Run enhanced `generate-batch-conversion.sh`
- [ ] Submit batch via `submit-batch.sh`
- [ ] Wait 24 hours
- [ ] Retrieve results via `retrieve-batch.sh`
- [ ] Run validation suite
- [ ] Deploy to test account
- [ ] Review and iterate

### Step 4: Production Deployment (Week 3)
- [ ] All validations passing
- [ ] Test account tests passing
- [ ] Create sudo command approval workflow
- [ ] Deploy to production
- [ ] Update manifest
- [ ] Commit to main branch

---

## Success Metrics

**Phase 2 Complete When:**
- ✅ All validation scripts implemented and tested
- ✅ Test account set up and working
- ✅ At least 1 script converted, validated, and deployed successfully
- ✅ Documentation complete and up-to-date

**Production Ready When:**
- ✅ All 3 recipe scripts converted
- ✅ All tests passing on test account
- ✅ Dry-run mode verified for all scripts
- ✅ Sudo commands documented and approved
- ✅ GUILE_GOTCHAS.md updated with any new discoveries

---

## Related Files

- [tools/README.md](README.md) - Batch conversion workflow
- [docs/GUILE_CONVERSION.md](../docs/GUILE_CONVERSION.md) - Overall strategy
- [docs/GUILE_GOTCHAS.md](../docs/GUILE_GOTCHAS.md) - Common mistakes
- [CHECKLIST.md](../CHECKLIST.md) - Main project status

---

## Workflow Assumptions & Risks

### Intended Workflow

**Temporal Separation:**
- **Daytime**: Work on downstream tasks (e.g., GNOME installation, framework-dual testing)
- **Nighttime**: Launch batch conversion (runs overnight while you sleep)
- **Between sessions**: Check batch results, validate, iterate

**Code Lifecycle:**
```
┌─────────────────────────────────────────────────────┐
│ UPSTREAM CODE (What batch converts)                │
│ - Installation scripts (bootstrap, run-remote)     │
│ - Pre-GNOME setup (partitioning, base system)      │
│ - Recipe scripts (add-spacemacs.sh, etc.)          │
│ ✅ Stable - not actively changing                  │
└─────────────────────────────────────────────────────┘
                    ⬇
            Batch Conversion
                    ⬇
┌─────────────────────────────────────────────────────┐
│ DOWNSTREAM CODE (What you're actively developing)  │
│ - GNOME installation workflow                      │
│ - framework-dual postinstall                        │
│ - New service integrations                          │
│ 🔄 Active development - changes frequently         │
└─────────────────────────────────────────────────────┘
```

### Known Risks

#### Risk: Stale Upstream Code During Batch Conversion

**Scenario**:
1. You discover an install bug in `bootstrap-installer.sh`
2. You fix it and commit
3. Meanwhile, batch conversion is running with the **old, unfixed version**
4. 24 hours later, converted scripts have the same bug

**Likelihood**: Low (you're currently working downstream, not upstream)

**Impact**: Medium (waste of batch API credits, need to re-run)

**Mitigation Strategies:**

**Strategy 1: Pre-flight Freeze (Recommended for First Batch)**
```bash
# Before submitting batch:
1. Commit all upstream changes
2. Push to GitHub
3. Tag the commit: git tag batch-conversion-v1
4. Submit batch from this tagged commit
5. Don't modify upstream code during batch processing
```

**Strategy 2: Timestamp Tracking**
```bash
# In tools/generate-batch-conversion.sh, add:
echo "Batch generated from commit: $(git rev-parse HEAD)" >> batch-metadata.txt
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> batch-metadata.txt

# When results return, check:
git log --since="timestamp" -- postinstall/recipes/*.sh
# If changes found, re-run batch
```

**Strategy 3: Scope Isolation (Current Approach)**
- Work on downstream code (GNOME installation)
- Only convert stable upstream code (recipes)
- Natural separation reduces conflict risk

**Strategy 4: Acceptance Criteria**
- First batch is **exploratory** - expect to iterate
- If upstream changes during batch, consider it a learning opportunity
- Re-run batch with fixes is low cost (~$0.12)

### Best Practices

**Before Launching Batch:**
- [ ] Review git status - no uncommitted upstream changes
- [ ] Push all changes to GitHub
- [ ] Note current commit hash in batch notes
- [ ] Don't plan upstream changes during batch window

**During Batch Processing (24 hours):**
- ✅ Work on downstream code (GNOME, postinstall, etc.)
- ✅ Update documentation
- ✅ Test on real hardware
- ⚠️  Avoid changing upstream scripts (recipes, bootstrap, etc.)
- ⚠️  If urgent upstream fix needed, note it for re-batch

**After Batch Completes:**
- [ ] Check git log for upstream changes during batch window
- [ ] If changes found, decide: use old conversion or re-run batch
- [ ] Update BATCH_CONVERSION_PLAN.md with lessons learned

### Future Consideration

If batch conversions become frequent, consider:
- **CI/CD integration**: Auto-trigger batch on upstream changes
- **Incremental batches**: Only convert changed files
- **Version pinning**: Batch always uses tagged releases

---

## Notes

This is a **parallel project** that can be worked on independently of the main framework-dual setup goal. The batch conversion system is production-ready, but these enhancements will significantly improve safety and reliability.

**Timeline**: 2-3 weeks to complete all enhancements, running in parallel with framework-dual development.

**Workflow Safety**: Designed for overnight batch processing while you work on downstream code during the day. Temporal separation reduces risk of code conflicts.
