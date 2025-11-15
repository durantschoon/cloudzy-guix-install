#!/usr/bin/env bash
# Verify that batch conversion system is ready for future runs
# Checks all prerequisites, scripts, and configuration

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[1;34m'
NC='\033[0m'

ERRORS=0
WARNINGS=0

echo -e "${BLUE}=== Batch Conversion System Verification ===${NC}"
echo ""

# Check required scripts
echo -e "${BLUE}Checking required scripts...${NC}"
REQUIRED_SCRIPTS=(
  "generate-batch-conversion.sh"
  "submit-batch.sh"
  "check-batch-status.sh"
  "retrieve-batch.sh"
  "validate-comment-structure.sh"
  "diff-conversions.sh"
  "view-jsonl.sh"
)

for script in "${REQUIRED_SCRIPTS[@]}"; do
  if [ -f "$SCRIPT_DIR/$script" ] && [ -x "$SCRIPT_DIR/$script" ]; then
    echo -e "  ${GREEN}✓${NC} $script"
  else
    echo -e "  ${RED}✗${NC} $script (missing or not executable)"
    ERRORS=$((ERRORS + 1))
  fi
done
echo ""

# Check API key setup
echo -e "${BLUE}Checking API key configuration...${NC}"
# Check for .env in repo root first, then tools directory
ENV_FILE=""
if [ -f "$REPO_ROOT/.env" ]; then
  ENV_FILE="$REPO_ROOT/.env"
elif [ -f "$SCRIPT_DIR/.env" ]; then
  ENV_FILE="$SCRIPT_DIR/.env"
fi

if [ -n "$ENV_FILE" ]; then
  if grep -q "ANTHROPIC_API_KEY=" "$ENV_FILE" && ! grep -q "your-api-key-here" "$ENV_FILE"; then
    echo -e "  ${GREEN}✓${NC} API key configured in .env file ($(basename "$(dirname "$ENV_FILE")")/.env)"
  else
    echo -e "  ${YELLOW}⚠${NC} .env file exists but API key not configured ($(basename "$(dirname "$ENV_FILE")")/.env)"
    WARNINGS=$((WARNINGS + 1))
  fi
elif [ -n "${ANTHROPIC_API_KEY:-}" ]; then
  echo -e "  ${GREEN}✓${NC} API key set via environment variable"
else
  echo -e "  ${YELLOW}⚠${NC} API key not configured (set ANTHROPIC_API_KEY or create .env in repo root)"
  WARNINGS=$((WARNINGS + 1))
fi

if [ -f "$SCRIPT_DIR/.env.example" ]; then
  echo -e "  ${GREEN}✓${NC} .env.example template exists"
else
  echo -e "  ${RED}✗${NC} .env.example template missing"
  ERRORS=$((ERRORS + 1))
fi
echo ""

# Check required dependencies
echo -e "${BLUE}Checking dependencies...${NC}"
if command -v python3 >/dev/null 2>&1; then
  echo -e "  ${GREEN}✓${NC} python3"
else
  echo -e "  ${RED}✗${NC} python3 (required for JSON processing)"
  ERRORS=$((ERRORS + 1))
fi

if command -v curl >/dev/null 2>&1; then
  echo -e "  ${GREEN}✓${NC} curl"
else
  echo -e "  ${RED}✗${NC} curl (required for API requests)"
  ERRORS=$((ERRORS + 1))
fi

if command -v git >/dev/null 2>&1; then
  echo -e "  ${GREEN}✓${NC} git"
else
  echo -e "  ${RED}✗${NC} git (required for path operations)"
  ERRORS=$((ERRORS + 1))
fi

# Optional but recommended
if command -v jq >/dev/null 2>&1; then
  echo -e "  ${GREEN}✓${NC} jq (optional, for better JSON viewing)"
else
  echo -e "  ${YELLOW}⊘${NC} jq (optional, install for better JSON viewing)"
fi

if command -v guile >/dev/null 2>&1; then
  echo -e "  ${GREEN}✓${NC} guile (for testing converted scripts)"
else
  echo -e "  ${YELLOW}⊘${NC} guile (optional, install for local testing)"
fi
echo ""

# Check documentation files
echo -e "${BLUE}Checking documentation files...${NC}"
DOC_FILES=(
  "$REPO_ROOT/docs/GUILE_KNOWLEDGE.md"
  "$REPO_ROOT/docs/GUILE_BEST_PRACTICES.md"
  "$REPO_ROOT/docs/GUILE_GOTCHAS.md"
  "$REPO_ROOT/docs/GUILE_CONVERSION.md"
  "$SCRIPT_DIR/README.md"
)

for doc in "${DOC_FILES[@]}"; do
  if [ -f "$doc" ]; then
    echo -e "  ${GREEN}✓${NC} $(basename "$doc")"
  else
    echo -e "  ${RED}✗${NC} $(basename "$doc") (missing)"
    ERRORS=$((ERRORS + 1))
  fi
done
echo ""

# Check .gitignore configuration
echo -e "${BLUE}Checking .gitignore configuration...${NC}"
if grep -q "batch-requests.jsonl" "$SCRIPT_DIR/.gitignore" 2>/dev/null || grep -q "batch-requests.jsonl" "$REPO_ROOT/.gitignore" 2>/dev/null; then
  echo -e "  ${GREEN}✓${NC} batch-requests.jsonl ignored"
else
  echo -e "  ${YELLOW}⚠${NC} batch-requests.jsonl not in .gitignore (should be ignored)"
  WARNINGS=$((WARNINGS + 1))
fi

if grep -q "batch-results.jsonl" "$SCRIPT_DIR/.gitignore" 2>/dev/null || grep -q "batch-results.jsonl" "$REPO_ROOT/.gitignore" 2>/dev/null; then
  echo -e "  ${GREEN}✓${NC} batch-results.jsonl ignored"
else
  echo -e "  ${YELLOW}⚠${NC} batch-results.jsonl not in .gitignore (should be ignored)"
  WARNINGS=$((WARNINGS + 1))
fi

# Check if converted-scripts is properly configured (not ignored, will be tracked)
if grep -q "^converted-scripts/" "$SCRIPT_DIR/.gitignore" 2>/dev/null; then
  echo -e "  ${YELLOW}⚠${NC} converted-scripts/ is ignored in tools/.gitignore (should be tracked)"
  WARNINGS=$((WARNINGS + 1))
else
  echo -e "  ${GREEN}✓${NC} converted-scripts/ will be tracked (correctly configured)"
fi
echo ""

# Check conversion prompt includes test generation
echo -e "${BLUE}Checking conversion prompt...${NC}"
if grep -q "TEST GENERATION" "$SCRIPT_DIR/generate-batch-conversion.sh"; then
  echo -e "  ${GREEN}✓${NC} Test generation included in prompt"
else
  echo -e "  ${RED}✗${NC} Test generation not found in conversion prompt"
  ERRORS=$((ERRORS + 1))
fi

if grep -q "COMMENT STRUCTURE PRESERVATION" "$SCRIPT_DIR/generate-batch-conversion.sh"; then
  echo -e "  ${GREEN}✓${NC} Comment structure preservation included"
else
  echo -e "  ${RED}✗${NC} Comment structure preservation not found"
  ERRORS=$((ERRORS + 1))
fi
echo ""

# Check retrieve script handles test files
echo -e "${BLUE}Checking retrieve script...${NC}"
if grep -q "TEST FILE" "$SCRIPT_DIR/retrieve-batch.sh"; then
  echo -e "  ${GREEN}✓${NC} Test file extraction implemented"
else
  echo -e "  ${RED}✗${NC} Test file extraction not implemented"
  ERRORS=$((ERRORS + 1))
fi
echo ""

# Check test runner includes converted scripts
echo -e "${BLUE}Checking test runner...${NC}"
if grep -q "converted-scripts" "$REPO_ROOT/run-tests.sh"; then
  echo -e "  ${GREEN}✓${NC} run-tests.sh includes converted script tests"
else
  echo -e "  ${YELLOW}⚠${NC} run-tests.sh may not include converted script tests"
  WARNINGS=$((WARNINGS + 1))
fi
echo ""

# Summary
echo -e "${BLUE}=== Summary ===${NC}"
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
  echo -e "${GREEN}✓ All checks passed! System is ready for batch conversion.${NC}"
  exit 0
elif [ $ERRORS -eq 0 ]; then
  echo -e "${YELLOW}⚠ System is ready but has $WARNINGS warning(s) (see above)${NC}"
  exit 0
else
  echo -e "${RED}✗ System has $ERRORS error(s) and $WARNINGS warning(s)${NC}"
  echo ""
  echo "Fix errors before running batch conversion."
  exit 1
fi

