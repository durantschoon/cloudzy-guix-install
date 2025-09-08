#!/usr/bin/env bash
# run-remote-steps.sh — fetch N scripts from one repo and run them interactively

set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures
IFS=$'\n\t'

### --- CONFIG ---------------------------------------------------------------

# 1) Point to your repo. You can use a branch or PIN to a commit SHA.
OWNER_REPO="durantschoon/cloudzy-guix-install"
REF="${GUIX_INSTALL_REF:-v0.1.5}"  # Set GUIX_INSTALL_REF env var to override (e.g., "main" for debugging)
RAW_BASE="https://raw.githubusercontent.com/${OWNER_REPO}/${REF}"

# 2) List the scripts (in order) relative to the repo root.
SCRIPTS=(
  "01-partition.sh"
  "02-mount-bind.sh"
  "03-config-write.sh"
  "04-system-init.sh"
  "05-postinstall-console.sh"
  "06-postinstall-own-terminal.sh"
)

# 3) Optional: expected sha256 checksums (filename -> sha256).
# Leave empty to skip verification.
declare -A SHA256=(
  ["01-partition.sh"]="3404c26a940d3402ea3daf51a7a17c41e4fdfec355e7f75996c9dae333be8df5"
  ["02-mount-bind.sh"]="f43d90c86f96f891fabaec2ab80ac0d86b9665aa2ce94d780f047a9a1321531a"
  ["03-config-write.sh"]="c6b1ebbafa06d3dd9d3e7d9f3a55a55a96e605bf2e295622fb63fce07dd48358"
  ["04-system-init.sh"]="031d7e94ac31405efa387d3ee44d57a2fa4e1a94ff172473decb061c410c265d"
  ["05-postinstall-console.sh"]="767758bce87af36f579286edd93197958f1c69c26d7768ef9ad87196fbd52c33"
  ["06-postinstall-own-terminal.sh"]="2637f51250c713591778757587aff023358e02e3216b67f58064436a1872f2c5"
)

# 4) Where to store downloads & logs locally
WORKDIR="$(mktemp -d /tmp/run-steps.XXXXXX)"
LOGDIR="${WORKDIR}/logs"

# 5) Extra env for child scripts (edit if useful)
export TMPDIR="${TMPDIR:-/var/tmp}"
export GUIX_BUILD_OPTIONS="${GUIX_BUILD_OPTIONS:---max-jobs=1 --cores=1}"

### --- UTIL ----------------------------------------------------------------

msg(){ printf "\n\033[1;34m==> %s\033[0m\n" "$*"; }
warn(){ printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"; }
err(){ printf "\n\033[1;31m[err]\033[0m  %s\n" "$*"; }
ask_yes(){
  # ask_yes "Prompt?" default_yes|default_no
  local prompt="$1" default="$2" ans
  if [[ "$default" == default_yes ]]; then
    read -r -p "$prompt [Y/n] " ans
    [[ -z "$ans" || "$ans" =~ ^[Yy]$ ]]
  else
    read -r -p "$prompt [y/N] " ans
    [[ "$ans" =~ ^[Yy]$ ]]
  fi
}

fetch_file(){ # fetch_file path/to/script
  local rel="$1" dest="${WORKDIR}/${rel}"
  mkdir -p "$(dirname "$dest")"
  local url="${RAW_BASE}/${rel}"
  curl -fsSL "$url?$(date +%s)" -o "$dest" || { err "Download failed: $url"; return 1; }
  chmod +x "$dest"
  # Skip SHA256 verification when using main branch for debugging
  if [[ -n "${SHA256[$rel]:-}" ]] && [[ "$REF" != "main" ]]; then
    echo "${SHA256[$rel]}  ${dest}" | shasum -a 256 -c - >/dev/null 2>&1 || {
      err "SHA256 mismatch for $rel"; return 1; }
  fi
  # Only echo the path if everything succeeded
  if [[ -f "$dest" ]]; then
    echo "$dest"
  else
    err "File not found after download: $dest"
    return 1
  fi
}

run_step(){ # run_step local_script_path
  local script="$1" name="$(basename "$script")"
  mkdir -p "$LOGDIR"
  local log="${LOGDIR}/${name}.log"
  msg "Running ${name}"
  # Run in a clean subshell; preserve PATH for tiny ISOs
  # Source any existing variables from previous scripts
  if [[ -f "${WORKDIR}/script_vars.sh" ]]; then
    source "${WORKDIR}/script_vars.sh"
  fi
  ( set -o pipefail; bash "$script" 2>&1 | tee "$log" )
  local rc=${PIPESTATUS[0]}
  msg "Exit status for ${name}: ${rc}"
  echo "Log saved to: $log"
  # Show tail and offer to view all
  echo "---- last 40 lines ----"
  tail -n 40 "$log" || true
  echo "-----------------------"
  if ! ask_yes "Continue to next step?" default_yes; then
    warn "User chose to stop. Workdir: $WORKDIR"
    exit $rc
  fi
  return $rc
}

### --- MAIN ----------------------------------------------------------------

msg "Workdir: ${WORKDIR}"
msg "Fetching from: ${RAW_BASE}"

# Basic prereqs
command -v curl >/dev/null 2>&1 || { err "curl is required"; exit 1; }
mkdir -p "$WORKDIR" "$LOGDIR"

# Clear any existing script variables from previous runs
rm -f /tmp/script_vars.sh

for rel in "${SCRIPTS[@]}"; do
  msg "Fetch $rel"
  local_path="$(fetch_file "$rel")" || {
    err "Failed to fetch $rel"; exit 1; }
  # Ensure file is fully written before reading
  sleep 0.5
  # Wait for file to be readable
  echo "Checking if file is readable: $local_path"
  ls -la "$local_path" || echo "File not found or not accessible"
  timeout=30  # 30 second timeout
  count=0
  while [[ ! -r "$local_path" ]] && [[ $count -lt $timeout ]]; do
    echo "Waiting for file to be readable: $local_path (attempt $((count + 1))/$timeout)"
    sleep 1
    ((count++))
  done
  if [[ ! -r "$local_path" ]]; then
    err "Timeout waiting for file to be readable: $local_path"
    exit 1
  fi
  # Preview the script head
  echo "---- ${rel} (head) ----"
  sed -n '1,30p' "$local_path"
  echo "-----------------------"
  if ! ask_yes "Run ${rel} now?" default_yes; then
    warn "Skipping ${rel} per user request"
    continue
  fi
  run_step "$local_path" || warn "${rel} returned non-zero; you may want to stop."
done

msg "All done. Logs in: $LOGDIR"
