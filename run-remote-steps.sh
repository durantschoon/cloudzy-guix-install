#!/usr/bin/env bash
# run-remote-steps.sh — fetch N scripts from one repo and run them interactively

set -u  # (avoid -e here so we can inspect non-zero exits without aborting)
IFS=$'\n\t'

### --- CONFIG ---------------------------------------------------------------

# 1) Point to your repo. You can use a branch or PIN to a commit SHA.
OWNER_REPO="durantschoon/cloudzy-guix-install"
REF="v0.1.0-alpha"             # e.g. "main" or a full commit SHA like "4b7f1d9...", PREFER a tag or release
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
  ["01-partition.sh"]="86c0b5ea174127337e8a0d34c542b52c57931f837b37a4033f2001b37dd4352a"
  ["02-mount-bind.sh"]="1bd7ec7049776f6fee97c7a09c84132b0f9ffa7070e268aed7d6e8fbc5e7e00c"
  ["03-config-write.sh"]="a77b17bdbdee73c2a51d520a74b4aea0a12c6c249241940cfd35a4b64305fd67"
  ["04-system-init.sh"]="50cb2fd7c569d8c43fc48d047abf77029332279e609d8ba0ca20df84a3482144"
  ["05-postinstall-console.sh"]="d2fb7ce4e73f54c2922f42d53aae3863ea1fa76ddad39c15d13d3fd2e5820112"
  ["06-postinstall-own-terminal.sh"]="0f0a24c6d9ac21ced8bd8c73e109c6051e779355057127722ef75a3b593ff8bc"
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
  curl -fsSL "$url" -o "$dest" || { err "Download failed: $url"; return 1; }
  chmod +x "$dest"
  if [[ -n "${SHA256[$rel]:-}" ]]; then
    echo "${SHA256[$rel]}  ${dest}" | sha256sum -c - || {
      err "SHA256 mismatch for $rel"; return 1; }
  fi
  echo "$dest"
}

run_step(){ # run_step local_script_path
  local script="$1" name="$(basename "$script")"
  mkdir -p "$LOGDIR"
  local log="${LOGDIR}/${name}.log"
  msg "Running ${name}"
  # Run in a clean subshell; preserve PATH for tiny ISOs
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

for rel in "${SCRIPTS[@]}"; do
  msg "Fetch $rel"
  local local_path="$(fetch_file "$rel")" || {
    err "Failed to fetch $rel"; exit 1; }
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
