#!/usr/bin/env bash
# lib/runner-common.sh - Shared functions for runner scripts

# Output formatting
msg() {
  printf "\n\033[1;34m==> %s\033[0m\n" "$*"
}

warn() {
  printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"
}

err() {
  printf "\n\033[1;31m[err]\033[0m  %s\n" "$*"
}

# Verify required environment variables are set
# Usage: verify_required_vars VAR1 VAR2 VAR3
verify_required_vars() {
  local missing_vars=()
  for var in "$@"; do
    if [[ -z "${!var:-}" ]]; then
      missing_vars+=("$var")
    fi
  done

  if [[ ${#missing_vars[@]} -gt 0 ]]; then
    err "Missing required environment variables:"
    printf "  - %s\n" "${missing_vars[@]}"
    return 1
  fi
  return 0
}

# Interactive yes/no prompt
# Usage: ask_yes "Continue?" default_yes|default_no
ask_yes() {
  local prompt="$1" default="$2" ans
  if [[ "$default" == default_yes ]]; then
    read -r -p "$prompt [Y/n] " ans
    [[ -z "$ans" || "$ans" =~ ^[Yy]$ ]]
  else
    read -r -p "$prompt [y/N] " ans
    [[ "$ans" =~ ^[Yy]$ ]]
  fi
}

# Fetch and verify a script file
# Usage: fetch_file "path/to/script.sh"
# Requires: RAW_BASE, WORKDIR, SHA256 (associative array), REF
fetch_file() {
  local rel="$1"
  local dest="${WORKDIR}/${rel}"
  mkdir -p "$(dirname "$dest")"
  local url="${RAW_BASE}/${rel}"

  curl -fsSL "$url?$(date +%s)" -o "$dest" || {
    err "Download failed: $url"
    return 1
  }

  chmod +x "$dest"

  # Skip SHA256 verification when using main branch for debugging
  if [[ -n "${SHA256[$rel]:-}" ]] && [[ "$REF" != "main" ]]; then
    echo "${SHA256[$rel]}  ${dest}" | shasum -a 256 -c - >/dev/null 2>&1 || {
      err "SHA256 mismatch for $rel"
      return 1
    }
  fi

  # Only echo the path if everything succeeded
  if [[ -f "$dest" ]]; then
    echo "$dest"
  else
    err "File not found after download: $dest"
    return 1
  fi
}

# Run a script step with logging
# Usage: run_step /path/to/script.sh
run_step() {
  if [ "$#" -lt 1 ]; then
    err "run_step called without a script path"
    return 1
  fi

  local script="$1"
  local name="$(basename "$script")"
  mkdir -p "$LOGDIR"
  local log="${LOGDIR}/${name}.log"
  msg "Running ${name}"

  local old_stdout=$(mktemp)
  local old_stderr=$(mktemp)
  exec 3>&1 4>&2 # Save current stdout and stderr
  exec > >(tee "$log" >&3) 2> >(tee "$log" >&4) # Redirect to tee and original stdout/stderr

  local old_pipefail_setting
  if [[ "$(set -o | grep pipefail)" == "pipefail on" ]]; then
    old_pipefail_setting="on"
  else
    old_pipefail_setting="off"
  fi
  set -o pipefail

  . "$script" # Source the script
  local rc=$?

  if [[ "$old_pipefail_setting" == "off" ]]; then
    set +o pipefail
  fi

  exec 1>&3 2>&4 # Restore original stdout and stderr
  exec 3>&- 4>&- # Close saved file descriptors

  # Ensure log file is fully written before reading
  sync
  sleep 0.1

  msg "Exit status for ${name}: ${rc}"
  echo "Log saved to: $log"
  echo "---- last 40 lines ----"
  tail -n 40 "$log" || true
  echo "-----------------------"

  if ! ask_yes "Continue to next step?" default_yes; then
    warn "User chose to stop. Workdir: $WORKDIR"
    exit $rc
  fi
  return $rc
}

# Run a warning script step (sources in current shell to preserve exports)
# Usage: run_warning_step /path/to/warnings-script.sh
run_warning_step() {
  if [ "$#" -lt 1 ]; then
    err "run_warning_step called without a script path"
    return 1
  fi

  local script="$1"
  local name="$(basename "$script")"
  mkdir -p "$LOGDIR"
  local log="${LOGDIR}/${name}.log"
  msg "Running ${name}"

  # Save original stdout/stderr
  exec 3>&1 4>&2
  # Redirect current shell stdout/stderr through tee for logging
  exec > >(tee "$log") 2>&1

  # Source the script in the current shell so 'export VAR=...' persists
  . "$script"
  local rc=$?

  # Restore stdout/stderr
  exec 1>&3 2>&4
  exec 3>&- 4>&-

  # Ensure log file is fully written before reading
  sync
  sleep 0.1

  msg "Exit status for ${name}: ${rc}"
  echo "Log saved to: $log"
  echo "---- last 40 lines ----"
  tail -n 40 "$log" || true
  echo "-----------------------"

  if ! ask_yes "Continue to next step?" default_yes; then
    warn "User chose to stop. Workdir: $WORKDIR"
    exit $rc
  fi
  return $rc
}
