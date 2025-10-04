#!/usr/bin/env bash
# lib/mirrors.sh - Geo-distributed mirror configuration for Guix installations

# Detect region based on environment or allow manual override
detect_region() {
  local region="${GUIX_REGION:-auto}"

  if [[ "$region" == "auto" ]]; then
    # Try to detect region based on timezone
    local tz="${TIMEZONE:-$(timedatectl show -p Timezone --value 2>/dev/null || echo "UTC")}"

    case "$tz" in
      Asia/Shanghai|Asia/Hong_Kong|Asia/Tokyo|Asia/Seoul|Asia/Singapore)
        echo "asia"
        ;;
      Europe/*|Africa/*)
        echo "europe"
        ;;
      America/*|US/*)
        echo "americas"
        ;;
      *)
        echo "global"
        ;;
    esac
  else
    echo "$region"
  fi
}

# Get best mirrors for region
# Sets: GUIX_GIT_URL, NONGUIX_GIT_URL, SUBSTITUTE_URLS (array)
get_mirrors() {
  local region=$(detect_region)

  # Allow individual URL overrides to take precedence
  if [[ -n "${GUIX_GIT_URL:-}" ]] && [[ -n "${NONGUIX_GIT_URL:-}" ]]; then
    echo "Using user-specified mirror URLs (GUIX_GIT_URL and NONGUIX_GIT_URL set)"
    return 0
  fi

  case "$region" in
    asia|china|cn)
      echo "Detected region: Asia/China - using Chinese mirrors for faster downloads"
      GUIX_GIT_URL="${GUIX_GIT_URL:-https://mirror.sjtu.edu.cn/git/guix.git}"
      NONGUIX_GIT_URL="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"
      SUBSTITUTE_URLS=(
        "https://mirror.sjtu.edu.cn/guix"
        "https://mirrors.tuna.tsinghua.edu.cn/guix"
        "https://ci.guix.gnu.org"
        "https://bordeaux.guix.gnu.org"
      )
      ;;
    europe|eu)
      echo "Detected region: Europe - using European mirrors"
      GUIX_GIT_URL="${GUIX_GIT_URL:-https://git.savannah.gnu.org/git/guix.git}"
      NONGUIX_GIT_URL="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"
      SUBSTITUTE_URLS=(
        "https://bordeaux.guix.gnu.org"
        "https://ci.guix.gnu.org"
      )
      ;;
    americas|us|na)
      echo "Detected region: Americas - using American mirrors"
      GUIX_GIT_URL="${GUIX_GIT_URL:-https://git.savannah.gnu.org/git/guix.git}"
      NONGUIX_GIT_URL="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"
      SUBSTITUTE_URLS=(
        "https://ci.guix.gnu.org"
        "https://bordeaux.guix.gnu.org"
      )
      ;;
    *)  # global/default
      echo "Using global default mirrors"
      GUIX_GIT_URL="${GUIX_GIT_URL:-https://git.savannah.gnu.org/git/guix.git}"
      NONGUIX_GIT_URL="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"
      SUBSTITUTE_URLS=(
        "https://ci.guix.gnu.org"
        "https://bordeaux.guix.gnu.org"
      )
      ;;
  esac

  echo "  Guix Git URL: $GUIX_GIT_URL"
  echo "  Nonguix URL:  $NONGUIX_GIT_URL"
  echo "  Substitutes:  ${SUBSTITUTE_URLS[*]}"

  export GUIX_GIT_URL NONGUIX_GIT_URL SUBSTITUTE_URLS
}

# Print mirror information (for debugging/verification)
show_mirror_info() {
  local region=$(detect_region)
  echo "=== Mirror Configuration ==="
  echo "Region: $region"
  echo "Guix Git: ${GUIX_GIT_URL:-not set}"
  echo "Nonguix:  ${NONGUIX_GIT_URL:-not set}"
  echo "Substitutes:"
  for url in "${SUBSTITUTE_URLS[@]:-}"; do
    echo "  - $url"
  done
  echo "==========================="
}
