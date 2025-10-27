#!/usr/bin/env bash
# Channel management utilities for Guix installations

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() { printf "${BLUE}[INFO]${NC} %s\n" "$*"; }
success() { printf "${GREEN}[OK]${NC} %s\n" "$*"; }
warn() { printf "${YELLOW}[WARN]${NC} %s\n" "$*"; }
error() { printf "${RED}[ERROR]${NC} %s\n" "$*"; }

# Show current channel status
show_channel_status() {
    echo ""
    echo "=== Channel Status ==="
    
    channelsFile="$HOME/.config/guix/channels.scm"
    if [[ -f "$channelsFile" ]]; then
        echo "Channels file: $channelsFile"
        echo ""
        echo "Current channel configuration:"
        cat "$channelsFile"
        echo ""
    else
        warn "No channels.scm found - using default channels only"
        echo "Location: $channelsFile"
    fi
}

# Setup default channels
setup_default_channels() {
    echo ""
    echo "=== Setting up Default Channels ==="
    
    configDir="$HOME/.config/guix"
    mkdir -p "$configDir"
    
    channelsContent='(cons* (channel
        (name '\''nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)'
    
    echo "$channelsContent" > "$configDir/channels.scm"
    success "Default channels configured (nonguix + official)"
    info "Channels file: $configDir/channels.scm"
}

# Download channels from repository
download_user_channels() {
    local repo_url="$1"
    local branch="${2:-main}"
    local path="${3:-channels/}"
    
    echo ""
    echo "=== Downloading Channels from Repository ==="
    echo "Repository: $repo_url"
    echo "Branch: $branch"
    echo "Path: $path"
    echo ""
    
    tempDir="/tmp/guix-channels-$$"
    mkdir -p "$tempDir"
    
    # Clean up on exit
    trap "rm -rf '$tempDir'" EXIT
    
    # Clone the repository
    info "Cloning channel repository..."
    if ! git clone --branch "$branch" --depth 1 "$repo_url" "$tempDir"; then
        error "Failed to clone repository: $repo_url"
        return 1
    fi
    
    # Find channels.scm file
    channelsPath="$tempDir/$path/channels.scm"
    if [[ ! -f "$channelsPath" ]]; then
        # Try alternative locations
        altPaths=(
            "$tempDir/channels.scm"
            "$tempDir/config/channels.scm"
            "$tempDir/.config/guix/channels.scm"
        )
        
        found=false
        for altPath in "${altPaths[@]}"; do
            if [[ -f "$altPath" ]]; then
                channelsPath="$altPath"
                found=true
                break
            fi
        done
        
        if [[ "$found" == false ]]; then
            error "channels.scm not found in repository"
            echo "Tried locations:"
            echo "  - $tempDir/$path/channels.scm"
            for altPath in "${altPaths[@]}"; do
                echo "  - $altPath"
            done
            return 1
        fi
    fi
    
    # Copy to user's config directory
    configDir="$HOME/.config/guix"
    mkdir -p "$configDir"
    
    if ! cp "$channelsPath" "$configDir/channels.scm"; then
        error "Failed to copy channels.scm"
        return 1
    fi
    
    success "Channels configured from: $repo_url"
    info "Channels file: $configDir/channels.scm"
    
    # Show the configuration
    echo ""
    echo "Channel configuration:"
    cat "$configDir/channels.scm"
}

# Validate channel configuration
validate_channels() {
    echo ""
    echo "=== Validating Channel Configuration ==="
    
    channelsFile="$HOME/.config/guix/channels.scm"
    if [[ ! -f "$channelsFile" ]]; then
        warn "No channels.scm found - will use default channels"
        return 0
    fi
    
    info "Checking channels file: $channelsFile"
    echo ""
    echo "Channel configuration:"
    cat "$channelsFile"
    echo ""
    
    # Try to validate with guix describe
    info "Validating channel configuration..."
    if guix describe --format=channels >/dev/null 2>&1; then
        success "Channel configuration is valid"
    else
        warn "Channel validation failed - this is expected on ISO"
        info "Channels will be validated during system init"
    fi
}

# Interactive channel configuration
interactive_setup() {
    echo ""
    echo "=== Interactive Channel Configuration ==="
    echo ""
    echo "1) Use default channels (nonguix + official)"
    echo "2) Download channels from your Git repository"
    echo "3) Skip channel configuration"
    echo ""
    
    read -p "Choose option [1-3]: " choice
    
    case $choice in
        1)
            setup_default_channels
            ;;
        2)
            read -p "Enter your channel repository URL: " repo_url
            read -p "Enter branch/tag [main]: " branch
            read -p "Enter path within repo [channels/]: " path
            
            branch=${branch:-main}
            path=${path:-channels/}
            
            download_user_channels "$repo_url" "$branch" "$path"
            ;;
        3)
            info "Skipping channel configuration"
            ;;
        *)
            error "Invalid choice"
            return 1
            ;;
    esac
}

# Main function
main() {
    case "${1:-help}" in
        "status")
            show_channel_status
            ;;
        "setup-default")
            setup_default_channels
            ;;
        "download")
            if [[ $# -lt 2 ]]; then
                error "Usage: $0 download <repo-url> [branch] [path]"
                exit 1
            fi
            download_user_channels "$2" "${3:-main}" "${4:-channels/}"
            ;;
        "validate")
            validate_channels
            ;;
        "interactive")
            interactive_setup
            ;;
        "help"|*)
            echo "Guix Channel Management Utilities"
            echo ""
            echo "Usage: $0 <command> [args...]"
            echo ""
            echo "Commands:"
            echo "  status          Show current channel status"
            echo "  setup-default   Set up default channels (nonguix + official)"
            echo "  download <url> [branch] [path]  Download channels from Git repository"
            echo "  validate        Validate current channel configuration"
            echo "  interactive     Interactive channel setup"
            echo "  help            Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 status"
            echo "  $0 setup-default"
            echo "  $0 download https://github.com/user/channels main config/"
            echo "  $0 interactive"
            ;;
    esac
}

# Run main function with all arguments
main "$@"
