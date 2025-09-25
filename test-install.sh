#!/bin/bash

# Test script for install.sh
# Creates a temporary environment to test the installation

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log() { echo -e "${GREEN}[TEST]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Create temporary test environment
TEST_HOME=$(mktemp -d)
export HOME="$TEST_HOME"

log "Created test environment at: $TEST_HOME"

# Copy dotfiles to test location
TEST_DOTFILES="$TEST_HOME/my-dotfiles"
cp -r "$(dirname "$0")" "$TEST_DOTFILES"

log "Running install.sh in test environment..."

# Run the install script
cd "$TEST_DOTFILES"
./install.sh

log "Installation complete. Checking results..."

# Test what was created/linked
echo ""
echo "=== INSTALLATION VERIFICATION ==="

# Check main files
check_file() {
    local file="$1"
    local description="$2"

    if [ -e "$HOME/$file" ]; then
        if [ -L "$HOME/$file" ]; then
            local target=$(readlink "$HOME/$file")
            log "✓ $description: $HOME/$file -> $target"
        else
            warn "! $description exists but is not a symlink: $HOME/$file"
        fi
    else
        error "✗ $description missing: $HOME/$file"
    fi
}

# Check vim configuration
check_file ".vimrc" "Vim config"
check_file ".vim/config" "Vim modules directory"
check_file ".vim/coc-settings.json" "CoC settings"

# Check vim modules exist
if [ -d "$HOME/.vim/config" ]; then
    log "Checking vim modules:"
    for module in basic plugins keymaps completion writing programming; do
        if [ -f "$HOME/.vim/config/$module.vim" ]; then
            log "  ✓ $module.vim"
        else
            error "  ✗ $module.vim missing"
        fi
    done
fi

# Check vim directories
for dir in autoload bundle colors plugin plugged; do
    if [ -d "$HOME/.vim/$dir" ]; then
        log "✓ Vim directory created: ~/.vim/$dir"
    else
        error "✗ Vim directory missing: ~/.vim/$dir"
    fi
done

# Check vim-plug installation
if [ -f "$HOME/.vim/autoload/plug.vim" ]; then
    log "✓ vim-plug installed"
else
    error "✗ vim-plug not installed"
fi

# Check emacs (if present)
if [ -d "$TEST_DOTFILES/.emacs.d" ]; then
    check_file ".emacs.d" "Emacs config"
fi

echo ""
log "Test completed. Test environment: $TEST_HOME"
log "To inspect manually: export HOME=$TEST_HOME && cd \$HOME"

# Optionally clean up
read -p "Clean up test environment? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$TEST_HOME"
    log "Test environment cleaned up"
else
    log "Test environment preserved at: $TEST_HOME"
fi