#!/bin/bash

# Dotfiles installation script
# Usage: ./install.sh

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to create symlink with backup
link_config() {
    local src="$1"
    local dest="$2"

    # Create destination directory if it doesn't exist
    mkdir -p "$(dirname "$dest")"

    # If destination exists and is not a symlink, back it up
    if [ -e "$dest" ] && [ ! -L "$dest" ]; then
        warn "Backing up existing $dest to $dest.backup"
        mv "$dest" "$dest.backup"
    fi

    # Remove existing symlink if it exists
    if [ -L "$dest" ]; then
        rm "$dest"
    fi

    # Create symlink
    ln -sf "$src" "$dest"
    log "Linked $src -> $dest"
}

echo "Installing dotfiles from $DOTFILES_DIR"

# Vim configuration
if [ -d "$DOTFILES_DIR/vim" ]; then
    link_config "$DOTFILES_DIR/vim/.vimrc" "$HOME/.vimrc"
    link_config "$DOTFILES_DIR/vim/coc-settings.json" "$HOME/.vim/coc-settings.json"

    # Link the entire config directory for modular structure
    link_config "$DOTFILES_DIR/vim/config" "$HOME/.vim/config"

    # Create vim directories
    mkdir -p "$HOME/.vim/"{autoload,bundle,colors,plugin,plugged}

    # Install vim-plug if not present
    if [ ! -f "$HOME/.vim/autoload/plug.vim" ]; then
        log "Installing vim-plug..."
        curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    fi

    log "Modular Vim configuration linked successfully"
fi

# Emacs configuration
if [ -d "$DOTFILES_DIR/.emacs.d" ]; then
    link_config "$DOTFILES_DIR/.emacs.d" "$HOME/.emacs.d"
fi

# Git configuration (if present)
if [ -f "$DOTFILES_DIR/.gitconfig" ]; then
    link_config "$DOTFILES_DIR/.gitconfig" "$HOME/.gitconfig"
fi

# Shell configurations (if present)
for shell_config in .bashrc .zshrc .profile; do
    if [ -f "$DOTFILES_DIR/$shell_config" ]; then
        link_config "$DOTFILES_DIR/$shell_config" "$HOME/$shell_config"
    fi
done

echo ""
log "Dotfiles installation complete!"
echo ""
echo "Next steps:"
echo "  1. Restart your terminal or source your shell config"
echo "  2. Open vim and run :PlugInstall to install plugins"
echo "  3. Use :VimConfigStatus to check modular vim configuration"
echo "  4. Use :VimConfigEdit <module> to edit specific vim modules"
echo "  5. For Emacs, packages will auto-install on first run"