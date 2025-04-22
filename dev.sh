#!/bin/bash

# Exit on error
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Log functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Detect OS
detect_os() {
    if [ -f /etc/debian_version ]; then
        OS="debian"
        log_info "Detected Debian/Ubuntu"
    elif [ -f /etc/arch-release ]; then
        OS="arch"
        log_info "Detected Arch Linux"
    else
        log_error "Unsupported OS. This script only supports Debian/Ubuntu and Arch Linux."
        exit 1
    fi
}

# Install base dependencies
install_base_dependencies() {
    log_info "Installing base dependencies"
    
    if [ "$OS" = "debian" ]; then
        sudo apt update
        sudo apt install -y git curl wget build-essential gcc g++ make cmake pkg-config libssl-dev
    elif [ "$OS" = "arch" ]; then
        sudo pacman -Syu --noconfirm
        sudo pacman -S --needed --noconfirm git curl wget base-devel openssl
    fi
}

# Install zsh and oh-my-zsh
install_zsh() {
    log_info "Installing zsh and oh-my-zsh"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y zsh
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm zsh
    fi
    
    # Install Oh My Zsh if not already installed
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
    else
        log_info "Oh My Zsh already installed"
    fi
    
    # Install zsh plugins
    ZSH_CUSTOM=${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}
    
    # Install zsh-syntax-highlighting
    if [ ! -d "${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting" ]; then
        git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting"
    fi
    
    # Set zsh as default shell
    if [[ $SHELL != *"zsh"* ]]; then
        chsh -s "$(which zsh)"
        log_info "Shell changed to zsh. You may need to log out and back in for this to take effect."
    else
        log_info "Shell is already set to zsh"
    fi
}

# Install Neovim
install_neovim() {
    log_info "Installing Neovim"
    
    if [ "$OS" = "debian" ]; then
        # Install from PPA for latest version
        sudo apt install -y software-properties-common
        sudo add-apt-repository ppa:neovim-ppa/unstable -y
        sudo apt update
        sudo apt install -y neovim
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm neovim
    fi
}

# Install Awesome WM
install_awesome() {
    log_info "Installing Awesome WM"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y awesome awesome-extra
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm awesome
    fi
}

# Install WezTerm
install_wezterm() {
    log_info "Installing WezTerm"
    
    if [ "$OS" = "debian" ]; then
        WEZTERM_RELEASE="20230712-072601-f4abf8fd"
        curl -LO "https://github.com/wez/wezterm/releases/download/${WEZTERM_RELEASE}/wezterm-${WEZTERM_RELEASE}.Ubuntu22.04.deb"
        sudo apt install -y ./wezterm-*.deb
        rm -f ./wezterm-*.deb
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm wezterm
    fi
}

# Install Node.js and npm
install_node() {
    log_info "Installing Node.js and npm"
    
    if [ "$OS" = "debian" ]; then
        curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
        sudo apt install -y nodejs
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm nodejs npm
    fi
    
    # Install global npm packages
    npm_packages=(
        "typescript"
        "ts-node"
        "eslint"
        "prettier"
    )
    
    log_info "Installing global npm packages"
    sudo npm install -g "${npm_packages[@]}"
}

# Install Rust and Cargo
install_rust() {
    log_info "Installing Rust and Cargo"
    
    if ! command -v rustup &> /dev/null; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source "$HOME/.cargo/env"
    else
        log_info "Rust already installed, updating..."
        rustup update
    fi
    
    # Install common Rust tools
    log_info "Installing Rust tools"
    cargo install ripgrep exa bat fd-find starship
}

# Install Python and pip
install_python() {
    log_info "Installing Python and pip"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y python3 python3-pip python3-venv
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm python python-pip
    fi
    
    # Install common Python packages
    log_info "Installing Python packages"
    python3 -m pip install --user pynvim black flake8 isort mypy
}

# Install Lua and LuaRocks
install_lua() {
    log_info "Installing Lua and LuaRocks"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y lua5.3 liblua5.3-dev luarocks
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm lua luarocks
    fi
    
    # Install common Lua packages
    log_info "Installing Lua packages"
    sudo luarocks install luasocket
    sudo luarocks install inspect
    sudo luarocks install luafilesystem
}

# Install FZF
install_fzf() {
    log_info "Installing FZF"
    
    if [ ! -d "$HOME/.fzf" ]; then
        git clone --depth 1 https://github.com/junegunn/fzf.git "$HOME/.fzf"
        "$HOME/.fzf/install" --all
    else
        log_info "FZF already installed, updating..."
        cd "$HOME/.fzf" && git pull && "$HOME/.fzf/install" --all
    fi
}

# Install docker
install_docker() {
    log_info "Installing Docker"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y apt-transport-https ca-certificates curl gnupg lsb-release
        curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
        echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
        sudo apt update
        sudo apt install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin
        
        # Add user to docker group
        sudo usermod -aG docker "$USER"
        log_info "Added user to docker group. You may need to log out and back in for this to take effect."
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm docker docker-compose
        
        # Enable and start Docker service
        sudo systemctl enable docker.service
        sudo systemctl start docker.service
        
        # Add user to docker group
        sudo usermod -aG docker "$USER"
        log_info "Added user to docker group. You may need to log out and back in for this to take effect."
    fi
}

# Install devpod for remote development
install_devpod() {
    log_info "Installing devpod"
    
    curl -L -o devpod "https://github.com/loft-sh/devpod/releases/latest/download/devpod-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m | sed 's/x86_64/amd64/')"
    chmod +x devpod
    sudo mv devpod /usr/local/bin
}

# Install additional tools
install_additional_tools() {
    log_info "Installing additional tools"
    
    if [ "$OS" = "debian" ]; then
        sudo apt install -y tmux htop neofetch jq tree unzip net-tools
        
        # Install ripgrep, fd-find, and other tools if not already installed via Rust
        if ! command -v rg &> /dev/null; then
            sudo apt install -y ripgrep
        fi
        if ! command -v fd &> /dev/null; then
            sudo apt install -y fd-find
            # Create symlink from fdfind to fd
            if [ ! -f "$HOME/.local/bin/fd" ]; then
                mkdir -p "$HOME/.local/bin"
                ln -s "$(which fdfind)" "$HOME/.local/bin/fd"
            fi
        fi
    elif [ "$OS" = "arch" ]; then
        sudo pacman -S --needed --noconfirm tmux htop neofetch jq tree unzip net-tools ripgrep fd
    fi
}

# Main installation function
main() {
    log_info "Starting installation of development environment"
    
    detect_os
    install_base_dependencies
    install_zsh
    install_neovim
    install_awesome
    install_wezterm
    install_node
    install_rust
    install_python
    install_lua
    install_fzf
    install_docker
    install_devpod
    install_additional_tools
    
    log_info "Installation complete!"
    log_info "Please log out and log back in to apply all changes."
}

# Execute main function
main
