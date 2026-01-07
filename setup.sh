#!/usr/bin/env bash
# Emacs setup script for macOS and Linux
# Installs Emacs and sets up config symlink

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_LINK="${HOME}/.config/emacs"
REINSTALL_EMACS=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --reinstall) REINSTALL_EMACS=true; shift ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Detect OS
detect_os() {
    case "$(uname -s)" in
        Darwin*) echo "macos" ;;
        Linux*)  echo "linux" ;;
        *)       echo "unknown" ;;
    esac
}

OS=$(detect_os)

echo "=== Emacs Setup ($OS) ==="
echo ""

################################################################################
# Emacs Installation
################################################################################

install_emacs_macos() {
    if ! command -v brew &> /dev/null; then
        echo "Error: Homebrew is not installed."
        echo "Install it from https://brew.sh"
        exit 1
    fi

    echo ">> Tapping d12frosted/emacs-plus..."
    brew tap d12frosted/emacs-plus 2>/dev/null || true

    echo ""
    echo ">> Installing emacs-plus@31 (Emacs 31 with native compilation)..."
    echo "   This may take a while as it compiles from source..."
    echo ""

    if brew list d12frosted/emacs-plus/emacs-plus@31 &>/dev/null; then
        echo "   emacs-plus@31 is already installed"
    else
        brew install d12frosted/emacs-plus/emacs-plus@31
    fi

    BREW_PREFIX="$(brew --prefix)"
    EMACS_BIN_DIR="${BREW_PREFIX}/opt/emacs-plus@31/bin"
}

install_emacs_amazon_linux() {
    echo ">> Building Emacs 31 from source for Amazon Linux..."
    echo ""

    # Install build dependencies
    echo "   Installing build dependencies..."
    sudo yum groupinstall -y "Development Tools" 2>/dev/null || true
    sudo yum install -y \
        ncurses-devel \
        libxml2-devel \
        jansson-devel \
        texinfo \
        autoconf \
        git

    # gnutls-devel is required for HTTPS package downloads
    # Try multiple approaches for Amazon Linux 2
    if ! pkg-config --exists gnutls 2>/dev/null; then
        echo "   Installing gnutls-devel..."
        sudo yum install -y gnutls-devel 2>/dev/null || \
        sudo amazon-linux-extras install -y epel && sudo yum install -y gnutls-devel 2>/dev/null || \
        echo "   Warning: gnutls-devel install failed, trying gnutls3..."
        sudo yum install -y gnutls3-devel 2>/dev/null || true
    fi

    # Optional: native compilation (may not be available)
    sudo yum install -y libgccjit-devel 2>/dev/null || true

    # Tree-sitter support
    if ! pkg-config --exists tree-sitter 2>/dev/null; then
        echo "   Installing tree-sitter from source..."
        TREESIT_SRC="${HOME}/tree-sitter-src"
        git clone --depth 1 https://github.com/tree-sitter/tree-sitter.git "$TREESIT_SRC" 2>/dev/null || \
            (cd "$TREESIT_SRC" && git pull)
        cd "$TREESIT_SRC"
        make -j$(nproc)
        sudo make install
        # Ensure /usr/local/lib is in linker path
        echo "/usr/local/lib" | sudo tee /etc/ld.so.conf.d/local.conf
        sudo ldconfig
        cd -
    fi

    # Clone and build
    EMACS_SRC="${HOME}/emacs-31-src"
    if [ -d "$EMACS_SRC" ]; then
        echo "   Source directory exists, updating..."
        cd "$EMACS_SRC" && git pull
    else
        echo "   Cloning Emacs 31 (master branch)..."
        git clone --depth 1 git://git.savannah.gnu.org/emacs.git "$EMACS_SRC"
    fi
    cd "$EMACS_SRC"

    echo "   Running autogen.sh..."
    ./autogen.sh

    echo "   Configuring (terminal-only build)..."
    CONFIGURE_OPTS="--without-x --with-tree-sitter"

    # Check for gnutls - required for HTTPS/TLS
    if ! pkg-config --exists gnutls 2>/dev/null; then
        echo "   ERROR: gnutls not found. Cannot build without TLS support."
        exit 1
    fi

    # Check for native compilation (optional)
    if pkg-config --exists libgccjit 2>/dev/null; then
        CONFIGURE_OPTS="$CONFIGURE_OPTS --with-native-compilation=aot"
    fi

    # Ensure pkg-config can find tree-sitter in /usr/local
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

    echo ""
    echo "   Running: ./configure $CONFIGURE_OPTS"
    echo ""
    ./configure $CONFIGURE_OPTS

    echo "   Building (this may take 5-15 minutes)..."
    make -j$(nproc)

    echo "   Installing to /usr/local..."
    sudo make install

    EMACS_BIN_DIR="/usr/local/bin"
}

install_emacs_linux() {
    echo ">> Checking for Emacs..."

    # Detect Amazon Linux
    if [ -f /etc/os-release ] && grep -q 'ID="amzn"' /etc/os-release; then
        echo "   Detected Amazon Linux"

        if [ -x /usr/local/bin/emacs ] && [ "$REINSTALL_EMACS" = false ]; then
            EMACS_VERSION=$(/usr/local/bin/emacs --version | head -1)
            MAJOR_VERSION=$(/usr/local/bin/emacs --version | head -1 | grep -oE '[0-9]+' | head -1)
            if [ "$MAJOR_VERSION" -ge 31 ]; then
                echo "   Found: $EMACS_VERSION"
                EMACS_BIN_DIR="/usr/local/bin"
                return
            fi
        fi

        if [ "$REINSTALL_EMACS" = true ]; then
            echo "   Reinstalling Emacs..."
            install_emacs_amazon_linux
            return
        fi

        echo ""
        read -p "   Build and install Emacs 31 from source? [y/N] " -n 1 -r
        echo ""
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            install_emacs_amazon_linux
        else
            echo "   Skipped. You can run this script again to install later."
            EMACS_BIN_DIR=""
        fi
        return
    fi

    if command -v emacs &> /dev/null; then
        EMACS_VERSION=$(emacs --version | head -1)
        echo "   Found: $EMACS_VERSION"
        EMACS_BIN_DIR=""  # Use system PATH

        # Check version
        MAJOR_VERSION=$(emacs --version | head -1 | grep -oE '[0-9]+' | head -1)
        if [ "$MAJOR_VERSION" -lt 29 ]; then
            echo ""
            echo "   Warning: Emacs $MAJOR_VERSION detected. This config requires Emacs 29+."
            echo "   Consider upgrading via your package manager or building from source."
        fi
    else
        echo "   Emacs not found. Installing..."
        echo ""

        # Detect package manager and install
        if command -v apt-get &> /dev/null; then
            echo "   Detected apt (Debian/Ubuntu)"
            echo "   For latest Emacs, consider: sudo add-apt-repository ppa:ubuntuhandbook1/emacs"
            read -p "   Install emacs with apt? [y/N] " -n 1 -r
            echo ""
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                sudo apt-get update && sudo apt-get install -y emacs
            fi
        elif command -v dnf &> /dev/null; then
            echo "   Detected dnf (Fedora/RHEL)"
            read -p "   Install emacs with dnf? [y/N] " -n 1 -r
            echo ""
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                sudo dnf install -y emacs
            fi
        elif command -v pacman &> /dev/null; then
            echo "   Detected pacman (Arch)"
            read -p "   Install emacs with pacman? [y/N] " -n 1 -r
            echo ""
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                sudo pacman -S emacs
            fi
        elif command -v brew &> /dev/null; then
            echo "   Detected Homebrew on Linux"
            read -p "   Install emacs with brew? [y/N] " -n 1 -r
            echo ""
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                brew install emacs
            fi
        else
            echo "   Could not detect package manager."
            echo "   Please install Emacs 29+ manually."
        fi

        EMACS_BIN_DIR=""
    fi
}

# Install based on OS
case "$OS" in
    macos)
        install_emacs_macos
        ;;
    linux)
        install_emacs_linux
        ;;
    *)
        echo "Error: Unsupported operating system"
        exit 1
        ;;
esac

################################################################################
# Config Symlink
################################################################################

mkdir -p "${HOME}/.config"

echo ""
echo ">> Setting up config symlink..."

if [ -L "$CONFIG_LINK" ]; then
    CURRENT_TARGET="$(readlink "$CONFIG_LINK")"
    if [ "$CURRENT_TARGET" = "$SCRIPT_DIR" ]; then
        echo "   Symlink already points to this directory"
    else
        echo "   Updating symlink (was: $CURRENT_TARGET)"
        rm "$CONFIG_LINK"
        ln -s "$SCRIPT_DIR" "$CONFIG_LINK"
    fi
elif [ -e "$CONFIG_LINK" ]; then
    echo "   Warning: $CONFIG_LINK exists and is not a symlink"
    echo "   Please back it up and remove it, then run this script again"
    exit 1
else
    ln -s "$SCRIPT_DIR" "$CONFIG_LINK"
    echo "   Created symlink: $CONFIG_LINK -> $SCRIPT_DIR"
fi

################################################################################
# Verify Installation
################################################################################

echo ""
echo ">> Verifying installation..."

if [ -n "$EMACS_BIN_DIR" ] && [ -x "${EMACS_BIN_DIR}/emacs" ]; then
    EMACS_PATH="${EMACS_BIN_DIR}/emacs"
    EMACS_VERSION=$("$EMACS_PATH" --version | head -1)
    echo "   Found: $EMACS_VERSION"
elif command -v emacs &> /dev/null; then
    EMACS_VERSION=$(emacs --version | head -1)
    echo "   Found: $EMACS_VERSION"
    EMACS_BIN_DIR=""
else
    echo "   Warning: Could not find emacs binary"
fi

################################################################################
# Shell Configuration
################################################################################

echo ""
echo ">> Shell configuration"

# Detect shell config file
if [ -n "$ZSH_VERSION" ] || [ -f "${HOME}/.zshrc" ]; then
    SHELL_RC="${HOME}/.zshrc"
elif [ -f "${HOME}/.bashrc" ]; then
    SHELL_RC="${HOME}/.bashrc"
else
    SHELL_RC="${HOME}/.profile"
fi

# Build PATH export if needed
if [ -n "$EMACS_BIN_DIR" ]; then
    PATH_EXPORT="export PATH=\"${EMACS_BIN_DIR}:\$PATH\""
else
    PATH_EXPORT="# Emacs is in system PATH"
fi

SHELL_CONFIG_BLOCK='
# Emacs configuration (added by emacs.d/setup.sh)
'"${PATH_EXPORT}"'
export COLORTERM=truecolor

# Emacs client aliases (auto-starts daemon if needed)
alias e='"'"'emacsclient -t -a ""'"'"'       # open in terminal (blocking)
alias en='"'"'emacsclient -c -n -a ""'"'"'   # open new GUI frame (non-blocking)
'

# Check if config already exists
if [ -f "$SHELL_RC" ] && grep -q "emacs.d/setup.sh" "$SHELL_RC" 2>/dev/null; then
    echo "   Emacs configuration already exists in $SHELL_RC"
else
    echo ""
    echo "   The following will be added to $SHELL_RC:"
    echo "   ─────────────────────────────────────────"
    echo "$SHELL_CONFIG_BLOCK"
    echo "   ─────────────────────────────────────────"
    echo ""
    read -p "   Add this configuration to $SHELL_RC? [y/N] " -n 1 -r
    echo ""

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        touch "$SHELL_RC"
        echo "$SHELL_CONFIG_BLOCK" >> "$SHELL_RC"
        echo "   ✓ Configuration added to $SHELL_RC"
        echo "   Run 'source $SHELL_RC' or restart your terminal to apply"
    else
        echo "   Skipped. You can manually add the configuration later."
    fi
fi

################################################################################
# tmux Configuration
################################################################################

TMUX_CONF="${HOME}/.tmux.conf"
TMUX_CONFIG_BLOCK='
# Truecolor support (added by emacs.d/setup.sh)
set -g default-terminal "xterm-256color"
set -as terminal-overrides ",xterm-256color:RGB"
'

echo ""
if [ -f "$TMUX_CONF" ] && grep -q "terminal-overrides.*RGB" "$TMUX_CONF" 2>/dev/null; then
    echo "   Truecolor configuration already exists in ~/.tmux.conf"
elif command -v tmux &> /dev/null; then
    echo "   tmux detected. For truecolor support, add to ~/.tmux.conf:"
    echo "   ─────────────────────────────────────────"
    echo "$TMUX_CONFIG_BLOCK"
    echo "   ─────────────────────────────────────────"
    echo ""
    read -p "   Add this configuration to ~/.tmux.conf? [y/N] " -n 1 -r
    echo ""

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        touch "$TMUX_CONF"
        echo "$TMUX_CONFIG_BLOCK" >> "$TMUX_CONF"
        echo "   ✓ Configuration added to ~/.tmux.conf"
    else
        echo "   Skipped."
    fi
fi

################################################################################
# Done
################################################################################

echo ""
echo "=== Setup complete! ==="
echo ""
echo "Config location: $SCRIPT_DIR"
echo "Symlinked to:    $CONFIG_LINK"
echo ""
echo "Usage (after sourcing $SHELL_RC):"
echo "  e file.txt    # open in terminal (uses daemon)"
echo "  en file.txt   # open GUI frame, return to shell"
echo "  emacs -nw     # standalone terminal emacs (no daemon)"

if [ "$OS" = "linux" ]; then
    echo ""
    echo "Note: For clipboard support over SSH, OSC 52 (clipetty) should work"
    echo "automatically with Ghostty as your local terminal."
fi
