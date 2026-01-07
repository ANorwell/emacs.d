# Emacs Configuration - AI Assistant Guidelines

This document provides context for AI assistants working with this Emacs configuration.

## Repository Structure

```
emacs.d/
├── init.el           # Main configuration file
├── early-init.el     # Early initialization (before GUI)
├── setup.sh          # Installation script (macOS + Linux)
├── extras/           # Optional feature modules (from Bedrock)
│   ├── base.el       # UI/UX enhancements (Embark, Avy, etc.)
│   ├── dev.el        # Development tools (Eglot, Magit, etc.)
│   ├── vim-like.el   # Evil mode configuration
│   ├── org.el        # Org-mode setup
│   ├── email.el      # Email (mu4e) configuration
│   ├── researcher.el # Academic tools (citar, etc.)
│   └── writer.el     # Writing tools
├── elpa/             # Installed packages (git-ignored)
├── eln-cache/        # Native compilation cache (git-ignored)
├── tree-sitter/      # Tree-sitter grammars (git-ignored)
├── emacs-backup/     # Centralized backup files (git-ignored)
├── .local/           # Cache/state files (git-ignored)
├── AGENTS.md         # This file
└── .gitignore
```

## Configuration Philosophy

Based on [Emacs Bedrock](https://codeberg.org/ashton314/emacs-bedrock):
- **Minimal by default**: Start with built-in features, add packages as needed
- **Documented**: Every setting should have a comment explaining why
- **Terminal-focused**: Optimized for CLI use with Emacs 31's TTY improvements

## Target Environment

- **Emacs Version**: 31+ (required for TTY child frames, best tree-sitter support)
- **Primary Use**: Terminal (`emacs -nw` or `emacsclient -t`)
- **Platforms**: macOS (via emacs-plus@31), Amazon Linux (built from source)

### Key Emacs 31 Features Used
- `tty-child-frames`: Popup completion in terminal
- Native compilation (`--with-native-compilation`)
- Tree-sitter for syntax highlighting
- Eglot (built-in LSP client)

## setup.sh Script

The setup script handles installation and configuration:

### macOS
- Installs `emacs-plus@31` via Homebrew (d12frosted tap)
- Creates symlink `~/.config/emacs` → this directory

### Amazon Linux / Linux
- Builds Emacs 31 from source with:
  - `--without-x` (terminal only)
  - `--with-tree-sitter`
  - `--with-native-compilation` (if libgccjit available)
- Installs tree-sitter library from source
- Requires: gnutls-devel, ncurses-devel, jansson-devel, texinfo

### Shell Configuration
The script offers to add to `~/.bashrc` or `~/.zshrc`:
- PATH export for Emacs binary
- `COLORTERM=truecolor` for 24-bit color
- Aliases: `e` (terminal client), `en` (GUI frame)

### Usage
```bash
./setup.sh              # Install/configure
./setup.sh --reinstall  # Force rebuild Emacs from source
```

## Installed Packages (init.el)

Core packages loaded by default:

| Package | Purpose |
|---------|---------|
| vertico | Vertical minibuffer completion |
| orderless | Flexible completion matching |
| marginalia | Minibuffer annotations |
| consult | Enhanced search/navigation |
| corfu | In-buffer completion popup |
| cape | Completion-at-point extensions |
| which-key | Keybinding discovery |
| magit | Git interface |
| diff-hl | Git gutter indicators |
| treesit-auto | Auto-install tree-sitter grammars |
| clipetty | OSC 52 clipboard sync |
| multiple-cursors | Multi-cursor editing |

## Adding Packages

1. **Prefer built-in**: Check if functionality exists in Emacs core first
2. **Use use-package**: All package configs should use `use-package` macro
3. **ELPA first**: Prefer GNU ELPA and NonGNU ELPA over MELPA
4. **Document purpose**: Add a comment explaining what the package does

Example:
```elisp
;; Completion framework for better minibuffer experience
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
```

## Enabling Extras

The `extras/` directory contains optional modules from Bedrock. To enable:

1. Add `(load-file (expand-file-name "extras/MODULE.el" user-emacs-directory))` to init.el
2. Or copy specific configurations into init.el

Note: `extras/base.el` and `extras/dev.el` overlap with init.el - they're reference configs.

## Terminal/CLI Considerations

- Avoid packages that only work in GUI mode
- Test changes in terminal: `emacs -nw`
- Use `(display-graphic-p)` guards for GUI-specific code
- diff-hl uses margin mode in terminal (no fringe)
- Corfu works in terminal via TTY child frames (Emacs 31)

## Key Bindings

Custom bindings defined in init.el:

| Binding | Command | Description |
|---------|---------|-------------|
| `C-x g` | magit-status | Open Magit |
| `C-x b` | consult-buffer | Switch buffer |
| `C-s` | consult-line | Search in buffer |
| `M-s r` | consult-ripgrep | Search in project |
| `M-s f` | consult-find | Find file in project |
| `C-c p f` | project-find-file | Find file in project |
| `C-c p s` | consult-ripgrep | Search in project |
| `C->` | mc/mark-next-like-this | Multiple cursors |
| `C-<` | mc/mark-previous-like-this | Multiple cursors |
| `C-<up>` | backward-paragraph | Navigate paragraphs |
| `C-<down>` | forward-paragraph | Navigate paragraphs |
| `Shift-<arrows>` | windmove | Navigate windows |

## LSP Setup

Eglot is configured for Python and Java. Install language servers:

```bash
# Python
pip install pyright

# Java (macOS)
brew install jdtls
```

## Testing Changes

1. Start Emacs with minimal config: `emacs -Q -nw`
2. Load specific file: `emacs -nw -l init.el`
3. Check for errors: `M-x toggle-debug-on-error`
4. Byte-compile check: `emacs --batch -f batch-byte-compile init.el`

## File Locations

- Config directory: This repo
- Symlinked to: `~/.config/emacs`
- Backup files: `./emacs-backup/` (centralized)
- Package directory: `./elpa/`
- Native comp cache: `./eln-cache/`

## Do Not Modify

- `custom-set-variables` / `custom-set-faces` blocks (managed by Emacs)
- Files in `elpa/` directory (package manager controlled)
- Files in `eln-cache/` (native compilation output)

## Common Tasks

### Adding a keybinding
```elisp
(keymap-global-set "C-c x" 'some-command)
;; Or in a specific mode:
(keymap-set some-mode-map "C-c x" 'some-command)
```

### Adding a hook
```elisp
(add-hook 'some-mode-hook 'some-function)
```

### Conditional GUI/Terminal code
```elisp
(if (display-graphic-p)
    (some-gui-function)
  (some-tty-function))
```

### Installing a tree-sitter grammar
```
M-x treesit-install-language-grammar RET python RET
```
Or set `(treesit-auto-install t)` for automatic installation.
