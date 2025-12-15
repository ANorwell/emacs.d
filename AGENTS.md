# Emacs Configuration - Claude Code Guidelines

This document provides context and guidelines for AI assistants (Claude) managing this Emacs configuration.

## Repository Structure

```
emacs.d/
├── init.el           # Main configuration file
├── early-init.el     # Early initialization (before GUI)
├── extras/           # Optional feature modules (from Bedrock)
│   ├── base.el       # UI/UX enhancements (Vertico, Corfu, etc.)
│   ├── dev.el        # Development tools (Eglot, Magit, etc.)
│   ├── vim-like.el   # Evil mode configuration
│   ├── org.el        # Org-mode setup
│   ├── email.el      # Email (mu4e) configuration
│   ├── researcher.el # Academic tools (citar, etc.)
│   └── writer.el     # Writing tools
├── setup.sh          # macOS installation script
├── AGENTS.md         # This file (AI guidelines)
└── .gitignore        # Git ignore patterns
```

## Configuration Philosophy

This config is based on [Emacs Bedrock](https://codeberg.org/ashton314/emacs-bedrock):
- **Minimal by default**: Start with built-in features, add packages as needed
- **Documented**: Every setting should be explained
- **Terminal-focused**: Optimized for CLI use with Emacs 31's TTY improvements

## Emacs Version

- **Target**: Emacs 31 (via emacs-plus@31 on macOS)
- **Key features used**:
  - TTY child frames (`tty-child-frames` feature)
  - Native compilation
  - Tree-sitter
  - Eglot (built-in LSP)

## Adding Packages

When adding packages:

1. **Prefer built-in**: Check if functionality exists in Emacs core first
2. **Use use-package**: All package configs should use `use-package` macro
3. **ELPA first**: Prefer GNU ELPA and NonGNU ELPA over MELPA when possible
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

The `extras/` directory contains optional modules. To enable one:

1. Uncomment the corresponding `load-file` line in `init.el`
2. Or copy specific configurations into `init.el`

## Terminal/CLI Considerations

Since this config is terminal-focused:
- Avoid packages that only work in GUI mode
- Test changes in terminal: `emacs -nw`
- Check for `(display-graphic-p)` guards when needed
- Prefer themes that work well in terminal (e.g., modus-vivendi)

## Common Tasks

### Adding a new keybinding
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
    ;; GUI-only code
    (some-gui-function)
  ;; Terminal-only code
  (some-tty-function))
```

## Testing Changes

1. Start Emacs with minimal config to test: `emacs -Q -nw`
2. Load specific file: `emacs -nw -l init.el`
3. Check for errors: `M-x toggle-debug-on-error`

## File Locations

- Config directory: `~/work/emacs.d`
- Symlinked to: `~/.config/emacs`
- Backup files: `~/.config/emacs/emacs-backup/`
- Package directory: `~/.config/emacs/elpa/`

## Do Not Modify

- `custom-set-variables` / `custom-set-faces` blocks (managed by Emacs)
- Files in `elpa/` directory (package manager controlled)
