;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-
 
;;; Commentary:
;; Based on Emacs Bedrock - minimal starter kit
;; Optimized for terminal/CLI use with Emacs 31
;; See extras/ directory for optional modules

;;; Code:

;;; Guardrail
(when (< emacs-major-version 29)
  (error "This config requires Emacs 29 or newer; you have version %s" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package initialization
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Add site-lisp for manually installed packages
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt initial-major-mode 'fundamental-mode)
(setopt display-time-default-load-average nil)

;; Auto-revert files when changed on disk
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save minibuffer history
(savehist-mode)

;; Window navigation with Shift-<arrow keys> (C-<arrows> used for paragraph nav)
(windmove-default-keybindings 'shift)

;; Modern sentence ending
(setopt sentence-end-double-space nil)

;; Never use hard tabs
(setopt indent-tabs-mode nil)

;; Backup files in a single directory
(defun bedrock--backup-file-name (fpath)
  "Return a backup file path for FPATH in a centralized directory."
  (let* ((backup-root-dir (concat user-emacs-directory "emacs-backup/"))
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir file-path "~"))))
    (make-directory (file-name-directory backup-file-path) (file-name-directory backup-file-path))
    backup-file-path))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Terminal/TTY enhancements (Emacs 31)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable TTY child frames if available (Emacs 31 feature)
(when (featurep 'tty-child-frames)
  (tty-tip-mode 1))

;; Mouse support in terminal (enabled by default in Emacs 31 for compatible terminals)
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Ensure truecolor support - set COLORTERM=truecolor in your shell
;; This is handled by the terminal emulator, but we can check:
(when (and (not (display-graphic-p))
           (>= (display-color-cells) 16777216))
  (message "Truecolor (24-bit) support detected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show help on startup (disabled - use C-h q to show)
;; (add-hook 'after-init-hook 'help-quick)

;; which-key: popup of available keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt enable-recursive-minibuffers t)
(setopt tab-always-indent 'complete)

;; Vertico: vertical minibuffer completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Orderless: flexible completion matching (space-separated patterns)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

;; Marginalia: annotations in minibuffer (file sizes, docstrings, etc.)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Embark: actions on completion candidates (C-. for menu, C-SPC to select multiple)
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         :map minibuffer-local-map
         ("C-SPC" . embark-select)))

;; Consult: enhanced search and navigation commands
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)      ; better buffer switching
         ("C-s" . consult-line)           ; search in buffer
         ("M-g g" . consult-goto-line)    ; goto line
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)      ; search in project (needs ripgrep)
         ("M-s f" . consult-find)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line
(setopt line-number-mode t)
(setopt column-number-mode t)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt show-trailing-whitespace nil)
(setopt indicate-buffer-boundaries 'left)

;; Cursor and scrolling
(blink-cursor-mode -1)
(when (display-graphic-p)
  (pixel-scroll-precision-mode))

;; Line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)

;; Visual line mode for text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight current line
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   In-buffer completion (Corfu)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu: completion popup (works in terminal with Emacs 31 TTY child frames)
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)           ; enable auto-completion
  (corfu-auto-delay 0.2)   ; delay before popup
  (corfu-auto-prefix 2)    ; minimum chars before popup
  (corfu-cycle t)          ; cycle through candidates
  (corfu-preselect 'prompt) ; don't auto-select first candidate
  :init
  (global-corfu-mode))

;; Cape: completion-at-point extensions (adds more completion sources)
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt tab-bar-show 1)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modus-vivendi is a great dark theme that works well in terminal
(use-package emacs
  :config
  (load-theme 'modus-vivendi t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Git
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: the best git interface
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; diff-hl: highlight uncommitted changes in the gutter
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Use margin in terminal (no fringe available)
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Programming (Eglot LSP)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eglot is built-in (Emacs 29+), just configure it
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure))
  :config
  ;; Shutdown server when last buffer is closed
  (setopt eglot-autoshutdown t))

;; Tree-sitter: auto-install grammars and use ts-modes
(use-package treesit-auto
  :ensure t
  :when (treesit-available-p)
  :custom
  (treesit-auto-install 'prompt)  ; prompt before installing (change to t for auto)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Tree-sitter settings
(setopt treesit-font-lock-level 4)  ; max highlighting

;; Language server setup notes:
;; Python: Install pyright or python-lsp-server
;;   pip install pyright
;; Java: Install jdtls (Eclipse JDT Language Server)
;;   brew install jdtls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org-mode (notes)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :custom
  ;; Set a default notes directory
  (org-directory "~/notes")
  ;; Better bullet display in terminal
  (org-hide-leading-stars t)
  ;; Log completion time for TODO items
  (org-log-done 'time)
  ;; More intuitive indentation
  (org-startup-indented t)
  ;; Open links with Enter
  (org-return-follows-link t)
  :config
  ;; Create notes directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project navigation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom project detection for Brazil workspaces
;; Uses <workspace>/src as root to exclude env/ and other metadata
(defun my/brazil-workspace-root (dir)
  "Find Brazil workspace src/ directory as project root."
  (let ((root nil))
    (while-let ((found (locate-dominating-file dir
                         (lambda (d)
                           (file-directory-p (expand-file-name "src" d))))))
      (setq root found
            dir (file-name-directory (directory-file-name found))))
    (when root (cons 'brazil (expand-file-name "src" root)))))

(cl-defmethod project-root ((project (head brazil)))
  "Return root directory of a Brazil workspace PROJECT."
  (cdr project))

(cl-defmethod project-files ((project (head brazil)) &optional _dirs)
  "Return files in PROJECT using fd (respects .gitignore)."
  (let* ((root (project-root project))
         (default-directory root))
    (mapcar (lambda (f) (expand-file-name f root))
            (split-string
             (shell-command-to-string "fd --type f --hidden --exclude .git")
             "\n" t))))

;; Built-in project.el
(use-package project
  :bind (("s-g" . project-find-file)
         ("s-i" . consult-ripgrep)
         ("C-c p f" . project-find-file)
         ("C-c p s" . consult-ripgrep))
  :config
  (add-to-list 'project-find-functions #'my/brazil-workspace-root))

;; Insert file path from project with fuzzy search
(defun my/insert-project-file-path ()
  "Fuzzy-search for files in the project and insert relative paths.
Select with RET, empty input to finish."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (candidates (project-files (project-current t)))
         files file)
    (while (progn
             (setq file (completing-read
                         (format "File [%d] (empty to finish): " (length files))
                         candidates nil nil))
             (not (string-empty-p file)))
      (push file files)
      (setq candidates (delete file candidates)))
    (when files
      (insert (mapconcat (lambda (f) (file-relative-name f root))
                         (nreverse files) "\n")))))

(keymap-global-set "C-c p i" #'my/insert-project-file-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Multiple cursors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Navigation keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Paragraph navigation with C-up/C-down
(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-<down>") 'forward-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Clipboard integration (terminal)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clipetty: sync kill ring with system clipboard via OSC 52
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Custom variables (managed by Emacs)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Restore GC threshold
(setq gc-cons-threshold (if (boundp 'bedrock--initial-gc-threshold)
                            bedrock--initial-gc-threshold
                          800000))

;;; init.el ends here
