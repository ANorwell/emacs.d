;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Based on Emacs Bedrock - minimal starter kit
;; Optimized for terminal/CLI use with Emacs 31

;;; Code:

;; Startup speed optimization
(setq bedrock--initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Frame configuration - minimal for terminal focus
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)
(menu-bar-mode -1)  ; Disable menu bar for cleaner terminal experience

;; Default frame settings (applies to GUI if used)
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;;; early-init.el ends here
