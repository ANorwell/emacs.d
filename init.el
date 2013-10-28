(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(add-to-list 'load-path "~/.emacs.d/custom")
;;(add-to-list 'load-path "~/.emacs.d/other_paths")


(load "00common-setup.el")
(load "01projectile.el")


(add-to-list 'load-path "~/.emacs.d/smartparens/")
(load "smartparens.el")
(load "02smartparens.el")

(load "03key-chord.el")

;;ensime-mode for scala
(add-to-list 'load-path "~/.emacs.d/ensime/src/main/elisp/")
(require 'ensime)

;;; theme
(load-theme 'zenburn t)

;;ido-mode
(require 'ido)
(ido-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
