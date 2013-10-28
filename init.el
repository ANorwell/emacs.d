(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(add-to-list 'load-path "~/.emacs.d/custom")
;;(add-to-list 'load-path "~/.emacs.d/other_paths")


(load "00common-setup.el")
(load "01projectile.el")


(load "smartparens/smartparens.el")


;;ido-mode
(require 'ido)
(ido-mode t)
