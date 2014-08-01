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

(load "04webmode.el")

(load "05rinari.el")

(load "csv-mode.el")
(load "06csv-mode.el")

;;ensime-mode for scala
;;(add-to-list 'load-path "~/misc/ensime/elisp/")
;;(add-to-list 'load-path "~/.emacs.d/ensime/")
;;(require 'ensime)

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
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-etags company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-begin-commands nil)
 '(company-dabbrev-code-modes
   (quote
    (asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode emacs-lisp-mode erlang-mode f90-mode fortran-mode haskell-mode java-mode javascript-mode jde-mode js2-mode lisp-mode lua-mode objc-mode perl-mode php-mode prog-mode python-mode ruby-mode scheme-mode shell-script-mode enh-ruby-mode)))
 '(company-etags-use-main-table-list t)
 '(custom-safe-themes
   (quote
    ("8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enh-ruby-bounce-deep-indent (quote true))
 '(helm-buffers-fuzzy-matching t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(max-lisp-eval-depth 600)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(server-start)
