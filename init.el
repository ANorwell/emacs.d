(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(add-to-list 'load-path "~/.emacs.d/custom")
;;(add-to-list 'load-path "~/.emacs.d/other_paths")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

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
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-capf
                  (company-dabbrev-code company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-begin-commands nil)
 '(company-dabbrev-code-modes
   (quote
    (asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode emacs-lisp-mode erlang-mode f90-mode fortran-mode haskell-mode java-mode javascript-mode jde-mode js2-mode lisp-mode lua-mode objc-mode perl-mode php-mode prog-mode python-mode ruby-mode scheme-mode shell-script-mode enh-ruby-mode)))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "00a19ebc359b6419e1e4ab05406d29b265dedd99bcbbf8f870da91098f546e99" "fa94f0c2ddd30df2bca56ddee6854c5926a8a67125d0c28326fd504e377563a9" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enh-ruby-bounce-deep-indent (quote true))
 '(enh-ruby-use-encoding-map nil)
 '(fci-rule-color "#383838")
 '(helm-buffers-fuzzy-matching t)
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-indent-chained t)
 '(js2-pretty-multiline-declarations nil)
 '(max-lisp-eval-depth 600)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ruby . t) (shell . t))))
 '(org-confirm-babel-evaluate nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(show-trailing-whitespace t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-done ((t (:foreground "PaleGreen"))))
 '(org-block-begin-line ((t (:inherit org-meta-line))))
 '(org-code ((t (:inherit shadow :background "gray15"))))
 '(org-default ((t (:inherit default))))
 '(org-document-title ((t (:foreground "pale turquoise" :weight bold :height 1.75))))
 '(org-level-1 ((t (:inherit default :foreground "orange1" :box (:line-width 5 :color "#3F3F3F") :slant normal :weight normal :height 1.75 :width normal :foundry "nil" :family "Lucida Grande"))))
 '(org-level-2 ((t (:inherit default :foreground "turquoise2" :box (:line-width 5 :color "#3F3F3F") :slant normal :weight normal :height 1.25 :width normal :foundry "nil" :family "Lucida Grande"))))
 '(org-level-3 ((t (:foreground "#7CB8BB" :height 1.1 :family "Lucida Grande"))))
 '(org-level-4 ((t (:foreground "#D0BF8F" :family "Lucida Grande"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :background "gray21" :height 0.8))))
 '(org-verbatim ((t (:inherit shadow)))))

(load "org-mode.el")
(load "ob-ruby.el")

(server-start)
