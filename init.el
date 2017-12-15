;;(require 'cask "~/.cask/cask.el")
;;(cask-initialize)
;;(require 'pallet)
;;(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/custom")

(load "packages.el")

;;ido-mode
;; do this early -- some other modes detect it
(require 'ido)
(ido-mode t)

(load "00common-setup.el")
(load "01projectile.el")


;;(add-to-list 'load-path "~/.emacs.d/smartparens/")
;;(load "smartparens.el")
(load "02smartparens.el")

(load "03key-chord.el")

(load "04webmode.el")

(load "05rinari.el")

(load "csv-mode.el")
(load "06csv-mode.el")
(load "10helm.el")
(load "11magit.el")

(add-to-list 'load-path "~/.emacs.d/ensime-emacs/")
(load "ensime.el")

;;; ensime manually
;; (add-to-list 'load-path "~/work/ensime-emacs")
;; (load "ensime.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-auto-complete nil)
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-capf
                  (company-dabbrev-code company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-begin-commands t)
 '(company-dabbrev-code-modes
   (quote
    (asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode emacs-lisp-mode erlang-mode f90-mode fortran-mode haskell-mode java-mode javascript-mode jde-mode js2-mode lisp-mode lua-mode objc-mode perl-mode php-mode prog-mode python-mode ruby-mode scheme-mode shell-script-mode enh-ruby-mode)))
 '(company-idle-delay 1.2)
 '(company-minimum-prefix-length 4)
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "00a19ebc359b6419e1e4ab05406d29b265dedd99bcbbf8f870da91098f546e99" "fa94f0c2ddd30df2bca56ddee6854c5926a8a67125d0c28326fd504e377563a9" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enh-ruby-bounce-deep-indent (quote true))
 '(enh-ruby-use-encoding-map nil t)
 '(ensime-goto-test-config-defaults
   (quote
    (:test-class-names-fn ensime-goto-test--test-class-names :test-class-suffixes
                          ("Test" "Spec" "Specification" "Check")
                          :impl-class-name-fn ensime-goto-test--impl-class-name :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir :is-test-dir-fn ensime-goto-test--is-test-dir :test-template-fn ensime-goto-test--test-template-scalatest-2)))
 '(ensime-use-helm t)
 '(fci-rule-color "#383838")
 '(global-wakatime-mode t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(hs-hide-comments-when-hiding-all t)
 '(ido-use-virtual-buffers t)
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-indent-chained t)
 '(js2-pretty-multiline-declarations nil)
 '(magit-diff-refine-hunk (quote all))
 '(magit-refs-sections-hook
   (quote
    (magit-insert-error-header magit-insert-branch-description magit-insert-local-branches)))
 '(magit-refs-show-commit-count (quote all))
 '(magit-refs-show-margin (quote branch))
 '(max-lisp-eval-depth 600)
 '(minimap-hide-fringes t)
 '(minimap-major-modes (quote (nil)))
 '(minimap-minimum-width 10)
 '(minimap-mode t)
 '(minimap-width-fraction 0.01)
 '(neo-theme (quote classic))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ruby . t))))
 '(org-confirm-babel-evaluate nil)
 '(package-load-list (quote (\(ensime\ nil\)\ all)))
 '(package-selected-packages
   (quote
    (wakatime-mode yard-mode org ox-qmd origami hide-comnt hideshowvis memoize font-lock+ all-the-icons php-mode diminish bind-key scala-mode groovy-mode projectile flx-ido markdown-mode markdown-mode+ flycheck-scala-sbt zenburn-theme yaml-mode wgrep web-mode undo-tree sml-modeline smex smartparens smart-mode-line rubocop robe rinari psgml projectile-rails pallet org-bullets neotree multiple-cursors multi-web-mode mmm-mode minitest minimap magit-gh-pulls key-chord json-mode js2-mode imenu-anywhere highlight-symbol helm-rubygems-local helm-rb helm-projectile helm-ls-git helm-flx helm-ag grizzl go-mode git-gutter flycheck ess enh-ruby-mode company-php color-theme coffee-mode aggressive-indent ag ace-jump-mode ac-php)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(persp-mode-prefix-key "x")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
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
 '(vc-annotate-very-old-color "#DC8CC3")
 '(wakatime-api-key "7cf60366-e6eb-4fc6-9f66-a54ba8432972")
 '(wakatime-cli-path "wakatime")
 '(wakatime-python-bin nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(yaml-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:background "RoyalBlue1" :foreground "labelColor"))))
 '(magit-section-highlight ((t (:background "gray28"))))
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

(load "bottom-bar.el")

;; the package manager
;; (require 'package)
;; (setq
;;  use-package-always-ensure t
;;  package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                     ("org" . "http://orgmode.org/elpa/")
;;                     ("melpa" . "http://melpa.org/packages/")))

;; (package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; ;;(require 'use-package)

;;; theme
(load-theme 'zenburn t)

(server-start)
(put 'narrow-to-region 'disabled nil)
