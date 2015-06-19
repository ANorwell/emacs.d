;;;;;;; projectile

;;(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-i") 'projectile-ag)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-s") 'helm-occur)

;(key-chord-define-global "pf" 'projectile-find-file)
;(key-chord-define-global "pg" 'projectile-ag)
;(key-chord-define-global "pb" 'projectile-switch-to-buffer)
;(key-chord-define-global "pj" 'projectile-recentf)
;(key-chord-define-global "pk" 'projectile-multi-occur)

(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-persp-switch-project)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)

(global-set-key (kbd "s-g") 'helm-ls-git-ls)
