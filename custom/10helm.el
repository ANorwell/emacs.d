(require 'helm)
(require 'helm-config)

(helm-flx-mode +1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
