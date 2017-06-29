;; (require 'dash)
;; (global-set-key (kbd "s-k") 'magit-status)

;; (global-set-key (kbd "C-c m") 'magit-status)

;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; (defun magit-open-repo ()
;;   (interactive)
;;   (setq current-prefix-arg '(4)) ; C-u
;;   (setq current-prefix-arg '(4)) ; C-u
;;   (call-interactively 'magit-status))
;; (global-set-key (kbd "C-c C-m") 'magit-open-repo)
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green3")
;;      (set-face-foreground 'magit-diff-del "red3")))
