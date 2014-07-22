;;;;;;; projectile

;;(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-i") 'projectile-ag)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-s") 'helm-occur)

(key-chord-define-global "pf" 'projectile-find-file)
(key-chord-define-global "pg" 'projectile-ag)
(key-chord-define-global "pb" 'projectile-switch-to-buffer)
(key-chord-define-global "pj" 'projectile-recentf)
(key-chord-define-global "pk" 'projectile-multi-occur)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)

(defun helm-projectile-init-buffer-with-files (project-root files)
  (with-current-buffer (helm-candidate-buffer project-root)
    (set (make-local-variable 'helm-projectile-current-project-root)
         project-root)
    (dolist (file files)
      (insert (concat file "\n")))))

(defun helm-projectile-coerce-file (candidate)
  (with-current-buffer (helm-candidate-buffer)
    (expand-file-name candidate helm-projectile-current-project-root)))

(defvar helm-source-projectile-files-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                      (projectile-current-project-files))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (lambda (file) (find-file file))))
  "Helm source definition.")

(defun helm-my-buffers ()
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                         helm-source-recentf
                         helm-source-projectile-files-list
                         helm-source-buffer-not-found)
                       "*helm mini*")))

(global-set-key (kbd "s-h") 'helm-my-buffers)


(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun my-helm-multi-projectile ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil (projectile-current-project-files)
         )))
