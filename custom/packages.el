(setq
 package-archives '(;;("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))


(package-initialize)


;; NOTE: ensime shpuld probably be installed using melpa-stable! comment out melpa and edit package list

(setq package-selected-packages
      '(
        company
        ensime
        neotree
        markdown-mode
        yaml-mode
        web-mode
        idomenu
        projectile
        helm
        multiple-cursors
        perspective
;;        persp-projectile
        smartparens
        undo-tree
        org-bullets
;;        labburn-theme
        zenburn-theme
        smart-mode-line
        helm-ls-git
        highlight-symbol
        php-mode
        flycheck
        all-the-icons
        ag
        hideshowvis
        origami
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)
