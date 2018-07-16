(windmove-default-keybindings)

(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
;; basic appearance
(setq-default tab-width 2)
(set-face-attribute 'default nil :height 160)
(global-auto-revert-mode t)
(column-number-mode t)
(scroll-bar-mode -1)
(setq create-lockfiles nil)

(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
(setenv "TERM" "eterm-color")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq org-src-fontify-natively t)

(setq backup-directory-alist
      `((".*" . ,"~/.emacs-backup")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-backup" t)))
(setq backup-by-copying t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;;before-save hook for scala
(add-hook 'scala-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

(add-to-list 'auto-mode-alist '("\\.js|\\.twig" . web-mode))

;;newline and indent for various modes
(mapcar (lambda (hooksym)
          (add-hook hooksym
                    (lambda ()
                      (local-set-key  (kbd "RET") 'newline-and-indent)
                      )))
        '(
          emacs-lisp-mode-hook
          java-mode-hook
          js2-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          makefile-mode-hook
          nxml-mode-hook
          python-mode-hook
          ruby-mode-hook
          scheme-mode-hook
          scala-mode-hook
          sh-mode-hook
          ))

(defun ensime-start-or-reload ()
  "Start or reload ensime"
  (interactive)
  (condition-case nil
    (ensime-shutdown)
    (error nil))
  (condition-case nil
    (ensime-reload)
    (error (ensime)))
  (setq company-idle-delay 1.3)
  (set (make-local-variable 'company-idle-delay) 1.4))

(defun set-company-idle ()
  "fix stupid ensime"
  (interactive)
  (setq company-idle-delay 1.3)
  (set (make-local-variable 'company-idle-delay) 1.3))

(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)

;; php stuff
;;(require 'ac-php)
(add-hook 'web-mode-hook
          '(lambda ()
             ;;(require 'company-php)
             (company-mode t)
             (flycheck-mode)
             ;;(add-to-list 'company-backends 'company-ac-php-backend )
             ))

(add-hook 'php-mode-hook
          '(lambda ()
             ;;(require 'company-php)
             (company-mode t)
             (flycheck-mode)
             ;;(add-to-list 'company-backends 'company-ac-php-backend )
             ))




;;outline-minor-mode / origami-mode
;;keys for outline mode
;; (require 'origami)
;; (global-origami-mode 1)

;; (defcustom origami-parser-alist
;;   `((java-mode             . origami-java-parser)
;;     (scala-mode            . origami-c-parser)
;;     (c-mode                . origami-c-parser)
;;     (c++-mode              . origami-c-style-parser)
;;     (perl-mode             . origami-c-style-parser)
;;     (cperl-mode            . origami-c-style-parser)
;;     (js-mode               . origami-c-style-parser)
;;     (js2-mode              . origami-c-style-parser)
;;     (js3-mode              . origami-c-style-parser)
;;     (go-mode               . origami-c-style-parser)
;;     (php-mode              . origami-c-style-parser)
;;     (python-mode           . origami-python-parser)
;;     (emacs-lisp-mode       . origami-elisp-parser)
;;     (lisp-interaction-mode . origami-elisp-parser)
;;     (clojure-mode          . origami-clj-parser)
;;     (triple-braces         . ,(origami-markers-parser "{{{" "}}}")))
;;   "alist mapping major-mode to parser function."
;;   :type 'hook
;;   :group 'origami)

;; (add-to-list 'origami-parser-alist '(scala-mode . origami-java-parser))


(global-set-key [C-M-left] 'hide-body)
(global-set-key [C-M-right] 'show-all)
(global-set-key [M-up] 'outline-previous-heading)
(global-set-key [M-down] 'outline-next-heading)
(global-set-key [M-left] 'hide-entry)
(global-set-key [M-right] 'show-entry)

;; (global-set-key [C-M-left] 'origami-close-all-nodes)
;; (global-set-key [C-M-right] 'origami-open-all-nodes)
;; (global-set-key [M-up] 'outline-previous-heading)
;; (global-set-key [M-down] 'outline-next-heading)
;; (global-set-key [M-left] 'origami-close-node-recursively)
;; (global-set-key [M-right] 'origami-open-node-recursively)
(global-set-key (kbd "C-M-S-<left>") 'hide-entry)
(global-set-key (kbd "C-M-S-<right>") 'show-entry)
(global-set-key [C-M-up] 'outline-previous-visible-heading)
(global-set-key [C-M-down] 'outline-next-visible-heading)
(global-set-key (kbd "s-p") 'outline-previous-visible-heading)
(global-set-key (kbd "s-n") 'outline-next-visible-heading)
(global-set-key (kbd "s-h") 'origami-recursively-toggle-node)
(global-set-key (kbd "s-k") 'origami-forward-toggle-node)
(global-set-key (kbd "s-j") 'origami-show-only-node)


;;other keys
(global-set-key [f8] 'neotree-project-dir)
(global-set-key [f4] 'goto-line)
(global-set-key [f11] 'set-buffer-file-coding-system)
(global-set-key [f9] 'ensime-start-or-reload)
(global-set-key [f10] 'set-company-idle)
(global-set-key (kbd "S-<tab>") 'yas-expand)

(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'cperl-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq
              cperl-indent-level 4
              cperl-indent-parens-as-block t
              cperl-close-paren-offset -4)))

(add-hook 'after-change-major-mode-hook
          'set-company-idle)

(add-hook 'scala-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (highlight-symbol-mode)
             (setq outline-regexp "[\s\r\t]*\\(class\\|def\\|package\\|import\\|case class\\|case object\\|object\\|trait\\|abstract\\|mixin\\|protected def\\|sealed\\|override\\|private def\\|describe\\|it(\\)")
             (subword-mode)
             (setq company-idle-delay 1.3)
             (ensime-mode)
             (setq company-idle-delay 1.3)
             (electric-pair-mode 1)
             (local-set-key (kbd "C-,") 'spec-buffer-switch)))

(add-hook 'web-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")))

(add-hook 'php-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")))


(add-hook 'c++-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp "^[^\s\r\t\n]")
             (hide-sublevels 1)))

(add-hook 'python-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
             (hide-sublevels 1)))

(add-hook 'enh-ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|require\\|describe\\|public\\|private\\|protected\\|context\\|module\\|require\\|should\\|xshould\\)")
             (subword-mode)
             (yard-mode)
             (local-set-key (kbd "C-,") 'rails-test-buffer-switch)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|require\\|describe\\|public\\|private\\|context\\|module\\|require\\|should\\)")
             (subword-mode)
             (yard-mode)
             (local-set-key (kbd "C-,") 'rails-test-buffer-switch)))


(add-hook 'js-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(function\\|describe(\\|it(\\)")))

(add-hook 'js2-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (electric-indent-local-mode -1)
             (setq outline-regexp " *\\(.*function\\|describe(\\|it(\\|.*: *{\\|.*= *{\\)")))


;;Scala mode- switch between test and main projects
(defun spec-buffer-switch ()
  "Switch to/from the Spec file in the test folder of a scala/maven
project"
  (interactive)
  (switch-to-buffer (find-file-noselect
   (if (string-match "Test.scala$" (buffer-file-name))
       (concat
        (substring (replace-regexp-in-string "test" "main"
                                             (buffer-file-name)) 0 -10)
        ".scala")
     (concat
      (substring (replace-regexp-in-string "main" "test"
                                           (buffer-file-name)) 0 -6)
      "Test.scala")))))

;;(global-set-key (kbd "C-,") 'spec-buffer-switch)


;;push the current file to spin. Assumes 'spin serve' has been run.
(defun spin-push-current-buffer-file ()
  "Push this file to spin"
  (interactive)
  (shell-command (format "%s %s" "spin push" buffer-file-name)))
(global-set-key (kbd "s-r") 'spin-push-current-buffer-file)

;;helper to get a project dir, assuming the path is /Users/work/anorwell/name/sdfsdf/sdfs
(defun project-dir (path)
  (mapconcat 'identity (subseq (split-string path "/") 0  5) "/"))

;;push the current file to test in m.
(defun m-test-current-buffer-file ()
  "Run this file in m"
  (interactive)
  ;(if (get-buffer "<m-test>") (kill-buffer "<m-test>"))
  ;(start-process-shell-command "m-test" "<m-test>" (format "%s %s" "bundle exec m" buffer-file-name))
  (async-shell-command (format "cd %s && bundle exec m %s" (project-dir buffer-file-name) buffer-file-name)))
  ;(view-buffer-other-window "<m-test>"))
(global-set-key (kbd "s-m") 'm-test-current-buffer-file)

;;push the current file and line number to test in m.
(defun m-test-current-buffer-file-and-line ()
  "Run this file in m"
  (interactive)
  ;(if (get-buffer "<m-test>") (kill-buffer "<m-test>"))
                                        ;(start-process-shell-command "m-test" "<m-test>" (format "cd %s && bundle exec m %s:%s" (project-dir buffer-file-name)  buffer-file-name (line-number-at-pos)))
  (async-shell-command (format "cd %s && bundle exec m %s:%s" (project-dir buffer-file-name)  buffer-file-name (line-number-at-pos))))
  ;(view-buffer-other-window "<m-test>"))
(global-set-key (kbd "s-M") 'm-test-current-buffer-file-and-line)


(defun rails-test-buffer-switch ()
  "Switch to/from the test file in the test folder of a rails project
project"
  (interactive)
  (switch-to-buffer (find-file-noselect
   (if (string-match "_test.rb$" (buffer-file-name))
       (replace-regexp-in-string "test\\(.*\\)_test.rb" "app\\1.rb" (buffer-file-name))
     (replace-regexp-in-string "app\\(.*\\).rb" "test\\1_test.rb" (buffer-file-name))))))

(defun toggle-xshould-on ()
  (interactive)
  (let ((case-fold-search t)) ; or nil
  (goto-char (point-min))
  (while (search-forward-regexp "^\\s-+\\(should\\)" nil t)
    (replace-match "xshould" t t nil 1))))

(defun toggle-xshould-off ()
  (interactive)
  (let ((case-fold-search t)) ; or nil
  (goto-char (point-min))
  (while (search-forward-regexp "^\\s-+\\(xshould\\)" nil t)
    (replace-match "should" t t nil 1))))

(defun toggle-should ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^\\s-+\\(xshould\\)" nil t)
        (toggle-xshould-off)
      (print "toggling on!!!")
      (toggle-xshould-on))))
(global-set-key (kbd "C-c C-t") 'toggle-should)


(defun toggle-ignore-on ()
  (interactive)
  (replace-regexp "\\(\\s-+\\)it(" "\\1ignore("))

(defun toggle-ignore-off ()
  (interactive)
  (replace-regexp "\\(\\s-+\\)ignore(" "\\1it("))

(defun toggle-ignore ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (if (search-forward-regexp "^\\s-+\\(ignore\\)" nil t)
        (progn
          (print "toggling off!!!!!!")
          (goto-char 0)
          (toggle-ignore-off))
      (print "toggling on!!!")
      (toggle-ignore-on))))
(global-set-key (kbd "C-c t") 'toggle-ignore)


(defun fix-ruby ()
  (interactive)
  (ruby-mode)
  (enh-ruby-mode))
(global-set-key (kbd "s-y") 'fix-ruby)


(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

;;(define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp)


(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))


;;;;;; ediff mode diffing binary files

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ediff binary file stuff

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

(define-minor-mode code-wrap-mode
	"Visually wrap the buffer text to the previous lines indent, plus a tab."
	:lighter ""
	(if code-wrap-mode
		(progn
			(visual-line-mode 1)
			(jit-lock-register #'indent-prefix-wrap-function))
		(visual-line-mode 0)
		(jit-lock-unregister #'indent-prefix-wrap-function)
		(with-silent-modifications
			(save-restriction
				(widen)
				(remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(setq indent-prefix-extra "\t")
(defun indent-prefix-wrap-function (&optional beg end)
	"Indent the region between BEG and END with the existing indent, plus a little extra."
	(when (and beg end)
		(let ((tab-as-spaces (string-repeat " " tab-width)))
			(goto-char beg)
			(beginning-of-line)
			(while (< (point) end)
				(let* ((line-start (point))
					   (indent-start (progn (skip-syntax-forward " " end) (point)))
					   (line-end (progn (beginning-of-line 2) (- (point) 1)))
					   (prefix (concat (buffer-substring line-start indent-start) indent-prefix-extra)))
					;; replace tabs with spaces because the tab indent gets messed up
					(setq prefix (replace-regexp-in-string "\t" tab-as-spaces prefix))
					;; if we are indenting more than half the window width, give up and go back to a one-tab indent
					(when (> (length prefix) (/ (window-width) 2))
						(setq prefix tab-as-spaces))
					(put-text-property line-start line-end 'wrap-prefix prefix))))))

(global-set-key (kbd "C-c C-a") 'ag-kill-other-buffers)

(add-hook 'after-init-hook 'global-company-mode)

;; setting this would only run company on the keyboard command
;;(setq company-begin-commands '(self-insert-command))
(global-set-key (kbd "M-/") 'company-complete)

(global-set-key (kbd "s-j") 'imenu-anywhere)

;;yasnippet
;;(require 'yasnippet)
;;(yas-global-mode 1)

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-use-encoding-map nil)


(defun disable-enh-ruby-encoding ()
  (interactive)
  (defun enh-ruby-mode-set-encoding () ))
(defun enh-ruby-mode-set-encoding () )
(add-hook 'enh-ruby-mode-hook 'disable-enh-ruby-encoding)

(setq system-uses-terminfo nil)

(winner-mode 1)

(defun term-mode-no-whitespace ()
  "disable trailing whitespace highlighting"
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook 'term-mode-no-whitespace)

(when (fboundp 'winner-mode)
      (winner-mode 1))

(defun start-terminal ()
  "start backupify terminal"
  (interactive)
  (cd "~/work/backupify")
  (term "zsh"))

(global-set-key (kbd "C-c C-u u") 'undo-tree-visualize)


(defun s-slice-at (regexp s)
  "Slices S up at every index matching REGEXP."
  (save-match-data
    (let (i)
      (setq i (string-match regexp s 1))
      (if i
          (cons (substring s 0 i)
                (s-slice-at regexp (substring s i)))
        (list s)))))


;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<s-return>") 'newline-without-break-of-line)


(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)
