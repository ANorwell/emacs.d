(windmove-default-keybindings)

(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(set-face-attribute 'default nil :height 150)
(global-auto-revert-mode t)
(column-number-mode t)
(scroll-bar-mode -1)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;before-save hook for scala
(add-hook 'scala-mode-hook
     (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;;markdown mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;newline and indent for various modes
(mapcar (lambda (hooksym)
          (add-hook hooksym
                    (lambda ()
                      (local-set-key  (kbd "RET") 'newline-and-indent)
                      )))
        '(
          emacs-lisp-mode-hook
          java-mode-hook
          js-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          makefile-mode-hook
          nxml-mode-hook
          python-mode-hook
          ruby-mode-hook
          scheme-mode-hook
          sh-mode-hook
          ))

;;outline-minor-mode
;;keys for outline mode
(global-set-key [C-M-left] 'hide-body)
(global-set-key [C-M-right] 'show-all)
(global-set-key [M-up] 'outline-previous-heading)
(global-set-key [M-down] 'outline-next-heading)
(global-set-key [M-left] 'hide-entry)
(global-set-key [M-right] 'show-entry)
(global-set-key [C-M-up] 'outline-previous-visible-heading)
(global-set-key [C-M-down] 'outline-next-visible-heading)

;;other keys
(global-set-key [f4] 'goto-line)
(global-set-key [f8]  'grep)
(global-set-key [f10] 'menu-bar-open)
(global-set-key [f11] 'set-buffer-file-coding-system)

;;magit
(global-set-key (kbd "C-c m") 'magit-status)
(defun magit-open-repo ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'magit-status))
(global-set-key (kbd "C-c C-m") 'magit-open-repo)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (hide-sublevels 1)
             (setq
              cperl-indent-level 4
              cperl-indent-parens-as-block t
              cperl-close-paren-offset -4)))
    

(add-hook 'scala-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp "[\s\r\t]*\\(class\\|def\\|package\\|import\\|case class\\|object\\|trait\\|abstract\\|mixin\\|protected def\\|sealed\\|override\\|private def\\|describe\\|it(\\)")
             (local-set-key (kbd "C-,") 'spec-buffer-switch)))

(add-hook 'php-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")
             (hide-sublevels 1)))

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

(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|clas\\|require\\|describe\\|public\\|private\\)")
             (local-set-key (kbd "C-,") 'rails-test-buffer-switch)))


;;Scala mode- switch between test and main projects
(defun spec-buffer-switch ()
  "Switch to/from the Spec file in the test folder of a scala/maven
project"
  (interactive)
  (switch-to-buffer (find-file-noselect
   (if (string-match "Spec.scala$" (buffer-file-name))
(concat
  (substring (replace-regexp-in-string "test" "main"
      (buffer-file-name)) 0 -10)
  ".scala")
(concat
  (substring (replace-regexp-in-string "main" "test"
      (buffer-file-name)) 0 -6)
  "Spec.scala")))))

(global-set-key (kbd "C-,") 'spec-buffer-switch)


(defun rails-test-buffer-switch ()
  "Switch to/from the test file in the test folder of a rails project
project"
  (interactive)
  (switch-to-buffer (find-file-noselect
   (if (string-match "_test.rb$" (buffer-file-name))
       (replace-regexp-in-string "test\\(.*\\)_test.rb" "app\\1.rb" (buffer-file-name))
     (replace-regexp-in-string "app\\(.*\\).rb" "test\\1_test.rb" (buffer-file-name))))))


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


(require 'no-easy-keys)
(no-easy-keys 1)
