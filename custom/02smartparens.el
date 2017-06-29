(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;; after ';', go to the next line and indent everything
(sp-local-pair 'scala-mode "{" nil :post-handlers '(:add "\n||\n[i]"))
