(use-package scala-mode2
  :ensure t
  :config
  (add-hook 'scala-mode-hook '(lambda ()
                                ;; clean-up whitespace at save
                                (make-local-variable 'before-save-hook)
                                (add-hook 'before-save-hook 'whitespace-cleanup)

                                ;; turn on highlight. To configure what is highlighted, customize
                                ;; the *whitespace-style* variable. A sane set of things to
                                ;; highlight is: face, tabs, trailing
                                (whitespace-mode)
                                )))
  ; :mode "\\.scala\\'")
