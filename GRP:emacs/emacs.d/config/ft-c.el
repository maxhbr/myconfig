(use-package cc-mode
  :ensure t
  :config
  (setq c-default-style "linux"
        c-basic-offset 2)
  (setq-default tab-width 2 indent-tabs-mode nil)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

  (define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)
  ;; replace C-S-<return> with a key binding that you want 
  )
