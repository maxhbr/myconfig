;(use-package malabar-mode
;  :mode "\\.java\\'")
;; (use-package java-mode
;;   :mode "\\.java\\'"
;;   :ensure t)

(use-package eclim
  :config
  (global-eclim-mode)
  (require 'eclimd)

  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)

  :mode "\\.java\\'"
  :ensure emacs-eclim)

(add-to-list 'auto-mode-alist '("\\.java$\\'" . java-mode))
