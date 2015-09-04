(use-package eclim
  :config
  (global-eclim-mode)
  (require 'eclimd)

  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)

  :mode "\\.java\\'"
  :ensure emacs-eclim)
