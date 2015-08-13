;; see: https://github.com/clojure-emacs/cider#prerequisites
(use-package cider
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :ensure t)
