(use-package flyspell
  :ensure t
  :defer 1
  :config
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(latex-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))))
