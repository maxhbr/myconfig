(use-package web-mode
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (setq indent-tabs-mode nil
                                   c-basic-offset 2
                                   tab-width 2)))
  (add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jspf?$\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsp$\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jspf$\\'" . web-mode))
  :ensure t)
