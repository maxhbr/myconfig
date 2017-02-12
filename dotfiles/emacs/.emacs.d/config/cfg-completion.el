;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion
(use-package company
  :ensure t
  :config
  ;; Auto-complete
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Abort company-mode when exiting insert mode
  (defun abort-company-on-insert-state-exit ()
    (company-abort))
  (add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)

  (defun my-company-tab ()
    (interactive)
    (when (null (yas-expand))
      (company-complete)))
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  ;; (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-b") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))

  :diminish company-mode)

;; (yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
;; (yas/load-directory yas/root-directory)
(use-package yasnippet
  :ensure t
  :init
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-also-auto-indent-first-line t)
  (setq yas-snippet-dirs `(,"~/.emacs.d/snippets"
                           ,"~/.emacs.d/mysnippets"))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package dropdown-list
  :ensure t
  :init
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt)))
