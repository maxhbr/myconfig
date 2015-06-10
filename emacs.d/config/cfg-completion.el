;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion
(require-package 'company)

;; Auto-complete
(add-hook 'after-init-hook 'global-company-mode)

;; Abort company-mode when exiting insert mode
(defun abort-company-on-insert-state-exit ()
  (company-abort))
(add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)

(after 'company
  (defun my-company-tab ()
    (interactive)
    (when (null (yas-expand))
      (company-complete)))
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  ;; (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-b") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
