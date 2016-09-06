;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package restclient
  :ensure t)
(use-package json-mode
  :ensure t)

; (use-package intero
;   :init
;   (add-hook 'haskell-mode-hook 'intero-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package multiple-cursors
;;   :config
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (after 'evil
;;          (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
;;          (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)))

;; (use-package evil-mc
;;   (global-evil-mc-mode 1))
