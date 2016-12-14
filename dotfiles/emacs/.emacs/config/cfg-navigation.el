;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-SPC") 'buffer-menu)
;; (global-set-key (kbd "C-SPC") 'ibuffer)
;; Open in a different frame
(defun Buffer-menu-other-frame ()
  (interactive)
  (switch-to-buffer-other-frame (Buffer-menu-buffer t)))

(define-key Buffer-menu-mode-map "5"
  'Buffer-menu-other-frame)

(use-package sr-speedbar
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :ensure t
  :init
  (setq recentf-max-menu-items 50)
  :config
  (recentf-mode 1)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-S-SPC") 'recentf-open-files)
  )
