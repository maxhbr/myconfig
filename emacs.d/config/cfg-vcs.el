(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  ;; (setq magit-auto-revert-mode nil)
  :config

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  ;; Actual changes lost in a sea of whitespace diffs?
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh))

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

;        (setq magit-diff-options '("--histogram"))
;        (setq magit-stage-all-confirm nil)
;        (setq magit-unstage-all-confirm nil)
;        (setq magit-status-buffer-switch-function 'switch-to-buffer)
;        (setq magit-show-child-count t))
