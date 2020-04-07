(global-set-key (kbd "<f5>") 'save-buffer)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
                                        ; TODO: if (other-buffer (current-buffer) 1) = buffer-menu
(evil-leader/set-key "," 'switch-to-previous-buffer
                     "dt" 'delete-trailing-whitespace)

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))
(global-set-key (kbd "<f9>") 'my-window-killer)

(global-set-key (kbd "<f11>") 'delete-other-windows)

(defun doWithRetainVisual (cmd)
  "Do something while retaining the visual selection"
  (interactive)
  ; ensure mark is less than point
  (when (> (mark) (point))
    (exchange-point-and-mark)
    )
  (evil-normal-state)
  (funcall cmd (mark) (point))
  (evil-visual-restore) ; re-select last visual-mode selection
  )

; (use-package evil-numbers
;   :ensure t
;   :config
;   (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
;   (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)))

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change))
  :diminish flyspell-mode)

(global-set-key (kbd "<f12>") 'fd-switch-dictionary)

(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)
