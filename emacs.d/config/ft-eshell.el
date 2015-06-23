(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-buffer-shorthand t)

;; em-hist
(setq eshell-history-size 1024)

;; em-compl
(setq eshell-cmpl-ignore-case t)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun eshell/clear ()
  "Clears the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))


(defun eshell/ff (&rest args)
  "Opens a file in emacs."
  (when (not (null args))
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))
