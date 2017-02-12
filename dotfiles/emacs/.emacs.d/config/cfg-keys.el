(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f6>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f7>") 'switch-to-next-buffer)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
  ; TODO: if (other-buffer (current-buffer) 1) = buffer-menu

(evil-leader/set-key "," 'switch-to-previous-buffer)
(global-set-key (kbd "<mouse-8>") 'switch-to-next-buffer)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
