(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

(global-set-key (kbd "<f1>") 'switch-to-previous-buffer)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f7>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f8>") 'switch-to-next-buffer)
(global-set-key (kbd "<f9>") 'my-window-killer)

