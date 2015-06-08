(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<f1>") 'switch-to-previous-buffer)

(global-set-key (kbd "<f5>") 'save-buffer)
