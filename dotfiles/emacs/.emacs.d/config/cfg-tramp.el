(use-package tramp
  :ensure t
  :config

  ;; Stolen from
  ;; https://tsdh.wordpress.com/2008/08/20/re-open-read-only-files-as-root-automagically/
  (defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
      (rename-buffer
      (format "%s:%s"
              (file-remote-p (buffer-file-name) 'method)
              (buffer-name)))))

  (add-hook 'find-file-hook
          'th-rename-tramp-buffer)

  (defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
          (y-or-n-p (concat "File "
                              (ad-get-arg 0)
                              " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
      ad-do-it))

  (defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))
  )
