;; (use-package php-mode
;;   :ensure t
;;   :mode "\\.php\\'")
(use-package web-mode
  :ensure t
  :mode "\\.php\\'")

(defun my-php ()
  (add-to-list 'company-backends 'company-my-php-backend))

(add-hook 'php-mode-hook 'my-php)
 (defun company-my-php-backend (command &optional arg &rest ignored)
    (case command
      (prefix (and (eq major-mode 'php-mode)
                    (company-grab-symbol)))
      (sorted t)
      (candidates (all-completions
                   arg
                   (if (and (boundp 'my-php-symbol-hash)
                            my-php-symbol-hash)
                      my-php-symbol-hash

                     (with-temp-buffer
                          (call-process-shell-command "php -r '$all=get_defined_functions();foreach ($all[\"internal\"] as $fun) { echo $fun . \";\";};'"\
     nil t)
                       (goto-char (point-min))
                       (let ((hash (make-hash-table)))
                         (while (re-search-forward "\\([^;]+\\);" (point-max) t)
                           (puthash (match-string 1) t hash))
                         (setq my-php-symbol-hash hash))))))))
