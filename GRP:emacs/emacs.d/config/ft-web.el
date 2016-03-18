;; (use-package flymake
;;   :config
;;   (defun flymake-php-init ()
;;     "Use php to check the syntax of the current file."
;;     (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;            (local (file-relative-name temp (file-name-directory buffer-file-name))))
;;       (list "php" (list "-f" local "-l"))))

;;   (add-to-list 'flymake-err-line-patterns
;;                '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

;;   (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

;;   (add-hook 'php-mode-hook (lambda () (flymake-mode 1)))
;;   ;; (define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
;;   ;; (define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)
;;   )

(use-package php-mode
  :config
  (add-hook 'php-mode-hook (lambda ()
                             (setq indent-tabs-mode nil
                                   c-basic-offset 2
                                   tab-width 2)))
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)) 
  (use-package flymake-easy
    :ensure t
    :config
    (use-package flymake-php
      :ensure t
      :config
      (add-hook 'php-mode-hook 'flymake-php-load)))
  :ensure t)

(use-package web-mode
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (setq indent-tabs-mode nil
                                   c-basic-offset 2
                                   tab-width 2)))
  (add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jspf?$\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsp$\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jspf$\\'" . web-mode))
  :ensure t)

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
