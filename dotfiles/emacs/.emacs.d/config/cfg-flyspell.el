(use-package flyspell
  :ensure t
  ;; :defer 1
  :init
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq flyspell-issue-message-flag nil)
  (setq ispell-dictionary "english")
  (setq ispell-list-command "--list")
  :config
  (ispell-change-dictionary "english")
  (dolist (hook '(latex-mode-hook
                  text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))))
  (add-hook 'LaTeX-mode-hook (ispell-change-dictionary "english"))

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change))
  :diminish flyspell-mode)

(global-set-key (kbd "<f12>") 'fd-switch-dictionary)
