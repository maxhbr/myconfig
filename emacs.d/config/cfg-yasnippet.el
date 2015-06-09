(delayed-init
    (require-packages '(yasnippet haskell-snippets))

    (setq yas-fallback-behavior 'return-nil)
    (setq yas-also-auto-indent-first-line t)
    ;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
    
    (yas-reload-all)
    ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
    ;; (add-hook 'html-mode-hook 'yas-minor-mode)
    (add-hook 'latex-mode-hook 'yas-minor-mode)

    ;; (let* ((yas-install-dir (car (file-expand-wildcards (concat package-user-dir "/yasnippet-*"))))
           ;; (dir (concat yas-install-dir "/snippets/js-mode")))
      ;; (if (file-exists-p dir)
          ;; (delete-directory dir t)))

    ;; (require 'yasnippet)



    ;; (yas-load-directory (concat user-emacs-directory "/snippets"))

    )
