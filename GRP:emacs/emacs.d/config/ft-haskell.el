;; (use-package ghc-mod
;;   :ensure t)
(use-package haskell-mode
  ;; :ensure t ;; produces error ...
  :config
  ;; Indent-mode simple
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

  ;; Indent-mode indent
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  ;; Indent-mode indentation
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  ;; Indent-mode hi2
  (use-package hi2
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'turn-on-hi2))

  (use-package haskell-snippets
    :ensure t)

  ;; (use-package haskell-interactive-mode
  ;;   :ensure t)
  ;; (use-package inf-haskell
  ;;   :ensure t)
  
  (use-package company-ghc
    :ensure t
    :config
    (add-to-list 'company-backends 'company-ghc)
    (custom-set-variables '(company-ghc-show-info t)))

  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  ;; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  ;; (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  ;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  ;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  ;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  ;; (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  ;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (custom-set-variables
   '(haskell-process-type 'cabal-repl))
  :mode "\\.hs\\'")

(setq initial-major-mode 'haskell-mode)
(setq initial-scratch-message nil)
