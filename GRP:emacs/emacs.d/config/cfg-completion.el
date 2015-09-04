;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion
(use-package company
  :ensure t
  :config
  ;; Auto-complete
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Abort company-mode when exiting insert mode
  (defun abort-company-on-insert-state-exit ()
    (company-abort))
  (add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)

  (defun my-company-tab ()
    (interactive)
    (when (null (yas-expand))
      (company-complete)))
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  ;; (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-b") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))

  :diminish company-mode)

;; (yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
;; (yas/load-directory yas/root-directory)
(use-package yasnippet
  :ensure t
  :init
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-also-auto-indent-first-line t)
  (setq yas-snippet-dirs `(,"~/.emacs.d/snippets"
                           ,"~/.emacs.d/mysnippets"))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package dropdown-list
  :ensure t
  :init
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt)))

;; Use ido everywhere
(use-package ido-ubiquitous
  :config
  (require 'ido)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1)
  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))

  ;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
  (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
  (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet))

; (use-package predictive
;   :ensure t
;   :init
;   (add-to-list 'load-path "~/elisp/predictive")
;   (set-default 'predictive-auto-add-to-dict t)
;   (setq predictive-main-dict 'rpg-dictionary
;         predictive-auto-learn t
;         predictive-add-to-dict-ask nil
;         predictive-use-auto-learn-cache nil
;         predictive-which-dict t)
;   :config
;   (autoload 'predictive-mode "predictive" "predictive" t)
;   )
