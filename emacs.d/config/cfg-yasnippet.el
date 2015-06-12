(require-packages '(yasnippet haskell-snippets))
(require 'yasnippet)
(setq yas-fallback-behavior 'return-nil)
(setq yas-also-auto-indent-first-line t)
;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
;; (add-hook 'prog-mode-hook 'yas-minor-mode)
;; (add-hook 'html-mode-hook 'yas-minor-mode)
;; (add-hook 'latex-mode-hook 'yas-minor-mode)

; (delayed-init
;  (yas-reload-all))
 ;; (yas-load-directory (concat user-emacs-directory "/snippets")))

;; Use ido everywhere
(require-package 'ido-ubiquitous)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)
