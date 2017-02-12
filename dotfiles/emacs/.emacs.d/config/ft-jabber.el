(use-package jabber
  :config
  (add-hook 'jabber-chat-mode-hook
          (lambda()
            (flyspell-mode t)
            (visual-line-mode nil)
            (ispell-change-dictionary "deutsch8"))))
