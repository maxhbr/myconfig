(load-theme 'wombat)

(set-default-font "Inconsolata-16")
(set-face-attribute 'mode-line nil :font "Inconsolata-12")

(show-paren-mode)
(setq show-paren-delay 0)

(line-number-mode t)
(column-number-mode t)
(display-time-mode -1)
(size-indication-mode t)

(require-package 'diminish)
(diminish 'visual-line-mode)
(diminish 'auto-fill-function)
(after 'undo-tree (diminish 'undo-tree-mode))
;; (after 'auto-complete (diminish 'auto-complete-mode))
;; (after 'projectile (diminish 'projectile-mode))
(after 'yasnippet (diminish 'yas-minor-mode))
;; (after 'guide-key (diminish 'guide-key-mode))
;; (after 'eldoc (diminish 'eldoc-mode))
;; (after 'smartparens (diminish 'smartparens-mode))
(after 'company (diminish 'company-mode))
;; (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter (diminish 'git-gutter-mode))
;; (after 'magit (diminish 'magit-auto-revert-mode))
;; (after 'highlight-symbol (diminish 'highlight-symbol-mode))
;; (after 'indent-guide (diminish 'indent-guide-mode))

;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil)
