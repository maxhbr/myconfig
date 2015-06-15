(load-theme 'wombat)

(set-default-font "Inconsolata-16")
(set-face-attribute 'mode-line nil :font "Inconsolata-12")

(show-paren-mode)
(setq show-paren-delay 0)

(line-number-mode t)
(column-number-mode t)
(display-time-mode -1)
(size-indication-mode t)

(diminish 'visual-line-mode)
(diminish 'auto-fill-function)

;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil)
