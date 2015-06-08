(ensure-package-installed 'evil
                          'evil-leader
                          'evil-nerd-commenter
                          ;; 'column-marker
                          ;; 'column-enforce-mode
                          'fill-column-indicator 
                          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   EEEE V     V III L
;   E    V     V  I  L
;   EEE   V   V   I  L
;   E      V V    I  L
;   EEEE    V    III LLLL
;
; vim key bindings and modes
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'column-enforce-mode)
;; (global-column-enforce-mode t)
;; (require 'column-marker)
;; (add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80)))
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "red")
(setq fci-rule-character-color nil)
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'wombat)

;; Vim key bindings
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)

(define-key evil-motion-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
