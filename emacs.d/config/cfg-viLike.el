;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   EEEE V     V III L
;   E    V     V  I  L
;   EEE   V   V   I  L
;   E      V V    I  L
;   EEEE    V    III LLLL
;
; vim key bindings and modes

(use-package evil
  :ensure t
  :init
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  :config
  (evil-mode 1)

  (defun doWithRetainVisual (cmd)
    "Do something while retaining the visual selection"
    (interactive)
    ; ensure mark is less than point
    (when (> (mark) (point))
      (exchange-point-and-mark)
      )
    (evil-normal-state)
    (funcall cmd (mark) (point))
    (evil-visual-restore) ; re-select last visual-mode selection
    )
  (define-key evil-visual-state-map ">" (lambda ()
                                          (interactive)
                                          (doWithRetainVisual #'evil-shift-right)))
  (define-key evil-visual-state-map "<" (lambda ()
                                          (interactive)
                                          (doWithRetainVisual #'evil-shift-left)))
  (define-key evil-visual-state-map "=" (lambda ()
                                          (interactive)
                                          (doWithRetainVisual #'evil-indent)))
  (evil-leader/set-key "Q" (if mark-active
                               (fill-individual-paragraphs)
                             'fill-paragraph))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))

  (use-package evil-nerd-commenter
    :ensure t
    :config
    (define-key evil-visual-state-map "gc" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-motion-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
    (evil-leader/set-key
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "\\" 'evilnc-comment-operator ; if you prefer backslash key
      ))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(setq-default fill-column 80)
(turn-on-auto-fill)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'column-enforce-mode)
;; (global-column-enforce-mode t)
;; (require 'column-marker)
;; (add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80)))
(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-column 80)
  (setq fci-rule-width 1)
  (setq fci-rule-color "red")
  (setq fci-rule-character-color nil)
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package saveplace
  :ensure t
  :init
  (setq save-place-file (concat dotemacs-cache-directory "saveplace.el"))
  (setq-default save-place t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
   :ensure t
   :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist `(("." . ,dotemacs-cache-directory)))
  :config
  (defadvice undo-tree-make-history-save-file-name
             (after undo-tree-compressed activate)
             (setq ad-return-value (concat ad-return-value ".gz")))
  (global-set-key (kbd "<f6>") 'undo-tree-visualize)
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
