(require-packages '(evil
                    evil-leader
                    evil-nerd-commenter
                    ;; column-marker
                    ;; column-enforce-mode
                    fill-column-indicator 
                    company
                    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   EEEE V     V III L
;   E    V     V  I  L
;   EEE   V   V   I  L
;   E      V V    I  L
;   EEEE    V    III LLLL
;
; vim key bindings and modes
(require-package 'evil)
(evil-mode 1)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
(require 'powerline)
(powerline-default-theme)

;; (require-package 'smart-mode-line)
;; (setq sml/show-client t)
;; (setq sml/show-eol t)
;; (setq sml/show-frame-identification t)
;; (sml/setup)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion
;; ;; (global-set-key (kbd "C-b") ')
;; ;; (define-key minibuffer-local-map (kbd "C-b") 'hippie-expand)
;; (require 'cl)
;; (defvar cdsb-include-re "^\\s-*#\\s-*\\(?:include\\|import\\)\\s-*\\([\"<]\\)\\([^\">]+\\)[\">]\\s-*$")
;; (defun cdsb-extract-includes-in-buffer (buffer)
;;   (with-current-buffer buffer
;;     (save-excursion
;;       (goto-char (point-min))
;;       (loop while (re-search-forward cdsb-include-re nil t)
;;             collect (cons (match-string-no-properties 2) (not (string-equal (match-string-no-properties 1) "\"")))))))
;; (defun cdsb-acc-buffers (buffer non-system-files buffer-list)
;;   (loop with new-buffer-list = nil
;;         for (name . system-include-p) in (cdsb-extract-includes-in-buffer buffer)
;;         do (let* ((file (if system-include-p
;;                             (ffap-c-mode name)
;;                           (let ((name-re (concat "/" (regexp-quote name) "$")))
;;                             (loop for file in non-system-files
;;                                   if (string-match name-re file)
;;                                   return file))))
;;                   (buf (and file (find-file-noselect file t))))
;;              ;; (message "check %s => %s" name file)
;;              (and buf
;;                   (not (position buf buffer-list))
;;                   (setq buffer-list (cdsb-acc-buffers buf non-system-files (cons buf buffer-list)))))
;;         finally return buffer-list))
;; (defun c-dabbrev--select-buffers ()
;;   (if (memq major-mode '(c-mode c++-mode objc-mode))
;;       (save-excursion
;;         (require 'ffap)
;;         (let ((top (project-top-directory))
;;               (cur (current-buffer)))
;;           (nreverse
;;            (cdr
;;             (nreverse
;;              (cdsb-acc-buffers cur (and top (project-source-files top)) (list cur)))))))
;;     (dabbrev--select-buffers)))

;; (add-hook 'c-mode-common-hook 
;;   (lambda () 
;;     (set (make-variable-buffer-local 'dabbrev-select-buffers-function) 'c-dabbrev--select-buffers)))

;; Auto-complete
(add-hook 'after-init-hook 'global-company-mode)

;; Abort company-mode when exiting insert mode
(defun abort-company-on-insert-state-exit ()
  (company-abort))
(add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm settings (TAB in helm window for actions over selected items,
;; C-SPC to select items)
;; (require 'helm-config)
;; (require 'helm-misc)
;; (require 'helm-projectile)
;; (require 'helm-locate)
;; (setq helm-quick-update t)
;; (setq helm-bookmark-show-location t)
;; (setq helm-buffers-fuzzy-matching t)

;; (after 'projectile
;;   (package 'helm-projectile))
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; (defun helm-my-buffers ()
;;   (interactive)
;;   (let ((helm-ff-transformer-show-only-basename nil))
;;   (helm-other-buffer '(helm-c-source-buffers-list
;;                        helm-c-source-elscreen
;;                        helm-c-source-projectile-files-list
;;                        helm-c-source-ctags
;;                        helm-c-source-recentf
;;                        helm-c-source-locate)
;;                      "*helm-my-buffers*")))
