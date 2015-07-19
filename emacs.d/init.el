:;; ~/.emacs
;;
;;  partialy stolen from:
;;    * https://github.com/bling/dotemacs
;;    * http://whattheemacsd.com/ and http://emacsrocks.com/
;;    * http://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
;;    * and more

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; (unless (display-graphic-p) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directories
(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))
(defvar dotemacs-cache-directory (dot-emacs ".cache/"))
(unless (file-exists-p dotemacs-cache-directory)
  (make-directory dotemacs-cache-directory))
;; store most files in the cache
(setq backup-directory-alist `((".*" . ,(dot-emacs "backups")))
      auto-save-file-name-transforms `((".*" ,(dot-emacs "backups") t))
      auto-save-list-file-prefix (dot-emacs "auto-save-list/saves-"))

(add-to-list 'load-path (dot-emacs "config"))
(add-to-list 'load-path (dot-emacs "use-package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common and utils
(load-library "use-package")
(load-library "cfg-common")
;; (use-package auto-async-byte-compile
;;   :ensure t
;;   :init
;;   (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic behaviour
(load-library "cfg-general")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :ensure t)
(load-library "cfg-helm")
(load-library "cfg-flycheck")
(load-library "cfg-flyspell")
(load-library "cfg-gitGutter")
(load-library "cfg-completion")
(load-library "cfg-vcs")
(load-library "cfg-testing")
(use-package tramp
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advanced behaviour and styling
(load-library "cfg-undoTree")
(load-library "cfg-viLike")
(load-library "cfg-style")
(load-library "cfg-keys")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file types and buffer types
(load-library "ft-org")
(load-library "ft-eshell")
(load-library "ft-haskell")
(load-library "ft-tex")
(load-library "ft-web")
(load-library "ft-jabber")
(use-package markdown-mode
  :ensure t)

(let ((priv (dot-emacs "private-init.el")))
  (when (file-exists-p priv)
    (load priv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all files in the elisp dir
(let ((base (dot-emacs "elisp")))
  (unless (file-exists-p base)
    (make-directory base))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; This has to be last, such that there is a menu bar in the error-case
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(custom-safe-themes
   (quote
    ("987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" default)))
 '(undo-tree-history-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/.cache/"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-chat-prompt-foreign ((t (:foreground "dark orange" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "saddle brown" :weight normal)))))
