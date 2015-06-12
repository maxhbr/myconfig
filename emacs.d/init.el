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

(add-to-list 'load-path (concat user-emacs-directory "/config"))
(let ((base (concat user-emacs-directory "/elisp")))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common and utils
(load-library "cfg-common")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cache directory
(defvar dotemacs-cache-directory (concat user-emacs-directory ".cache/"))
;; store most files in the cache
(setq backup-directory-alist
      `((".*" . ,(concat dotemacs-cache-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat dotemacs-cache-directory "backups") t))
      auto-save-list-file-prefix
      (concat dotemacs-cache-directory "auto-save-list/saves-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic behaviour
(load-library "cfg-general")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file types and modes
(load-library "cfg-eshell")
(load-library "cfg-helm")
(load-library "cfg-flycheck")
(load-library "cfg-flyspell")
(load-library "cfg-yasnippet")
(load-library "cfg-haskell")
(load-library "cfg-gitGutter")
(load-library "cfg-completion")
(load-library "cfg-latex")
(load-library "cfg-vcs")
(load-library "cfg-testing")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advanced behaviour and styling
(load-library "cfg-viLike")
(load-library "cfg-style")
(load-library "cfg-keys")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
