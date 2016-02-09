;; ===== Set standard indent to 2 rather that 4 ====
(setq standard-indent 2)

;; ========== Support Wheel Mouse Scrolling ==========
(mouse-wheel-mode t)

;; ===== Turn on Auto Fill mode automatically in all modes =====
;; Auto-fill-mode the the automatic wrapping of lines and insertion of
;; newlines when the cursor goes over the column limit.
;; This should actually turn on auto-fill-mode by default in all major
;; modes. The other way to do this is to turn on the fill for specific modes
;; via hooks.
(setq auto-fill-mode 1)

;; always end a file with a newline
(setq require-final-newline t)

;; I use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; quit message
(fset 'yes-or-no-p 'y-or-n-p)

;; Do not use tabs!
(setq-default indent-tabs-mode nil)

;; save position in files
(setq-default save-place t)

(setq
  ;; follow symlinks and don't ask
  ;vc-follow-symlinks t
  ;; see what you type
  echo-keystrokes 0.01
  ;; Insert space/slash after completion
  comint-completion-addsuffix t
  ;; delete line in one stage
  kill-whole-line t
  ;; default mode
  default-major-mode 'text-mode
  ;; meaning are the same as the name :)
  delete-key-deletes-forward t
  ;; next-line don't add new lines
  next-line-add-newlines nil
  ;; don't add new lines when scrolling down
  next-line-add-newlines nil
  ;; make sure file ends with NEWLINE
  require-final-newline t
  ;; setting the default tabulation
  default-tab-width 2
  ;; paste at cursor NOT at mouse pointer position
  ;mouse-yank-at-point t
  ;; apropos works better but slower
  apropos-do-all t
  ;; don't beep
  visible-bell t
  cursor-in-non-selected-windows nil
  ;; dired settings
  dired-recursive-copies t
  dired-recursive-deletes t
  )

(setq create-lockfiles nil)

;; Backup Files
;; Enable backup files.
(setq make-backup-files t)
;; (setq make-backup-files nil)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/tmp/"))))
(setq
 ;; Enable versioning with default values (keep five last versions, I think!)
 version-control t
 ;; autosave every 512 keyboard inputs
 auto-save-interval 512
 ;; limit the number of newest versions
 kept-new-versions 5
 ;; limit the number of oldest versions
 kept-old-versions 5
 auto-save-list-file-prefix "~/.emacs.d/backups/save-"
 )
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq
  ;; text scrolling
  scroll-conservatively 50
  scroll-preserve-screen-position t
  scroll-margin 5
  ;; Scroll by one line at a time
  scroll-step 1
  )

(setq
  ;; display time in the modeline
  display-time-24hr-format t
  display-time-day-and-date t
  ;; calendar customizing
  european-calendar-style t
  calendar-week-start-day 1
  ;; delete excess backup versions
  delete-old-versions t
  )

(which-function-mode t)
;; (blink-cursor-mode -1)
(global-auto-revert-mode t)
(electric-indent-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(random t) ;; seed
(electric-pair-mode t)

(setenv "EDITOR" "emacsclient")

;; show more info in taskbar/icon than just "Emacs"
(setq-default frame-title-format (list "%b @Emacs"))

;; (load "folding" 'nomessage 'noerror)
;; (folding-mode-add-find-file-hook)
;; (folding-add-to-marks-list 'Emacs-Lisp  ";{{{" ";}}}" nil t)
;; (folding-add-to-marks-list 'ruby-mode   "#{{{" "#}}}" nil t)
;; (folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
;; (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;; (folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)

(setq undo-limit        120000)
(setq undo-strong-limit 1200000)
(setq undo-outer-limit  12000000)

;; Display tilde at the end of file
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

;; (setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
;; (setq whitespace-display-mappings '(;(space-mark 32 [183] [46])
;;                                     (newline-mark 10 [8617 10])
;;                                     (tab-mark 9 [9655 9] [92 9])))
(require 'whitespace)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
(setq whitespace-style '(face tabs trailing tab-mark))
(set-face-attribute 'whitespace-tab nil
                    :background "#f0f0f0"
                    :foreground "#00a8a8"
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(add-hook 'prog-mode-hook 'whitespace-mode)


(use-package adaptive-wrap
  :ensure t
  :config
  (adaptive-wrap-prefix-mode))

; minibuffer history
(setq savehist-file "~/.emacs.d/.cache/savehist"
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      history-length 1000)
(savehist-mode t)

;; clean up old buffers periodically
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay 0))

;; better buffer names for duplicates
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
        uniquify-after-kill-buffer-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make ibuffer default
;; (defalias 'list-buffers 'ibuffer)

;; (add-to-list 'ibuffer-never-show-regexps "^\\*")

;; Get rid of title and summary
(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1)) 
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
;;   "Create parent directory if not exists while visiting file."
;;   (unless (file-exists-p filename)
;;     (let ((dir (file-name-directory filename)))
;;       (unless (file-exists-p dir)
;;         (make-directory dir)))))
