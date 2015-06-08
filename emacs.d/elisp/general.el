(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)

;; ===== Set the highlight current line minor mode =====
;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

;; ===== Set standard indent to 2 rather that 4 ====
(setq standard-indent 2)

;; ========== Support Wheel Mouse Scrolling ==========
(mouse-wheel-mode t)

;; ========== Place Backup Files in Specific Directory ==========
;; Enable backup files.
;(setq make-backup-files t)
(setq make-backup-files nil)
;; Enable versioning with default values (keep five last versions, I think!)
;(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/tmp/"))))

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
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

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
  ;; text scrolling
  scroll-conservatively 50
  scroll-preserve-screen-position 't
  scroll-margin 5
  ;; Insert space/slash after completion
  comint-completion-addsuffix t
  ;; number of chars in line
  fill-column 80
  ;; delete line in one stage
  kill-whole-line t
  ;; default mode
  default-major-mode 'text-mode
  ;; meaning are the same as the name :)
  delete-key-deletes-forward t
  ;; next-line don't add new lines
  next-line-add-newlines nil
  ;; Scroll by one line at a time
  scroll-step 1
  ;; don't add new lines when scrolling down
  next-line-add-newlines nil
  ;; make sure file ends with NEWLINE
  require-final-newline t
  ;; delete excess backup versions
  delete-old-versions t
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

(setq
  ;; autosave every 512 keyboard inputs
  auto-save-interval 512
  ;; limit the number of newest versions
  kept-new-versions 5
  ;; limit the number of oldest versions
  kept-old-versions 5
  auto-save-list-file-prefix "~/.emacs.d/backups/save-"
  )

(setq
  ;; display time in the modeline
  display-time-24hr-format t
  display-time-day-and-date t
  ;; calendar customizing
  european-calendar-style t
  calendar-week-start-day 1
  )

;; show more info in taskbar/icon than just "Emacs"
(setq-default frame-title-format (list "%b @Emacs"))

(load "folding" 'nomessage 'noerror)
; (folding-mode-add-find-file-hook)
; (folding-add-to-marks-list 'Emacs-Lisp  ";{{{" ";}}}" nil t)
; (folding-add-to-marks-list 'ruby-mode   "#{{{" "#}}}" nil t)
; (folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
; (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
; (folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
