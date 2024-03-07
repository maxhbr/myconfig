;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(mapc 'load (file-expand-wildcards "~/.doom.d/imports/*.el"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Maximilian Huber"
      user-mail-address "gh@maxhbr.de")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Inconsolata Nerd Font" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Inconsolata Nerd Font" :size 20 :weight 'semi-light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; do not prompt when exit
(setq confirm-kill-emacs nil)

(after! evil
  (after! ido
    (load! "./keybindings.el")))


(after! haskell
  (setq dante-repl-command-line '("nix-shell" "--pure" "--run" "cabal repl --builddir=dist/dante")))

;;(use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (:map copilot-completion-map
;;              ("<tab>" . 'copilot-accept-completion)
;;              ("TAB" . 'copilot-accept-completion)
;;              ("C-TAB" . 'copilot-accept-completion-by-word)
;;              ("C-<tab>" . 'copilot-accept-completion-by-word)))
