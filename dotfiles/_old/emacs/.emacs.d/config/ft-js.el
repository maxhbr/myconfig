;; js-mode
;; ;; js2-mode
;; (use-package js2-mode
;;   :ensure t)
;; js3-mode
(use-package js3-mode
  :ensure t
  :config
  (setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
  (setq js3-enter-indents-newline t) ; don't need to push tab before typing
  (setq js3-indent-on-enter-key t)   ; fix indenting before moving on
  )
