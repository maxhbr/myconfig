(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (define-key evil-ex-map "e " 'ido-find-file)
  (define-key evil-ex-map "b " 'ido-switch-buffer)
  )

;; ;; Use ido everywhere
;; (use-package ido-ubiquitous
;;   :config
;;   (use-package ido
;;     :config
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-everywhere t)
;;     (ido-mode 1)
;;     )
;;   (ido-ubiquitous-mode 1)
;; ;;   ;; Fix ido-ubiquitous for newer packages
;; ;;   (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;; ;;     `(eval-after-load ,package
;; ;;        '(defadvice ,cmd (around ido-ubiquitous-new activate)
;; ;;           (let ((ido-ubiquitous-enable-compatibility nil))
;; ;;             ad-do-it))))

;; ;;   ;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; ;;   (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; ;;   (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)
;;   )

