;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
   :ensure t
   :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist `(("." . ,dotemacs-cache-directory)))
  :config
  ;; (defadvice undo-tree-make-history-save-file-name
  ;;            (after undo-tree-compressed activate)
  ;;            (setq ad-return-value (concat ad-return-value ".gz")))
  (global-set-key (kbd "<f6>") 'undo-tree-visualize)
  :diminish undo-tree-mode)
