;; (defun guess-TeX-master (filename)
;;   "Guess the master file for FILENAME from currently open .tex files."
;;   (let ((candidate nil)
;;         (filename (file-name-nondirectory filename)))
;;     (save-excursion
;;       (dolist (buffer (buffer-list))
;;         (with-current-buffer buffer
;;           (let ((name (buffer-name))
;;                 (file buffer-file-name))
;;             (if (and file (string-match "\\.tex$" file))
;;                 (progn
;;                   (goto-char (point-min))
;;                   (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
;;                       (setq candidate file))
;;                   (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
;;                       (setq candidate file))))))))
;;     (if candidate
;;         (message "TeX master document: %s" (file-name-nondirectory candidate)))
;;     candidate))

(use-package tex-site
  :mode ("\\.tex\\'" . latex-mode)
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq tex-dvi-view-command "xdvi -watchfile 3 -expertmode 0")
  (setq TeX-PDF-mode t)

  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "zathura %o")
          ("DVI Viewer" "xdvi %o")
          ("HTML Viewer" "chromium %o")))
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)))

  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup))

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("myLuatexmk" "latexmk  -pdflatex=lualatex -outdir=\"_lualatexmk\" -pdf -synctex=1 -pvc  %s" TeX-run-TeX nil t
                                  :help "Run Latexmk with luatex on file")
                                TeX-command-list)
                               (push
                                '("myLatexmk" "latexmk -outdir=\"_latexmk\" -pdf -synctex=1 -pvc  %s" TeX-run-TeX nil t
                                  :help "Run Latexmk on file")
                                TeX-command-list))))
