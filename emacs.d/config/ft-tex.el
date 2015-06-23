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
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode)
              (LaTeX-math-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (push
                '("myLuatexmk" "latexmk  -pdflatex=lualatex -outdir=\"_lualatexmk\" -pdf -synctex=1 -pvc  %s" TeX-run-TeX nil t
                  :help "Run Latexmk with luatex on file")
                TeX-command-list)
              (push
                '("myLatexmk" "latexmk -outdir=\"_latexmk\" -pdf -synctex=1 -pvc  %s" TeX-run-TeX nil t
                  :help "Run Latexmk on file")
                TeX-command-list)
              (local-set-key (kbd "RET") 'newline-and-indent)))

  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup))

  (use-package outline
    :ensure t
    :init
    (add-hook 'LaTeX-Output-mode-hook 'fci-mode)
    ;; extra (fake) outline headers
    (setq TeX-outline-extra
          '(("%chapter" 1)
            ("%section" 2)
            ("%subsection" 3)
            ("%subsubsection" 4)
            ("%paragraph" 5)))
    (font-lock-add-keywords
     'latex-mode
     '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
        0 'font-lock-keyword-face t)
       ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
       ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
       ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
       ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
       ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))
    :config
    (eval-after-load 'outline
      '(progn
         (use-package outline-magic
           :ensure t
           :config
           (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle))))
    (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
    :diminish outline-minor-mode)
  )


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
