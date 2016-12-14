;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :init
  (defcustom org-beamer-fragile-re "\\\\\\(verb\\|lstinline\\)\\|^[ \t]*\\\\begin{\\(verbatim\\|lstlisting\\|minted\\)}"
    "If this regexp matches in a frame, the frame is marked as fragile."
    :group 'org-beamer
    :type 'regexp)
  (setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f")))
