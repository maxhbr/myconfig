
(add-hook 'sh-mode '(lambda ()
                      (load "folding" 'nomessage 'noerror)
                      (folding-mode-add-find-file-hook)
                      (folding-add-to-marks-list 'sh-mode "{{{" "}}}" nil t)))
