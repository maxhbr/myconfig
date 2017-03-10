(add-hook 'makefile-mode-hook
          (lambda ()
            (whitespace-mode)
            (setq indent-tabs-mode t)))
