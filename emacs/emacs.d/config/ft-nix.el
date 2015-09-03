(use-package nix-mode
  :config
  (autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
  (push '("\\.nix\\'" . nix-mode) auto-mode-alist)
  (push '("\\.nix\\.in\\'" . nix-mode) auto-mode-alist)
  :ensure t)
