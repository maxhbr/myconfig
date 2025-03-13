# partially based on:
# https://gist.github.com/dltacube/280c82b3426690558341a3ac3a71428d
{ inputs, pkgs, config, lib, ... }: let
  cfg = config.myconfig;
in {
  imports = [
    ({ ... }: {
      nixpkgs.overlays = [ inputs.emacs.overlay ];
    })
  ];
  options.myconfig = with lib; {
    editor.emacs.enable = mkEnableOption "emacs";
  };
  config = lib.mkIf config.myconfig.editor.emacs.enable {
    home-manager.sharedModules = [
      ./mu
      ({ config, ... }: {
        home.file = {
          ".emacs.d/config.org" = {
            source = ./config.org;
            recursive = true;
            onChange = "${pkgs.writeShellScript "remove-emacs-config-el" ''
              [[ -f ~/.emacs.d/config.el ]] && rm ~/.emacs.d/config.el
            ''}";
          };
          ".emacs.d/imports" = {
            source = ./imports;
            recursive = true;
          };
        };
        programs.emacs = {
          enable = true;
          package = pkgs.emacs-unstable;
          # package = if cfg.desktop.wayland.enable
          #           then pkgs.emacs-pgtk
          #           else pkgs.emacs-unstable;
          extraConfig = ''
            ; prevent emacs from saving customizations to this file
            (setq custom-file (concat user-emacs-directory ".emacs-customize.el"))

            (package-initialize)
            (org-babel-load-file "~/.emacs.d/config.org")
            (mapc 'load (file-expand-wildcards "~/.emacs.d/imports/*.el"))
          '';
          extraPackages = epkgs:
            with epkgs; [
              use-package
              better-defaults
              auto-compile
              evil
              evil-leader
              evil-collection
              evil-surround
              evil-nerd-commenter
              general
              which-key

              org

              magit

              company

              projectile

              nix-mode

              copilot

              # dired-single
              nerd-icons
              all-the-icons
              all-the-icons-dired
              all-the-icons-ivy-rich
              emojify
              eshell-prompt-extras
              vterm
              multi-vterm

              # User interface packages.
              neotree
              ivy
              counsel
              ivy-rich
              ivy-posframe
              ivy-prescient
              desktop-environment
              doom-themes
              doom-modeline

              # exwm:
              exwm
              # exwm-systemtray
            ];
        };
        programs.fish.functions = {
          emacs = "command emacs $argv > /dev/null 2>&1 & disown";
        };
        home.shellAliases = {
          magit = ''emacs -e "(magit-status \"$(pwd)\")"'';
          emacs-cage-exwm = ''exec cage -- emacs -Q --eval "(require 'exwm)" --eval "(exwm-enable)"''; # --eval "(require 'exwm-systemtray)" --eval "(exwm-systemtray-enable)"'';
        };
      })
    ];
  };
}
