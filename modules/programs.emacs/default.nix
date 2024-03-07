# partially based on:
# https://gist.github.com/dltacube/280c82b3426690558341a3ac3a71428d
{ inputs, pkgs, config, lib, ... }:
{
  imports = [
    ({ ... }: {
      nixpkgs.overlays = [ inputs.emacs.overlay ];
      home-manager.sharedModules = [ 
        ./mu
      ];
    })
  ];
  options.myconfig = with lib; {
    editor.emacs.enable = mkEnableOption "emacs";
  };
  config = lib.mkIf config.myconfig.editor.emacs.enable {
    home-manager.sharedModules = [
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
          extraConfig = ''
          ; prevent emacs from saving customizations to this file
          (setq custom-file (concat user-emacs-directory ".emacs-customize.el"))

          (package-initialize)
          (org-babel-load-file "~/.emacs.d/config.org")
          (mapc 'load (file-expand-wildcards "~/.emacs.d/imports/*.el"))
          '';
          extraPackages = epkgs: [
            epkgs.use-package
            epkgs.better-defaults
            epkgs.auto-compile
            # epkgs.exwm
            epkgs.evil
            epkgs.evil-leader
            epkgs.evil-collection
            epkgs.evil-surround
            epkgs.evil-nerd-commenter
            epkgs.general
            epkgs.which-key

            epkgs.org
            
            epkgs.magit

            epkgs.company

            epkgs.projectile

            epkgs.nix-mode

            epkgs.dired-single
            epkgs.nerd-icons
            epkgs.all-the-icons
            epkgs.all-the-icons-dired
            epkgs.all-the-icons-ivy-rich
            epkgs.emojify
            epkgs.eshell-prompt-extras
            epkgs.vterm
            epkgs.multi-vterm

            # User interface packages.
            epkgs.neotree
            epkgs.ivy
            epkgs.counsel
            epkgs.ivy-rich
            epkgs.ivy-posframe
            epkgs.ivy-prescient
            epkgs.desktop-environment
            epkgs.doom-themes
            epkgs.doom-modeline
          ];
        };
        programs.fish.functions = {
          emacs = "command emacs $argv > /dev/null 2>&1 & disown";
        };
        home.shellAliases = {
          magit = ''emacs -e "(magit-status \"$(pwd)\")"'';
        };
      })
    ];
  };
}
