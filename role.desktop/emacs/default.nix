# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/nix-doom-emacs.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  doom-emacs = pkgs.unstable.callPackage (builtins.fetchGit {
    url = "https://github.com/vlaci/nix-doom-emacs.git";
    inherit (json) rev ref;
  }) {
    doomPrivateDir =
      ./doom.d; # Directory containing your config.el init.el and packages.el files
    extraPackages = epkgs: [ pkgs.mu ];
    extraConfig = ''
      (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
    '';
  };
  doom-emacs-bin-path = "${doom-emacs}/bin/emacs";
  xclipedit = pkgs.writeShellScriptBin "xclipedit" ''
    #!/usr/bin/env bash
    set -euo pipefail
    tempfile="$(mktemp)"
    trap "rm $tempfile" EXIT
    if [ -t 0 ]; then
        xclip -out > "$tempfile"
    else
        cat > "$tempfile"
    fi
    ${doom-emacs-bin-path} "$tempfile"
    ${pkgs.xclip}/bin/xclip < "$tempfile"
      '';
in {
  imports = [ ./spacemacs.nix ];
  config = {
    nixpkgs.overlays = [
      (self: super: {
        emacs = super.emacs.overrideDerivation (drv: { nativeComp = true; });
      })
    ];

    environment.systemPackages = with pkgs; [ doom-emacs ];
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        xclipedit

        aspell
        aspellDicts.de
        aspellDicts.en
        shellcheck
        ripgrep
      ];
      programs.zsh.shellAliases = {
        magit = ''${doom-emacs-bin-path} -e "(magit-status \"$(pwd)\")"'';
      };
      home.file = {
        ".emacs" = {
          source = let
            chemacs = (builtins.fetchTarball {
              url = "https://github.com/plexus/chemacs/archive/master.tar.gz";
            });
          in "${chemacs}/.emacs";
          recursive = true;
        };
        ".emacs.d/init.el".text = ''
          (load "default.el")
        '';
        ".doom.d/emacs.d/init.el".text = ''
          (load "default.el")
        '';
        ".emacs-profiles.el".text = ''
          (("spacemacs" . ((user-emacs-directory . "~/.spacemacs.d")))
           ("doom" . ((user-emacs-directory . "~/.doom.d/emacs.d")))
           ("empty" . ((user-emacs-directory . "~/.emacs.d")))
           )
        '';
        ".doom.d/imports" = {
          source = ./doom.d/imports;
          recursive = true;
        };
        ".emacs-profile".text = "doom";
      };
    };
    environment = {
      variables = { EDITOR = "${doom-emacs-bin-path} -nw"; };
      shellAliases = {
        ec = "${doom-emacs-bin-path}";
        vim = "${doom-emacs-bin-path} -nw";
        emacs = "${doom-emacs-bin-path}";
      };
    };

    # systemd.user.services.emacs = {
    #   description = "Emacs: the extensible, self-documenting text editor";
    #   serviceConfig = {
    #     Type      = "forking";
    #     ExecStart = "${pkgs.emacs}/bin/emacs --daemon --user=${user}";
    #     ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
    #     Restart   = "always";
    #   };
    #   wantedBy = [ "default.target" ];
    #   environment = {
    #     SSH_AUTH_SOCK = "%t/keyring/ssh";
    #     # Make sure aspell will find its dictionaries
    #     ASPELL_CONF   = "dict-dir /run/current-system/sw/lib/aspell";
    #     # Make sure locate will find its database
    #     LOCATE_PATH   = "/var/cache/locatedb";
    #   };
    #   enable = true;
    # };

  };
}
