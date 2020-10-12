# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
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
  doom-emacs-bin = pkgs.writeShellScriptBin "doom-emacs" ''
    exec "${doom-emacs}/bin/emacs" --with-profile empty "$@" &disown
  '';
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
${doom-emacs}/bin/emacs "$tempfile"
${pkgs.xclip}/bin/xclip < "$tempfile"
  '';
               
  # doom-emacsclient-bin = pkgs.writeShellScriptBin "doom-emacsclient" ''
  #   exec "${doom-emacs}/bin/emacsclient" "$@"
  # '';
  my-emacs-wrapper = (emacs:
    pkgs.writeShellScriptBin "ec" ''
      # Copyright 2018-2020 Maximilian Huber <oss@maximilian-huber.de>
      # SPDX-License-Identifier: MIT
      #
      #
      # partially stolen from
      # - http://mjwall.com/blog/2013/10/04/how-i-use-emacs/
      # - https://gist.github.com/alexmurray/337ac19014d769f4b219
      # - https://www.emacswiki.org/emacs/EmacsClient
      #
      #
      # call with
      #   $0 [-t|-d|-k|--wait]
      set -e

      if [ "$1" = "-k" ]; then
          server_ok=$(${emacs}/bin/emacsclient -a "false" -e "(boundp 'server-process)")
          if [ "t" == "$server_ok" ]; then
              echo "Shutting down Emacs server"
              ${emacs}/bin/emacsclient -e '(kill-emacs)'
          else
              echo "Emacs server not running"
          fi
      elif [ "$1" = "-d" ]; then
          ${emacs}/bin/emacs --daemon --user=$USER
      elif [ "$1" = "-nw" ] || [ "$1" = "-t" ] || [ "$1" = "--tty" ] || [ -z "$DISPLAY" ]; then
          if [[ "$#" -ne "0" ]]; then
              exec ${emacs}/bin/emacsclient --alternate-editor="" --tty "$@"
          else
              exec ${emacs}/bin/emacsclient --alternate-editor="" --tty ./
          fi
      else
          if [ "$1" = "--wait" ]; then
              shift
              exec ${emacs}/bin/emacsclient --alternate-editor="" --create-frame "$@"
          else
              exec ${emacs}/bin/emacsclient --no-wait --alternate-editor="" --create-frame "$@"
          fi
      fi
    '') doom-emacs;
in {
  imports = [ ./spacemacs.nix ];
  config = {
    nixpkgs.overlays = [
      (self: super: {
        emacs = super.emacs.overrideDerivation (drv: { nativeComp = true; });
      })
    ];

    environment = { systemPackages = with pkgs; [ doom-emacs ]; };
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        my-emacs-wrapper
        doom-emacs-bin
        xclipedit

        aspell
        aspellDicts.de
        aspellDicts.en
        shellcheck
        ripgrep
      ];
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
        ".emacs-profile".text = "doom";
      };
    };
    environment = {
      variables = { EDITOR = "${my-emacs-wrapper}/bin/ec -t"; };
      shellAliases = {
        vim = "${my-emacs-wrapper}/bin/ec -t";
        emacs = "${my-emacs-wrapper}/bin/ec";
        magit = ''${my-emacs-wrapper}/bin/ec -e "(magit-status \"$(pwd)\")"'';
        mu4e = "${pkgs.emacs}/bin/emacs -name ScratchMu4e &disown";
      };
    };

    # systemd.user.services.emacs = {
    #   description = "Emacs: the extensible, self-documenting text editor";
    #   serviceConfig = {
    #     Type      = "forking";
    #     ExecStart = "${pkgs.emacs}/bin/emacs --daemon --user=mhuber";
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
