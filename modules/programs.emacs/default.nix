# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/nix-doom-emacs.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  doom-emacs = pkgs.unstable.callPackage (builtins.fetchGit {
    url = "https://github.com/vlaci/nix-doom-emacs.git";
    inherit (json) rev ref;
  }) {
    doomPrivateDir = ./doom.d;
    extraPackages = epkgs: [ pkgs.mu pkgs.inconsolata ];
    extraConfig = ''
      (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
    '';
  };
  doom-emacs-bin-path = "${doom-emacs}/bin/emacs";
  xclipedit = pkgs.writeShellScriptBin "xclipedit" ''
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
  config = {
    nixpkgs.overlays = [
      (self: super: {
        emacs = super.emacs.overrideDerivation (drv: { nativeComp = true; });
      })
    ];

    environment.systemPackages = with pkgs; [ doom-emacs ];
    home-manager.users."${user}" = {
      home.packages = with pkgs;
        [
          emacs-all-the-icons-fonts

          aspell
          aspellDicts.de
          aspellDicts.en

          shellcheck
        ] ++ lib.optional config.services.xserver.enable xclipedit;
      programs.zsh.shellAliases = {
        magit = ''${doom-emacs-bin-path} -e "(magit-status \"$(pwd)\")"'';
      };
      home.file = {
        ".emacs.d/init.el".text = ''
          (load "default.el")
          (mapc 'load (file-expand-wildcards "~/.doom.d/imports/*.el"))
        '';
        ".doom.d/imports" = {
          source = ./doom.d/imports;
          recursive = true;
        };
      };
      programs.emacs.package = doom-emacs;
      programs.emacs.enable = true;
    };
    environment = {
      variables = { EDITOR = "${doom-emacs-bin-path} -nw"; };
      shellAliases = {
        ec = "${doom-emacs-bin-path}";
        vim = "${doom-emacs-bin-path} -nw";
        emacs = "${doom-emacs-bin-path}";
      };
    };
  };
}
