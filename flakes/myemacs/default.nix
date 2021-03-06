# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  # xclipedit = pkgs.writeShellScriptBin "xclipedit" ''
  #   set -euo pipefail
  #   tempfile="$(mktemp)"
  #   trap "rm $tempfile" EXIT
  #   if [ -t 0 ]; then
  #       xclip -out > "$tempfile"
  #   else
  #       cat > "$tempfile"
  #   fi
  #   ${doom-emacs-bin-path} "$tempfile"
  #   ${pkgs.xclip}/bin/xclip < "$tempfile"
  # '';
in {
  config = {
    nixpkgs.overlays = [
      (self: super: {
        emacs = super.emacs.overrideDerivation (drv: { nativeComp = true; });
      })
    ];

    # home-manager.users."${myconfig.user}" = {
    #   programs.doom-emacs = {
    #     enable = true;
    #     doomPrivateDir = let
    #       doomPrivateDeriv = pkgs.stdenv.mkDerivation rec {
    #         name = "doomPrivateDeriv-1.0";
    #         src = ./doom.d;
    #         installPhase = ''
    #           mkdir -p "$out"
    #           cp -r * "$out"
    #         '';
    #       };
    #     in "${doomPrivateDeriv}";
    #     extraPackages = epkgs: [
    #       pkgs.mu
    #       (pkgs.nerdfonts.override { fonts = [ "Inconsolata" ]; })
    #     ];
    #     extraConfig = ''
    #       (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
    #     '';
    #   };
    #   home.packages = with pkgs;
    #     [
    #       emacs-all-the-icons-fonts

    #       aspell
    #       aspellDicts.de
    #       aspellDicts.en

    #       shellcheck
    #     ] ++ lib.optional config.services.xserver.enable xclipedit;
    #   # programs.zsh.shellAliases = {
    #   #   magit = ''${doom-emacs-bin-path} -e "(magit-status \"$(pwd)\")"'';
    #   # };
    #   home.file = {
    #     ".emacs.d/init.el".text = ''
    #       (load "default.el")
    #       (mapc 'load (file-expand-wildcards "~/.doom.d/imports/*.el"))
    #     '';
    #     ".doom.d/imports" = {
    #       source = ./doom.d-imports;
    #       recursive = true;
    #     };
    #   };
    # };
    environment = {
      variables = { EDITOR = "emacs -nw"; };
      shellAliases = {
        ec = "emacs";
        vim = "emacs -nw";
        emacs = "emacs";
      };
    };
  };
}
