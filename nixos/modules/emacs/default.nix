# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    nixpkgs.overlays = [
      (final: previous: {
        my-emacs-wrapper = previous.stdenv.mkDerivation rec {
          version = "1.0";
          name = "my-emacs-wrapper-${version}";

          src = ./.;

          buildPhase = ''
            # replace nix-shell headers
            find . -iname '*.sh' \
              -exec sed -i "1s%.*%#!/usr/bin/env bash%" {} \; \
              -exec sed -i -e '2d' {} \;

            sed -i -e 's%emacsclient%${pkgs.emacs}/bin/emacsclient%g' ec
          '';
          installPhase = ''
            bin="$out/bin"
            mkdir -p $bin
            cp ec $bin
          '';

          meta = with previous.stdenv.lib; {
            description = "My small wrapper around emacs";
            homepage = https://github.com/maxhbr/myconfig;
            license =  licenses.mit;
            platforms = platforms.unix;
            maintainers = [ ];
          };
        };
      })
    ];
    environment = {
      systemPackages = with pkgs; [ my-emacs-wrapper emacs aspell aspellDicts.de aspellDicts.en ];
      variables = {
        EDITOR = "${pkgs.my-emacs-wrapper}/bin/ec -t";
      };
      interactiveShellInit = ''
        alias vim="${pkgs.my-emacs-wrapper}/bin/ec -t"
        alias emacs="${pkgs.my-emacs-wrapper}/bin/ec"
      '';
    };

    systemd.user.services.emacs = {
      description = "Emacs: the extensible, self-documenting text editor";
      serviceConfig = {
        Type      = "forking";
        ExecStart = "${pkgs.emacs}/bin/emacs --daemon --user=mhuber";
        ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart   = "always";
      };
      wantedBy = [ "default.target" ];
      environment = {
        SSH_AUTH_SOCK = "%t/keyring/ssh";
        # Make sure aspell will find its dictionaries
        ASPELL_CONF   = "dict-dir /run/current-system/sw/lib/aspell";
        # Make sure locate will find its database
        LOCATE_PATH   = "/var/cache/locatedb";
      };
      enable = true;
    };

    # services.emacs = {
    #   enable = true;
    #   install = true;
    #   defaultEditor = true;
    #   package = pkgs.emacs;
    #   # package = import /home/mhuber/.emacs.d { pkgs = pkgs; };
    # };
  };
}
