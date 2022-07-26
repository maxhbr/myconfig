# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:
let
  wireguardKeypairToPassStore = with pkgs;
    writeScriptBin "wireguardKeypairToPassStore.sh"
    (lib.fileContents ./wireguardKeypairToPassStore.sh);
  otpPass = pkgs.writeShellScriptBin "otpPass" ''
    ${pkgs.oathToolkit}/bin/oathtool --totp -b "$(${pkgs.pass}/bin/pass -p "$1")"
  '';
in {
  config = {
    nixpkgs.overlays = [
      (self: super:
        let
          pass = super.pass.overrideDerivation (drv: {
            # should work for 1.7.3
            patches = drv.patches ++ [ ./patches/pass_-_copy_by_default.diff ];
            doInstallCheck = false;
          });
        in {
          inherit pass;

          # https://github.com/NixOS/nixpkgs/pull/181281#issuecomment-1182841233
          #   packages failed to build:
          #     datree
          #     git-credential-gopass
          #     gopass
          #     gopass-hibp
          #     gopass-jsonapi
          #     gopass-summon-provider
          gopass = self.nixos-2111.gopass;
          gopass-jsonapi = self.nixos-2111.gopass-jsonapi;

          pass-git-helper =
            super.python3Packages.callPackage ./pass-git-helper.nix {
              inherit (super.python3Packages) buildPythonApplication;
              inherit (self.python3Packages) pyxdg;
            };
          gopassWrapper = with pkgs;
            writeShellScriptBin "gopass_wrapper.sh" ''
              if [ -f ~/.gpg-agent-info ] && [ -n "$(${procps}/bin/pgrep gpg-agent)" ]; then
                source ~/.gpg-agent-info
                export GPG_AGENT_INFO
              else
                eval $(${gnupg}/bin/gpg-agent --daemon)
              fi
              export GPG_TTY="$(tty)"

              exec ${gopass-jsonapi}/bin/gopass-jsonapi listen
            '';
        })
    ];
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [ pass pass-git-helper wireguardKeypairToPassStore otpPass ]
        ++ lib.optionals config.myconfig.desktop.full [ gopass gopass-jsonapi ];
      home.file = {
        ".config/pass-git-helper/git-pass-mapping.ini".source =
          ./config/pass-git-helper/git-pass-mapping.ini;
      };
    }];
  };
}
