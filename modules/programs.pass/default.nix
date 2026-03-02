# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  osConfig = config;
  wireguardKeypairToPassStore =
    with pkgs;
    writeScriptBin "wireguardKeypairToPassStore.sh" (lib.fileContents ./wireguardKeypairToPassStore.sh);
  otpPass = pkgs.writeShellScriptBin "otpPass" ''
    ${pkgs.oath-toolkit}/bin/oathtool --totp -b "$(${pkgs.pass}/bin/pass -p "$1")"
  '';
in
{
  config = {
    nixpkgs.overlays = [
      (
        self: super:
        let
          pass = super.pass.overrideDerivation (drv: {
            # should work for 1.7.3
            patches = drv.patches ++ [ ./patches/pass_-_copy_by_default.diff ];
            doInstallCheck = false;
          });
        in
        {
          inherit pass;
          gopassWrapper =
            with pkgs;
            writeShellScriptBin "gopass_wrapper.sh" ''
              if [ -f ~/.gpg-agent-info ] && [ -n "$(${procps}/bin/pgrep gpg-agent)" ]; then
                source ~/.gpg-agent-info
                export GPG_AGENT_INFO
              else
                eval $(${gnupg}/bin/gpg-agent --daemon)
              fi
              export GPG_TTY="$(tty)"

              exec ${pkgs.gopass-jsonapi}/bin/gopass-jsonapi listen
            '';
        }
      )
    ];
    home-manager.sharedModules = [
      (
        { config, ... }:
        let
          accounts = lib.attrValues config.myconfig.accounts;
          primaryAccount = lib.findFirst (a: a.primary) (builtins.head accounts) accounts;
        in
        {
          programs.password-store = {
            enable = true;
            # package = pass;
            settings = {
              PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
              PASSWORD_STORE_KEY = lib.mkIf (accounts != [ ]) primaryAccount.pgp-key-id;
              PASSWORD_STORE_CLIP_TIME = "60";
            };
          };
          services.pass-secret-service.enable = true;
          programs.browserpass.enable = osConfig.myconfig.desktop.enable;
          home.packages =
            with pkgs;
            [
              # pass
              pass-git-helper
              wireguardKeypairToPassStore
              otpPass
            ]
            ++ lib.optionals osConfig.myconfig.desktop.enable [
              gopass
              gopass-jsonapi
              dmenu-wayland
            ];
          home.file = {
            ".config/pass-git-helper/git-pass-mapping.ini".source =
              ./config/pass-git-helper/git-pass-mapping.ini;
          };
          programs.git.extraConfig.credential.helper = "${pkgs.pass-git-helper}/bin/pass-git-helper";
        }
      )
    ];
  };
}
