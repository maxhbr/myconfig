# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.hermes — NixOS declarative container backend.
#
# When `myconfig.ai.hermes.enable` and `myconfig.ai.hermes.container.enable`
# are both true, runs the hermes-agent gateway inside an isolated NixOS
# declarative container (`containers.hermes`) instead of as a host-level
# systemd service (see service.nix). The container reuses the same
# `hermesServiceCfg` as the native backend (from shared.nix) so the two
# modes stay in sync.
#
# NOTE: the API server host advertised to hermes (`apiServerHost` in
# shared.nix) is derived from `config.containers.hermes.localAddress`, i.e.
# this module's own output — this works because NixOS option evaluation is
# lazy.
{
  config,
  lib,
  pkgs,
  inputs,
  myconfig,
  ...
}:
let
  shared = import ./shared.nix {
    inherit
      config
      lib
      pkgs
      myconfig
      ;
  };
  inherit (shared)
    hermesServiceCfg
    stateDir
    hostConfig
    ;
  cfg = config.myconfig.ai.hermes;
in
{
  config = lib.mkIf (cfg.enable && cfg.container.enable) {
    containers.hermes = {
      autoStart = cfg.container.autostart;
      privateNetwork = true;
      hostAddress = "192.168.111.10";
      localAddress = "192.168.111.11";
      hostAddress6 = "fc00::1";
      localAddress6 = "fc00::2";
      bindMounts = {
        "${stateDir}" = {
          hostPath = stateDir;
          mountPoint = stateDir;
          isReadOnly = false;
        };
        "/home/mhuber/.hermes-secrets" = {
          hostPath = "/home/mhuber/.hermes-secrets";
          mountPoint = "/home/mhuber/.hermes-secrets";
          isReadOnly = false;
        };
      };

      config =
        {
          config,
          pkgs,
          lib,
          ...
        }:
        let
          containerConfig = config;
        in
        {
          imports = [
            inputs.hermes-agent.nixosModules.default
            inputs.home.nixosModules.home-manager
          ];

          services.hermes-agent = hermesServiceCfg;

          users.users.mhuber = lib.mkForce {
            isNormalUser = true;
            home = "/home/mhuber";
            createHome = true;
            uid = hostConfig.users.users.mhuber.uid or 1000;
            extraGroups = [ "mhuber" ];
          };
          users.groups.mhuber = { };

          home-manager.users.mhuber =
            { pkgs, ... }:
            {
              imports = [
              ];

              home.stateVersion = containerConfig.system.stateVersion;
            };

          system.stateVersion = "25.11";

          networking = {
            firewall = {
              enable = true;
              # allowedTCPPorts = [ 80 ];
            };
            # Use systemd-resolved inside the container
            # Workaround for bug https://github.com/NixOS/nixpkgs/issues/162686
            useHostResolvConf = lib.mkForce false;
          };

          services.resolved.enable = true;

        };
    };
  };
}
