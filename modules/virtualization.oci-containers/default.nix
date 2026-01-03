# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  enablePodman = config.virtualisation.podman.enable;
  enableDocker = config.virtualisation.docker.enable;
  backend = if enablePodman then "podman" else "docker";
in
{
  imports = [
    ./podman.nix
    ./docker.nix
  ];
  config = (
    lib.mkIf (enablePodman || enableDocker) {
      virtualisation.podman.dockerCompat = !enableDocker;
      home-manager.sharedModules = [ { home.packages = with pkgs; [ buildkit ]; } ];
      virtualisation = {
        # oci-containers = {
        #   inherit backend;
        # };
        containers = {
          policy = {
            "default" = [ { "type" = "reject"; } ];
            "transports" = {
              "docker" = {
                "docker.io" = [ { "type" = "insecureAcceptAnything"; } ];
                "docker.io/library" = [ { "type" = "insecureAcceptAnything"; } ];
                "ghcr.io" = [ { "type" = "insecureAcceptAnything"; } ];
                "quay.io" = [ { "type" = "insecureAcceptAnything"; } ];
              };

              "docker-archive" = {
                "" = [ { "type" = "insecureAcceptAnything"; } ];
              };

              # local images written while building
              "containers-storage" = {
                "" = [ { "type" = "insecureAcceptAnything"; } ];
              };

              # `docker-daemon =` pulls (e.g. `docker save | podman load`)
              "docker-daemon" = {
                "" = [ { "type" = "insecureAcceptAnything"; } ];
              };
            };
          };
        };
      };
    }
  );
}
