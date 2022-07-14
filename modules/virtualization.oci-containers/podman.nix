{ config, lib, pkgs, ... }:

let enable = config.virtualisation.podman.enable;
in {
  config = {
    home-manager.sharedModules =
      lib.optional enable { home.packages = with pkgs; [ podman-compose ]; };
    virtualisation.oci-containers = lib.mkIf enable { backend = "podman"; };
  };
}
