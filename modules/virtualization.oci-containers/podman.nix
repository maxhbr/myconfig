{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    (
      { config, myconfig, ... }:
      let
        nixosConfig = config;
        user = myconfig.user;
      in
      lib.mkIf
        (nixosConfig.virtualisation.podman.enable && nixosConfig.myconfig.persistence.impermanence.enable)
        {
          home-manager.sharedModules = [
            (
              { config, ... }:
              {
                home.file.".config/containers/storage.conf".text = ''
                  [storage]
                  driver    = "overlay"
                  graphroot = "/persistent/cache/${config.home.username}-podman-containers"
                  runroot   = "/run/user/${toString nixosConfig.users.users.${config.home.username}.uid}/podman"

                  [storage.options]
                  mount_program = "${pkgs.fuse-overlayfs}/bin/fuse-overlayfs"
                '';
              }
            )
          ];
          system.activationScripts = {
            script.text = ''
              install -d -m 700 "/persistent/cache/${user}-podman-containers" -o ${
                toString nixosConfig.users.extraUsers.${user}.uid
              } -g ${toString nixosConfig.users.extraGroups.${user}.gid}
            '';
          };
        }
    )
    (
      { config, myconfig, ... }:
      let
        nixosConfig = config;
        user = myconfig.user;
      in
      lib.mkIf (nixosConfig.virtualisation.podman.enable) {
        boot.kernelModules = [ "tun" ];
        services.udev.extraRules = ''
          KERNEL=="tun", NAME="net/tun", MODE="0666"
        '';
      }
    )
  ];
  config = lib.mkIf config.virtualisation.podman.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          dive # look into docker image layers
          podman-tui # status of containers in the terminal
          podman-compose
        ];
        myconfig.persistence.cache-directories = [ ".local/share/containers/cache/" ];
      }
    ];
    virtualisation.podman = {
      autoPrune.enable = true;
      # # Create a `docker` alias for podman, to use it as a drop-in replacement
      # dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };
    environment.systemPackages = [
      pkgs.netavark
    ];
  };
}
