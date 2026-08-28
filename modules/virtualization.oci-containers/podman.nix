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
                services.podman.settings.storage.storage = {
                  driver = "overlay";
                  graphroot = "/persistent/cache/${config.home.username}-podman-containers";
                  runroot = "/run/user/${toString nixosConfig.users.users.${config.home.username}.uid}/podman";
                  options.mount_program = "${pkgs.fuse-overlayfs}/bin/fuse-overlayfs";
                };
                services.podman.settings.containers.engine = {
                  image_copy_tmp_dir = "/persistent/cache/${config.home.username}-podman-tmp";
                  tmp_dir = "/persistent/cache/${config.home.username}-podman-tmp";
                };
              }
            )
          ];
          system.activationScripts = {
            script.text = ''
              install -d -m 700 "/persistent/cache/${user}-podman-containers" -o ${
                toString nixosConfig.users.extraUsers.${user}.uid
              } -g ${toString nixosConfig.users.extraGroups.${user}.gid}
              install -d -m 700 "/persistent/cache/${user}-podman-tmp" -o ${
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

        # home-manager's podman module renders ~/.config/containers/{registries,
        # storage,containers,policy}.conf. Keep policy in sync with the
        # system-wide one from ./default.nix so the user config does not weaken
        # or diverge from /etc/containers/policy.json.
        services.podman = {
          enable = true;
          package = config.virtualisation.podman.package;
          settings = {
            registries.search = [
              "docker.io"
              "quay.io"
            ];
            policy = config.virtualisation.containers.policy;
          };
        };
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
