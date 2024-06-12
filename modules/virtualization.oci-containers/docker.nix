{ config, lib, pkgs, ... }:

let enable = config.virtualisation.docker.enable;
in {
  config = {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ docker docker-compose ];
      # home.file = {
      #   "bin/docker" = {
      #     source = ./bin;
      #     recursive = true;
      #   };
      #   "dockerfiles" = {
      #     source = ./bin;
      #     recursive = true;
      #   };
      # };
    }];
    environment = {
      variables = { DOCKER_BUILDKIT = "1"; };
      shellAliases = {
        d = "docker";
        dc = "docker-compose";
      };
      interactiveShellInit = ''
        dssh(){
            containerID=""
            [ $1 ] && {
                containerID=$1
            } || {
                containers="$(docker ps | tail -n +2)"

                echo "choose number of container:"
                echo
                echo $containers | nl

                if [ -n "$BASH_VERSION" ]; then
                    read -p "Num of container: " input
                elif [ -n "$ZSH_VERSION" ]; then
                    read "input?Num of container: "
                fi
                if [ -n "$input" ]; then
                    if [ $input -eq $input ]; then
                        containerID="$(echo $containers | sed "$input"'q;d' | awk '{print $1;}')"
                    fi
                fi
            }
            [ "$containerID" ] && docker exec -i -t "$containerID" bash
        }
      '';
    };

    systemd.services.docker.restartIfChanged = lib.mkForce false;
    virtualisation.docker = {
      daemon.settings = { ip = "127.0.0.1"; };
      extraOptions = "--data-root /home/docker";
      storageDriver = if config.fileSystems."/".fsType == "btrfs" then
        "btrfs"
      else
        "overlay2"; # 'overlay2' for systemd; 'btrfs' for btrfs ; etc.
      # socketActivation = false;
      rootless = {
        enable = true;
        setSocketVariable = true; # Set DOCKER\_HOST for rootless Docker
      };
      autoPrune.enable = true;
    };
  };
}
