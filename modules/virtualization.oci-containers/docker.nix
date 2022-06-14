{ config, lib, pkgs, ... }:

let
  enable = config.virtualisation.docker.enable;
in
{
  config = {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ docker docker-machine docker-compose ];
      home.file = {
        "bin/docker" = {
          source = ./bin;
          recursive = true;
        };
        "dockerfiles" = {
          source = ./bin;
          recursive = true;
        };
      };
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
      extraOptions = "--data-root /home/docker";
      storageDriver = "overlay2";
      # socketActivation = false;
      autoPrune.enable = true;
    };
  };
}
