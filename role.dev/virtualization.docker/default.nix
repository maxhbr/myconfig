# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        docker_compose
      ];
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
    };
    environment = {
      systemPackages = with pkgs; [
        docker
        docker-machine
      ];
      shellAliases = {
        d = "docker";
        dc = "docker-compose";
      };
      interactiveShellInit = ''
dcclean(){
    # call `dclean -a` to clean all containers
    docker ps -q $1 |xargs --no-run-if-empty docker rm
}

diclean(){
    # call `dclean -a` to clean all images
    remAll() {
        docker images -q -a |xargs -r docker rmi
    }
    remUntagged() {
        docker images | awk '/^<none>/ {print $3}' |xargs -r docker rmi
    }

    [[ "$1" == "-a" ]] && remAll || remUntagged
}

dclean(){
    # remove all dangling volumes
    docker volume ls -qf dangling=true |xargs -r docker volume rm

    # remove all dangling networks
    docker network prune
    # remove more stull (what does that remove exactly?)
    docker system prune

    confirm "also do dcclean?" && dcclean $@
    confirm "also do diclean?" && diclean $@
}

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

dsaveAllRunning() {
    docker ps --format "{{.Image}}" |\
        while read -r image ; do
            echo "save image $image to tar..."
            docker save -o "$image.tar" "$image"
        done
}

dstop(){
    docker update --restart=no $(docker ps -a -q)
    docker ps -q |xargs docker stop
}
'';
    };

    virtualisation.docker = {
        enable = true;
        extraOptions = "--data-root /home/docker";
        storageDriver = "overlay2";
        # socketActivation = false;
        autoPrune.enable = true;
        package = pkgs.docker-edge;
    };
  };
}
