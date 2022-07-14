{ config, lib, pkgs, ... }:

{
  environment.variables = { BUILDKIT_HOST = "podman://buildkitd"; };

  virtualisation.oci-containers.containers = {
    buildkitd = {
      image = "docker.io/moby/buildkit:latest";
      autoStart = true;
      extraOptions = [ "--privileged" "--network=host" ];
    };
  };
}
