{ pkgs, lib, config, ... }:
let
  nlm-ingestor = {
    image = "ghcr.io/nlmatics/nlm-ingestor:latest";
    ports = [
      "127.0.0.1:5010:5001" # Ensures we listen only on localhost
    ];
    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=nlm-ingestor"
      "--hostname=nlm-ingestor"
      # "--network=host"
      # "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {
    ai.container.nlm-ingestor.enable =
      mkEnableOption "myconfig.ai.container.nlm-ingestor ";
  };
  config = lib.mkIf (config.myconfig.ai.enable
    && config.myconfig.ai.container.nlm-ingestor.enable) {
      virtualisation.oci-containers.containers = { inherit nlm-ingestor; };
    };
}
