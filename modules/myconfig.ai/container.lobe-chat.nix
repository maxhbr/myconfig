{ pkgs, lib, config, ... }:
let
  lobe-chat = {
    image = "docker.io/lobehub/lobe-chat:latest";
    ports = [
      "127.0.0.1:3210:3210" # Ensures we listen only on localhost
    ];
    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=lobe-chat"
      "--hostname=lobe-chat"
      # "--network=host"
      # "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {
    ai.container.lobe-chat.enable = mkEnableOption "myconfig.ai.container.lobe-chat ";
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.lobe-chat.enable) {
    virtualisation.oci-containers.containers = {
      inherit lobe-chat;
    };
  };
}
