{ pkgs, lib, config, ... }:
let
  lobe-chat = {
    image = "docker.io/lobehub/lobe-chat:latest";
    environment = {
      "OLLAMA_PROXY_URL" = "http://host.containers.internal:11434";
    };
    ports = [
      "${config.myconfig.ai.container.lobe-chat.host}:${toString config.myconfig.ai.container.lobe-chat.port}:3210"
    ];
    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=lobe-chat"
      "--hostname=lobe-chat"
      "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {
    ai.container.lobe-chat = {
      enable = mkEnableOption "myconfig.ai.container.lobe-chat ";
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      port = mkOption {
        type = types.int;
        default = 3210;
      };
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.lobe-chat.enable) {
    virtualisation.oci-containers.containers = {
      inherit lobe-chat;
    };
  };
}
