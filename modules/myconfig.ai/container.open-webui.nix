{ pkgs, lib, config, ... }:
let
  open-webui = {
    image = "ghcr.io/open-webui/open-webui:main";

    environment = rec {
      "TZ" = "Europe/Amsterdam";
      "OLLAMA_BASE_URL" = "http://${config.services.ollama.host}:${toString config.services.ollama.port}";
      "OLLAMA_API_BASE_URL" = "${OLLAMA_BASE_URL}/api";
    };

    volumes = [ "/home/open-webui/data:/app/backend/data" ];

    ports = [
      "127.0.0.1:3000:8080" # Ensures we listen only on localhost
    ];

    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=open-webui"
      "--hostname=open-webui"
      "--network=host"
      "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {  
    ai.container.open-webui.enable = mkEnableOption "myconfig.ai.container.open-webui";
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.open-webui.enable && config.services.ollama.enable) {
    virtualisation.oci-containers.containers = {
      inherit open-webui;
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 /home/open-webui/data -o root -g root
      '';
    };
  };
}
