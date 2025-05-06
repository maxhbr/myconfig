{ pkgs, lib, config, ... }:

let
  openedai-speech = {
    image = "ghcr.io/matatonic/openedai-speech:latest";
    environment = rec {
      "TZ" = "Europe/Amsterdam";
      "TTS_HOME" = "/app/voices";
      "HF_HOME" = "/app/voices";
      "PRELOAD_MODEL" = "xtts";
      "EXTRA_ARGS" = "--log-level DEBUG --unload-t";
    };
    ports = [
      "127.0.0.1:8000:8000"
    ];
    volumes = [
      "/home/open-webui/openedai/voices:/app/voices"
      "/home/open-webui/openedai/config:/app/config"
    ];
    extraOptions = [
      "--pull=always"
      "--name=openedai-speech"
      "--hostname=openedai-speech"
    ];
  };
  open-webui = {
    image = "ghcr.io/open-webui/open-webui:main";

    environment = rec {
      "TZ" = "Europe/Amsterdam";
      "OLLAMA_BASE_URL" = "http://localhost:${toString config.services.ollama.port}";
      "OLLAMA_API_BASE_URL" = "${OLLAMA_BASE_URL}/api";
    };

    volumes = [ "/home/open-webui/data:/app/backend/data" ];

    # ports = [
    #   "${config.myconfig.ai.container.open-webui.host}:${toString config.myconfig.ai.container.open-webui.port}:8080" # Ensures we listen only on localhost
    # ];

    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=open-webui"
      "--hostname=open-webui"
      "--network=host"
      # "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {  
    ai.container.open-webui = {
      enable = mkEnableOption "myconfig.ai.container.open-webui";
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.open-webui.enable && config.services.ollama.enable) {
    virtualisation.oci-containers.containers = {
      inherit open-webui;
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 /home/open-webui/data -o root -g root
        install -d -m 755 /home/open-webui/openedai/voices -o root -g root
        install -d -m 755 /home/open-webui/openedai/config -o root -g root
      '';
    };
  };
}
