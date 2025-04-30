{ pkgs, lib, config, ... }:
let
  SILLYTAVERN_VERSION="latest";
  CONFIG_PATH="/home/open-webui/config";
  DATA_PATH="/home/open-webui/data";
  PLUGINS_PATH="/home/open-webui/plugins";
  EXTENSIONS_PATH="/home/open-webui/extensions";
# docker run \
#   --name="sillytavern" \
#   -p "$PUBLIC_PORT:8000/tcp" \
#   -v "$CONFIG_PATH:/home/node/app/config:rw" \
#   -v "$DATA_PATH:/home/node/app/data:rw" \
#   -v "$EXTENSIONS_PATH:/home/node/app/public/scripts/extensions/third-party:rw" \
#   -v "$PLUGINS_PATH:/home/node/app/plugins:rw" \
#   ghcr.io/sillytavern/sillytavern:"$SILLYTAVERN_VERSION"
  sillytavern = {
    image = "ghcr.io/sillytavern/sillytavern:${SILLYTAVERN_VERSION}";

    environment = rec {
      "TZ" = "Europe/Amsterdam";
      # "OLLAMA_BASE_URL" = "http://localhost:${toString config.services.ollama.port}";
      # "OLLAMA_API_BASE_URL" = "${OLLAMA_BASE_URL}/api";
    };

    volumes = [
      "${CONFIG_PATH}:/app/backend/config"
      "${DATA_PATH}:/app/backend/data"
      "${EXTENSIONS_PATH}:/app/public/scripts/extensions/third-party"
      "${PLUGINS_PATH}:/app/backend/plugins"
    ];

    ports = [
      "${config.myconfig.ai.container.sillytavern.publicPort}:8000/tcp"
    ];

    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=sillytavern"
      "--hostname=sillytavern"
      # "--network=host"
      "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {  
    ai.container.sillytavern = {
      enable = mkEnableOption "myconfig.ai.container.sillytavern";
      publicPort = mkOption {
        type = types.int;
        default = 8000;
      };
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.sillytavern.enable && config.services.ollama.enable) {
    virtualisation.oci-containers.containers = {
      inherit sillytavern;
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 ${CONFIG_PATH} -o root -g root
        install -d -m 755 ${DATA_PATH} -o root -g root
        install -d -m 755 ${EXTENSIONS_PATH} -o root -g root
        install -d -m 755 ${PLUGINS_PATH} -o root -g root
      '';
    };
  };
}
