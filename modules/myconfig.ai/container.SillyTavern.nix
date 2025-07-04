{ pkgs, lib, config, inputs, ... }:
let
  SILLYTAVERN_DIR = "/home/sillytavern";
  CONFIG_PATH = "${SILLYTAVERN_DIR}/config";
  DATA_PATH = "${SILLYTAVERN_DIR}/data";
  PLUGINS_PATH = "${SILLYTAVERN_DIR}/plugins";
  EXTENSIONS_PATH = "${SILLYTAVERN_DIR}/extensions";
  sillytavern = {
    image =
      "ghcr.io/sillytavern/sillytavern:${config.myconfig.ai.container.sillytavern.version}";

    environment = rec {
      "TZ" = "Europe/Amsterdam";
      "NODE_ENV" = "production";
      "FORCE_COLOR" = "1";
      "OLLAMA_BASE_URL" = "http://host.containers.internal:${
          toString config.services.ollama.port
        }";
      "OLLAMA_API_BASE_URL" = "${OLLAMA_BASE_URL}/api";
    };

    volumes = [
      "${CONFIG_PATH}:/app/backend/config"
      "${DATA_PATH}:/app/backend/data"
      "${EXTENSIONS_PATH}:/app/public/scripts/extensions/third-party"
      "${PLUGINS_PATH}:/app/backend/plugins"
    ];

    ports = [
      "${config.myconfig.ai.container.sillytavern.host}:${
        toString config.myconfig.ai.container.sillytavern.port
      }:8000/tcp"
    ];

    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=sillytavern"
      "--hostname=sillytavern"
      "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
  options.myconfig = with lib; {
    ai.container.sillytavern = {
      enable = mkEnableOption "myconfig.ai.container.sillytavern";
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      port = mkOption {
        type = types.int;
        default = 8000;
      };
      version = mkOption {
        type = types.str;
        default = "latest";
      };
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable
    && config.myconfig.ai.container.sillytavern.enable
    && config.services.ollama.enable) {

      virtualisation.oci-containers.containers = { inherit sillytavern; };
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
