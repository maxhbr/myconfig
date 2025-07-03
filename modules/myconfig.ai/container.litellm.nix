{
  pkgs,
  lib,
  config,
  ...
}:
{
  options.myconfig = with lib; {
    ai.container.litellm = {
      enable = mkEnableOption "myconfig.ai.container.litellm";
      config = mkOption {
        type = types.attrsOf types.anything;
        default = { };
        description = "Arbitrary YAML configuration";
      };
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      port = mkOption {
        type = types.int;
        default = 4000;
      };
      extraOptions = [
        "--pull=always" # Pull if the image on the registry is always
        "--name=litellm"
        "--hostname=litellm"
        # "--network=host"
        "--add-host=host.containers.internal:host-gateway"
      ];
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.litellm.enable) (
    let
      yamlFormat = pkgs.formats.yaml { };
      yamlFile = yamlFormat.generate "litellm_config.yaml" config.myconfig.ai.container.litellm.config;
    in
    {
      virtualisation.oci-containers.containers = {
        litellm = {
          image = "ghcr.io/berriai/litellm:main-latest";
          volumes = [ "${yamlFile}:/app/config.yaml" ];
          ports = [
            "${config.myconfig.ai.container.litellm.host}:${toString config.myconfig.ai.container.litellm.port}:4000/tcp"
          ];
          cmd = [
            "--config"
            "/app/config.yaml"
            "--detailed_debug"
          ];
        };
      };
    }
  );
}
