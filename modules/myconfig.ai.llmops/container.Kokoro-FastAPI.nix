{
  pkgs,
  lib,
  config,
  ...
}:
let
  VERSION = "latest";
in
{

  options.myconfig = with lib; {
    ai.llmops.container.kokoro-fastapi.enable =
      mkEnableOption "myconfig.ai.llmops.container.kokoro-fastapi";
  };

  config =
    lib.mkIf
      (config.myconfig.ai.llmops.enable && config.myconfig.ai.llmops.container.kokoro-fastapi.enable)
      {
        virtualisation.oci-containers = {
          containers = {
            kokoro-fastapi = {
              image = "ghcr.io/remsky/kokoro-fastapi-gpu:${VERSION}";
              ports = [ "8880:8880" ];
              environment = {
                PYTHONPATH = "/app:/app/api";
                USE_GPU = "true";
                PYTHONUNBUFFERED = "1";
              };
              extraOptions = [
                "--pull=always" # Pull if the image on the registry is always
                "--name=kokoro-fastapi"
                "--hostname=kokoro-fastapi"
                # "--network=host"
                # "--add-host=host.containers.internal:host-gateway"
                "--gpus"
                "all"
              ];
            };
            kokoro-fastapi-ui = {
              image = "ghcr.io/remsky/kokoro-fastapi-ui:${VERSION}";
              ports = [ "8881:7860" ];
              environment = {
                GRADIO_WATCH = "1";
                PYTHONUNBUFFERED = "1";
                DISABLE_LOCAL_SAVING = "true";
                API_HOST = "kokoro-fastapi";
                API_PORT = "8880";
              };
              extraOptions = [
                "--pull=always" # Pull if the image on the registry is always
                "--name=kokoro-fastapi-ui"
                "--hostname=kokoro-fastapi-ui"
                # "--network=host"
                # "--add-host=host.containers.internal:host-gateway"
              ];
            };
          };
        };
      };
}
