{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.container.n8n = {
      enable = mkEnableOption "myconfig.ai.container.n8n";
    };
  };
  config = lib.mkIf config.myconfig.ai.container.n8n.enable {
    containers.n8n = {
      autoStart = true;
      bindMounts = {
        "${config.containers.n8n.config.services.n8n.N8N_ENCRYPTION_KEY_FILE}" = {
          hostPath = config.containers.n8n.config.services.n8n.N8N_ENCRYPTION_KEY_FILE;
          isReadOnly = true;
        };
        "${config.containers.n8n.config.services.n8n.N8N_RUNNERS_AUTH_TOKEN_FILE}" = {
          hostPath = config.containers.n8n.config.services.n8n.N8N_RUNNERS_AUTH_TOKEN_FILE;
          isReadOnly = true;
        };
      };

      imports = [ ./services.n8n.nix ];
      config =
        { pkgs, lib, ... }:
        {
          services.n8n = {
            enable = true;
          };
        };
    };
  };
}
