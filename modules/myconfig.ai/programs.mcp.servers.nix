{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.myconfig.ai.enable {
    home-manager.sharedModules = [
      {
        programs.mcp = {
          enable = true;
          servers = {
            mcp-nixos = {
              command = "${lib.getExe pkgs.mcp-nixos}";
              args = [ ];
            };
            context7 = {
              url = "https://mcp.context7.com/mcp";
              # headers = {
              #   CONTEXT7_API_KEY = "{env:CONTEXT7_API_KEY}";
              # };
            };
          };
        };
      }
    ];
  };
}
