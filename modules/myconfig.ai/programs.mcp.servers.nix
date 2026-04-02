{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    home-manager.sharedModules = [
      ({config, ...}: lib.mkIf config.programs.mcp.enable {
        programs.mcp = {
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
            # weather = {
            #   command = "${lib.getExe pkgs.emcee}";
            #   args = ["https://api.weather.gov/openapi.json"];
            # };
          };
        };
      })
    ];
  };
}
