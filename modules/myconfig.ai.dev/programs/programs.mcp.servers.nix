{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    home-manager.sharedModules = [
      (
        { config, ... }:
        lib.mkIf config.programs.mcp.enable {
          programs.mcp = {
            # No server ships by default: every consumer of the registry
            # (opencode, claude-code, the sandbox mcp.json mount) is
            # opt-in per entry, and an empty registry leaves the sandbox
            # mount off entirely.
            servers = {
              # mcp-nixos = {
              #   command = "${lib.getExe pkgs.mcp-nixos}";
              #   args = [ ];
              # };
              # weather = {
              #   command = "${lib.getExe pkgs.emcee}";
              #   args = ["https://api.weather.gov/openapi.json"];
              # };
            };
          };
        }
      )
    ];
  };
}
