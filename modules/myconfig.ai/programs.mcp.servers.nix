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
        programs.mcp.servers = {
          everything = {
            command = "npx";
            args = [
              "-y"
              "@modelcontextprotocol/server-everything"
            ];
          };
          context7 = {
            url = "https://mcp.context7.com/mcp";
            # headers = {
            #   CONTEXT7_API_KEY = "{env:CONTEXT7_API_KEY}";
            # };
          };
        };
      }
    ];
  };
}
