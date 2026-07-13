{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.skills.playwright;
  playwright-cli = pkgs.playwright-cli;
in
{
  options.myconfig.ai.skills.playwright = with lib; {
    enable = mkEnableOption "myconfig.ai.skills.playwright";
    browserName = mkOption {
      type = types.enum [
        "chromium"
        "firefox"
      ];
      default = "chromium";
      description = "The browser to use for playwright-cli";
    };
  };
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      (
        { config, ... }:
        let
          executablePath =
            {
              chromium = "${config.programs.chromium.package or pkgs.chromium}/bin/chromium";
              firefox = "${config.programs.firefox.package or pkgs.firefox}/bin/firefox";
            }
            .${cfg.browserName};
        in
        {
          home.packages = [ playwright-cli ];

          home.sessionVariables = {
            PLAYWRIGHT_MCP_BROWSER = cfg.browserName;
            PLAYWRIGHT_MCP_EXECUTABLE_PATH = executablePath;
            PLAYWRIGHT_MCP_ALLOW_UNRESTRICTED_FILE_ACCESS = "true";
          };

          programs.opencode.skills.playwright-cli = "${playwright-cli.src}/skills/playwright-cli";
          programs.claude-code.skills.playwright-cli = "${playwright-cli.src}/skills/playwright-cli";
          programs.codex.skills.playwright-cli = "${playwright-cli.src}/skills/playwright-cli";
        }
      )
    ];
  };
}
