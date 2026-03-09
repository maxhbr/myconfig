{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.skills.playwright;
  playwright-cli = pkgs.buildNpmPackage rec {
    pname = "playwright-cli";
    version = "0.1.1";

    src = pkgs.fetchFromGitHub {
      owner = "microsoft";
      repo = "playwright-cli";
      rev = "v${version}";
      hash = "sha256-Ao3phIPinliFDK04u/V3ouuOfwMDVf/qBUpQPESziFQ=";
    };

    npmDepsHash = "sha256-4x3ozVrST6LtLoHl9KtmaOKrkYwCK84fwEREaoNaESc=";

    dontNpmBuild = true;

    passthru = {
      # Newer upstream tags intentionally print a deprecation message and exit.
      skipBulkUpdate = true;
    };

    meta = with lib; {
      description = "Playwright CLI for browser automation";
      homepage = "https://github.com/microsoft/playwright-cli";
      changelog = "https://github.com/microsoft/playwright-cli/releases/tag/v${version}";
      license = licenses.asl20;
      maintainers = with maintainers; [ imalison ];
      mainProgram = "playwright-cli";
    };
  };
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
          home.file.".config/playwright/cli.config.json".text = builtins.toJSON {
            browser = {
              browserName = cfg.browserName;
              launchOptions = {
                inherit executablePath;
              };
              allowUnrestrictedFileAccess = true;
            };
          };
          home.sessionVariables = {
            PLAYWRIGHT_MCP_CONFIG = "$HOME/.config/playwright/cli.config.json";
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
