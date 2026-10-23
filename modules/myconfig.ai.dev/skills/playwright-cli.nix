# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The `playwright-cli` skill (https://github.com/microsoft/playwright-cli):
# drive a real browser from the shell — `open`/`goto`/`click`/`snapshot` —
# instead of loading an MCP tool schema, which is what makes the CLI the
# token-cheaper variant of Playwright MCP for coding agents.
#
# Two halves, and this module owns both:
#
#   * the *skill*: upstream ships the Agent Skills content inside the
#     Playwright the CLI runs against (`playwright-core/lib/tools/skills/
#     playwright-cli`: `SKILL.md` plus `references/`). Registering that exact
#     directory — not the copy in the CLI's own source tarball — keeps the
#     deployed skill identical to the one `playwright-cli` checks installed
#     copies against (`skillCheck.js`), so no "skill does not match the tool
#     version" banner appears.
#   * the *binary*: added to the shared `myconfig.ai.dev.sandboxTools` hook,
#     so it exists wherever an agent runs, not only on the host.
#
# Browser: the CLI launches whatever `PLAYWRIGHT_MCP_EXECUTABLE_PATH` names;
# `PLAYWRIGHT_MCP_BROWSER` only picks the family (`chromium`/`firefox`). This
# module pins the host's own chromium/firefox, so the default `package` is built
# WITHOUT Playwright's bundled browser revisions — they would be several GiB of
# dead weight next to the executable that is actually launched. A host that has
# no browser at all gets the nixpkgs default pulled in; set `browserName` to
# choose which.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.skills.playwright-cli;

  # The CLI with NO Playwright browser revisions: `browserName` always pins an
  # explicit executable, and playwright-core launches that binary (instead of
  # resolving a revision from `PLAYWRIGHT_BROWSERS_PATH`) as soon as
  # `PLAYWRIGHT_MCP_EXECUTABLE_PATH` is set. Without this the package would pull
  # `playwright-driver.passthru.browsers` (chromium, the headless shell, firefox,
  # webkit, ffmpeg) into every host with the skill.
  package = pkgs.playwright-cli.override { playwrightBrowsers = null; };
in
{
  options.myconfig.ai.dev.skills.playwright-cli = with lib; {
    enable = mkEnableOption "myconfig.ai.dev.skills.playwright-cli";

    package = mkOption {
      type = types.package;
      default = package;
      defaultText = literalExpression "pkgs.playwright-cli with `playwrightBrowsers = null`";
      description = ''
        The `playwright-cli` package to use. The default carries no Playwright
        browser revisions because `browserName` pins an executable anyway; set
        this to `pkgs.playwright-cli` to ship the bundled browsers (several
        GiB) instead.
      '';
    };

    browserName = mkOption {
      type = types.enum [
        "chromium"
        "firefox"
      ];
      default = "chromium";
      description = ''
        Browser family the CLI drives, exported as `PLAYWRIGHT_MCP_BROWSER`.
        The executable itself is pinned through
        `PLAYWRIGHT_MCP_EXECUTABLE_PATH`.
      '';
    };

    allowUnrestrictedFileAccess = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Export `PLAYWRIGHT_MCP_ALLOW_UNRESTRICTED_FILE_ACCESS`. Playwright
        restricts file upload/download paths to the workspace and the output
        directory; an agent that hands the CLI absolute paths outside the
        current repository needs this to reach them.
      '';
    };
  };

  config = lib.mkMerge [
    # Auto-enabled by the `myconfig.ai.dev` umbrella
    # (`myconfig.ai.dev.skills.playwright-cli.enable = lib.mkDefault true` in
    # ../default.nix), like `agent-browser`: the skill is useless without the
    # binary, and the umbrella is where this repo turns its agent tooling on. The
    # `enable` option itself stays false, so a host without `myconfig.ai.dev`
    # never grows the chromium closure `browserName` falls back to.
    (lib.mkIf cfg.enable {
      # Register the skill source; `skills/default.nix` applies it to every
      # enabled agent harness via the `handcrafted` registry. The directory is
      # the one the installed CLI considers current, not the tarball copy.
      myconfig.ai.dev.skills.handcrafted.playwright-cli = cfg.package.passthru.skillDir;

      home-manager.sharedModules = [
        (
          { config, ... }:
          let
            # `finalPackage`, not `package`: the wrapper home-manager builds
            # (`commandLineArgs`, plasma support) is what launches. It is `null`
            # when the host does not enable the browser in home-manager at all —
            # fall back to the plain nixpkgs build then.
            finalPackage =
              {
                chromium = config.programs.chromium.finalPackage;
                firefox = config.programs.firefox.finalPackage;
              }
              .${cfg.browserName};
            executable = if finalPackage != null then finalPackage else pkgs.${cfg.browserName};
          in
          {
            home.packages = [ cfg.package ];

            home.sessionVariables = {
              PLAYWRIGHT_MCP_BROWSER = cfg.browserName;
              PLAYWRIGHT_MCP_EXECUTABLE_PATH = lib.getExe executable;
            }
            // lib.optionalAttrs cfg.allowUnrestrictedFileAccess {
              PLAYWRIGHT_MCP_ALLOW_UNRESTRICTED_FILE_ACCESS = "true";
            };
          }
        )
      ];

      # Sandbox tiers: the skill is deployed to every enabled harness, so the
      # CLI has to exist where those harnesses run — not only on the host.
      myconfig.ai.dev.sandboxTools.extraPackages = [ cfg.package ];
      myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = cfg.browserName;
    })
  ];
}
