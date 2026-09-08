# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# agent-browser (https://github.com/vercel-labs/agent-browser) — headless
# browser automation CLI for AI agents. The package is available in nixpkgs
# (`pkgs.by-name/ag/agent-browser/package.nix`), so no extra flake input is
# needed: this module just consumes `pkgs.agent-browser`.
#
# agent-browser provides browser automation capabilities to AI agents,
# enabling them to interact with web pages, fill forms, click buttons, and
# extract information. It requires a browser runtime (chromium by default).
#
# The module follows the same pattern as `programs.hunk`: it auto-enables
# wherever `myconfig.ai.enable` is true, because browser automation is a
# core capability for agentic coding workflows.
#
# Browser runtime in sandboxes: agent-browser needs a browser at runtime.
# For sandbox environments (bubblewrap jails, microvm guests, gVisor, mysbx),
# the browser must be explicitly added to the sandbox closure. This module
# adds agent-browser itself to all sandbox tiers via
# `myconfig.ai.sandboxTools.extraPackages`, but NOT the browser — that
# remains an explicit per-host opt-in (closure size, security surface).
# Hosts that want browser automation in sandboxes should add:
#
#   myconfig.ai.sandboxTools.extraPackages = with pkgs; [
#     chromium  # or firefox
#   ];
#   myconfig.ai.sandboxTools.extraEnv.AGENT_BROWSER_EXECUTABLE_PATH =
#     "${pkgs.chromium}/bin/chromium";
#   myconfig.ai.mysbx.extraTools = with pkgs; [ chromium ];
#
# Upstream ships skills and skill-data alongside the binary; these are
# copied to `$out/skills` and `$out/skill-data` in the package and deployed
# to agent harnesses via the handcrafted skill registry.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.agent-browser;

  # Default browser configuration for agent-browser
  # agent-browser uses `which` to find browser executables at runtime
  browserExecutable = "${config.programs.chromium.package or pkgs.chromium}/bin/chromium";
in
{
  options.myconfig = with lib; {
    ai.agent-browser = {
      enable = mkEnableOption "myconfig.ai.agent-browser";

      package = mkPackageOption pkgs "agent-browser" { };

      browserName = mkOption {
        type = types.enum [
          "chromium"
          "firefox"
        ];
        default = "chromium";
        description = ''
          The browser to use for agent-browser. agent-browser uses `which`
          to locate the browser executable at runtime.
        '';
      };

      browserExecutablePath = mkOption {
        type = types.str;
        default = browserExecutable;
        example = literalExpression ''"\${pkgs.chromium}/bin/chromium"'';
        description = ''
          Absolute path to the browser executable. This is set based on
          `browserName` but can be overridden manually.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Register the agent-browser skill source (NixOS-level);
    # `skills/default.nix` applies it to every enabled agent harness via
    # the `handcrafted` registry.
    myconfig.ai.skills.handcrafted.agent-browser = "${cfg.package}/skills";

    # Set environment variables for agent-browser to find the browser
    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];

        home.sessionVariables = {
          AGENT_BROWSER_BROWSER = cfg.browserName;
          AGENT_BROWSER_EXECUTABLE_PATH = cfg.browserExecutablePath;
        };
      }
    ];

    # Sandbox tiers: agent-browser needs to exist where agents run — inside
    # the sandboxes, not only on the host. Browser automation is a core
    # capability for agentic coding workflows.
    #
    # `myconfig.ai.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point.
    #
    # NOTE: agent-browser requires a browser at runtime. Hosts that want
    # browser automation in sandboxes must also add chromium (or another
    # browser) to `myconfig.ai.sandboxTools.extraPackages` and set the
    # appropriate environment variable in `myconfig.ai.sandboxTools.extraEnv`.
    myconfig.ai.sandboxTools.extraPackages = [ cfg.package ];

    # mysbx integration: add agent-browser to the sandbox tool closure
    myconfig.ai.mysbx = lib.mkIf config.myconfig.ai.mysbx.enable {
      extraTools = [ cfg.package ];
    };

  };
}
