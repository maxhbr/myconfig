# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared sandbox tooling — configured ONCE, applied to the mysbx sandbox.
#
# Packages and environment variables declared here are threaded into the
# agent sandbox this repo builds, so a tool like `playwright-cli` (which a
# skill's SKILL.md references but which no single feature "owns") can be
# made available inside the sandbox with one option instead of a per-tier
# edit:
#
#   * `mysbx` — `myconfig.ai.dev.mysbx` folds the packages into its
#     `extraTools` (the dev-tool closure on the sandbox `PATH`,
#     `toolsEnv` in `mysbx/nix/mysbx.nix`) and the env into the generated
#     user-layer `[env]` table (`mysbx/default.nix`; tier-baseline keys
#     like `RIPGREP_CONFIG_PATH` win over hook keys on a clash).
#
# Deliberately EMPTY by default *from the host's side*: the sandbox is
# minimal by design (headless, no GUI closures), so heavy tooling — a
# browser, for example — is opt-in per host:
#
#   myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [
#     chromium
#   ];
#   myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = "chromium";
#   myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_EXECUTABLE_PATH =
#     "\${pkgs.chromium}/bin/chromium";
#
# Feature modules may add to the list too, gated behind their own enable
# option, when their tool is wanted in the sandbox and belongs to no single
# one: ../../../modules/myconfig.ai.dev/programs/programs.hunk, ../../../modules/myconfig.ai.dev/programs/programs.agent-browser and
# ../../../modules/myconfig.ai.dev/skills/playwright-cli append their package this way, so the reviewing
# and browser-automation tools exist wherever an agent produces a changeset.
{ lib, ... }:
{
  options.myconfig.ai.dev.sandboxTools = with lib; {
    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = ''
        Extra packages added to the `mysbx` dev-tool closure (the sandbox
        `PATH`), in addition to the sandbox's own default toolset. Default:
        none from the host — the sandbox stays minimal; enabled feature
        modules (e.g. `myconfig.ai.dev.hunk`) may append their own tool.
      '';
    };

    extraEnv = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Extra environment variables set inside the `mysbx` sandbox, merged
        into the generated user-layer `[env]` table (tier-baseline keys
        like `RIPGREP_CONFIG_PATH` win over hook keys on a clash). Package
        references can be interpolated as usual, e.g.:

        ```nix
        myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = "chromium";
        myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_EXECUTABLE_PATH =
          "\${pkgs.chromium}/bin/chromium";
        ```
      '';
    };
  };
}
