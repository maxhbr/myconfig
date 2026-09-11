# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared sandbox tooling — configured ONCE, applied to EVERY sandbox tier.
#
# Packages and environment variables declared here are threaded into every
# agent sandbox this repo builds, so a tool like `playwright-cli` (which a
# skill's SKILL.md references but which no single tier "owns") can be made
# available everywhere with one option instead of one edit per tier:
#
#   * bubblewrap jails — `fns/bubblewrap-app.nix` reads this via the same
#     `osconfig` mechanism as `myconfig.ai.dev.jail.fwdEnvs` and appends the
#     packages to its `add-pkg-deps` permission (and sets the env via
#     `set-env`) for every `agent-bubblewrap-*` wrapper (`agent-bubblewrap-pi`, `agent-bubblewrap-claude`,
#     `agent-bubblewrap-opencode`, ...).
#   * `myconfig.ai.dev.microvm` guests — `myconfig.ai.microvm/guest.nix` appends
#     the packages to the guest's `environment.systemPackages` (§7) and the
#     env to `environment.variables`.
#   * `agent-qemu-pi` / `agent-qemu-workmux-tmux` / `agent-qemu-herdr` impure
#     runners — the host-side wrappers bake the package store paths into a
#     `SANDBOXED_*_EXTRA_PACKAGES` JSON env var (same pattern as
#     `AGENT_QEMU_HERDR_AGENT_PACKAGES`); the standalone runner expression
#     passes it through to `mkSandboxedRunner`
#     (`modules/myconfig.ai.dev/sandboxes/myconfig.ai.qemu-agent-sandbox/builders.nix`),
#     which folds it into the guest package set.
#   * gVisor sandbox image — `myconfig.ai.dev.gvisor-agent-sandbox` appends the
#     packages to its `extraImagePackages` default.
#   * `mysbx` — `myconfig.ai.dev.mysbx` folds the packages into its
#     `extraTools` (the dev-tool closure on the sandbox `PATH`,
#     `toolsEnv` in `mysbx/nix/mysbx.nix`) and the env into the generated
#     user-layer `[env]` table (`mysbx/default.nix`; tier-baseline keys
#     like `RIPGREP_CONFIG_PATH` win over hook keys on a clash).
#
# Deliberately EMPTY by default *from the host's side*: the sandbox tiers are
# minimal by design
# (headless, no GUI closures), so heavy tooling — a browser behind
# `playwright-cli`, for example — is opt-in per host:
#
#   myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [
#     playwright-cli
#     chromium
#   ];
#   myconfig.ai.dev.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = "chromium";
#
# Feature modules may add to the list too, gated behind their own enable
# option, when their tool is wanted in every tier and belongs to no single
# one: ../../programs/programs.hunk and ../../programs/programs.agent-browser
# append their package this way, so the reviewing tool exists wherever an
# agent produces a changeset.
{ lib, ... }:
{
  options.myconfig.ai.dev.sandboxTools = with lib; {
    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = ''
        Extra packages added to EVERY agent sandbox tier (bubblewrap `agent-bubblewrap-*`
        wrappers, `myconfig.ai.dev.microvm` guests, the `sandboxed-*` microVM
        runners, the gVisor sandbox image and the `mysbx` dev-tool closure), in
        addition to each tier's own default toolset. Default: none from the
        host — the sandboxes stay minimal; enabled feature modules (e.g.
        `myconfig.ai.dev.hunk`) may append their own tool.
      '';
    };

    extraEnv = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Extra environment variables set inside EVERY agent sandbox tier. In
        the bubblewrap jails these are applied unconditionally via `set-env`
        (not forwarded from the host); in the VM guests they land in the
        guest's `environment.variables`; in mysbx they are merged into the
        generated user-layer `[env]` table (tier-baseline keys like
        `RIPGREP_CONFIG_PATH` win over hook keys on a clash). Package
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
