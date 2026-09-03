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
#   * bubblewrap jails — `fns/jail-app.nix` reads this via the same
#     `osconfig` mechanism as `myconfig.ai.jail.fwdEnvs` and appends the
#     packages to its `add-pkg-deps` permission (and sets the env via
#     `set-env`) for every `agent-bubblewrap-*` wrapper (`agent-bubblewrap-pi`, `agent-bubblewrap-claude`,
#     `agent-bubblewrap-opencode`, ...).
#   * `myconfig.ai.microvm` guests — `myconfig.ai.microvm/guest.nix` appends
#     the packages to the guest's `environment.systemPackages` (§7) and the
#     env to `environment.variables`.
#   * `agent-qemu-pi` / `agent-qemu-workmux-tmux` / `agent-qemu-herdr` impure
#     runners — the host-side wrappers bake the package store paths into a
#     `SANDBOXED_*_EXTRA_PACKAGES` JSON env var (same pattern as
#     `AGENT_QEMU_HERDR_AGENT_PACKAGES`); the standalone runner expression
#     passes it through to `mkSandboxedRunner` (`modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix`),
#     which folds it into the guest package set.
#   * gVisor sandbox image — `myconfig.ai.gvisor-agent-sandbox` appends the
#     packages to its `extraImagePackages` default.
#
# Deliberately EMPTY by default: the sandbox tiers are minimal by design
# (headless, no GUI closures), so heavy tooling — a browser behind
# `playwright-cli`, for example — is opt-in per host:
#
#   myconfig.ai.sandboxTools.extraPackages = with pkgs; [
#     playwright-cli
#     chromium
#   ];
#   myconfig.ai.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = "chromium";
{ lib, ... }:
{
  options.myconfig.ai.sandboxTools = with lib; {
    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = ''
        Extra packages added to EVERY agent sandbox tier (bubblewrap `agent-bubblewrap-*`
        wrappers, `myconfig.ai.microvm` guests, the `sandboxed-*` microVM
        runners and the gVisor sandbox image), in addition to each tier's own
        default toolset. Default: none — the sandboxes stay minimal.
      '';
    };

    extraEnv = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Extra environment variables set inside EVERY agent sandbox tier. In
        the bubblewrap jails these are applied unconditionally via `set-env`
        (not forwarded from the host); in the VM guests they land in the
        guest's `environment.variables`. Package references can be
        interpolated as usual, e.g.:

        ```nix
        myconfig.ai.sandboxTools.extraEnv.PLAYWRIGHT_MCP_BROWSER = "chromium";
        myconfig.ai.sandboxTools.extraEnv.PLAYWRIGHT_MCP_EXECUTABLE_PATH =
          "\${pkgs.chromium}/bin/chromium";
        ```
      '';
    };
  };
}
