# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.mysbx.mcp — turn on home-manager's MCP registry
# (`programs.mcp`, the single source of truth seeded by
# ../programs/programs.mcp.servers.nix) for hosts that run mysbx
# sandboxes. The flag only seeds `programs.mcp.enable`: the registry
# alone changes nothing on the host — every consumer (opencode,
# claude-code, the sandbox mount of bd myconfig-9y4c) reads it opt-in.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.dev.mysbx.mcp;
in
{
  options.myconfig.ai.dev.mysbx.mcp = with lib; {
    enable = mkEnableOption ''
      the MCP server registry for mysbx sandboxes: sets
      `programs.mcp.enable = true` via `home-manager.sharedModules`,
      so the registry (bd myconfig-9y4c) exists even on hosts that
      install no opencode/claude-code. This module cannot enforce the
      three operational requirements a usable sandbox MCP setup needs:
      (a) a stdio server's package must ALSO be on the sandbox PATH
      PER BACKEND — under `myconfig.ai.dev.mysbx.extraTools` it rides
      the wrapper's tool closure, which the bubblewrap backend's
      read-only /nix and the VM backends' baked store resolve, but
      the PODMAN backends run the image's own `/bin`, so it must ALSO
      be baked there via `myconfig.ai.dev.mysbx.podman.imagePackages`
      (or, for tooling every tier should carry,
      `myconfig.ai.dev.sandboxTools.extraPackages`, which feeds both);
      (b) auth tokens reach the sandbox only through
      `forwardedEnvVars` (a `{env:…}` reference resolves to an
      empty string otherwise);
      (c) a remote (`url`) server needs a network policy that permits
      it — `shared` (the default) works; the allowlist keys
      `allowDomains`/`connectPorts` gate it on `nono` (with `shared`)
      and on the broker-channel backends (with `proxy-only`, the
      CONNECT-lane pairing, config.md D21); on `proxy-only` WITHOUT
      the broker pairing a url server is unreachable.
    '';
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        programs.mcp.enable = true;
      }
    ];
  };
}
