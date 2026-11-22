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
      (a) a stdio server's package must ALSO go into
      `myconfig.ai.dev.mysbx.extraTools` — its `command` is a host
      store path, resolvable under the bubblewrap backend's read-only
      /nix but absent from the baked podman/VM images unless declared;
      (b) auth tokens reach the sandbox only through
      `forwardedEnvVars` (a `{env:…}` header reference resolves to an
      empty string otherwise);
      (c) a remote (`url`) server needs a network policy that permits
      it — `shared` (the default) works, `nono` needs matching
      `allowDomains`/`connectPorts`, and `proxy-only` cannot reach it
      at all.
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
