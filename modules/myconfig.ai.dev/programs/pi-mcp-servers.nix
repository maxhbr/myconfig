# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The pi shape of home-manager's MCP registry: rendered once, here, so
# "the mount exists" and "the mounted content exists" can never drift.
# A drift between the two consumers would make `mkSandboxConfig`
# materialize an EMPTY DIRECTORY named `mcp.json` under
# `.pi/agent/` — a file-name bind whose source is a directory, which
# pi then fails to parse (pi would see a directory where it expects
# JSON).
#
# A plain function file, no module machinery — the same pattern as
# ./pi-sandbox-subtrees.nix (the subtrees list) and
# ../mysbx/nix/sandbox-config.nix (the mkSandboxConfig helper). Both
# consumers import it directly:
#
#   * programs.pi-coding-agent/default.nix — the extraFiles entry of its
#     mkSandboxConfig call;
#   * programs.thinkrail.nix — same, for its own sandbox tree.
#
# Reads the registry of THE user the sandbox tree renders for
# (home-manager keys it per user; both consumers render for `mhuber`).
# Does not read `programs.mcp.enable`: a caller gates its mount on
# this file's `enable` (the content gate), NOT on the
# `myconfig.ai.dev.mysbx.mcp` option — that option only seeds
# `programs.mcp.enable` so hosts without opencode/claude-code still
# build the registry.
{
  config,
  lib,
  pkgs,
}:
let
  mcpServers = config.home-manager.users.mhuber.programs.mcp.servers;

  jsonFormat = pkgs.formats.json { };

  # home-manager's shared placeholder spelling is `{env:NAME}` (see
  # modules/programs/mcp.nix of the home-manager input), pi's is
  # `${NAME}` (upstream docs/mcp.md). Anchored: the match must span the
  # WHOLE value, so a placeholder embedded in a longer string is left
  # alone rather than silently mangled by a blanket `}` rewrite.
  rewritePlaceholder =
    value:
    let
      m = builtins.match "^[{]env:([A-Za-z_][A-Za-z0-9_]*)[}]$" value;
    in
    if m == null then value else "\${${builtins.head m}}";

  # A file ref (`env.<name>.file`) cannot ride a read-only store tree:
  # the rendered JSON names a host path the sandbox cannot read (and
  # home-manager's own wrapper trick reads the file at STARTUP on the
  # host, which never happens for a sandboxed pi). Refuse loudly
  # instead of shipping a server whose secret silently resolves empty.
  assertNoFileRefs =
    name: server:
    let
      fileRefs = lib.filterAttrs (_: lib.isAttrs) (server.env or { });
    in
    if fileRefs != { } then
      throw "programs.mcp.servers.${name}: pi's sandboxed mcp.json cannot carry env.<name>.file references (${lib.concatStringsSep ", " (lib.attrNames fileRefs)}) — inline the secret via forwardedEnvVars and \${NAME} placeholders instead"
    else
      server;

  renderServer =
    name: server:
    let
      s = assertNoFileRefs name server;
    in
    # `enabled`/`disabled` resolution mirrors home-manager's own
    # `resolveEnabled` (modules/lib/mcp.nix): explicit `enabled` wins,
    # then `!disabled`, else the server is on.
    lib.filterAttrs (_: v: v != null && v != [ ] && v != { }) (
      (lib.optionalAttrs (s ? command) { inherit (s) command; })
      // lib.optionalAttrs (s ? args) { inherit (s) args; }
      // lib.optionalAttrs (s ? url) { inherit (s) url; }
      // lib.optionalAttrs (s ? headers) {
        headers = lib.mapAttrs (_: rewritePlaceholder) s.headers;
      }
      // lib.optionalAttrs (s ? env) {
        env = lib.mapAttrs (_: rewritePlaceholder) s.env;
      }
      // lib.optionalAttrs (s ? description) { inherit (s) description; }
      // lib.optionalAttrs (s ? timeout) { inherit (s) timeout; }
      // {
        enabled =
          if s ? enabled && s.enabled != null then
            s.enabled
          else if s ? disabled && s.disabled != null then
            !s.disabled
          else
            true;
      }
    );
in
{
  # The mount gate AND the extraFiles key share this one value: a
  # caller passes it to `mkSandboxConfig` both as the subtree in
  # `subtrees` and as the key in `extraFiles`, so the two can never
  # drift even by a typo.
  path = ".pi/agent/mcp.json";

  # Off when the registry is empty: mounting an empty `mcpServers`
  # object buys nothing and costs a store tree.
  enable = mcpServers != { };

  json = jsonFormat.generate "pi-mcp-servers" {
    mcpServers = lib.mapAttrs renderServer mcpServers;
  };
}
