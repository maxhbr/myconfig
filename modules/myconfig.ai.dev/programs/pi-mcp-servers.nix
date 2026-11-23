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
# ../../mysbx flake lib (nix/sandbox-config.nix) (the mkSandboxConfig helper). Both
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
  # `${NAME}` (upstream docs/mcp.md). Every valid occurrence is
  # translated, embedded ones included: the common real-world shape
  # is `Bearer {env:TOKEN}`, whose braces would reach pi unchanged and
  # leave the header literal (forward-env cannot help).
  #
  # The LITERAL segments are escaped for pi's resolver
  # (dist/core/resolve-config-value.js): it reads `$NAME` as an
  # environment lookup, `${NAME}` likewise, and a LEADING `!` as a
  # shell command — so a literal `price$USD` header fails the
  # connection when USD is unset, and a literal `!foo` value would
  # EXECUTE `foo` in a shell. The resolver documents `$$` as the
  # escape for a literal `$` and `$!` for a literal `!` (in
  # non-command values). So: every `$` of a literal segment becomes
  # `$$`, and a literal `!` at the value start becomes `$!`. With that,
  # every hm string has an exact pi representation — nothing needs to
  # be rejected for shape.
  rewritePlaceholder =
    value:
    let
      # builtins.split yields alternating plain segments and, per
      # match, a list of the capture groups — here the single NAME.
      parts = builtins.split "[{]env:([A-Za-z_][A-Za-z0-9_]*)[}]" value;
      escapeLiteral =
        seg:
        let
          dollarsDoubled = builtins.replaceStrings [ "$" ] [ "$$" ] seg;
          escaped =
            if builtins.substring 0 1 dollarsDoubled == "!" then
              "$!" + builtins.substring 1 (-1) dollarsDoubled
            else
              dollarsDoubled;
        in
        escaped;
      render = part: if builtins.isList part then "\${${builtins.elemAt part 0}}" else escapeLiteral part;
    in
    lib.concatStrings (map render parts);

  # A file ref (`env.<name>.file`) cannot ride a read-only store tree:
  # the rendered JSON names a host path the sandbox cannot read (and
  # home-manager's own wrapper trick reads the file at STARTUP on the
  # host, which never happens for a sandboxed pi). Refuse loudly
  # instead of shipping a server whose secret silently resolves empty.
  # Runs AFTER enablement resolution (below): a DISABLED server never
  # renders, so its file refs must not block the servers that do.
  assertNoFileRefs =
    name: server:
    let
      fileRefs = lib.filterAttrs (_: lib.isAttrs) (server.env or { });
    in
    if fileRefs != { } then
      throw "programs.mcp.servers.${name}: pi's sandboxed mcp.json cannot carry env.<name>.file references (${lib.concatStringsSep ", " (lib.attrNames fileRefs)}) — inline the secret via forwardedEnvVars and {env:NAME} placeholders instead"
    else
      server;

  # pi's complete server field set (dist/core/mcp-servers.d.ts plus
  # pi-mcp's config.js validate*Fields): everything a rendered entry
  # may carry. Anything else a server sets is either home-manager's
  # own (`disabled`, `serverUrl` — consumed by the resolver below) or
  # unknown to pi, which would silently run with pi defaults — so it
  # throws instead.
  piServerFields = [
    "command"
    "args"
    "env"
    "cwd"
    "url"
    "headers"
    "oauth"
    "type"
    "auth"
    "description"
    "timeout"
    "exposure"
    "toolExposure"
  ];

  assertNoUnknownFields =
    name: server:
    let
      known = piServerFields ++ [
        "enabled"
        "disabled"
        "serverUrl"
      ];
      unknown = lib.subtractLists known (lib.attrNames server);
    in
    if unknown != [ ] then
      throw "programs.mcp.servers.${name}: pi does not support these fields: ${lib.concatStringsSep ", " unknown} — the sandboxed mcp.json renders only pi's server fields (${lib.concatStringsSep ", " piServerFields})"
    else
      server;

  renderServer =
    name: server:
    let
      # `enabled`/`disabled` resolution mirrors home-manager's own
      # `resolveEnabled` (modules/lib/mcp.nix): explicit `enabled` wins,
      # then `!disabled`, else the server is on. A resolved-off server
      # is OMITTED entirely, not rendered with `enabled: false`: pi
      # would still carry the entry (and its secret-adjacent config)
      # in the sandbox file for nothing.
      isEnabled =
        if server ? enabled && server.enabled != null then
          server.enabled
        else if server ? disabled && server.disabled != null then
          !server.disabled
        else
          true;
    in
    if !isEnabled then
      null
    else
      let
        s = assertNoFileRefs name (assertNoUnknownFields name server);
      in
      lib.filterAttrs (_: v: v != null && v != [ ] && v != { }) (
        (lib.optionalAttrs (s ? command) { inherit (s) command; })
        // lib.optionalAttrs (s ? args) { inherit (s) args; }
        // lib.optionalAttrs (s ? env) {
          env = lib.mapAttrs (_: rewritePlaceholder) s.env;
        }
        // lib.optionalAttrs (s ? cwd) { inherit (s) cwd; }
        // lib.optionalAttrs (s ? url) { inherit (s) url; }
        // lib.optionalAttrs (s ? headers) {
          headers = lib.mapAttrs (_: rewritePlaceholder) s.headers;
        }
        # `oauth.clientSecret` is the one oauth field pi resolves
        # through the same resolver as env/header values
        # (runtime.js `oauthSettings`: clientSecret goes through
        # `resolveConfigValueOrThrow`, every other oauth field is
        # copied raw) — so it gets the same placeholder translation
        # and literal escaping. The other fields stay verbatim.
        // lib.optionalAttrs (s ? oauth) {
          oauth =
            if s.oauth ? clientSecret then
              s.oauth // { clientSecret = rewritePlaceholder s.oauth.clientSecret; }
            else
              s.oauth;
        }
        // lib.optionalAttrs (s ? type) { inherit (s) type; }
        // lib.optionalAttrs (s ? auth) { inherit (s) auth; }
        // lib.optionalAttrs (s ? description) { inherit (s) description; }
        // lib.optionalAttrs (s ? timeout) { inherit (s) timeout; }
        # pi's tool-exposure fields, passed through verbatim: pi
        # defaults a missing `exposure` to `codemode`, so dropping a
        # server set `hidden` here would silently make its tools
        # callable in the sandbox.
        // lib.optionalAttrs (s ? exposure) { inherit (s) exposure; }
        // lib.optionalAttrs (s ? toolExposure) { inherit (s) toolExposure; }
      );
  renderedServers = lib.filterAttrs (_: v: v != null) (lib.mapAttrs renderServer mcpServers);
in
{
  # The mount gate AND the extraFiles key share this one value: a
  # caller passes it to `mkSandboxConfig` both as the subtree in
  # `subtrees` and as the key in `extraFiles`, so the two can never
  # drift even by a typo.
  path = ".pi/agent/mcp.json";

  # Off when nothing renders: mounting an empty `mcpServers` object
  # buys nothing and costs a store tree.
  enable = renderedServers != { };

  json = jsonFormat.generate "pi-mcp-servers" {
    mcpServers = renderedServers;
  };
}
