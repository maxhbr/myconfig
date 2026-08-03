# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — THE AUTHORITATIVE supported-agent registry.
#
# This file is the SINGLE SOURCE OF TRUTH for which coding agents a microVM
# sandbox supports. Everything agent-shaped in this module tree is derived
# from it; there must be no second, hand-maintained list anywhere:
#
#   guest.nix    — the guest closure's agent packages (§7) and the generated
#                  `agent-run` guest-side dispatch table (§19).
#   launcher.nix — `--agent` validation (`validate_agent_name`) and the
#                  `agent-microvm --help` output.
#   workmux.nix  — the `myconfig.ai.workmux.agents.microvm-*` registrations
#                  (§29) and their per-agent pane launchers.
#   default.nix  — assertions that every registry entry is well-formed.
#   tests/microvm.nix — the shellcheck-gate list of workmux launchers.
#
# Adding an agent is therefore a ONE-LINE change in `specs` below.
#
# Schema of a `specs.<name>` entry (name = the `--agent <name>` CLI token):
#
#   package          (required) host-built package baked into the immutable
#                    guest closure. NEVER installed at runtime (§8: no
#                    `pip`/`npm`/`curl | sh` inside the guest).
#   executable       binary name `agent-run` execs inside the guest, resolved
#                    from the guest PATH. Defaults to the registry key.
#   workmuxType      the workmux built-in agent `type` (prompt injection /
#                    resume flags). Defaults to the registry key.
#   interactiveArgs  extra argv prepended by `agent-run <name>` for the
#                    interactive `--attach` session. Defaults to `[ ]`.
#                    Deliberately NOT a place for dangerous auto-approve
#                    flags — those stay explicit per workmux agent (§19).
#
# Derived (never write these by hand): `workmuxName` = "microvm-<name>".
{ lib, pkgs }:
let
  # ---- the registry ----------------------------------------------------
  # Keys are the `--agent` CLI tokens. Attribute-set order is alphabetical in
  # Nix, so every generated list (guest packages, help text, validation) has a
  # stable, deterministic order.
  specs = {
    claude = {
      package = pkgs.claude-code;
      executable = "claude";
    };
    codex = {
      package = pkgs.codex;
      executable = "codex";
    };
    opencode = {
      package = pkgs.opencode;
      executable = "opencode";
    };
    pi = {
      package = pkgs.nixos-unstable.pi-coding-agent;
      executable = "pi";
    };
  };

  normalise =
    name: spec:
    {
      inherit name;
      executable = name;
      workmuxType = name;
      interactiveArgs = [ ];
    }
    // spec
    // {
      workmuxName = "microvm-${name}";
    };

  agents = lib.mapAttrs normalise specs;

  agentList = lib.attrValues agents;

  # ---- well-formedness (surfaced as NixOS assertions by default.nix) ----
  # A malformed entry would otherwise fail late and confusingly (a broken
  # guest closure, or a launcher that accepts an agent the guest cannot run).
  entryErrors =
    a:
    lib.optional (
      !(a ? package) || a.package == null
    ) "myconfig.ai.microvm: agent '${a.name}' has no package."
    ++ lib.optional (
      !lib.isString a.executable || a.executable == ""
    ) "myconfig.ai.microvm: agent '${a.name}' has an empty/non-string executable."
    ++ lib.optional (
      !lib.isString a.workmuxName || a.workmuxName == ""
    ) "myconfig.ai.microvm: agent '${a.name}' has an empty/non-string workmuxName."
    ++ lib.optional (
      !lib.isString a.workmuxType || a.workmuxType == ""
    ) "myconfig.ai.microvm: agent '${a.name}' has an empty/non-string workmuxType."
    ++ lib.optional (
      !lib.isList a.interactiveArgs
    ) "myconfig.ai.microvm: agent '${a.name}' interactiveArgs must be a list."
    ++
      lib.optional (builtins.match "[a-z][a-z0-9-]*" a.name == null)
        "myconfig.ai.microvm: agent name '${a.name}' must match [a-z][a-z0-9-]* (it crosses the host→guest control channel).";
in
rec {
  inherit agents;

  # Alphabetically ordered `--agent` tokens.
  names = lib.attrNames agents;

  # Guest closure packages, in registry order (§7).
  packages = map (a: a.package) agentList;

  # `claude|codex|opencode|pi` — used verbatim in launcher help/error text and
  # in the guest dispatch's `case` fallback message.
  namesAlternation = lib.concatStringsSep "|" names;

  # Every malformed-entry error message; empty list == registry is well-formed.
  errors = lib.concatMap entryErrors agentList;

  # ---- generated shell fragments ---------------------------------------
  # A bash `case` body dispatching `$1` (already shifted off) to the agent's
  # guest executable plus its interactiveArgs. Used by `agent-run` (guest) so
  # the guest, too, only knows the agents this registry declares.
  guestDispatchCases = lib.concatStringsSep "\n" (
    map (
      a:
      "    ${a.name}) exec ${
            lib.concatStringsSep " " (map lib.escapeShellArg ([ a.executable ] ++ a.interactiveArgs))
          } \"$@\" ;;"
    ) agentList
  );

  # A bash `case` pattern (`claude | codex | ...`) for host-side `--agent`
  # validation in launcher.nix.
  namesCasePattern = lib.concatStringsSep " | " names;
}
