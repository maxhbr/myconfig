# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — THE AUTHORITATIVE supported-agent registry.
#
# This file is the SINGLE SOURCE OF TRUTH for which coding agents a microVM
# sandbox supports. Everything agent-shaped in this module tree is derived
# from it; there must be no second, hand-maintained list anywhere:
#
#   guest.nix    — the guest closure's agent packages (§7), the per-agent
#                  guest environment, the generated `agent-run` guest-side
#                  dispatch table (§19) and the generated BATCH dispatch table
#                  of the untrusted `agent-job-worker` (ticket 4/7).
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
#                    resume flags). Defaults to the registry key. An unknown
#                    type is NOT an error — workmux falls back to its default
#                    profile (no prompt injection / resume flags).
#   interactiveArgs  extra argv prepended by `agent-run <name>` for the
#                    interactive `--attach` session. Defaults to `[ ]`.
#                    Deliberately NOT a place for dangerous auto-approve
#                    flags — those stay explicit per workmux agent (§19).
#   batchArgs        argv for UNATTENDED batch execution (`agent-job-worker`,
#                    ticket 4/7), VERIFIED against the pinned build's `--help`. The
#                    token `%PROMPT%` is replaced with the prompt TEXT read
#                    from the job directory. `null` (the default) means the
#                    agent cannot run unattended, and `submit --agent <name>`
#                    is rejected for it.
#   batchStdin       when true, the worker pipes the prompt FILE on stdin
#                    instead of substituting `%PROMPT%` (for CLIs that read
#                    instructions from stdin). Defaults to `false`.
#   configPaths      the agent's ALLOWLIST of host configuration paths, relative
#                    to `myconfig.ai.microvm.configSeed.hostHome`, staged into
#                    the disposable guest home at LAUNCH time (lightweight plan
#                    phase 3, see ./config-seed.nix). EXACT files and EXACT
#                    directories only — never a whole agent configuration root,
#                    because those mix configuration with CREDENTIALS
#                    (`.codex/auth.json`, `.hermes/.env`, …). Model-provider
#                    credentials stay in the host LiteLLM proxy and are never
#                    staged; a credential-shaped entry is rejected at
#                    evaluation time by the denylist in ./config-seed.nix.
#                    Missing paths are simply absent in the guest, so an entry
#                    may name something this host does not have. Defaults to
#                    `[ ]` (the agent gets only the module-wide
#                    `configSeed.extraPaths`).
#   extraPackages    additional guest packages this agent NEEDS at runtime
#                    (e.g. a language runtime it shells out to). Added to the
#                    guest closure only while the agent is SELECTED
#                    (`enabledAgents`), so a deselected agent takes its
#                    dependencies with it. Defaults to `[ ]` — most agents wrap
#                    their own PATH.
#   guestEnvironment attrs merged into the guest's `environment.variables`.
#                    Model-endpoint plumbing ONLY — never a real credential
#                    (§17: the upstream key lives only in the host LiteLLM
#                    proxy). Defaults to `{ }`.
#   persistentState  the agent's declared, VERIFIED state paths, relative to
#                    the guest `agent` home. `enabledByDefault = false` keeps
#                    the guest home DISPOSABLE (the secure default); ticket 5
#                    turns `directories` into opt-in, task-scoped mounts.
#                    Never GUESS these paths — read the agent's source.
#
# Derived (never write these by hand): `workmuxName` = "microvm-<name>".
#
# The registry is instantiated EXACTLY ONCE (default.nix) and handed to the
# other modules via `_module.args.agentRegistry`, so every consumer shares the
# same instance and the same context arguments below.
{
  lib,
  pkgs,
  inputs,
  # Guest-visible loopback LiteLLM port (`myconfig.ai.microvm.litellmPort`).
  # The guest-side socket proxy forwards 127.0.0.1:<port> to the host bridge
  # endpoint, which is the ONLY model-API peer a guest can reach (§17).
  litellmPort,
  # Model name for agents that cannot discover one themselves
  # (`config.myconfig.ai.hermes.model.default`, a LiteLLM route).
  hermesModel,
  # SELECTED agents (lightweight plan phase 2), resolved ONCE in default.nix
  # from `myconfig.ai.microvm.enabledAgents` and the profile's own default.
  # `null` means "every agent this registry declares" — the historical
  # behaviour. Everything agent-shaped downstream (guest closure + guest env,
  # `agent-run` dispatch, batch dispatch, launcher validation/help, workmux
  # registrations, persistent-state directories) is derived from the FILTERED
  # `agents` set below, so selecting a subset removes those runtimes from the
  # guest instead of merely hiding them.
  enabledNames ? null,
}:
let
  # The single OpenAI-compatible endpoint every guest agent must use (§17).
  litellmBaseUrl = "http://127.0.0.1:${toString litellmPort}/v1";

  # Model selection for hermes, identical in interactive and batch mode.
  hermesModelArgs = [
    "--model"
    hermesModel
  ];

  # ---- the registry ----------------------------------------------------
  # Keys are the `--agent` CLI tokens. Attribute-set order is alphabetical in
  # Nix, so every generated list (guest packages, help text, validation) has a
  # stable, deterministic order.
  specs = {
    # Batch invocations below are taken from each pinned build's own `--help`:
    #   claude   "-p, --print   Print response and exit (useful for pipes)"
    #   codex    "codex exec [PROMPT] … if `-` is used, instructions are read
    #             from stdin"
    #   opencode "opencode run [message..]"
    #   pi       "--print, -p   Non-interactive mode: process prompt and exit"
    #   hermes   "-z/--oneshot PROMPT   One-shot mode: send a single prompt and
    #             print ONLY the final response text"
    claude = {
      package = pkgs.claude-code;
      executable = "claude";
      batchArgs = [
        "-p"
        "%PROMPT%"
      ];
      # `~/.claude` also holds `.credentials.json`, so only the two paths the
      # host home-manager config actually RENDERS are allowlisted.
      configPaths = [
        ".agents/skills"
        ".claude/settings.json"
        ".claude/skills"
      ];
    };
    codex = {
      package = pkgs.codex;
      executable = "codex";
      # `codex exec -` reads the instructions from stdin, so the prompt file
      # never becomes an argv element.
      batchArgs = [
        "exec"
        "-"
      ];
      batchStdin = true;
      # Codex keeps its OAuth/API credential in `~/.codex/auth.json` (and its
      # session transcripts in `~/.codex/sessions`), so the DIRECTORY is never
      # staged — only the rendered configuration and the skills tree.
      configPaths = [
        ".agents/skills"
        ".codex/config.toml"
        ".codex/hooks.json"
        ".codex/skills"
      ];
    };
    opencode = {
      package = pkgs.opencode;
      executable = "opencode";
      batchArgs = [
        "run"
        "%PROMPT%"
      ];
      # `~/.config/opencode` also collects `auth.json` and host-coupled
      # plugins, so only the rendered configuration, agents/commands and the
      # skills tree are staged. The LIVE model list is provisioned separately,
      # at guest boot, by ./guest-model-config.nix.
      configPaths = [
        ".agents/skills"
        ".config/opencode/agents"
        ".config/opencode/commands"
        ".config/opencode/opencode.json"
        ".config/opencode/skills"
        ".config/opencode/tui.json"
      ];
    };
    pi = {
      package = pkgs.nixos-unstable.pi-coding-agent;
      executable = "pi";
      batchArgs = [
        "--print"
        "%PROMPT%"
      ];
      # `~/.pi` also holds session state and provider credentials, so only the
      # rendered agent configuration is staged.
      configPaths = [
        ".agents/skills"
        ".pi/agent/agents"
        ".pi/agent/extensions"
        ".pi/agent/keybindings.json"
        ".pi/agent/prompts"
        ".pi/agent/themes"
      ];
    };
    # Hermes Agent (NousResearch). The SAME flake input + package attr the
    # host `myconfig.ai.hermes` backends use, so the guest runs the identical
    # build — baked into the immutable guest closure, never installed at
    # runtime through the upstream `curl | bash` installer, pip or npm (§8).
    # The package wraps its own PATH with nodejs/ripgrep/git, so it needs no
    # extra guest packages.
    hermes = {
      package = inputs.hermes-agent.packages.${pkgs.stdenv.hostPlatform.system}.default;
      executable = "hermes";
      # workmux knows no `hermes` profile, so it resolves to workmux's DEFAULT
      # profile (no prompt injection / resume flags). Not an error — the pane
      # still launches (see workmux `resolve_profile_with_type`).
      workmuxType = "hermes";
      # Hermes has no model auto-discovery: with no provider configured it
      # drops into `hermes setup`. Pin the LiteLLM route explicitly (the same
      # model name the host `myconfig.ai.hermes` backends use) in BOTH modes.
      interactiveArgs = hermesModelArgs;
      batchArgs = hermesModelArgs ++ [
        "--oneshot"
        "%PROMPT%"
      ];
      # Hermes resolves its endpoint via config.yaml base_url →
      # CUSTOM_BASE_URL → OPENROUTER_BASE_URL → openrouter.ai
      # (hermes_cli/runtime_provider.py). Point it at the guest loopback
      # LiteLLM endpoint. For a non-openrouter base_url it picks up
      # OPENAI_API_KEY, which guest.nix already sets to a placeholder; that
      # also satisfies hermes' first-run "any provider configured?" guard, so
      # no setup wizard appears on the disposable guest home.
      guestEnvironment = {
        OPENROUTER_BASE_URL = litellmBaseUrl;
      };
      # VERIFIED against the pinned hermes source (`hermes_constants.py`
      # `get_hermes_home()`, default `~/.hermes`, overridable via
      # `HERMES_HOME`): config.yaml, .env, auth.json, state.db, sessions/,
      # memories/, skills/, logs/, plugins/, cron/, scripts/ all live under
      # that ONE root. Disposable by default — a fresh sandbox therefore
      # starts with no memories, skills or sessions (see ticket 5 for the
      # opt-in, task-scoped persistence that consumes this declaration).
      persistentState = {
        enabledByDefault = false;
        directories = [ ".hermes" ];
      };
      # DELIBERATELY EMPTY: hermes keeps config.yaml, `.env`, `auth.json`,
      # state.db and sessions/ in ONE root (`~/.hermes`), i.e. configuration is
      # inseparable from credentials there, and even `config.yaml` carries a
      # provider key. Nothing of it may be staged; the guest gets its endpoint
      # from `guestEnvironment` above instead. Only the module-wide
      # `configSeed.extraPaths` reach a hermes guest.
      configPaths = [ ];
    };
  };

  normalise =
    name: spec:
    {
      inherit name;
      executable = name;
      workmuxType = name;
      interactiveArgs = [ ];
      batchArgs = null;
      batchStdin = false;
      configPaths = [ ];
      extraPackages = [ ];
      guestEnvironment = { };
      persistentState = {
        enabledByDefault = false;
        directories = [ ];
      };
    }
    // spec
    // {
      workmuxName = "microvm-${name}";
    };

  allAgents = lib.mapAttrs normalise specs;

  # The selection is applied HERE, at the single source of truth, rather than at
  # each consumer — so a disabled agent cannot leak into one derived list while
  # being absent from another.
  agents =
    if enabledNames == null then
      allAgents
    else
      lib.filterAttrs (n: _: lib.elem n enabledNames) allAgents;

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
    ++ lib.optional (
      a.batchArgs != null && !lib.isList a.batchArgs
    ) "myconfig.ai.microvm: agent '${a.name}' batchArgs must be a list or null."
    ++ lib.optional (
      a.batchArgs != null && !lib.all lib.isString a.batchArgs
    ) "myconfig.ai.microvm: agent '${a.name}' batchArgs must contain only strings."
    ++ lib.optional (
      a.batchStdin && a.batchArgs == null
    ) "myconfig.ai.microvm: agent '${a.name}' sets batchStdin but has no batchArgs."
    ++ lib.optional (
      a.batchStdin && lib.elem "%PROMPT%" (a.batchArgs or [ ])
    ) "myconfig.ai.microvm: agent '${a.name}' cannot combine batchStdin with the %PROMPT% placeholder."
    ++ lib.optional (
      a.batchArgs != null && !a.batchStdin && !lib.elem "%PROMPT%" a.batchArgs
    ) "myconfig.ai.microvm: agent '${a.name}' batchArgs must contain %PROMPT% (or set batchStdin)."
    ++ lib.optional (
      !lib.isList a.configPaths
    ) "myconfig.ai.microvm: agent '${a.name}' configPaths must be a list."
    ++
      lib.optional
        (
          !lib.all (
            p:
            lib.isString p
            && p != ""
            && !lib.hasPrefix "/" p
            && !lib.hasPrefix "-" p
            && !lib.hasSuffix "/" p
            && !lib.hasInfix ".." p
          ) a.configPaths
        )
        "myconfig.ai.microvm: agent '${a.name}' configPaths must be non-empty, relative, '..'-free paths that neither start with '-'/'/' nor end with '/' (they are joined onto the host home and re-checked by ./config-seed.nix, which also applies the credential denylist)."
    ++ lib.optional (
      !lib.isList a.extraPackages
    ) "myconfig.ai.microvm: agent '${a.name}' extraPackages must be a list."
    ++ lib.optional (
      !lib.isAttrs a.guestEnvironment
    ) "myconfig.ai.microvm: agent '${a.name}' guestEnvironment must be an attrset."
    ++ lib.optional (
      !lib.all lib.isString (lib.attrValues a.guestEnvironment)
    ) "myconfig.ai.microvm: agent '${a.name}' guestEnvironment values must be strings."
    ++ lib.optional (
      !lib.isBool a.persistentState.enabledByDefault
    ) "myconfig.ai.microvm: agent '${a.name}' persistentState.enabledByDefault must be a bool."
    ++ lib.optional (
      !lib.isList a.persistentState.directories
    ) "myconfig.ai.microvm: agent '${a.name}' persistentState.directories must be a list."
    ++
      lib.optional
        (
          !lib.all (
            d: lib.isString d && d != "" && !lib.hasPrefix "/" d && !lib.hasInfix ".." d
          ) a.persistentState.directories
        )
        "myconfig.ai.microvm: agent '${a.name}' persistentState.directories must be non-empty, relative, '..'-free paths."
    ++
      lib.optional (builtins.match "[a-z][a-z0-9-]{0,32}" a.name == null)
        "myconfig.ai.microvm: agent name '${a.name}' must match [a-z][a-z0-9-]{0,32} (it crosses the host→guest control channel and is re-validated by the batch result verifier, which bounds it to 33 characters).";
in
rec {
  inherit agents;

  # Every agent this registry DECLARES, whether selected or not. Used by
  # default.nix to reject an unknown `enabledAgents` entry with a message that
  # can name the valid tokens.
  declaredNames = lib.attrNames allAgents;

  # ... and the batch-capable subset of them, for the same reason.
  declaredBatchNames = lib.attrNames (lib.filterAttrs (_: a: a.batchArgs != null) allAgents);

  # `enabledAgents` entries that no spec declares (empty when the selection is
  # valid). Surfaced as a NixOS assertion by default.nix — a typo must fail at
  # EVAL, not silently produce a guest without any agent.
  unknownEnabled =
    if enabledNames == null then [ ] else lib.filter (n: !(allAgents ? ${n})) enabledNames;

  # Alphabetically ordered `--agent` tokens of the SELECTED agents.
  names = lib.attrNames agents;

  # Guest closure packages, in registry order (§7).
  packages = map (a: a.package) agentList;

  # Per-agent extra runtime packages of the SELECTED agents, deduplicated
  # (two agents may need the same runtime).
  extraPackages = lib.unique (lib.concatMap (a: a.extraPackages) agentList);

  # The union of the SELECTED agents' configuration ALLOWLISTS (lightweight plan
  # phase 3), sorted + deduplicated so the generated stager is stable. Because
  # it is derived from the FILTERED `agents` set, the staged configuration
  # follows `enabledAgents`: a deselected agent's config never reaches a guest.
  # ./config-seed.nix validates this union (shape + credential denylist) and
  # renders it into the host-side stager.
  configPaths = lib.unique (lib.sort (a: b: a < b) (lib.concatMap (a: a.configPaths) agentList));

  # `claude|codex|opencode|pi` — used verbatim in launcher help/error text and
  # in the guest dispatch's `case` fallback message.
  namesAlternation = lib.concatStringsSep "|" names;

  # Merged per-agent guest environment (§17 endpoint plumbing only, never a
  # credential). Two agents may declare the SAME key only with the same value;
  # a conflicting declaration is a registry error, not a silent last-one-wins
  # merge.
  guestEnvironment = lib.foldl' (acc: a: acc // a.guestEnvironment) { } agentList;

  guestEnvironmentConflicts = lib.concatMap (
    a:
    lib.mapAttrsToList (
      k: _:
      "myconfig.ai.microvm: agent '${a.name}' guestEnvironment.${k} conflicts with another agent's value."
    ) (lib.filterAttrs (k: v: guestEnvironment.${k} != v) a.guestEnvironment)
  ) agentList;

  # Every malformed-entry error message; empty list == registry is well-formed.
  errors = lib.concatMap entryErrors agentList ++ guestEnvironmentConflicts;

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

  # ---- batch execution (ticket 4) --------------------------------------
  batchAgents = lib.filter (a: a.batchArgs != null) agentList;

  # `--agent` tokens usable with `agent-microvm submit`.
  batchNames = map (a: a.name) batchAgents;
  batchNamesAlternation = lib.concatStringsSep "|" batchNames;
  batchNamesCasePattern = lib.concatStringsSep " | " batchNames;

  # Does ANY selected batch agent take the prompt TEXT as an argv token
  # (`%PROMPT%`), as opposed to reading the prompt FILE from stdin?
  #
  # This is a property of the FILTERED registry, so it differs per host: a host
  # whose `enabledAgents` selects only stdin-driven agents (e.g.
  # `[ "codex" ]`) generates a dispatch that never reads the worker's `prompt`
  # variable.
  # ../myconfig.ai.microvm/job.nix needs to know, because `writeShellApplication`
  # runs shellcheck and an unread variable is an SC2034 BUILD failure there.
  batchUsesPromptText = lib.any (
    a: lib.elem "%PROMPT%" ([ a.executable ] ++ a.batchArgs)
  ) batchAgents;

  # A bash `case` body for the UNTRUSTED guest batch worker
  # (`agent-job-worker`). Each arm calls one of its two helpers:
  #   run_agent        argv…   (prompt substituted for %PROMPT%)
  #   run_agent_stdin  argv…   (prompt FILE piped on stdin)
  # Neither wraps the invocation in a timeout: the deadline belongs to the
  # TRUSTED controller (which stops the worker's whole cgroup) plus the worker
  # unit's own static `TimeoutStartSec` ceiling — a limit enforced by the
  # untrusted worker itself would be worthless as evidence.
  # The prompt therefore never appears in a HOST command line, and the guest
  # can only ever run an executable this registry declares.
  batchDispatchCases = lib.concatStringsSep "\n" (
    map (
      a:
      let
        runner = if a.batchStdin then "run_agent_stdin" else "run_agent";
        argv = [ a.executable ] ++ a.batchArgs;
        render = tok: if tok == "%PROMPT%" then "\"$prompt\"" else lib.escapeShellArg tok;
      in
      "    ${a.name}) ${runner} ${lib.concatMapStringsSep " " render argv} ;;"
    ) batchAgents
  );
}
