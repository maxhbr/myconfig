# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  jail,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };
  callJailLib =
    file:
    import file {
      inherit
        lib
        pkgs
        jail
        osconfig
        ;
    };
  jail-app = callJailLib ../fns/jail-app.nix;

  # Jail library handle + the jail-to-host channel combinator. The combinator
  # exposes a program *inside* the jail that forwards its single argument over
  # a FIFO to a handler running *outside* the jail. We use it so the jailed pi
  # can report its workmux status to the tmux server (which lives in the
  # worktree pane's un-jailed environment where $TMUX/$TMUX_PANE and the tmux
  # socket are valid) without exposing any tmux socket or $TMUX to the jail.
  jailLib = jail.init pkgs;
  inherit (jailLib.combinators) jail-to-host-channel;

  # Make the `workmux` binary available *inside* the agent sandboxes (jail and
  # bubblewrap) whenever workmux is enabled. The status-tracking hooks/
  # extensions installed by `myconfig.ai.workmux` all shell out to
  # `workmux set-window-status`, and un-jailed/plain worktree agents may run
  # `workmux merge` / `workmux remove --keep-branch` from their pane, so the
  # binary must resolve on PATH inside the sandbox (empty list on hosts without
  # workmux). NOTE: the *jailed* worktree variant
  # (`jailed-pi-worktree-inner`) does NOT carry the real binary; it installs
  # `workmuxStatusShim` instead, which can only route `set-window-status` (see
  # the shim below). `workmux merge`/`remove` from inside that jail cannot
  # reach the tmux socket and will fail.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.workmux.enable osconfig.myconfig.ai.workmux.package;

  # A jail-to-host channel exposing `workmux_status_channel` *inside* the jail.
  # Calling `workmux_status_channel <status>` sends `<status>` over a FIFO to
  # the handler below, which runs *outside* the jail in the worktree tmux
  # pane's environment. There, $TMUX/$TMUX_PANE and the tmux socket are valid,
  # so `workmux set-window-status` can update the pane's tmux window name.
  #
  # LOAD-BEARING: the handler runs *outside* the jail as a background process
  # forked by the launcher pane, and relies on inheriting the launcher pane's
  # $TMUX / $TMUX_PANE (and thus the tmux socket). This inheritance is correct
  # because the launcher execs the inner jail wrapper in-pane and the jail's
  # non-empty `cleanup` means the wrapper does not `exec bwrap` (so the
  # background handler survives). $TMUX is never leaked *into* the jail
  # (bwrap --clearenv).
  #
  # The channel name must be a valid POSIX identifier (the combinator asserts
  # `isValidPosixName`), hence underscores rather than hyphens. The handler is
  # wrapped in a `writeShellApplication` with no runtimeInputs, so it sets its
  # own PATH to make `workmux` and `tmux` resolvable. Gated on workmux being
  # enabled (empty list otherwise).
  #
  # Errors are logged (rather than fully swallowed) to $TMPDIR/-/tmp so that a
  # misconfigured channel leaves a diagnosable trail instead of failing
  # silently (the pi extension also `.catch()`es the in-jail call).
  workmuxStatusChannelPerms = lib.optional osconfig.myconfig.ai.workmux.enable (
    jail-to-host-channel "workmux_status_channel" ''
      export PATH=${
        lib.makeBinPath [
          osconfig.myconfig.ai.workmux.package
          pkgs.tmux
        ]
      }:$PATH
      workmux set-window-status "$1" >>"''${TMPDIR:-/tmp}/workmux_status_channel.log" 2>&1 || true
    ''
  );

  # A `workmux` PATH-shim installed *inside* the worktree jail in place of the
  # real workmux binary. It intercepts `set-window-status` and routes it
  # through the `workmux_status_channel` program (exposed by the channel
  # above), so status updates reach the host tmux server without the jail
  # needing a tmux socket. Any other subcommand is forwarded to the real
  # workmux binary by absolute store path. Installing the shim instead of the
  # real `workmux` avoids a PATH name collision on `workmux`.
  #
  # NOTE: only `set-window-status` is rescued. Other subcommands (`workmux
  # merge`, `workmux remove`, ...) exec the real binary, which still needs the
  # tmux socket that is not present inside the jail, so they fail. This matches
  # the pre-change behaviour (the inner jail previously carried the real binary,
  # equally unable to reach tmux from inside the sandbox).
  workmuxStatusShim = pkgs.writeShellApplication {
    name = "workmux";
    text = ''
      if [ "''${1-}" = "set-window-status" ]; then
        shift
        exec workmux_status_channel "''${1-}"
      fi
      exec ${lib.getExe osconfig.myconfig.ai.workmux.package} "$@"
    '';
  };

  # Build a lookup: model name (raw or provider-prefixed) -> contextWindow.
  # Covers both direct local-provider lookups (raw name) and
  # LiteLLM lookups (providerName:modelName).
  contextWindowLookup = lib.listToAttrs (
    lib.concatMap (
      provider:
      let
        hostPort = "${provider.host}:${toString provider.port}";
        providerName = if provider.name != null then provider.name else hostPort;
        rawModels = if provider.models != [ ] then provider.models else [ ];
      in
      lib.concatMap (
        m:
        if builtins.isAttrs m && m.contextWindow != null then
          [
            {
              name = m.name;
              value = m.contextWindow;
            }
            {
              name = "${providerName}:${m.name}";
              value = m.contextWindow;
            }
          ]
        else
          [ ]
      ) rawModels
    ) osconfig.myconfig.ai.localModels
  );

  # Build a provider entry for an OpenAI-compatible base URL.
  mkOpenAiCompatibleProvider =
    {
      key,
      name,
      baseUrl,
      models,
      contextWindowLookup ? { },
    }:
    {
      inherit key;
      value = {
        inherit name baseUrl;
        api = "openai-completions";
        apiKey = "dummy";
        authHeader = false;
        models = lib.map (
          modelId:
          let
            cw = contextWindowLookup.${modelId} or null;
          in
          {
            id = modelId;
            name = modelId;
            reasoning = false;
            input = [ "text" ];
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
            maxTokens = 4096;
          }
          // lib.optionalAttrs (cw != null) { contextWindow = cw; }
        ) models;
      };
    };

  localModelProviders = lib.map (
    model:
    let
      hostPort = "${model.host}:${toString model.port}";
      providerName = if model.name != null then model.name else hostPort;
      # localModels may contain strings or `{ name, kind ? null }`
      # submodules (computed kind tag is unused here).
      modelNames =
        let
          raw = if model.models != [ ] then model.models else [ providerName ];
        in
        lib.map (m: if builtins.isAttrs m then m.name else m) raw;
    in
    mkOpenAiCompatibleProvider {
      key = "local-${providerName}";
      name = hostPort;
      baseUrl = "http://${hostPort}/v1";
      models = modelNames;
      inherit contextWindowLookup;
    }
  ) osconfig.myconfig.ai.localModels;

  # `host` may be a wildcard (e.g. "0.0.0.0") for external exposure;
  # rewrite to localhost for in-host clients.
  litellmHost =
    if osconfig.services.litellm.host == "0.0.0.0" then "localhost" else osconfig.services.litellm.host;
  litellmProvider = lib.optional osconfig.services.litellm.enable (mkOpenAiCompatibleProvider {
    key = "litellm";
    name = "LiteLLM";
    baseUrl = "http://${litellmHost}:${toString osconfig.services.litellm.port}/v1";
    models = lib.map (m: m.model_name) osconfig.services.litellm.settings.model_list;
    inherit contextWindowLookup;
  });

  llamaSwapProvider = lib.optional osconfig.services.llama-swap.enable (mkOpenAiCompatibleProvider {
    key = "llama-swap";
    name = "llama-swap";
    baseUrl = "http://localhost:${toString osconfig.services.llama-swap.port}/v1";
    models = builtins.attrNames osconfig.services.llama-swap.settings.models;
    inherit contextWindowLookup;
  });

  allProviders = localModelProviders ++ litellmProvider ++ llamaSwapProvider;

  # Generate a TypeScript extension that registers all providers via
  # pi.registerProvider(). See:
  # https://github.com/badlogic/pi-mono/blob/main/packages/coding-agent/docs/custom-provider.md
  providersExtension =
    let
      providersJson = builtins.toJSON (
        lib.listToAttrs (
          lib.map (p: {
            name = p.key;
            value = p.value;
          }) allProviders
        )
      );
    in
    pkgs.writeText "pi-providers.ts" ''
      // Auto-generated by myconfig.ai.pi-coding-agent. Do not edit by hand.
      import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

      const providers: Record<string, any> = ${providersJson};

      export default function (pi: ExtensionAPI) {
        for (const [key, config] of Object.entries(providers)) {
          pi.registerProvider(key, config);
        }
      }
    '';

  # Custom theme to flag *non-jailed* pi sessions. Both `pi`/`piBwrap` and
  # `jailed-pi` wrap the same `pi-coding-agent` binary and share the same
  # `~/.pi/agent/settings.json` (hence the same theme), so the two are
  # otherwise indistinguishable in the TUI. The jailed (sandboxed) session is the safe
  # default and keeps the user's normal theme untouched; only the *un-jailed*
  # session is recolored, giving its editor box border a red warning color.
  #
  # The editor border color is not the static `border` token; it is driven by
  # the current *thinking level* via `theme.getThinkingBorderColor(level)`,
  # which maps each level to a dedicated `thinking*` token. See
  # `dist/modes/interactive/theme/theme.js`. To recolor the border regardless
  # of thinking level we override every `thinking*` token with the marker
  # color. `bashMode` is intentionally left untouched so bash mode keeps its
  # normal border color.
  #
  # The `myconfig-jail-marker.ts` extension (below) reads `PI_JAIL_MARKER`
  # (set to "1" inside the `jailed-pi` jail) and, only when it is *not* set,
  # switches to this theme on session start. Crucially it switches via the
  # Theme *object* overload of `ctx.ui.setTheme()`, which is in-memory only
  # and does NOT persist: the string overload writes the theme into the
  # shared `~/.pi/agent/settings.json`, which previously leaked the red marker
  # into the jailed session (both wrappers share `~/.pi`). The extension also
  # self-heals that legacy corruption once (see its header comment).
  pi-coding-agent-pkg = pkgs.nixos-unstable.pi-coding-agent;

  baseTheme = builtins.fromJSON (
    builtins.readFile "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/dist/modes/interactive/theme/light.json"
  );

  # Produce a forked theme: same as `light` but with all thinking-level border
  # tokens overridden to `color`, and renamed to `name`.
  mkForkedTheme =
    {
      name,
      color,
    }:
    let
      forked = baseTheme // {
        inherit name;
        # Inject the marker color as a var, then point every border-driving
        # token at it.
        vars = (baseTheme.vars or { }) // {
          jailMarker = color;
        };
        colors = (baseTheme.colors or { }) // {
          thinkingOff = "jailMarker";
          thinkingMinimal = "jailMarker";
          thinkingLow = "jailMarker";
          thinkingMedium = "jailMarker";
          thinkingHigh = "jailMarker";
          thinkingXhigh = "jailMarker";
          thinkingMax = "jailMarker";
        };
      };
    in
    pkgs.writeText "pi-theme-${name}.json" (builtins.toJSON forked);

  # Red border for the un-jailed (unsandboxed) session. The jailed session
  # keeps the user's default theme unchanged.
  unjailedTheme = mkForkedTheme {
    name = "unjailed";
    color = "#cc2222";
  };

  jailMarkerExtension = pkgs.writeText "pi-jail-marker.ts" ''
    // Auto-generated by myconfig.ai.pi-coding-agent. Do not edit by hand.
    //
    // Visually distinguishes the sandboxed `jailed-pi` wrapper (which sets
    // PI_JAIL_MARKER=1) from the un-sandboxed `pi`/`piBwrap` wrappers (which
    // do not). The un-sandboxed session is the dangerous one (full home
    // access), so it gets a red editor border via the `unjailed` theme; the
    // jailed session keeps the user's normal theme.
    //
    // Both wrappers share ~/.pi (and thus ~/.pi/agent/settings.json), so the
    // marker must be SESSION-LOCAL and must never be written to settings.json.
    // An earlier version of this extension called ctx.ui.setTheme("unjailed")
    // with a *string*. In the TUI the string overload of setTheme() persists
    // the theme to settings.json (interactive-mode.js calls
    // settingsManager.setTheme(), which writeFileSync()s the file). That
    // leaked the red marker into the shared settings, so the jailed session
    // inherited it and also turned red -- the "jail detection does not work"
    // symptom. (The PI_JAIL_MARKER check itself worked; the persisted theme was
    // the culprit.)
    //
    // Fix:
    //   * Apply the marker via the Theme *object* overload of setTheme()
    //     (ctx.ui.getTheme() -> ctx.ui.setTheme(themeObject)). The object
    //     overload uses setThemeInstance(), which is in-memory only and does
    //     NOT persist, so the marker never reaches settings.json.
    //   * Self-heal leftover corruption: if settings.json still has the marker
    //     theme persisted (from the old buggy extension), overwrite it once
    //     with a real built-in theme via the persisting string overload. This
    //     repairs both the on-disk settings and the in-memory SettingsManager,
    //     so it cannot be re-corrupted. "dark" is pi's own fallback default;
    //     users who prefer another theme can set it via /settings afterwards.
    import { readFileSync } from "node:fs";
    import { homedir } from "node:os";
    import { join } from "node:path";
    import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

    const MARKER_THEME = "unjailed"; // red-border theme, a fork of `light`
    const RECOVERY_THEME = "dark"; // pi's built-in fallback; repairs settings

    // Read the persisted `theme` field directly (read-only) to detect leftover
    // marker corruption. We never write settings.json ourselves -- that would
    // race the in-memory SettingsManager, which re-derives the file on save().
    function readPersistedTheme(): string | undefined {
      const agentDir =
        process.env.PI_CODING_AGENT_DIR ?? join(homedir(), ".pi", "agent");
      try {
        const raw = readFileSync(join(agentDir, "settings.json"), "utf-8");
        const parsed = JSON.parse(raw);
        return typeof parsed?.theme === "string" ? parsed.theme : undefined;
      } catch {
        return undefined;
      }
    }

    export default function (pi: ExtensionAPI) {
      pi.on("session_start", async (_event: any, ctx: any) => {
        // The marker is a TUI-only visual cue; setTheme() is a no-op outside
        // TUI, so there is nothing to do in rpc/print modes.
        if (ctx.mode !== "tui") return;

        const jailed = process.env.PI_JAIL_MARKER === "1";

        // Repair settings.json if the old buggy extension left the marker
        // theme persisted in it. The string overload persists (updating both
        // the on-disk settings and the in-memory SettingsManager), so this is a
        // one-time fix that a later settings save cannot undo.
        if (readPersistedTheme() === MARKER_THEME) {
          ctx.ui.setTheme(RECOVERY_THEME);
          ctx.ui.notify(
            `jail-marker: repaired settings.json (theme was stuck on the "''${MARKER_THEME}" marker); default reset to "''${RECOVERY_THEME}". Set your preferred theme via /settings.`,
            "info",
          );
        }

        if (jailed) {
          // Sandboxed session: keep the user's normal theme. Do not apply the
          // red marker inside the jail. (If we just repaired corruption above,
          // the session now shows RECOVERY_THEME; otherwise the user's theme.)
          return;
        }

        // Un-sandboxed session: show the red warning border for this session
        // only. Passing a Theme *object* (not a name string) to setTheme()
        // takes the setThemeInstance() path, which is in-memory only and does
        // not persist -- so the marker never leaks into the shared settings.
        const marker = ctx.ui.getTheme(MARKER_THEME);
        if (marker) {
          ctx.ui.setTheme(marker);
        } else {
          ctx.ui.notify(
            `jail-marker: theme "''${MARKER_THEME}" not found; is ~/.pi/agent/themes/unjailed.json deployed?`,
            "error",
          );
        }
      });
    }
  '';

  # The subagent example extension (examples/extensions/subagent/) ships as
  # a multi-file extension plus sample agent definitions and workflow prompt
  # templates. pi discovers each from a distinct location (see the example's
  # README.md):
  #   * the extension entry point (`index.ts`) and its sibling `agents.ts`
  #     helper go to ~/.pi/agent/extensions/subagent/ (pi loads
  #     `*/index.ts`; `agents.ts` is imported via a relative "./agents.ts"
  #     specifier, so it must sit next to `index.ts`);
  #   * sample agent markdown files go to ~/.pi/agent/agents/ (discovered by
  #     the extension's `discoverAgents()`, which reads `getAgentDir()/agents`);
  #   * workflow prompt templates go to ~/.pi/agent/prompts/ (invoked via
  #     `/implement`, `/scout-and-plan`, `/implement-and-review`).
  # The file list is enumerated with `builtins.readDir` rather than
  # hardcoded, so files added upstream are picked up automatically. Reading
  # the package store path at eval time is the same pattern already used
  # above for `baseTheme` (`builtins.readFile` of the theme JSON).
  subagentExampleDir = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/examples/extensions/subagent";

  subagentExtensionFiles = lib.mapAttrs' (
    name: _:
    lib.nameValuePair ".pi/agent/extensions/subagent/${name}" {
      source = "${subagentExampleDir}/${name}";
    }
  ) (lib.filterAttrs (name: _: lib.hasSuffix ".ts" name) (builtins.readDir subagentExampleDir));

  # The upstream sample agent `.md` files hard-code a `model:` frontmatter
  # field (e.g. `claude-haiku-4-5`, `claude-sonnet-4-5`) pointing at Anthropic
  # models that are not available through this config's local providers. The
  # field is optional in the subagent extension: `agents.ts` only requires
  # `name` and `description`, and `index.ts` passes `--model` to the spawned
  # subprocess only when `agent.model` is set. With the field absent the
  # subagent inherits the user's configured default model (from
  # `~/.pi/agent/settings.json` or pi's built-in default) -- the desired
  # behavior for a sample-extension deployment. Rather than fork the files to
  # hard-code a *different* model name (which would just move the
  # hard-coding), strip the `model:` line entirely so the sample agents stay
  # close to upstream and always follow the user's default model. The store
  # originals are read-only, so each file is re-emitted via `pkgs.writeText`
  # with the offending line filtered out.
  subagentAgentFiles =
    let
      agentsDir = "${subagentExampleDir}/agents";
      # Drop any frontmatter `model:` line so the subagent falls back to
      # the user's default model instead of a hard-coded upstream model.
      # The sample files have flat (un-indented) frontmatter and their
      # system-prompt bodies contain no line starting with `model:`, so a
      # simple prefix filter is exact here.
      stripModelLine =
        content:
        lib.concatStringsSep "\n" (
          lib.filter (line: !(lib.hasPrefix "model:" line)) (lib.splitString "\n" content)
        );
    in
    lib.mapAttrs' (
      name: _:
      lib.nameValuePair ".pi/agent/agents/${name}" {
        source = pkgs.writeText name (stripModelLine (builtins.readFile "${agentsDir}/${name}"));
      }
    ) (lib.filterAttrs (name: _: lib.hasSuffix ".md" name) (builtins.readDir agentsDir));

  subagentPromptFiles =
    lib.mapAttrs'
      (
        name: _:
        lib.nameValuePair ".pi/agent/prompts/${name}" {
          source = "${subagentExampleDir}/prompts/${name}";
        }
      )
      (
        lib.filterAttrs (name: _: lib.hasSuffix ".md" name) (
          builtins.readDir "${subagentExampleDir}/prompts"
        )
      );

  # The handoff example (examples/extensions/handoff.ts) is a single-file
  # extension that adds a `/handoff` command for transferring context to a
  # new focused session instead of lossy compaction. Deployed as a global
  # single-file extension at ~/.pi/agent/extensions/handoff.ts (pi
  # auto-discovers `~/.pi/agent/extensions/*.ts`). Its runtime value imports
  # (`@earendil-works/pi-ai`, `@earendil-works/pi-coding-agent`, ...) resolve
  # against pi's own node_modules at load time, unlike the generated
  # `myconfig-*` extensions above which only use type-only imports.
  handoffExtension = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/examples/extensions/handoff.ts";

  piBwrap = callLib ../fns/sandboxed-app.nix {
    name = "pi";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    extraRuntimeInputs = [
      pkgs.wget
      pkgs.curl
      pkgs.jq
    ]
    ++ workmuxDevTools;
    writableDirs = [
      ".pi"
    ];
    # Read-only bind of `~/.agents/skills/`, where `myconfig.ai.skills`
    # deploys handcrafted skills for pi (which has no `programs.pi.skills`
    # option). See `modules/myconfig.ai/skills/default.nix`.
    readOnlyConfigDirs = [ ".agents" ];
  };

  # `jailed-pi` is an alternative to `piBwrap` that uses the jail.nix library
  # (vendored at ./vendor/alexdavid-jail.nix) instead of a hand-rolled
  # bubblewrap wrapper. See `../fns/jail-app.nix` for the shared defaults.
  #
  # `~/.pi` is rw-bound because the agent picks up the auto-generated
  # provider extension installed by home-manager
  # (`~/.pi/agent/extensions/myconfig-providers.ts`) and so session and
  # credential state persists across invocations.
  jailed-pi = jail-app {
    name = "jailed-pi";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    userDataDirs = [ ".pi" ];
    # Read-only bind of `~/.agents/skills/` so pi discovers the handcrafted
    # skills deployed there by `myconfig.ai.skills`. Bound via
    # `extraConfigDirs` (try-ro-bind inside `jail-app.nix`), so the jail still
    # starts on a host where `.agents` has not been deployed.
    extraConfigDirs = [ ".agents" ];
    # Marker so `jailed-pi` sessions are visually distinguishable from the
    # plain `pi`/`piBwrap` wrappers (which share the same `~/.pi/agent/settings.json`,
    # hence the same theme). The `myconfig-jail-marker.ts` extension (deployed
    # below) reads this variable: when set (jailed) it leaves the user's theme
    # untouched (after self-healing any legacy marker corruption); when unset
    # (un-jailed) it applies the red-border `unjailed` theme in-memory only.
    extraRuntimeEnv.PI_JAIL_MARKER = "1";
    extraDevTools = workmuxDevTools;
    # Bind the host path named by `PI_WORKTREE_MAIN_REPO` read-only into the
    # jail. The `*-worktree` wrapper scripts set this to the *original* git
    # repository (the worktree's linked main repo) before exec'ing `jailed-pi`,
    # so git operations against the worktree can resolve the shared `.git`
    # object store, refs and config that live in the main repo. Without this
    # bind, `mount-cwd` only exposes the worktree directory itself and git
    # fails with `fatal: not a git repository` because the worktree's `.git`
    # file points into the main repo's `.git/worktrees/<name>/`. Unset for
    # the plain (`jailed-pi`, `jailed-pi-tmp`) variants, where the
    # conditional `--ro-bind-try` skips the bind silently.
    extraReadOnlyEnvPaths = [ "PI_WORKTREE_MAIN_REPO" ];
  };

  # Worktree-only variant: expose the linked main checkout read-only, then
  # remount its shared Git directory (`.git`) read-write. Git needs this for
  # objects, refs, index locks, and worktree administration when committing
  # from a linked worktree.
  # This is separate from `jailed-pi` so a normal invocation cannot request a
  # writable bind to another repository merely by setting an environment var.
  jailed-pi-worktree-inner = jail-app {
    name = "jailed-pi-worktree-inner";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    userDataDirs = [ ".pi" ];
    extraConfigDirs = [ ".agents" ];
    # Install the `workmux` PATH-shim (not the real binary) so
    # `workmux set-window-status` — invoked by the jailed pi's
    # `workmux-status.ts` extension — is routed through the jail-to-host
    # channel below instead of shelling out to tmux (which is unreachable
    # inside the jail). Gated on workmux being enabled.
    extraDevTools = lib.optional osconfig.myconfig.ai.workmux.enable workmuxStatusShim;
    extraRuntimeEnv.PI_JAIL_MARKER = "1";
    extraReadOnlyEnvPaths = [ "PI_WORKTREE_MAIN_REPO" ];
    # This bind is emitted after the read-only main-repo bind by jail-app.
    extraReadWriteEnvPaths = [ "PI_WORKTREE_GIT_DIR" ];
    # Expose the `workmux_status_channel` program inside the jail and run its
    # host-side handler in the worktree tmux pane's environment.
    extraPermissions = workmuxStatusChannelPerms;
  };

  # Thin workmux-driven replacement for the previous bespoke worktree script.
  # `mkWorkmuxWorktree` builds the launcher run inside the worktree pane (which
  # resolves the shared git dir, exports PI_WORKTREE_* and execs
  # `jailed-pi-worktree-inner`) and the user-facing `jailed-pi-worktree`
  # wrapper (which requires tmux and calls `workmux add --agent jailed-pi`).
  mkWorkmuxWorktree = callLib ../fns/workmux-worktree.nix;
  jailedPiWorktree = mkWorkmuxWorktree {
    name = "jailed-pi-worktree";
    agentName = "jailed-pi";
    agentType = "pi";
    innerPkg = jailed-pi-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
  };
  # Non-jailed variant driving the bubblewrap `piBwrap` wrapper. It exposes
  # the worktree's shared git dir via the sandboxed-app `WORKTREE_*` env vars.
  piWorktree = mkWorkmuxWorktree {
    name = "pi-worktree";
    agentName = "pi";
    agentType = "pi";
    innerPkg = piBwrap;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
in
{
  options.myconfig = with lib; {
    ai.pi-coding-agent = {
      enable = mkEnableOption "myconfig.ai.pi-coding-agent";
    };
  };
  config = lib.mkIf config.myconfig.ai.pi-coding-agent.enable {
    # Register the jailed worktree launcher as a workmux "named agent" so
    # `jailed-pi-worktree` (below) can `workmux add --agent jailed-pi`.
    myconfig.ai.workmux.agents.jailed-pi = jailedPiWorktree.agent;
    myconfig.ai.workmux.agents.pi = piWorktree.agent;
    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".pi" ];
        # Deploy the generated extensions/theme plus the subagent example
        # extension (multi-file, plus sample agents and workflow prompts) and
        # the handoff single-file extension. `mkMerge` is required because the
        # subagent files are produced as attrsets (enumerated with
        # `builtins.readDir` in the `let` bindings above) and must be combined
        # with the dotted `home.file.*` entries within this same module.
        home.file = lib.mkMerge [
          {
            ".pi/agent/extensions/myconfig-providers.ts".source = providersExtension;
            ".pi/agent/extensions/myconfig-jail-marker.ts".source = jailMarkerExtension;
            ".pi/agent/extensions/handoff.ts".source = handoffExtension;
            ".pi/agent/themes/unjailed.json".source = unjailedTheme;
            # Override the default `app.interrupt` (escape) with `ctrl+c`
            # for reliability across SSH and tmux; `app.clear` moves to
            # `ctrl+q` to avoid the conflict. `app.model.cycleForward`
            # (default `ctrl+p`) and `app.model.cycleBackward` (default
            # `shift+ctrl+p`) are disabled so those keys are never swallowed
            # by model cycling. See ./keybindings.json.
            ".pi/agent/keybindings.json".source = ./keybindings.json;
          }
          subagentExtensionFiles
          subagentAgentFiles
          subagentPromptFiles
        ];
        home.packages = [
          pkgs.nixos-unstable.pi-coding-agent
          piBwrap
          jailed-pi
          (pkgs.writeShellApplication {
            name = "pi-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe piBwrap} "$@"
            '';
          })
          (pkgs.writeShellApplication {
            name = "jailed-pi-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe jailed-pi} "$@"
            '';
          })
          piWorktree.wrapper
          jailedPiWorktree.wrapper
        ];
      }
    ];
  };
}
