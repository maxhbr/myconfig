# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# opencode and its wrappers (opencode/opencodeBwrap,
# agent-bubblewrap-opencode, worktree variants), plus the mysbx tier
# integration: opencode's config is mounted into mysbx sandboxes from
# a dereferenced store tree built by the shared
# ../../mysbx/nix/sandbox-config.nix helper (bd myconfig-ooh).
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
  jail-app = callJailLib ../../fns/bubblewrap-app.nix;

  # The shared mysbx sandbox-config helper (the dereferenced-store-tree
  # machinery, bd myconfig-ooh); consumed like the entry scripts consume
  # ../../mysbx/nix/mux-entry-lib.nix.
  sandboxConfigLib = import ../../mysbx/nix/sandbox-config.nix {
    inherit lib;
    runCommand = pkgs.runCommand;
  };
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;

  # Make the `workmux` binary available inside the sandboxes (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.dev.workmux.enable osconfig.myconfig.ai.dev.workmux.package;

  # home-manager uses `useGlobalPkgs`, so `pkgs.opencode` is the same package
  # `programs.opencode.package` defaults to. Building the wrappers at the
  # NixOS scope lets us register the workmux named agents from here.
  opencodeBwrap = callLib ../../fns/bubblewrap-simple-app.nix {
    name = "opencode";
    pkg = pkgs.opencode;
    writableDirs = [
      ".config/opencode"
      ".config/mcp"
    ];
    extraRuntimeInputs = workmuxDevTools;
  };
  # `agent-bubblewrap-opencode` is an alternative to `opencodeBwrap` that uses the
  # jail.nix library instead of a hand-rolled bubblewrap wrapper. See
  # `../../fns/bubblewrap-app.nix` for the shared defaults.
  agent-bubblewrap-opencode = jail-app {
    name = "agent-bubblewrap-opencode";
    pkg = pkgs.opencode;
    userDataDirs = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      ".config/mcp"
    ];
    extraDevTools = workmuxDevTools;
  };
  # Worktree variant of `agent-bubblewrap-opencode`: additionally binds the linked main
  # repository read-only and remounts its shared `.git` read-write, resolved
  # at runtime from the WORKTREE_* env vars set by the workmux launcher.
  agent-bubblewrap-opencode-worktree-inner = jail-app {
    name = "agent-bubblewrap-opencode-worktree-inner";
    pkg = pkgs.opencode;
    userDataDirs = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      ".config/mcp"
    ];
    extraDevTools = workmuxDevTools;
    extraReadOnlyEnvPaths = [ "WORKTREE_MAIN_REPO" ];
    extraReadWriteEnvPaths = [ "WORKTREE_GIT_DIR" ];
  };
  opencodeWorktree = mkWorkmuxWorktree {
    name = "opencode-worktree";
    agentName = "opencode";
    agentType = "opencode";
    innerPkg = opencodeBwrap;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
  agentBubblewrapOpencodeWorktree = mkWorkmuxWorktree {
    name = "agent-bubblewrap-opencode-worktree";
    agentName = "agent-bubblewrap-opencode";
    agentType = "opencode";
    innerPkg = agent-bubblewrap-opencode-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };

  # --- mysbx integration (../mysbx) -------------------------------------
  #
  # opencode is wired into the `mysbx` sandbox tier the same way pi is
  # (../programs.pi-coding-agent/default.nix):
  #
  #   1. the `opencode` binary on the sandbox PATH -> `myconfig.ai.dev.mysbx.extraTools`
  #   2. opencode's *configuration* visible inside the sandbox -> read-only
  #      mounts in the generated user config layer (`…mysbx.config.mounts`),
  #      each bound from a self-contained store tree of dereferenced copies
  #      built by the shared `mysbx/nix/sandbox-config.nix` helper (bd
  #      myconfig-ooh): the subtrees the helper copies mirror exactly what
  #      home-manager deploys, so a D8 missing-source mount error can no
  #      longer occur for these entries — the source is always the built
  #      tree, and the helper materialises an (empty) directory for a
  #      subtree home-manager deployed nothing into.
  #   3. opencode's *state* persists across runs -> `state-dirs` entries
  #      (`…mysbx.config.stateDirs`, ../../mysbx/docs/design/config.md D15)
  #      backed by `<repo>.mysbx/state/` in the sidecar — never the host
  #      `~/.local`, which stays out of the sandbox entirely.
  #
  # The host's own `~/.local/{share,state}/opencode` and the auth files are
  # deliberately NOT mounted: a sandboxed session starts unauthenticated
  # (it talks to the local LiteLLM / llama.cpp providers, which need no
  # credentials) and writes its state to the sidecar instead.

  # Home Manager writes `~/.config/mcp/mcp.json` only when at least one
  # MCP server is configured (upstream `modules/programs/mcp.nix`:
  # `xdg.configFile = mkIf (cfg.servers != { })`), so with no MCP servers
  # the `.config/mcp` subtree would be EMPTY in the copied tree (the
  # helper materialises it as an empty directory, keeping the mount
  # valid). The gate is kept anyway — read from the user whose config
  # layer mysbx generates — so the generated layer only carries the
  # entry when there is something to mount. Same pattern (and the same
  # laziness argument) as `hmRipgrep` in ../../mysbx/default.nix.
  hmMcpServers = config.home-manager.users.mhuber.programs.mcp.servers or { };

  # Read from the user whose config layer mysbx generates, like
  # `hmMcpServers` above (the helper needs the merged `home.file` as its
  # single source of truth, plus the home directory to normalize the
  # absolute `xdg.configFile` keys into relative targets). Only
  # referenced under `mkIf (…mysbx.enable …)` below (Nix is lazy); not
  # circular for the same reason as in ../programs.pi-coding-agent (no
  # subtree below matches `.config/mysbx`).
  hmHomeFile = config.home-manager.users.mhuber.home.file;
  hmHomeDirectory = config.home-manager.users.mhuber.home.homeDirectory;

  # Skills are the one exception to "home.file is the source of truth"
  # (see ../programs.pi-coding-agent): when the agent-skills framework
  # manages them, `~/.config/opencode/skills` is rsynced at ACTIVATION
  # time, not linked from `home.file`. Pass the framework's per-target
  # bundle for the `opencode` target through `extraFiles` so the sandbox
  # tree matches what the host deploys. Lazily read, like `hmHomeFile`.
  mysbxOpencodeSkillsBundle =
    let
      hmSkills = config.home-manager.users.mhuber.programs.agent-skills or null;
    in
    if hmSkills == null || !hmSkills.enable then
      { }
    else if !(hmSkills.targetBundlePaths ? opencode) then
      { }
    else
      { ".config/opencode/skills" = hmSkills.targetBundlePaths.opencode; };

  mysbxOpencodeMounts =
    (sandboxConfigLib.mkSandboxConfig {
      homeFile = hmHomeFile;
      # The deployed CHILDREN of `.config/opencode`, not the dir itself.
      # OpenCode 2's terminal client and background service write
      # `.config/opencode/service.json` (a per-installation lock + password)
      # on first start, so a read-only mount of the whole config dir makes
      # every invocation die with `EROFS … service.json.tmp` before it serves
      # anything (verified against opencode 2.1.1; the same reason the agent
      # modules mount their config dir per child, see
      # ../programs.claude-code). Leaving the dir root in the sandbox's
      # writable tmpfs home gives OpenCode somewhere to put that file while
      # everything Home Manager deploys below it stays read-only.
      subtrees =
        sandboxConfigLib.childSubtrees {
          homeFile = hmHomeFile;
          homeDirectory = hmHomeDirectory;
          extraFiles = mysbxOpencodeSkillsBundle;
          dir = ".config/opencode";
        }
        # MCP server definitions, activated by `enableMcpIntegration` below.
        # The whole `mcp` directory rather than the single file: one mount
        # covers whatever Home Manager writes there (and nothing writes to it
        # at run time).
        ++ lib.optional (hmMcpServers != { }) ".config/mcp";
      name = "opencode-sandbox-config";
      homeDirectory = hmHomeDirectory;
      extraFiles = mysbxOpencodeSkillsBundle;
    }).mounts;

  # opencode's model catalog entry for one model: its display name plus, when
  # the publisher knows it, the context window. `limit` is the field OpenCode
  # reads (`Config.ModelEncoded.limit`, 1.x and 2.x alike); the
  # `contextWindowSize` key this module used to emit is in neither schema, so
  # OpenCode silently dropped it and every model advertised as unknown-context.
  #
  # `limit.output` is required by the 1.x schema and optional in 2.x. A
  # completion cannot be longer than the per-request context, so the context is
  # the honest upper bound here.
  mkModelEntry =
    modelName: contextWindow:
    {
      name = modelName;
    }
    // lib.optionalAttrs (contextWindow != null) {
      limit = {
        context = contextWindow;
        output = contextWindow;
      };
    };

  # `localModels` may contain plain strings or `{ name, kind, contextWindow }`
  # submodules (`kind` is one of "base"/"variant"/"alias", computed by the
  # publisher). Flatten both shapes into `{ name, contextWindow }`; opencode
  # treats every name the same, so the kind tag is ignored here.
  flattenModels =
    modelNames:
    lib.map (
      m:
      if builtins.isAttrs m then
        {
          name = m.name;
          contextWindow = m.contextWindow or null;
        }
      else
        {
          name = m;
          contextWindow = null;
        }
    ) modelNames;

  # Build a lookup: model name (raw or provider-prefixed) -> contextWindow.
  contextWindowLookup = lib.listToAttrs (
    lib.concatMap (
      provider:
      let
        hostPort = "${provider.host}:${toString provider.port}";
        providerName = if provider.name != null then provider.name else hostPort;
      in
      lib.concatMap (
        m:
        if m.contextWindow != null then
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
      ) (flattenModels (if provider.models != [ ] then provider.models else [ ]))
    ) osconfig.myconfig.ai.llmops.localModels
  );
in
{
  options.myconfig = with lib; {
    ai.dev.opencode = {
      enable = mkEnableOption "myconfig.ai.dev.opencode";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.opencode.enable {
    myconfig.ai.dev.workmux.agents.opencode = opencodeWorktree.agent;
    myconfig.ai.dev.workmux.agents.agent-bubblewrap-opencode = agentBubblewrapOpencodeWorktree.agent;

    # mysbx tier integration (see `mysbxOpencodeMounts` above). Gated on
    # mysbx being enabled too: the two features are independent, and the
    # mounts would otherwise be generated for a host that has no mysbx
    # config file to carry them. The state dirs replace what the
    # bubblewrap jail tiers bind rw (`userDataDirs`): opencode's session
    # and TUI state persist per repository in the sidecar
    # (../mysbx/docs/design/config.md D15) instead of dying with the
    # tmpfs home.
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ pkgs.opencode ];
      config.mounts = mysbxOpencodeMounts;
      config.stateDirs = [
        ".local/share/opencode"
        ".local/state/opencode"
      ];
      # The litellm endpoint of the podman-gvisor backend: the mounted
      # configuration names the host loopback, which inside a container
      # is the container's own, so the sandboxed session needs the
      # host-side forwarder's URL instead
      # (../../myconfig.ai.dev.litellm-forwarder.nix). opencode merges
      # `OPENCODE_CONFIG_CONTENT` over its config files, so a fragment
      # carrying nothing but the provider's `baseURL` replaces that one
      # key and leaves the rest of the mounted configuration in place.
      #
      # Same native-v2 `providers.<id>.settings` shape as the `settings`
      # block below (a v1 `provider.<id>.options` fragment would still merge,
      # but mixing both shapes for one provider is exactly what the
      # migration guide warns about).
      #
      # `podman.env` reaches that backend ALONE / the bubblewrap backend
      # shares the host network namespace, where the baked URL is right.
      podman.env = lib.mkIf osconfig.myconfig.ai.dev.litellm-forwarder.enable {
        OPENCODE_CONFIG_CONTENT = builtins.toJSON {
          providers."${osconfig.networking.hostName}-litellm".settings.baseURL =
            osconfig.myconfig.ai.dev.litellm-forwarder.endpoint;
        };
      };
      # The litellm endpoint of the microvm-qemu backend (bd
      # myconfig-mzys): the SLiRP gateway `10.0.2.2` maps to the host
      # loopback — the same `OPENCODE_CONFIG_CONTENT` override as the
      # podman one above, with the qemu-guest URL instead of the
      # forwarder's.
      microvm-qemu.env = lib.mkIf osconfig.services.litellm.enable {
        OPENCODE_CONFIG_CONTENT = builtins.toJSON {
          providers."${osconfig.networking.hostName}-litellm".settings.baseURL =
            "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
        };
      };
    };

    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        {
          programs.mcp.enable = true;
          programs.opencode = {
            enable = true;
            enableMcpIntegration = true;
            web.enable = true;

            # Native OpenCode 2 shape: `update`, `permissions`, `providers`,
            # `agents`, `policies`. The deprecated v1 spellings (`autoupdate`,
            # `permission`, `provider`, `small_model`, `disabled_providers`)
            # are gone on purpose — keeping both would mean two sources of
            # truth for the same setting.
            #
            # This is also what `pkgs.opencode` pins: nixpkgs tracks OpenCode
            # 2.x, so the module never has to serve a 1.x package. (For the
            # record, 1.19.0 rejects the whole document on an unknown
            # top-level key rather than ignoring it; putting a 1.x package
            # behind this block would need the v1 spellings back.)
            settings = {
              # `false` (v1) and `"disable"` (v2) mean the same thing; the
              # schema of neither version accepts the other's spelling.
              "update" = "disable";
              "share" = "disabled";
              # Ordered rule list: the LAST matching rule wins, so the broad
              # `ask` comes first and the allowlist after it (v1 was a map per
              # tool, v2 an ordered array; `bash` is called `shell` in v2).
              #
              # `external_directory` restores the v1 posture: v2 allows paths
              # outside the project by DEFAULT and only asks when a rule says
              # so, so without this rule an upgrade silently widens what the
              # agent may touch.
              "permissions" = lib.mkForce (
                [
                  {
                    action = "shell";
                    resource = "*";
                    effect = "ask";
                  }
                ]
                ++
                  lib.map
                    (cmd: {
                      action = "shell";
                      resource = cmd;
                      effect = "allow";
                    })
                    [
                      "head *"
                      "tail *"
                      "git add *"
                      "git status *"
                      "git log *"
                      "git diff *"
                      "nix flake check *"
                      "nix flake *"
                      "nix fmt"
                      "nix fmt *"
                      "nix build --dry-run *"
                      "nix-instantiate *"
                      "go build *"
                      "go test *"
                      "go generate *"
                      "go fmt *"
                      "go vet *"
                      "npm run dev *"
                      "npm run build *"
                      "npm run lint *"
                      "npm test *"
                      "ls *"
                      "grep *"
                      "rg *"
                      "find *"
                      "mkdir *"
                    ]
                ++ [
                  {
                    action = "edit";
                    resource = "*";
                    effect = "ask";
                  }
                  {
                    action = "external_directory";
                    resource = "*";
                    effect = "ask";
                  }
                ]
              );
              # `disabled_providers` (v1, deprecated) is a provider policy in
              # v2. The v1 field is still normalised by both versions, but the
              # policy is the native spelling and it is what `policies` is for.
              "policies" = [
                {
                  action = "provider.use";
                  resource = "opencode";
                  effect = "deny";
                }
              ];
              "providers" = lib.mkMerge [
                (lib.mkIf (osconfig.myconfig.ai.llmops.localModels != [ ]) (
                  builtins.listToAttrs (
                    lib.map (
                      model:
                      let
                        hostPort = "${model.host}:${toString model.port}";
                        providerName = if model.name != null then model.name else hostPort;
                        models = flattenModels (if model.models != [ ] then model.models else [ providerName ]);
                      in
                      {
                        name = "local-${providerName}";
                        value = {
                          "package" = "aisdk:@ai-sdk/openai-compatible";
                          "name" = "${hostPort}";
                          "settings" = {
                            "baseURL" = "http://${hostPort}/v1";
                          };
                          "models" = builtins.listToAttrs (
                            lib.map (m: {
                              name = m.name;
                              value = mkModelEntry m.name m.contextWindow;
                            }) models
                          );
                        };
                      }
                    ) osconfig.myconfig.ai.llmops.localModels
                  )
                ))
                (lib.mkIf osconfig.services.litellm.enable (
                  let
                    opencodeModels = builtins.listToAttrs (
                      lib.map (
                        model:
                        let
                          cw = contextWindowLookup.${model.model_name} or null;
                        in
                        {
                          name = model.model_name;
                          value = mkModelEntry model.model_name cw;
                        }
                      ) osconfig.services.litellm.settings.model_list
                    );
                    # `host` may be a wildcard (e.g. "0.0.0.0") for external
                    # exposure; rewrite to localhost for in-host clients.
                    litellmHost =
                      if osconfig.services.litellm.host == "0.0.0.0" then "localhost" else osconfig.services.litellm.host;
                  in
                  {
                    "${osconfig.networking.hostName}-litellm" = {
                      "package" = "aisdk:@ai-sdk/openai-compatible";
                      "name" = "LiteLLM (${osconfig.networking.hostName})";
                      "settings" = {
                        "baseURL" = "http://${litellmHost}:${toString osconfig.services.litellm.port}/v1";
                      };
                      "models" = opencodeModels;
                    };
                  }
                ))
                (lib.mkIf osconfig.services.llama-swap.enable (
                  let
                    llamaSwapModels = builtins.listToAttrs (
                      lib.map (
                        model:
                        let
                          cw = contextWindowLookup.${model} or null;
                        in
                        {
                          name = model;
                          value = mkModelEntry model cw;
                        }
                      ) (builtins.attrNames osconfig.services.llama-swap.settings.models)
                    );
                  in
                  {
                    "llama-swap" = {
                      "package" = "aisdk:@ai-sdk/openai-compatible";
                      "name" = "llama-swap";
                      "settings" = {
                        "baseURL" = "http://localhost:${toString osconfig.services.llama-swap.port}/v1";
                      };
                      "models" = llamaSwapModels;
                    };
                  }
                ))
              ];
            };
            # Terminal-client settings live in `cli.json` as of OpenCode 2, and
            # its `theme` is an object (`{ name, mode }`), not the v1 scalar.
            # `mode = "system"` keeps following the terminal's light/dark mode
            # — that is what the old `tui.theme = "system"` meant.
            cli = {
              "theme" = {
                "name" = "system";
                "mode" = "system";
              };
            };
            agents = {
              code-reviewer = ''
                # Code Reviewer Agent

                You are a senior software engineer specializing in code reviews.
                Focus on code quality, security, and maintainability.

                ## Guidelines
                - Review for potential bugs and edge cases
                - Check for security vulnerabilities
                - Ensure code follows best practices
                - Suggest improvements for readability and performance
              '';
            };
            commands = {
              commit = ''
                # Commit Command

                Create a git commit following the `commit` skill: invoke the
                `skill` tool with name `commit` and follow its workflow,
                including its attribution trailer (a final line recording the
                model and harness that produced the commit).
                Usage: /commit [message]
              '';
              run = ''
                # Run Command

                Run the provided command as is
                Usage: /run command
              '';
            };
          };
          # `service.json` (the background service's registration: url, port,
          # password and pid) lives in `.local/state/opencode` in OpenCode 2, so
          # both state dirs are persisted — a non-impermanent reboot would
          # otherwise leave a stale registration behind.
          myconfig.persistence.directories = [
            ".local/share/opencode"
            ".local/state/opencode"
          ];
          home.packages = [
            pkgs.opencode-desktop
            opencodeBwrap
            agent-bubblewrap-opencode
            (pkgs.writeShellApplication {
              name = "opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "agent-bubblewrap-opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe agent-bubblewrap-opencode} "$@"
              '';
            })
            opencodeWorktree.wrapper
            agentBubblewrapOpencodeWorktree.wrapper
          ];
        }
      )
    ];
  };
}
