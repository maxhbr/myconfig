# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.gvisor-agent-sandbox — rootless Podman + gVisor agent sandboxes.
#
# A container-based isolation tier for coding agents, sitting between the
# bubblewrap `jail-app` wrappers (`myconfig.ai.jail`) and the Cloud Hypervisor
# microVM tier (`myconfig.ai.microvm`). Each session runs in a rootless Podman
# container with gVisor (`runsc`) as OCI runtime, on a Nix-built sandbox image,
# and gets its own git worktree from a disposable bare pool.
#
# The sources under ./bin and ./nix are vendored from
# https://github.com/maxhbr/gvisor-agent-sandbox via `git subtree`; see
# ./README.md for the import commit and how to pull upstream changes. The
# upstream standalone-flake plumbing (flake.nix / flake.lock /
# nixos/agent-sandboxes.nix) was dropped — this module replaces it.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;

  # The coding-agent CLIs this repo can install on the host, mapped from their
  # `myconfig.ai.<name>.enable` flag to the very package attribute the matching
  # host wrapper uses (see modules/myconfig.ai/programs.<name>). Whatever the
  # host has enabled is baked into the sandbox image by default, so the
  # sandbox offers the same agents as the host without any per-host list.
  # Deliberately NOT included: `aichat` / `llm`, which are host-side chat
  # front-ends rather than agents driving a checkout, and would only inflate
  # the image.
  agentPackagesByFlag = {
    pi-coding-agent = pkgs.nixos-unstable.pi-coding-agent;
    opencode = pkgs.opencode;
    claude-code = pkgs.claude-code;
    codex = pkgs.codex;
    github-copilot-cli = pkgs.github-copilot-cli;
    qwen-code = pkgs.qwen-code;
  };

  # Host part of the sandbox-reachable LiteLLM endpoint (`litellm.endpoint`
  # without the `/v1` suffix), used to rewrite the loopback URLs baked into the
  # seeded host configuration.
  litellmBase = "http://${cfg.litellm.address}:${toString cfg.litellm.port}";

  enabledAgentPackages = lib.attrValues (
    lib.filterAttrs (name: _: config.myconfig.ai.${name}.enable or false) agentPackagesByFlag
  );

  # `herdr` is the agent multiplexer that lives in the terminal. On the host it
  # is installed whenever at least one agentic coding agent is enabled (see
  # ../programs.herdr.nix, which uses exactly the flags of
  # `agentPackagesByFlag`), so mirror that condition here: if the sandbox
  # carries any agent, it also carries the multiplexer that drives them, and
  # `herdr` becomes the default command of a session.
  herdrEnabled = enabledAgentPackages != [ ];

  # The image actually used: either the configured one, or the default with
  # `extraImagePackages` folded in.
  image =
    if cfg.image == null then
      null
    else if cfg.extraImagePackages == [ ] then
      cfg.image
    else
      cfg.image.override { extraPackages = cfg.extraImagePackages; };

  # Thread the effective image through both helpers, so overriding the image
  # also changes the default image reference baked into `agent-session`.
  withImage = pkg: if image == null then pkg else pkg.override { agent-sandbox-image = image; };

  # Host-configured defaults baked into `agent-session`:
  #   * the home-seed allowlist and endpoint-rewrite rules. The seed SOURCE is
  #     resolved by the script at runtime (the activated home-manager
  #     generation of the calling user), so a dotfile change needs no rebuild
  #     here; only *which* paths are copied, and how host-only URLs are
  #     rewritten, is a build-time decision.
  #   * the model endpoint, so `agent-session doctor` can probe it from INSIDE
  #     a sandbox — the only place where the answer means anything — and the
  #     pasta network spec that makes that endpoint reachable (see
  #     ./litellm-endpoint.nix).
  # `--set-default` keeps every variable overridable per invocation.
  sessionEnv =
    lib.optionalAttrs cfg.home.enable {
      AGENT_SANDBOX_HOME_SEED_PATHS = lib.concatStringsSep " " cfg.home.seedPaths;
      AGENT_SANDBOX_HOME_SEED_REWRITE = lib.concatStringsSep " " cfg.home.rewriteEndpoints;
    }
    // lib.optionalAttrs cfg.litellm.enable {
      AGENT_SANDBOX_MODEL_ENDPOINT = cfg.litellm.endpoint;
      # pasta(1) network spec: --map-gw suppresses podman's --no-map-gw (which
      # would otherwise disable loopback mapping), and --map-host-loopback
      # translates the endpoint host (cfg.litellm.address) to the host's
      # 127.0.0.1, where the loopback-only LiteLLM proxy listens. runsc's own
      # netstack then just routes the endpoint address to the tap; pasta does
      # the translation. See ./litellm-endpoint.nix for why the old member-less
      # bridge could not work.
      AGENT_SANDBOX_NETWORK = "pasta:--map-gw,--map-host-loopback,${cfg.litellm.address}";
    }
    // lib.optionalAttrs (cfg.defaultCommand != null) {
      AGENT_SANDBOX_DEFAULT_COMMAND = cfg.defaultCommand;
    };

  withSessionEnv =
    pkg:
    if sessionEnv == { } then
      pkg
    else
      pkgs.runCommand "agent-session-configured"
        {
          nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
          meta = (pkg.meta or { }) // {
            mainProgram = "agent-session";
          };
        }
        ''
          makeWrapper ${pkg}/bin/agent-session $out/bin/agent-session \
            ${lib.concatStringsSep " \\\n            " (
              lib.mapAttrsToList (n: v: "--set-default ${n} ${lib.escapeShellArg v}") sessionEnv
            )}
        '';
in
{
  imports = [ ./litellm-endpoint.nix ];

  options.myconfig.ai.gvisor-agent-sandbox = with lib; {
    enable = mkEnableOption "myconfig.ai.gvisor-agent-sandbox";

    package = mkOption {
      type = types.package;
      default = pkgs.agent-session;
      defaultText = literalExpression "pkgs.agent-session";
      description = "The `agent-session` session manager package.";
    };

    image = mkOption {
      type = types.nullOr types.package;
      default = pkgs.agent-sandbox-image;
      defaultText = literalExpression "pkgs.agent-sandbox-image";
      description = ''
        Nix-built OCI image used as sandbox base. Set to `null` to manage
        images entirely outside this module (then no
        `agent-sandbox-load-image` is installed either).
      '';
    };

    extraImagePackages = mkOption {
      type = types.listOf types.package;
      default = enabledAgentPackages ++ lib.optional herdrEnabled pkgs.herdr;
      defaultText = literalExpression ''
        the packages of the coding agents enabled on this host, i.e. one entry
        per set `myconfig.ai.<pi-coding-agent|opencode|claude-code|codex|github-copilot-cli|qwen-code>.enable`,
        plus `pkgs.herdr` when any of them is enabled
      '';
      example = literalExpression "[ pkgs.claude-code ]";
      description = ''
        Extra packages baked into `image` — by default the coding-agent CLIs
        that are enabled on this host, so the sandbox ships the same agents as
        the host. Set explicitly to slim the image down or to add tooling.

        The upstream image deliberately ships no agent CLI; host binaries must
        not be bind-mounted, since that would drag the host `/nix` store into
        the sandbox.
      '';
    };

    home = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Seed every new session's `/home/agent` from the home-manager
          generation that is currently activated for the calling user, so an
          agent finds its own configuration (skills, prompts, settings) inside
          the sandbox.

          The generation is resolved by `agent-session` at RUNTIME (via
          `~/.local/state/home-manager/gcroots/current-home/home-files` and
          the legacy profile locations), not baked into the image: the session
          home is bind-mounted over `/home/agent`, so anything the image
          carried there would be masked anyway.

          Files are copied dereferenced, because the sandbox has no `/nix`.
          Configuration whose *content* references `/nix/store` paths still
          dangles inside the sandbox.
        '';
      };

      seedPaths = mkOption {
        type = types.listOf types.str;
        default = [
          ".agents"
          ".agignore"
          ".claude"
          ".codex"
          ".pi"
          ".config/git"
          ".config/herdr"
          ".config/opencode"
        ];
        description = ''
          Paths, relative to the home-manager `home-files` tree, copied into a
          new session home. Deliberately an ALLOWLIST: that tree also contains
          `.ssh`, mail and browser configuration, none of which an untrusted
          agent should see. Only add paths that carry no credentials —
          everything copied here is readable inside the sandbox and can be
          exfiltrated by a hostile agent over the network.
        '';
      };

      rewriteEndpoints = mkOption {
        type = types.listOf types.str;
        default = lib.optionals cfg.litellm.enable [
          "http://127.0.0.1:${toString cfg.litellm.port}=${litellmBase}"
          "http://localhost:${toString cfg.litellm.port}=${litellmBase}"
        ];
        defaultText = literalExpression ''
          rules pointing the host's loopback LiteLLM URLs at
          `myconfig.ai.gvisor-agent-sandbox.litellm.address`
        '';
        example = [ "http://127.0.0.1:8080=http://192.168.84.1:8080" ];
        description = ''
          `OLD=NEW` rules applied literally to the seeded files after copying.

          The host's configuration points at the loopback-only LiteLLM proxy
          (`http://127.0.0.1:4000/v1`), which does not exist inside a sandbox:
          there, `127.0.0.1` is the container's own loopback, so every seeded
          agent config would fail with a connection error. These rules rewrite
          such URLs to the endpoint address from `./litellm-endpoint.nix`,
          which pasta maps to the host loopback so a sandbox can reach it.

          Set to `[ ]` to copy the configuration verbatim.
        '';
      };
    };

    defaultCommand = mkOption {
      type = types.nullOr types.str;
      default = if herdrEnabled then "herdr" else null;
      defaultText = literalExpression ''"herdr" when any coding agent is enabled, else null'';
      example = "/bin/bash";
      description = ''
        Command `agent-session start` / `run` execute when no `-- COMMAND` is
        given, i.e. the session's entrypoint. Word-split, so `"herdr --flag"`
        works. `null` keeps the upstream default (`/bin/bash`).

        Defaults to `herdr`, the agent multiplexer, whenever the image carries
        at least one coding agent — so a bare `agent-session start` drops you
        into the multiplexer instead of a plain shell.
        `agent-session shell` is unaffected and always gives a shell.
      '';
    };

    runtime = mkOption {
      type = types.str;
      default = "runsc";
      description = "Name under which gVisor is registered as Podman OCI runtime.";
    };

    users = mkOption {
      type = types.listOf types.str;
      default = [ myconfig.user ];
      defaultText = literalExpression "[ myconfig.user ]";
      example = [ "alice" ];
      description = ''
        Users that launch sessions. They get subordinate UID/GID ranges,
        which rootless Podman requires.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.all (p: p != "" && !lib.hasPrefix "/" p && !lib.hasInfix ".." p) cfg.home.seedPaths;
        message = ''
          myconfig.ai.gvisor-agent-sandbox.home.seedPaths must contain only
          non-empty, relative paths without "..":
          ${lib.concatStringsSep ", " cfg.home.seedPaths}
        '';
      }
    ];

    nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

    virtualisation.podman.enable = true;

    # Register gVisor as an alternate OCI runtime. `agent-session` selects it
    # explicitly, so ordinary Podman containers keep Podman's default runtime.
    virtualisation.containers.containersConf.settings.engine.runtimes.${cfg.runtime} = [
      "${pkgs.gvisor}/bin/runsc"
    ];

    home-manager.sharedModules = [
      {
        home.packages = [
          (withSessionEnv (withImage cfg.package))
          pkgs.gvisor
        ]
        ++ lib.optional (image != null) (withImage pkgs.agent-sandbox-load-image);
      }
    ];

    # Rootless Podman needs subordinate ID mappings for these users.
    users.users = lib.genAttrs cfg.users (_: {
      autoSubUidGidRange = true;
    });
  };
}
