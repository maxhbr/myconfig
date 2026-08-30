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
# The sources under ./nix (and the historical bash CLI that lived in ./bin,
# deleted in favor of the Rust rewrite) are vendored from
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
  # Sandbox-facing base URL: the address the sandbox connects to, on the
  # forwarder port (NOT the LiteLLM port — the forwarder proxies to that).
  # See ./litellm-endpoint.nix for the mechanism.
  litellmBase = "http://${cfg.litellm.address}:${toString cfg.litellm.forwardPort}";

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
  # also changes the default image reference baked into `agent-gvisor`.
  withImage = pkg: if image == null then pkg else pkg.override { agent-gvisor-image = image; };

  # Host-configured defaults baked into `agent-gvisor`:
  #   * the home-seed allowlist and endpoint-rewrite rules. The seed SOURCE is
  #     resolved by the script at runtime (the activated home-manager
  #     generation of the calling user), so a dotfile change needs no rebuild
  #     here; only *which* paths are copied, and how host-only URLs are
  #     rewritten, is a build-time decision.
  #   * the model endpoint, so `agent-gvisor doctor` can probe it from INSIDE
  #     a sandbox — the only place where the answer means anything — and the
  #     pasta network spec that makes that endpoint reachable (see
  #     ./litellm-endpoint.nix).
  # `--set-default` keeps every variable overridable per invocation.
  sessionEnv =
    lib.optionalAttrs cfg.home.enable {
      AGENT_GVISOR_HOME_SEED_PATHS = lib.concatStringsSep " " cfg.home.seedPaths;
      AGENT_GVISOR_HOME_SEED_REWRITE = lib.concatStringsSep " " cfg.home.rewriteEndpoints;
    }
    // lib.optionalAttrs cfg.litellm.enable {
      AGENT_GVISOR_MODEL_ENDPOINT = cfg.litellm.endpoint;
      # pasta(1) network spec: --map-guest-addr translates the endpoint host
      # (cfg.litellm.address) to the host's global address (the address on the
      # default-route interface), where the port-scoped forwarder listens on
      # cfg.litellm.forwardPort. Unlike the old --map-host-loopback (which
      # translated to 127.0.0.1, exposing every loopback port), --map-guest-addr
      # maps to the host's global address, so loopback-ONLY services stay
      # unreachable. Podman's default --no-map-gw applies (no gateway→loopback
      # mapping). See ./litellm-endpoint.nix for the full mechanism and why
      # --map-gw was dropped.
      AGENT_GVISOR_NETWORK = "pasta:--map-guest-addr,${cfg.litellm.address}";
    }
    // lib.optionalAttrs (cfg.litellm.enable && cfg.litellm.loopbackForward) {
      # Relay the endpoint onto the sandbox's own loopback, from inside the
      # sandbox (the only place that can bind a port gVisor's netstack serves).
      # Makes `http://127.0.0.1:<litellm port>` work verbatim in the sandbox.
      # See ./litellm-endpoint.nix (option `litellm.loopbackForward`).
      AGENT_GVISOR_LOOPBACK_FORWARD = "${toString cfg.litellm.port}:${cfg.litellm.address}:${toString cfg.litellm.forwardPort}";
    }
    // lib.optionalAttrs (cfg.defaultCommand != null) {
      AGENT_GVISOR_DEFAULT_COMMAND = cfg.defaultCommand;
    };

  withSessionEnv =
    pkg:
    if sessionEnv == { } then
      pkg
    else
      pkgs.runCommand "agent-gvisor-configured"
        {
          nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
          meta = (pkg.meta or { }) // {
            mainProgram = "agent-gvisor";
          };
        }
        ''
          makeWrapper ${pkg}/bin/agent-gvisor $out/bin/agent-gvisor \
            ${lib.concatStringsSep " \\\n            " (
              lib.mapAttrsToList (n: v: "--set-default ${n} ${lib.escapeShellArg v}") sessionEnv
            )}

          # Carry the package payload besides the binary: the fish tab
          # completion (share/fish/vendor_completions.d) must survive the
          # re-wrap, or hosts that bake sessionEnv defaults would lose it.
          ln -s ${pkg}/share $out/share
        '';
in
{
  imports = [ ./litellm-endpoint.nix ];

  options.myconfig.ai.gvisor-agent-sandbox = with lib; {
    enable = mkEnableOption "myconfig.ai.gvisor-agent-sandbox";

    package = mkOption {
      type = types.package;
      default = pkgs.agent-gvisor;
      defaultText = literalExpression "pkgs.agent-gvisor";
      description = "The `agent-gvisor` session manager package.";
    };

    image = mkOption {
      type = types.nullOr types.package;
      default = pkgs.agent-gvisor-image;
      defaultText = literalExpression "pkgs.agent-gvisor-image";
      description = ''
        Nix-built OCI image used as sandbox base. Set to `null` to manage
        images entirely outside this module (then no
        `agent-gvisor-load-image` is installed either).
      '';
    };

    extraImagePackages = mkOption {
      type = types.listOf types.package;
      default =
        enabledAgentPackages
        ++ lib.optional herdrEnabled pkgs.herdr
        # Shared sandbox tooling (see ../myconfig.ai.sandboxTools.nix).
        ++ config.myconfig.ai.sandboxTools.extraPackages;
      defaultText = literalExpression ''
        the packages of the coding agents enabled on this host, i.e. one entry
        per set `myconfig.ai.<pi-coding-agent|opencode|claude-code|codex|github-copilot-cli|qwen-code>.enable`,
        plus `pkgs.herdr` when any of them is enabled,
        plus `myconfig.ai.sandboxTools.extraPackages`
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

          The generation is resolved by `agent-gvisor` at RUNTIME (via
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
          "http://127.0.0.1:${toString cfg.litellm.forwardPort}=${litellmBase}"
          "http://localhost:${toString cfg.litellm.forwardPort}=${litellmBase}"
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
          such URLs (and the forwarder port) to the endpoint address + forward
          port from `./litellm-endpoint.nix`, which pasta maps to the host's
          global address so a sandbox can reach the port-scoped forwarder.

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
        Command `agent-gvisor start` / `run` execute when no `-- COMMAND` is
        given, i.e. the session's entrypoint. Word-split, so `"herdr --flag"`
        works. `null` keeps the upstream default (`/bin/bash`).

        Defaults to `herdr`, the agent multiplexer, whenever the image carries
        at least one coding agent — so a bare `agent-gvisor start` drops you
        into the multiplexer instead of a plain shell.
        `agent-gvisor shell` is unaffected and always gives a shell.
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

    # Register gVisor as an alternate OCI runtime. `agent-gvisor` selects it
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
        ++ lib.optional (image != null) (withImage pkgs.agent-gvisor-load-image);
      }
    ];

    # Rootless Podman needs subordinate ID mappings for these users.
    users.users = lib.genAttrs cfg.users (_: {
      autoSubUidGidRange = true;
    });
  };
}
