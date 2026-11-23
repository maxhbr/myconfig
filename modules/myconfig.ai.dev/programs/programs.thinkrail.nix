# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# thinkrail (https://github.com/JetBrains/thinkrail) — the agentic IDE built
# around the pi coding agent, packaged in ../../../pkgs/thinkrail (the
# official prebuilt Bun-compiled binary).
#
# Inside mysbx sandboxes the interesting payload is the engine host's web
# UI: `thinkrail` boots an HTTP+WS server on localhost:24242
# (`THINKRAIL_HOST`/`THINKRAIL_PORT` override) and the operator drives it
# from a host browser. That endpoint is reachable from the host only on
# the shared-netns backends (`bubblewrap`, `nono`): the VM backends
# (krun's TSI, microvm-qemu's SLiRP) proxy outbound connections only and
# have no inbound port mapping, so a sandboxed `thinkrail` there serves a
# UI nothing outside the guest can open — the same network-shape
# limitation `orca` has (../../../mysbx/docs/design/config.md D17).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  osconfig = config;

  # The shared mysbx sandbox-config helper (the dereferenced-store-tree
  # machinery, bd myconfig-ooh), via the mysbx flake's lib output.
  sandboxConfigLib = inputs.mysbx.lib.sandboxConfigLib {
    inherit lib;
    runCommand = pkgs.runCommand;
  };

  # ThinkRail embeds the pi engine host
  # (packages/server/src/agent/agentSessionManager.ts builds pi's
  # DefaultResourceLoader with `agentDir = getAgentDir()`, i.e.
  # `~/.pi/agent`), so inside a sandbox it reads the same configuration
  # pi itself does: the generated myconfig providers/litellm extensions,
  # skills, agents and prompts. The subtrees are the SHARED list
  # (../pi-sandbox-subtrees.nix, also consumed by
  # ./programs.pi-coding-agent/default.nix), never a private copy: both
  # would bind the same in-sandbox dests, and mysbx lets a later bind
  # win (../../../mysbx/mysbx-rs/src/nono.rs), so two lists would silently
  # hand the payload a mixture of two store trees ordered by module
  # merge order.
  mysbxThinkrailSubtrees = (import ./pi-sandbox-subtrees.nix { inherit config lib pkgs; }).subtrees;
  # The gate/path/content of the sandbox mcp.json mount, from the same
  # shared generator the subtrees list gates on — `subtrees` and the
  # `extraFiles` entry below cannot drift.
  piMcp = import ./pi-mcp-servers.nix { inherit config lib pkgs; };

  # Like pi's integration: copies of the home-manager-deployed config,
  # NOT the host home (the podman-gvisor backend mounts nothing from the
  # host store). Referenced only under `mkIf` below — Nix is lazy, and no
  # subtree can match the `.config/mysbx` prefix the mysbx module
  # renders its own file under.
  #
  # What is deliberately NOT mounted: pi's own state files below
  # `~/.pi/agent` (`settings.json`, `models-store.json`, `auth.json`) —
  # the embedded engine creates them in the sandbox's tmpfs home on
  # first start, exactly like pi's own integration does not mount them
  # (credentials and per-install state never reach the sidecar).
  hmSkillsBundle =
    let
      hm = config.home-manager.users.mhuber.programs.agent-skills or null;
    in
    if hm == null || !hm.enable then
      { }
    else if !(hm.targetBundlePaths ? agents) then
      { }
    else
      { ".agents/skills" = hm.targetBundlePaths.agents; };

  mysbxThinkrailMounts =
    (sandboxConfigLib.mkSandboxConfig {
      homeFile = config.home-manager.users.mhuber.home.file;
      homeDirectory = config.home-manager.users.mhuber.home.homeDirectory;
      subtrees = mysbxThinkrailSubtrees;
      name = "thinkrail-sandbox-config";
      # pi's mcp.json rides `extraFiles`, exactly like pi's own
      # integration (bd myconfig-9y4c): the host copy stays
      # runtime-managed, the sandbox gets the rendered registry.
      extraFiles = (lib.optionalAttrs piMcp.enable { ${piMcp.path} = piMcp.json; }) // hmSkillsBundle;
    }).mounts;

  # pi's own mysbx integration mounts the SAME subtrees (`mysbxPiMounts`
  # in ./programs.pi-coding-agent/default.nix, built from the shared
  # list). Both enabled means both would bind the same in-sandbox dests
  # from two distinct store trees; mysbx keeps the LAST bind
  # (../../../mysbx/mysbx-rs/src/nono.rs "a later bind on the same dest
  # wins"), so the payload would get a mixture ordered by module merge
  # order. pi owns `~/.pi` when it is enabled — its tree is the one that
  # also carries the TUI-only `keybindings.json` and consumes
  # `PI_JAIL_MARKER`/session state — so thinkrail then contributes NO
  # pi-config mounts and relies on pi's. Only on a host without pi does
  # thinkrail mount the pi config itself, or the embedded engine would
  # start with no configuration at all.
  piOwnsPiMounts = (osconfig.myconfig.ai.dev.pi-coding-agent.enable or false);

  # The mirror image for the shared skills dest: when pi is off but codex
  # is on, codex's integration already binds `~/.agents/skills` from its
  # own tree (../../programs.codex/default.nix gates that mount on pi for
  # the same single-writer reason). ThinkRail reads the dest, so it does
  # not own a second bind of it.
  mysbxThinkrailOwnMounts = lib.optionals (!piOwnsPiMounts) (
    builtins.filter (
      m: !((osconfig.myconfig.ai.dev.codex.enable or false) && m.dest == "/mysbx-home/.agents/skills")
    ) mysbxThinkrailMounts
  );
in
{
  options.myconfig = with lib; {
    ai.dev.thinkrail = {
      enable = mkEnableOption "myconfig.ai.dev.thinkrail";
      package = mkPackageOption pkgs "thinkrail" { };
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.thinkrail.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ osconfig.myconfig.ai.dev.thinkrail.package ];
        # Host-side app state (sessions, workspace registry). The mysbx
        # integration persists `.thinkrail` only inside the sandbox sidecar
        # (`stateDirs` above); this entry covers thinkrail runs on the host
        # itself, which otherwise lose their state on impermanence hosts.
        myconfig.persistence.directories = [ ".thinkrail" ];
      }
    ];

    # mysbx tier integration (see `mysbxThinkrailMounts` above), gated on
    # mysbx being enabled too: the two features are independent. Same
    # shape as the pi integration
    # (../programs.pi-coding-agent/default.nix), minus the jail marker
    # (a ThinkRail session has no TUI border to color).
    myconfig.ai.dev.mysbx = lib.mkIf osconfig.myconfig.ai.dev.mysbx.enable {
      # The `thinkrail` binary on the sandbox PATH. The 24242 web UI is
      # host-reachable on the shared-netns backends only — see the
      # module header.
      extraTools = [ osconfig.myconfig.ai.dev.thinkrail.package ];
      # ThinkRail reads pi's mounted configuration (see
      # `mysbxThinkrailMounts` and `piOwnsPiMounts` above) but writes its
      # own state nowhere else: `~/.thinkrail` (persisted via the
      # state-dirs entry below and, on the host, via
      # myconfig.persistence.directories) holds the
      # app state, and per-workspace scratch lives in the repo's
      # `.thinkrail/` (the workspace mount, not sandbox home). The pi
      # session store `.pi/agent/sessions` is NOT persisted here: pi's
      # own integration owns that entry when pi is enabled, and
      # `state-dirs` may not nest — a second declaration would be a
      # `StateDirNesting` refusal.
      config.mounts = mysbxThinkrailOwnMounts;
      config.stateDirs = [ ".thinkrail" ];
      # Mute upstream analytics inside sandboxes
      # (packages/server/src/analytics/service.ts: any non-empty value
      # makes the host skip the analytics sink). A sandboxed session is
      # headless operator tooling — no telemetry egress beyond the work
      # itself.
      config.env.THINKRAIL_NO_ANALYTICS = "1";
      # The litellm endpoint of the podman-gvisor backend: the mounted
      # provider extension names the host loopback, which inside a
      # container is the container's own. The extension reads
      # `MYCONFIG_LITELLM_BASE_URL` (see the pi module's
      # `litellmBaseUrlEnvVar`), so the same override serves the embedded
      # engine. `podman.env` reaches that backend ALONE; the bubblewrap
      # backend shares the host netns, where the baked URL is right.
      podman.env = lib.mkIf osconfig.myconfig.ai.dev.litellm-forwarder.enable {
        MYCONFIG_LITELLM_BASE_URL = osconfig.myconfig.ai.dev.litellm-forwarder.endpoint;
      };
      # The litellm endpoint of the microvm-qemu backend (bd
      # myconfig-mzys): the SLiRP gateway `10.0.2.2` maps to the host
      # loopback. (The backend has no inbound mapping, so the web UI is
      # sandbox-local there — see the module header.)
      microvm-qemu.env = lib.mkIf osconfig.services.litellm.enable {
        MYCONFIG_LITELLM_BASE_URL = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };
      # The microvm-ch backend's passt guest uses the same address plan.
      microvm-ch.env = lib.mkIf osconfig.services.litellm.enable {
        MYCONFIG_LITELLM_BASE_URL = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };
    };
  };
}
