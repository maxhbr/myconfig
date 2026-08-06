# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — host microvm.nix integration, the fixed VM slot pool,
# and the minimal Cloud Hypervisor NixOS guest module.
#
# PHASE 2 (plan §2, §4, §5, §6):
#
#   §2  When the feature is enabled, import the microvm.nix *host* module
#       (`inputs.microvm.nixosModules.host`). The host module is imported
#       unconditionally so its `microvm.*` options always exist, but it is
#       neutralized with `microvm.host.enable = lib.mkDefault false` — exactly
#       the load-bearing gating pattern used by
#       `modules/myconfig.ai/hermes-agent/microvm.nix`. Only when
#       `myconfig.ai.microvm.enable` is true do we flip it to `true`, so a
#       disabled feature has zero config side effects (no tap/vhost_net
#       modules, no KSM, no `microvm` user).
#
#   §4  A fixed, declarative pool of microVM slots agent-0 .. agent-<n-1> with
#       deterministic per-slot name / hostname / MAC / IPv4 / TAP interface,
#       all generated from the slot index (never random).
#
#   §5  A MINIMAL Cloud Hypervisor NixOS guest: its own kernel (microvm.nix
#       builds a self-contained guest store disk — the host /nix/store is NOT
#       shared), cloud-hypervisor hypervisor, vcpu/mem from the module
#       options, serial console, graphics disabled, guest hostname = slot
#       name, current stateVersion.
#
#   §6  An unprivileged guest `agent` user (uid 1000, home /home/agent, no
#       extra groups, locked password, not root).
#
# DEFERRED to later phases (still inert stubs):
#   - network.nix  (plan §12–§16): private bridge, firewall, LiteLLM
#     forwarder, guest IP addressing / default route. Slots below therefore
#     declare a deterministic MAC + TAP but no guest-side IP configuration —
#     the interface is wired up but not yet addressed/routable.
#   - workmux.nix  (plan §29): Workmux agent registrations.
#
# IMPLEMENTED HERE (plan §10 workspace share, §11 UID/GID ownership):
#   Each slot declares EXACTLY ONE `microvm.shares` entry — the writable
#   virtiofs `/workspace` share, whose host source is the SAME path the
#   launcher bind-mounts the standalone clone onto
#   (${stateRoot}/<slot>/workspace). The launcher (launcher.nix) chowns that
#   clone to uid/gid 1000 so it appears owned by the guest `agent` user and is
#   read-write inside the guest (§11). See ./docs/agent-microvm.md.
{
  config,
  lib,
  pkgs,
  inputs,
  myconfig,
  # The effective resource-class table (see default.nix).
  agentResourceClasses,
  # The ONE authoritative supported-agent registry instance, built in
  # default.nix (`_module.args.agentRegistry`). See ./agents.nix.
  agentRegistry,
  # The ONE definition of the per-slot SSH host-key paths (host + guest side),
  # from hostkeys.nix (`_module.args.agentHostKeys`).
  agentHostKeys,
  # The ONE definition of the batch-job format/paths plus the guest-side
  # TRUSTED controller + UNTRUSTED worker units, from job.nix
  # (`_module.args.agentJobs`).
  agentJobs,
  # The ONE definition of the task-scoped agent-state paths + the guest-side
  # linker, from state.nix (`_module.args.agentState`).
  agentState,
  # The ONE resolved network decision (profile + capabilities + DNS policy),
  # from default.nix (`_module.args.agentNetwork`). The guest-side proxy / DNS /
  # forwarder configuration below is derived from the SAME decision the host
  # firewall is rendered from, so guest and host can never disagree about what
  # the guest is allowed to reach.
  agentNetwork,
  # The ONE definition of the guest boot-time model discovery (the
  # `agent-model-config` oneshot + the env var pointing the agents at its
  # output), from guest-model-config.nix
  # (`_module.args.agentModelConfig`).
  agentModelConfig,
  # The ONE definition of the RUNTIME configuration staging (lightweight plan
  # phase 3): the per-slot host staging directory, the read-only share's guest
  # side and the root-owned guest seeding oneshot, from config-seed.nix
  # (`_module.args.agentConfigSeed`).
  agentConfigSeed,
  # The ONE definition of the per-session tree (./session.nix, lightweight plan
  # phase 4): the layout table, its guest mount points/tags and the guest
  # fragment that bind-mounts the session tree's `workspace/` to `/workspace`.
  agentSession,
  # The ONE resolved capability set (see default.nix, lightweight plan phase 5).
  # Only two things here consult it directly: the guest INTERACTIVE entry point
  # `agent-run` (which only an interactive session ever invokes) and the
  # cross-module share assertion below. The batch units/packages and the session
  # subdirectories follow ../job.nix's and ../session.nix's own per-capability
  # decisions.
  agentCapabilities,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Deterministic fixed slot pool — the single source of truth lives in
  # slots.nix and is shared with default.nix (which asserts uniqueness and
  # the slot-count bound over this exact table).
  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), so every
  # module builds the SAME pool.
  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  netCaps = agentNetwork.caps;

  # Prefix length of the private subnet (e.g. "192.168.83.0/24" -> 24), used
  # for the guest-side static address. Derived from the SAME option the host
  # bridge uses, so host and guest agree.
  guestPrefixLength = lib.toInt (lib.last (lib.splitString "/" cfg.subnet));

  # The model-endpoint environment EVERY guest agent path must see: the
  # loopback LiteLLM forwarder address (127.0.0.1:<litellmPort>) plus the
  # per-agent endpoint plumbing from the registry (e.g. hermes'
  # OPENROUTER_BASE_URL), and — under `package-access` only — the package
  # proxy variables. Placeholder keys only (§17): the real upstream credential
  # never leaves the host LiteLLM proxy.
  #
  # This is the SINGLE source of truth consumed by BOTH agent entry paths:
  #   * the interactive LOGIN shell, via `environment.variables` (NixOS writes
  #     those to /etc/set-environment, which /etc/profile sources); and
  #   * the non-login batch WORKER unit, via its `environment=` below.
  # The batch worker (`agent-job-worker@<agent>.service`) is a systemd oneshot
  # that does NOT source /etc/profile, and NixOS does NOT inject
  # `environment.variables` into systemd's `DefaultEnvironment`, so without an
  # explicit `environment=` the worker would inherit only PATH — and pi/codex/
  # hermes batch jobs would have no model endpoint at all (the worker died ~2s
  # in for exactly this reason). Giving the worker the SAME attrset keeps the
  # two paths from drifting: there is one place that decides the endpoint, not
  # two.
  modelEndpointEnv = {
    OPENAI_BASE_URL = "http://127.0.0.1:${toString cfg.litellmPort}/v1";
    OPENAI_API_KEY = "not-needed";
    ANTHROPIC_BASE_URL = "http://127.0.0.1:${toString cfg.litellmPort}";
    ANTHROPIC_API_KEY = "not-needed";
  }
  // agentRegistry.guestEnvironment
  # Points opencode at the boot-time-rendered overlay config carrying the LIVE
  # model list of the host LiteLLM proxy (see guest-model-config.nix). Empty
  # when that discovery is disabled or the profile forbids the model API.
  // agentModelConfig.guestEnvironment
  // lib.optionalAttrs netCaps.packageProxy (
    let
      proxyUrl = "http://${cfg.gatewayAddress}:${toString cfg.packageProxyPort}";
    in
    {
      http_proxy = proxyUrl;
      https_proxy = proxyUrl;
      HTTP_PROXY = proxyUrl;
      HTTPS_PROXY = proxyUrl;
      no_proxy = "127.0.0.1,localhost";
      NO_PROXY = "127.0.0.1,localhost";
    }
  );

  # --- §19 guest agent entry point (`agent-run`) --------------------------
  # Refuses root, verifies /workspace is a mounted, writable mount, cds into
  # it, prints the guest identity + selected agent, then execs the selected
  # agent (no `eval`, so the exit status is the agent's own).
  #
  # The agent name is dispatched through a table GENERATED from ./agents.nix,
  # so the guest accepts exactly the agents the registry declares (and applies
  # their `interactiveArgs`). Extra argv after the agent name is forwarded.
  agent-run = pkgs.writeShellApplication {
    name = "agent-run";
    runtimeInputs = with pkgs; [
      coreutils
      util-linux # findmnt
    ];
    text = ''
      set -euo pipefail

      if [[ "$(id -u)" -eq 0 ]]; then
          echo "agent-run: refusing to run as root" >&2
          exit 1
      fi
      if [[ "$#" -lt 1 ]]; then
          echo "usage: agent-run <${agentRegistry.namesAlternation}> [args...]" >&2
          exit 2
      fi

      workspace=/workspace
      if ! findmnt -n -- "$workspace" >/dev/null 2>&1; then
          echo "agent-run: $workspace is not a mount point" >&2
          exit 1
      fi
      if [[ ! -w "$workspace" ]]; then
          echo "agent-run: $workspace is not writable" >&2
          exit 1
      fi

      cd "$workspace"
      printf 'agent-run: host=%s agent=%s workspace=%s\n' \
          "$(uname -n)" "$1" "$workspace" >&2

      # Generated dispatch table (single source of truth: ./agents.nix).
      agent="$1"
      shift
      case "$agent" in
      ${agentRegistry.guestDispatchCases}
          *)
              echo "agent-run: unknown agent '$agent' (expected: ${agentRegistry.namesAlternation})" >&2
              exit 2
              ;;
      esac
    '';
    meta = with lib; {
      description = "Guest-side agent entry point for myconfig.ai.microvm sandboxes";
      platforms = platforms.linux;
    };
  };

  # --- guest toolchain (plan §7, lightweight plan phase 8) ----------------
  # The interactive login shell of the guest `agent` user: plain bash. The guest
  # deliberately does NOT ship fish (the host primary user's shell) any more —
  # that dropped the fish closure and the `programs.fish` machinery from every
  # guest, and nothing in the guest configures fish since guest home-manager
  # provisioning was replaced by launch-time config staging.
  guestShell = pkgs.bashInteractive;

  # Generic CLI toolset available to the agent inside the guest. Everything the
  # module's OWN guest scripts need is already in their `writeShellApplication`
  # `runtimeInputs`, so this set exists purely for the agent and for a human
  # debugging a session over SSH.
  #
  # THE set (lightweight plan phase 8) — every entry has a named consumer:
  #   coreutils/findutils/gnugrep/gnused/gawk  the POSIX toolbox every agent and
  #                                            every repo script assumes
  #   bash                                     non-login script interpreter
  #                                            (`#!/usr/bin/env bash`)
  #   git                                      the workspace IS a git clone
  #   diffutils, patch                         agents produce/apply patches
  #   ripgrep, jq                              code/JSON inspection the agents
  #                                            invoke directly
  #   less                                     git's default pager
  #   openssh                                  interactive control channel
  #                                            (`enableSsh`)
  #   procps, util-linux                       the deliberately small
  #                                            troubleshooting set (ps, findmnt,
  #                                            mount, lsblk) needed to
  #                                            understand a broken session
  # Deliberately ABSENT from `guestCommonPackages`: curl, rsync, unzip, tree, fd,
  # file, which, gnumake — duplicate or workload-specific tools with no
  # in-guest consumer under the secure proxy-only profile.
  guestCommonPackages =
    with pkgs;
    [
      bash
      coreutils
      diffutils
      findutils
      gawk
      git
      gnugrep
      gnused
      jq
      less
      patch
      procps
      ripgrep
      util-linux
    ]
    ++ lib.optional cfg.enableSsh openssh;

  # --- the per-slot SHARE LIST (lightweight plan phase 4) ----------------
  # Everything the guest may WRITE lives in ONE per-session tree
  # (./session.nix): `workspace/`, `input/`, `controller/`, `worker/`,
  # `worker-logs/` and `state/`. The trust boundaries are expressed by
  # OWNERSHIP and MODES, which virtiofsd passes through unchanged: the session
  # root, `input/`, `controller/` (root-ONLY 0700) and `worker-logs/` stay
  # root-owned, only `workspace/`, `worker/` and `state/` belong to the
  # unprivileged guest `agent` user. The launcher verifies exactly that BEFORE
  # it starts the VM.
  #
  # The SECOND share is READ-ONLY (virtiofsd `--readonly`) and root-owned: the
  # slot's SSH host identity (hostkeys.nix) and the staged, allowlisted host
  # configuration (config-seed.nix). Deliberately NOT folded into the writable
  # tree — the plan itself forbids that for the host keys, and phase 3
  # established the same for the staged configuration (invariants 7 and 8
  # outrank the plan's layout sketch; see ./session.nix).
  #
  # `/workspace` itself is a BIND MOUNT of `<session>/workspace` inside the
  # guest (agentSession.guestModule), so `agent-run` and every agent keep
  # the path they already expect.
  #
  # Do NOT add further shares — no /nix, /home, host sockets or cross-slot
  # paths (plan §10). The guest keeps its own /nix/store on microvm.nix's store
  # disk, because no share source is "/nix/store".
  mkShares = slot: [
    {
      proto = "virtiofs";
      tag = agentSession.guestTag;
      source = agentSession.slotDir slot.name;
      mountPoint = agentSession.guestMountPoint;
    }
    {
      proto = "virtiofs";
      tag = agentSession.guestRoTag;
      source = agentSession.roSlotDir slot.name;
      mountPoint = agentSession.guestRoMountPoint;
      readOnly = true;
    }
  ];

  # --- minimal Cloud Hypervisor guest for a given slot --------------------
  mkGuest =
    slot:
    # NOTE: the guest runs NO home-manager activation at all. Its home is
    # provisioned at LAUNCH time from the host-staged, allowlisted copy
    # (config-seed.nix), so neither the home-manager NixOS module nor its
    # activation service is part of the guest closure.
    lib.mkMerge [
      # Unattended batch execution (ticket 4, trust-split in ticket 7): the
      # TRUSTED `agent-job-controller` oneshot (inert unless the host placed a
      # spec in the share) plus the UNTRUSTED `agent-job-worker@` template it
      # starts. Slot-independent, because the job share always appears at the
      # same guest path.
      (agentJobs.mkGuestModule slot)
      # Boot-time model discovery: query the loopback LiteLLM endpoint and
      # render the LIVE model list into pi + opencode config. Empty attrset when
      # disabled or when the profile has no model API at all.
      agentModelConfig.guestModule
      # Opt-in, task-scoped agent state (ticket 5 B): links only the DECLARED
      # directories the host prepared for this run; a run without
      # --persist-agent-state sees an empty share and keeps the disposable home.
      agentState.guestModule
      # Runtime configuration staging (lightweight plan phase 3): the root-owned
      # oneshot that copies the host-staged, allowlisted configuration into the
      # disposable home BEFORE sshd, the batch job controller and the
      # agent-state linker.
      agentConfigSeed.guestModule
      # The consolidated session share (lightweight plan phase 4): bind-mounts
      # `<session>/workspace` to `/workspace`, so `agent-run`'s findmnt +
      # writability checks and every agent's expectation of `/workspace` keep
      # working, and orders sshd after the read-only host-key mount.
      agentSession.guestModule
      # The BATCH worker (`agent-job-worker@<agent>.service`) is a non-login
      # systemd oneshot: it never sources /etc/set-environment, so the endpoint
      # vars in `environment.variables` would not reach it (NixOS puts them ONLY
      # in login profiles, not in systemd's `DefaultEnvironment`). Give it the
      # SAME endpoint environment the interactive login shell gets, so pi/codex/
      # hermes batch workers can actually reach the loopback forwarder. The
      # `creds` section of runtime-validation.sh asserts BOTH halves carry these
      # (and the negative controls stay absent) — on a host that selects `batch`;
      # on one that does not, that subtest reports the missing capability instead
      # of inspecting a unit that does not exist. Placeholder keys only (§17).
      # EMPTY without the `batch` capability — ../job.nix decides whether a
      # worker unit exists at all, so this cannot define one behind its back.
      (agentJobs.mkWorkerEnvironmentModule modelEndpointEnv)
      (mkGuestBase slot)
    ];

  mkGuestBase = slot: {
    # microvm.nix auto-imports its guest `microvm` module for VMs declared
    # via `microvm.vms.<name>.config`, so no explicit import is needed here.
    # Redundant with microvm.nix's `networking.hostName = mkDefault name`,
    # kept as an explicit, non-default assertion of the slot's identity.
    networking.hostName = slot.hostName;

    microvm = {
      hypervisor = "cloud-hypervisor";
      # Sizing comes from the slot's RESOURCE CLASS (ticket 5 A), so all slots
      # of a class are identical and prebuilt — no per-job Nix evaluation.
      vcpu = slot.vcpu;
      mem = slot.memoryMiB;

      # Deterministic per-slot TAP + MAC (plan §4). Guest-side IP
      # addressing / routing is intentionally deferred to network.nix.
      interfaces = [
        {
          type = "tap";
          id = slot.tap;
          mac = slot.mac;
        }
      ];

      # No graphics for a headless agent sandbox (also microvm's default).
      graphics.enable = false;

      # The per-slot share list (see `mkShares` in the `let` above, which is
      # also what the phase-4 assertions below inspect).
      shares = mkShares slot;

      # --- pinned guest store disk (lightweight plan phase 1) -------------
      # PIN microvm.nix's closure/startup optimizations and the store-disk
      # filesystem instead of inheriting upstream defaults, so a future
      # microvm.nix release cannot silently give the guest documentation, a
      # non-systemd initrd, network-wait-online or a slower store image. Both
      # values happen to be microvm.nix's current defaults; pinning them is
      # about keeping them that way.
      optimize.enable = true;
      storeDiskType = "erofs";
    };

    # Unprivileged guest user that runs the agent (plan §6). Not root; no
    # wheel/docker/kvm/host groups; login disabled (`!` = locked password).
    users.users.agent = {
      isNormalUser = true;
      uid = cfg.guestAgentUid;
      home = "/home/agent";
      createHome = true;
      extraGroups = [ ];
      hashedPassword = "!";
      shell = guestShell;
    }
    // lib.optionalAttrs (cfg.enableSsh && cfg.sshPublicKeyFile != null) {
      openssh.authorizedKeys = {
        # §18: the dedicated public key authorises the guest `agent` user.
        # NOT a host authorized_keys file.
        keyFiles = [ cfg.sshPublicKeyFile ];
        # When passwordlessControl is on, ALSO authorise the host operator's
        # own declared public keys, so `agent-microvm ssh <slot>` works
        # without sudo using the operator's default `~/.ssh/id_*` identity:
        # a non-root operator cannot read the root:root 0400 dedicated
        # private key, so the launcher passes no `-i` and ssh falls back to
        # the operator's own key, which the guest now accepts.
        #
        # Deliberate, opt-in relaxation of §18's "exactly one dedicated key":
        # these are the ALREADY-TRUSTED host operator's PUBLIC keys (never a
        # credential handed to the untrusted guest), added only when the
        # operator explicitly opts into passwordless control. `config` here
        # is the HOST config, so this reads the host `myconfig.user`'s keys.
        keys =
          lib.optionals cfg.passwordlessControl
            config.users.users.${myconfig.user}.openssh.authorizedKeys.keys;
      };
    };

    # --- §7 minimal guest toolchain + the §19 entry point ----------------
    # A deliberately small package set plus the registry's agent binaries so
    # `agent-run <agent>` can exec them inside the guest. The agent packages
    # come from ./agents.nix — the SAME repo package attrs the host
    # coding-agent modules use (programs.claude-code → pkgs.claude-code,
    # programs.codex → pkgs.codex, programs.opencode → pkgs.opencode,
    # programs.pi-coding-agent → pkgs.nixos-unstable.pi-coding-agent). They
    # are baked into the immutable guest closure (§8: no runtime CLI
    # download, no host Nix daemon).
    # The guest INTERACTIVE entry point `agent-run`: `agent-microvm run --attach`
    # execs it over SSH, and a human debugging a session runs it by hand. A
    # batch-only guest has neither path (the worker resolves the agent from the
    # registry itself), so it does not carry it.
    environment.systemPackages =
      lib.optional agentCapabilities.interactive agent-run
      ++ [
        guestShell
      ]
      # §7 agent binaries, GENERATED from the authoritative registry — only for
      # the SELECTED agents (`enabledAgents`, lightweight plan phase 2), together
      # with whatever extra runtime each of them declares.
      ++ agentRegistry.packages
      ++ agentRegistry.extraPackages
      ++ guestCommonPackages;

    # NixOS' `environment.defaultPackages` (perl, rsync, strace) is a
    # convenience set for interactive general-purpose machines. None of it has
    # an in-guest consumer — the module's own guest scripts carry their runtime
    # closures explicitly, and the agent gets the documented toolset above — so
    # drop it rather than ship perl in a single-purpose sandbox image. NixOS'
    # `requiredPackages` (coreutils-full, curl, openssh, ...) is NOT affected:
    # it is load-bearing for a bootable system.
    environment.defaultPackages = [ ];

    # --- §17 guest model-API config (no secrets) -------------------------
    # The guest reaches the model API ONLY via the bridge-only LiteLLM
    # forwarder. No real upstream key is ever placed in the guest.
    #
    # Canonical guest endpoint is loopback 127.0.0.1:<litellmPort> (see the
    # guest-side forwarder below), NOT the bridge gateway directly: the coding
    # agents provisioned from the host primary user's dotfiles
    # (~/.pi/agent/extensions/myconfig-providers.ts, ~/.config/opencode/
    # opencode.json, ...) hardcode `http://127.0.0.1:<litellmPort>/v1` — the
    # host's own loopback LiteLLM address. Presenting the SAME loopback
    # endpoint inside the guest lets those copied configs work verbatim.
    #
    # Per-agent env coverage of the four guest agents:
    #   - pi / opencode read the OpenAI-compatible LiteLLM route from their
    #     copied dotfiles AND honour OPENAI_BASE_URL / OPENAI_API_KEY.
    #   - codex has no litellm provider in its copied config.toml and falls
    #     back to its built-in `openai` provider, which reads
    #     OPENAI_BASE_URL / OPENAI_API_KEY (the same vars the host bwrap
    #     wrapper forwards).
    #   - claude-code does NOT read OPENAI_*; it uses the Anthropic-native
    #     env vars ANTHROPIC_BASE_URL (root, no `/v1`; the client appends
    #     `/v1/messages`) + ANTHROPIC_API_KEY. Point it at LiteLLM's
    #     Anthropic-compatible `/v1/messages` endpoint so it too routes
    #     through the forwarder instead of the real Anthropic API (which the
    #     proxy-only firewall would block anyway).
    # All keys are placeholders — the real upstream credential lives only in
    # the host LiteLLM proxy, never in the guest (§17).
    #   - hermes reads OPENROUTER_BASE_URL (its config.yaml → CUSTOM_BASE_URL
    #     → OPENROUTER_BASE_URL → openrouter.ai fallback chain) and, for a
    #     non-openrouter base_url, OPENAI_API_KEY. That var is contributed by
    #     the hermes entry's `guestEnvironment` in ./agents.nix, i.e. by the
    #     registry rather than by a hand-maintained list here.
    #
    # `modelEndpointEnv` (defined once in the `let` above) is applied to BOTH
    # the login shell (here, via `environment.variables`) AND the non-login
    # batch worker unit (next block), so the two paths cannot drift. See its
    # header for why the worker needs an explicit `environment=`.
    environment.variables = modelEndpointEnv;

    # --- guest-side loopback → bridge LiteLLM forwarder ------------------
    # Reverse of the host's bridge-only forwarder (network.nix): a
    # socket-activated systemd-socket-proxyd inside the guest listens on
    # 127.0.0.1:<litellmPort> and forwards to <gatewayAddress>:<litellmPort>
    # (the host bridge endpoint, the ONLY reachable model-API peer under the
    # proxy-only firewall). This makes `127.0.0.1:<litellmPort>` — the address
    # every host-provisioned agent config already points at — transparently
    # reach the host LiteLLM proxy over the private bridge, so pi / opencode /
    # codex all "rely on" the host forwarder without per-agent config
    # rewrites. Pure byte-shuffler: no filesystem, home or privileges needed.
    # Only exists when the profile actually allows the model API: under
    # `offline` there is nothing to forward to, so no listener is created.
    systemd.sockets.litellm-forwarder = lib.mkIf netCaps.litellm {
      description = "Loopback LiteLLM endpoint for host-provisioned agent configs";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "127.0.0.1:${toString cfg.litellmPort}";
        Accept = false;
      };
    };
    systemd.services.litellm-forwarder = lib.mkIf netCaps.litellm {
      description = "Forward 127.0.0.1:${toString cfg.litellmPort} to the host bridge LiteLLM proxy";
      requires = [ "litellm-forwarder.socket" ];
      wants = [ "network-online.target" ];
      after = [
        "litellm-forwarder.socket"
        "network-online.target"
      ];
      serviceConfig = {
        ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd ${cfg.gatewayAddress}:${toString cfg.litellmPort}";
        DynamicUser = true;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
      };
    };

    # --- guest-side static addressing (deterministic, matches the slot) ---
    # Assign the slot's fixed IPv4 via systemd-networkd, matched on the
    # deterministic MAC so it is independent of the kernel interface name.
    # Default route points at the host bridge address (the only reachable
    # peer under the proxy-only firewall policy). IPv6 stays disabled (§15).
    # Unconditional (NOT gated on enableSsh): the guest needs its address
    # and default route to reach the LiteLLM forwarder regardless of SSH
    # (§17/§31 — model-API access is independent of SSH).
    # --- explicit DNS policy (`internet` profile only) -------------------
    # The firewall allows port 53 ONLY towards these servers, so configure the
    # guest to use exactly them. In every other profile the guest gets no
    # resolver at all (and port 53 is dropped), which is intentional.
    networking.nameservers = lib.mkIf netCaps.dns agentNetwork.dnsServers;

    systemd.network = {
      enable = true;
      networks."10-agent" = {
        matchConfig.MACAddress = slot.mac;
        address = [ "${slot.ip}/${toString guestPrefixLength}" ];
        routes = [ { Gateway = cfg.gatewayAddress; } ];
        networkConfig.LinkLocalAddressing = "no";
        linkConfig.RequiredForOnline = "no";
      };
    };

    # --- §18 hardened SSH, private guest interface only ------------------
    services.openssh = lib.mkIf cfg.enableSsh {
      enable = true;
      # Deterministic per-slot host identity (ticket 3 B): use ONLY the
      # ed25519 key from the read-only hostkey share, and do NOT let the guest
      # generate its own throwaway keys (`generateHostKeys = false` disables
      # `sshd-keygen.service`, which would anyway fail against the read-only
      # mount). The host's known_hosts file pins exactly this key per slot IP,
      # so `agent-microvm ssh` can verify the guest strictly.
      generateHostKeys = false;
      hostKeys = [
        {
          type = "ed25519";
          path = agentHostKeys.guestKeyPath;
        }
      ];
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        AllowAgentForwarding = "no";
        X11Forwarding = false;
        PermitTunnel = "no";
        AllowTcpForwarding = "no";
      };
    };

    system.stateVersion = "25.11";
  };
in
{
  # §2: the host module is imported unconditionally so `microvm.*` options
  # always exist, then neutralized below unless the feature is enabled.
  imports = [
    inputs.microvm.nixosModules.host
  ];

  config = lib.mkMerge [
    # Neutralize the upstream default `microvm.host.enable = true` so merely
    # importing this module (feature disabled) has no side effects. Matches
    # hermes-agent/microvm.nix; both set the same `mkDefault false`, which
    # merges cleanly.
    { microvm.host.enable = lib.mkDefault false; }

    (lib.mkIf cfg.enable {
      microvm.host.enable = true;

      # --- lightweight plan phase 4: the share set is a trust boundary ------
      # These are CROSS-MODULE guards: ./session.nix owns the layout, but
      # ./hostkeys.nix and ./config-seed.nix derive their own paths from it, and
      # a future edit there could silently move key material or host-decided
      # input into the writable tree. Checked here, where every path definition
      # is in scope at once.
      assertions =
        let
          slot = lib.head slots;
          shares = mkShares slot;
          writableTree = agentSession.slotDir slot.name;
          roTree = agentSession.roSlotDir slot.name;
          inside = parent: p: p == parent || lib.hasPrefix "${parent}/" p;
        in
        [
          {
            assertion = inside roTree (agentHostKeys.slotDir slot.name);
            message = ''
              myconfig.ai.microvm: the per-slot SSH host-key directory
              (${agentHostKeys.slotDir slot.name}) must live in the READ-ONLY
              session tree (${roTree}), never in the writable one
              (${writableTree}). The private host key is the slot's identity;
              the untrusted guest must not be able to reach, replace or read it.
            '';
          }
          {
            assertion = !(inside writableTree (agentConfigSeed.hostPayloadDir slot.name));
            message = ''
              myconfig.ai.microvm: the staged host agent configuration
              (${agentConfigSeed.hostPayloadDir slot.name}) must not live in the
              WRITABLE session tree (${writableTree}) — it is host-decided input
              and is exposed through the READ-ONLY share only (invariant 7).
            '';
          }
          {
            # The acceptance criterion of the phase, asserted on the ACTUAL
            # share list rather than on the intent behind it.
            assertion =
              lib.length (lib.filter (s: !(s.readOnly or false)) shares) == 1
              && lib.length (lib.filter (s: s.readOnly or false) shares) <= 1;
            message = ''
              myconfig.ai.microvm: a guest must declare
              EXACTLY ONE writable virtiofs share and at most ONE read-only
              share; slot ${slot.name} declares ${
                toString (map (s: "${s.tag}${lib.optionalString (s.readOnly or false) " (ro)"}") shares)
              }.
            '';
          }
        ];

      # --- ticket 5 C: host-side limits on the HYPERVISOR units ------------
      # Drop-ins on microvm.nix's own `microvm@<slot>` units (it uses the same
      # `overrideStrategy = "asDropin"` pattern, so the definitions merge).
      # These bound what one sandbox can take from the HOST:
      #   * MemoryMax = the class's guest RAM + `hypervisorMemoryOverheadMiB`.
      #     It must never be BELOW the guest's configured memory plus
      #     hypervisor overhead, or the VM would be OOM-killed while behaving
      #     perfectly (virtiofsd, the CH process itself and the guest's page
      #     cache all live in this cgroup).
      #   * TasksMax bounds the hypervisor's own thread/process explosion.
      #   * CPUWeight/IOWeight are RELATIVE weights (default 100): agent
      #     sandboxes deliberately yield to interactive host work rather than
      #     being hard-capped, so a long agent run cannot make the laptop
      #     unusable while still being able to use idle capacity.
      systemd.services = builtins.listToAttrs (
        map (slot: {
          name = "microvm@${slot.name}";
          value = {
            overrideStrategy = "asDropin";
            serviceConfig = {
              MemoryMax = "${toString (slot.memoryMiB + cfg.hypervisorMemoryOverheadMiB)}M";
              TasksMax = cfg.hypervisorTasksMax;
              CPUWeight = cfg.hypervisorCPUWeight;
              IOWeight = cfg.hypervisorIOWeight;
            };
          };
        }) slots
      );

      # Fixed declarative slot pool → one microvm.nix VM per slot.
      microvm.vms = builtins.listToAttrs (
        map (slot: {
          name = slot.name;
          value = {
            # Do not autostart: slots are started on demand by the launcher
            # (later phase). Keeping them off avoids booting empty sandboxes.
            autostart = false;
            config = mkGuest slot;
          };
        }) slots
      );
    })
  ];
}
