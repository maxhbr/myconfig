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
  mkGuestHome,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Deterministic fixed slot pool — the single source of truth lives in
  # slots.nix and is shared with default.nix (which asserts uniqueness and
  # the slot-count bound over this exact table).
  slots = (import ./slots.nix { inherit lib; }).mkSlots cfg.slotCount;

  # Authoritative supported-agent registry (see ./agents.nix). The guest
  # package set (§7) and the `agent-run` dispatch table (§19) below are both
  # GENERATED from it — never hand-maintain an agent list here.
  agentRegistry = import ./agents.nix { inherit lib pkgs; };

  # Prefix length of the private subnet (e.g. "192.168.83.0/24" -> 24), used
  # for the guest-side static address. Derived from the SAME option the host
  # bridge uses, so host and guest agree.
  guestPrefixLength = lib.toInt (lib.last (lib.splitString "/" cfg.subnet));

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

  # --- minimal Cloud Hypervisor guest for a given slot --------------------
  mkGuest =
    slot:
    lib.mkMerge [
      # Guest dotfile provisioning: run home-manager inside the guest for the
      # `agent` user, copying the host primary user's allowlisted shell +
      # coding-agent dotfiles (see guest-home.nix). Empty attrset when the
      # feature is disabled, so a bare guest keeps no home-manager overhead.
      { imports = [ inputs.home.nixosModules.home-manager ]; }
      (mkGuestHome { inherit pkgs; })
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
      vcpu = cfg.defaultVcpu;
      mem = cfg.defaultMemoryMiB;

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

      # --- §10 workspace share (the ONLY share) --------------------------
      # Exactly one virtiofs share surfaces the slot's host-side workspace
      # directory into the guest as the single writable `/workspace` mount.
      # Its `source` is the SAME host path the launcher uses as its
      # bind-mount target — `mount_point()` in launcher.nix renders
      # "${cfg.stateRoot}/<slot>/workspace" — so host and guest agree on the
      # one writable path (plan §10: "Slot references stable relative
      # workspace path in microVM state dir"). Read-write on purpose
      # (readOnly is left at its `false` default); the guest agent edits the
      # clone in place.
      #
      # Ownership (§11): virtiofsd passes file ownership through unchanged
      # (no --translate-uid/gid), and the launcher chowns the clone tree to
      # uid/gid 1000 on the host, so `/workspace` appears owned by the guest
      # `agent` user (uid 1000) and is writable — satisfying `agent-run`'s
      # `test -w /workspace` check. posixAcl stays at its `true` default
      # (no conflict, since no UID/GID translation is used).
      #
      # This is the ONLY share: microvm.nix keeps the guest /nix/store on its
      # own storeDisk (microvm.storeOnDisk defaults to true unless a share's
      # source is "/nix/store", which this is not), so it does NOT add a
      # store share. The guest therefore has EXACTLY this one share. Do NOT
      # add /nix, /home, host sockets or any other share here (plan §10).
      shares = [
        {
          proto = "virtiofs";
          tag = "workspace";
          source = "${cfg.stateRoot}/${slot.name}/workspace";
          mountPoint = "/workspace";
        }
      ];
    };

    # Unprivileged guest user that runs the agent (plan §6). Not root; no
    # wheel/docker/kvm/host groups; login disabled (`!` = locked password).
    users.users.agent = {
      isNormalUser = true;
      uid = 1000;
      home = "/home/agent";
      createHome = true;
      extraGroups = [ ];
      hashedPassword = "!";
      # Same interactive login shell as the host primary user.
      shell = pkgs.fish;
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
    programs.fish.enable = true;

    environment.systemPackages = [
      agent-run
      pkgs.fish
    ]
    # §7 agent binaries, GENERATED from the authoritative registry.
    ++ agentRegistry.packages
    ++ (with pkgs; [
      bash
      coreutils
      curl
      diffutils
      fd
      file
      findutils
      git
      gnugrep
      gnumake
      gnused
      jq
      less
      openssh
      patch
      procps
      ripgrep
      rsync
      tree
      unzip
      util-linux
      which
    ]);

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
    environment.variables = {
      OPENAI_BASE_URL = "http://127.0.0.1:${toString cfg.litellmPort}/v1";
      OPENAI_API_KEY = "not-needed";
      ANTHROPIC_BASE_URL = "http://127.0.0.1:${toString cfg.litellmPort}";
      ANTHROPIC_API_KEY = "not-needed";
    };

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
    systemd.sockets.litellm-forwarder = {
      description = "Loopback LiteLLM endpoint for host-provisioned agent configs";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "127.0.0.1:${toString cfg.litellmPort}";
        Accept = false;
      };
    };
    systemd.services.litellm-forwarder = {
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
