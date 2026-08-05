# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Automated EVAL / BUILD test suite for the `myconfig.ai.microvm` Cloud
# Hypervisor agent-sandbox tier (plan §38, wired per §39).
#
# IMPORTANT — these are EVAL / BUILD checks ONLY. They prove that the module
# evaluates correctly, that the deterministic slot pool is well-formed, that
# the guest closure and the shell launchers actually build (and thus pass the
# `writeShellApplication` shellcheck gate). They deliberately DO NOT — and
# cannot, from `nix flake check` — exercise KVM, boot a guest, move network
# packets, or prove the firewall actually drops traffic. Per plan §38 those
# HARDWARE KVM / NETWORK / NEGATIVE tests (§40–§44) are a separate, out-of-CI
# tier; nothing here may be read as runtime proof.
#
# This file is imported by `flake.nix` and returns an attrset of named check
# derivations, mirroring the repo's existing `checks.<system>` style
# (`pkgs.runCommand` / `pkgs.stdenv.mkDerivation` marker outputs). It is only
# wired in for `x86_64-linux`, the system the enabled reference host
# `test-f13` is built for.
#
# The checks:
#   microvm-eval-disabled     §37/§38 — defaults (disabled) ⇒ no side effects
#   microvm-eval-enabled      §37/§38 — enabled ⇒ slots + bridge-only proxy +
#                                        terminal-DROP / metadata-DROP firewall
#   microvm-slot-uniqueness   §37     — pure-eval unique/well-formed IP+MAC pool
#   microvm-eval-rejects-invalid §37  — NEGATIVE tests: the module's own
#                                        assertions actually REJECT invalid
#                                        config (slotCount bound, enableSsh key,
#                                        insecure-network acknowledgement)
#   microvm-eval-workspace-share §10/§11 — the reference guest declares EXACTLY
#                                        one virtiofs share: the writable
#                                        /workspace whose source matches the
#                                        launcher bind-mount target (crit. 12)
#   microvm-guest-evaluates   §38     — the reference guest closure evaluates to a
#                                        realisable derivation (drvPath marker)
#   microvm-launcher-shellcheck §38   — host launcher + guest `agent-run` +
#                                        workmux per-agent launchers BUILD, so
#                                        their writeShellApplication shellcheck
#                                        gate has passed
{
  self,
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  lib = inputs.nixpkgs.lib;

  # --- shared eval helpers ------------------------------------------------

  # The enabled reference host. In this flake `test-f13` IS the f13 config
  # (generated from `nixosConfigurationsGen.host-f13`), and the microvm
  # feature is enabled there via hosts/host.f13/ai.f13.nix.
  enabledCfg = self.nixosConfigurations.test-f13.config;

  # The SAME host, but with the feature force-disabled. Using extendModules
  # isolates the single variable (`enable`) so any config difference is
  # attributable purely to the feature toggle, not to a different host.
  disabledCfg =
    (self.nixosConfigurations.test-f13.extendModules {
      modules = [ { myconfig.ai.microvm.enable = lib.mkForce false; } ];
    }).config;

  microvmOpts = enabledCfg.myconfig.ai.microvm;
  # The deterministic slot table of the ENABLED reference host, from the same
  # generator the module uses.
  enabledSlots =
    (import ../modules/myconfig.ai/myconfig.ai.microvm/slots.nix { inherit lib; }).mkSlots
      resourceClasses;
  # The EFFECTIVE resource-class table of the reference host (ticket 5 A),
  # including default.nix's legacy `slotCount` migration.
  resourceClasses = self.nixosConfigurations.test-f13._module.args.agentResourceClasses;
  # The reference slot every guest-level check inspects: the first slot of the
  # first class, taken from the generated pool rather than hardcoded.
  refSlot = lib.head enabledSlots;
  gateway = microvmOpts.gatewayAddress; # 192.168.83.1
  port = toString microvmOpts.litellmPort; # 4000
  slotCount = lib.length enabledSlots;

  # Turn a list of { assertion; message; } into a marker derivation. If any
  # assertion is false the eval THROWS (so the check fails at eval time with a
  # readable message); otherwise it realises a trivial `$out` marker. This
  # mirrors NixOS' own assertion mechanism but as a standalone check drv.
  mkEvalCheck =
    name: checks:
    let
      failures = builtins.filter (c: !c.assertion) checks;
      passed = builtins.length checks - builtins.length failures;
    in
    if failures != [ ] then
      throw "${name}: ${toString (builtins.length failures)} assertion(s) failed:\n  - ${
        lib.concatMapStringsSep "\n  - " (f: f.message) failures
      }"
    else
      pkgs.runCommand name { } ''
        printf 'EVAL CHECK %s: %d/%d assertions passed\n' ${lib.escapeShellArg name} ${toString passed} ${toString passed} > "$out"
      '';

  # The evaluated guest NixOS config for a declarative slot. microvm.nix
  # stores it under `microvm.vms.<name>.config.config` (see the vms submodule
  # `config` option in the host module). declaredRunner is the CH runner drv.
  guest0Cfg = enabledCfg.microvm.vms.${refSlot.name}.config.config;

  # Look up a package by its `name`/`pname` in an evaluated systemPackages
  # list (used to fish the module's internal `let`-bound writeShellApplication
  # derivations back out of the evaluated config).
  findPkg =
    pkgList: pname:
    lib.findFirst (
      p: (p.pname or p.name or "") == pname
    ) (throw "microvm tests: package '${pname}' not found in systemPackages") pkgList;

  # --- (c) pure-eval slot-pool generator ---------------------------------
  # Import the SAME slots.nix the module uses, so this test encodes §37's
  # duplicate-IP/MAC detection against the real generator.
  slotLib = import ../modules/myconfig.ai/myconfig.ai.microvm/slots.nix { inherit lib; };

  # The SAME authoritative agent registry INSTANCE the module uses (built once
  # in default.nix and handed around via `_module.args.agentRegistry`), so the
  # checks below cannot carry their own drifting list of supported agents, nor
  # re-instantiate the registry with different context (endpoint / model).
  # (`nixosSystem` exposes module args on the TOP-LEVEL attrset, not under
  # `.config`, which strips `_module`.)
  agentRegistry = self.nixosConfigurations.test-f13._module.args.agentRegistry;

  # The ONE definition of the per-slot SSH host-key paths (hostkeys.nix), for
  # the same reason: the checks must not hardcode a second copy of them.
  hostKeys = self.nixosConfigurations.test-f13._module.args.agentHostKeys;

  # Likewise the ONE definition of the batch-job format / paths (job.nix).
  jobs = self.nixosConfigurations.test-f13._module.args.agentJobs;

  # ... and of the task-scoped agent-state paths (state.nix).
  agentStatePaths = self.nixosConfigurations.test-f13._module.args.agentState;

  # Slot counts to exercise. Includes small pools, pools with index >= 10
  # (which exercise 2-hex-digit MAC formatting, e.g. i=10 → ...:1a), and the
  # generator's declared maximum.
  # Class tables to exercise: single class (incl. the generator's declared
  # maximum), multiple classes, and pools with a global index >= 10 (which
  # exercises 2-hex-digit MAC formatting, e.g. i=10 -> ...:1a).
  slotPoolsUnderTest = [
    {
      normal = {
        count = 1;
        vcpu = 2;
        memoryMiB = 1024;
      };
    }
    {
      normal = {
        count = 4;
        vcpu = 4;
        memoryMiB = 8192;
      };
    }
    {
      small = {
        count = 2;
        vcpu = 2;
        memoryMiB = 4096;
      };
      normal = {
        count = 4;
        vcpu = 4;
        memoryMiB = 8192;
      };
      large = {
        count = 1;
        vcpu = 8;
        memoryMiB = 16384;
      };
    }
    {
      small = {
        count = 8;
        vcpu = 2;
        memoryMiB = 4096;
      };
      normal = {
        count = 8;
        vcpu = 4;
        memoryMiB = 8192;
      };
    }
    {
      normal = {
        count = slotLib.maxSlotCount;
        vcpu = 4;
        memoryMiB = 8192;
      };
    }
    # The reference host's own pool.
    resourceClasses
  ];

  ipv4Re = "([0-9]{1,3}\\.){3}[0-9]{1,3}";
  # Derive the expected MAC OUI/prefix from the REAL slots.nix generator
  # (slot 0's MAC) instead of hardcoding it, so this test can't silently
  # drift if slots.nix ever changes the OUI. The trailing `[0-9a-f]{2}`
  # bound is intentional: it would correctly fail if maxSlotCount were ever
  # raised past the single-byte MAC ceiling (255).
  macPrefix =
    builtins.substring 0 15
      (slotLib.mkSlot {
        class = "normal";
        classIndex = 0;
        globalIndex = 0;
        vcpu = 1;
        memoryMiB = 1;
      }).mac; # "02:00:00:83:00:"
  macRe = "${macPrefix}[0-9a-f]{2}";

  # Per-pool structural assertions for slot count `n`.
  slotPoolChecks =
    classes:
    let
      n = lib.foldl' (acc: c: acc + c.count) 0 (lib.attrValues classes);
      pool = slotLib.mkSlots classes;
      ips = map (s: s.ip) pool;
      macs = map (s: s.mac) pool;
      cids = map (s: s.cid) pool;
      names = map (s: s.name) pool;
      taps = map (s: s.tap) pool;
      # Names are per-class contiguous: agent-<class>-0 .. agent-<class>-<c-1>,
      # with the classes in alphabetical order.
      expectedNames = lib.concatMap (
        cn: lib.genList (i: "agent-${cn}-${toString i}") classes.${cn}.count
      ) (lib.attrNames classes);
      ipsWellFormed = builtins.all (ip: builtins.match ipv4Re ip != null) ips;
      macsWellFormed = builtins.all (mac: builtins.match macRe mac != null) macs;
      indicesContiguous = builtins.all (i: (builtins.elemAt pool i).index == i) (
        builtins.genList (i: i) n
      );
    in
    [
      {
        assertion = lib.length (lib.unique ips) == n;
        message = "slotCount=${toString n}: IPv4 addresses not unique (${toString ips})";
      }
      {
        assertion = lib.length (lib.unique macs) == n;
        message = "slotCount=${toString n}: MAC addresses not unique (${toString macs})";
      }
      {
        assertion = ipsWellFormed;
        message = "slotCount=${toString n}: an IPv4 address is malformed (${toString ips})";
      }
      {
        assertion = macsWellFormed;
        message = "slotCount=${toString n}: a MAC address is malformed (${toString macs})";
      }
      {
        assertion = names == expectedNames;
        message = "pool ${toString n}: names not the expected agent-<class>-<i> sequence (${toString names})";
      }
      {
        # Ticket 5 A: every slot needs its OWN tap, within the 15-char limit.
        assertion = lib.length (lib.unique taps) == n;
        message = "pool ${toString n}: TAP names not unique (${toString taps})";
      }
      {
        assertion = builtins.all (t: lib.stringLength t <= slotLib.maxInterfaceNameLength) taps;
        message = "pool ${toString n}: a TAP name exceeds ${toString slotLib.maxInterfaceNameLength} chars (${toString taps})";
      }
      {
        # Sizing comes from the class, so a class's slots are identical.
        assertion = builtins.all (
          sl: sl.vcpu == classes.${sl.class}.vcpu && sl.memoryMiB == classes.${sl.class}.memoryMiB
        ) pool;
        message = "pool ${toString n}: a slot's vcpu/memory does not match its class";
      }
      {
        assertion = indicesContiguous;
        message = "slotCount=${toString n}: slot .index values not contiguous 0..${toString (n - 1)}";
      }
      {
        # ticket 3 B: every concurrently runnable slot needs a UNIQUE VSOCK
        # control-channel identity ...
        assertion = lib.length (lib.unique cids) == n;
        message = "slotCount=${toString n}: VSOCK CIDs not unique (${toString cids})";
      }
      {
        # ... that avoids the reserved CIDs 0 (hypervisor), 1 (loopback),
        # 2 (host) and VMADDR_CID_ANY (0xffffffff).
        assertion = builtins.all (c: c > 2 && c < 4294967295) cids;
        message = "slotCount=${toString n}: VSOCK CIDs must avoid reserved values (${toString cids})";
      }
    ];

  # --- (f) NEGATIVE eval tests -------------------------------------------
  # Prove the module's §37 assertions actually FIRE on invalid config, not
  # just that the happy path works. Each helper force-overrides ONE variable
  # into an invalid state on the real test-f13 host and inspects the SPECIFIC
  # assertion that must reject it.
  #
  # We inspect `config.assertions` directly rather than forcing
  # `system.build.toplevel` inside `tryEval`: (1) it is dramatically lighter
  # — forcing five full toplevels via `extendModules` in one eval exhausts
  # RAM — and (2) it is MORE precise: a fired NixOS assertion is a data
  # record `{ assertion = false; message; }`, so we can confirm the exact
  # guard fired by matching its message, instead of merely observing that
  # *some* unrelated eval error was thrown. NixOS' own top-level.nix turns
  # exactly this `assertion == false` record into the build-time `throw`, so
  # this is a faithful proxy for "the module rejects the config".
  failedAssertions =
    mods:
    let
      cfg = (self.nixosConfigurations.test-f13.extendModules { modules = mods; }).config;
    in
    map (a: a.message) (builtins.filter (a: !a.assertion) cfg.assertions);
  # True iff `mods` is REJECTED: either it fails at the option-TYPE level
  # (e.g. slotCount=0 violates the `positive integer` type, so forcing the
  # config throws — caught here by tryEval), or it trips a module ASSERTION
  # whose message contains `needle`. Both are genuine "module rejects invalid
  # config" outcomes; the needle pins the assertion-level cases to the exact
  # guard so the test can't pass on an unrelated failure.
  rejectsWith =
    mods: needle:
    let
      r = builtins.tryEval (failedAssertions mods);
    in
    if !r.success then true else builtins.any (m: lib.hasInfix needle m) r.value;
  # Warnings of a variant host, for the ticket-3 C migration checks.
  warningsOf =
    mods: (self.nixosConfigurations.test-f13.extendModules { modules = mods; }).config.warnings;
  # Migration variants that must NOT inherit test-f13's explicit
  # `networkProfile` definition (a definition cannot be removed by
  # extendModules, only outranked — and `profileExplicit` deliberately keys off
  # the DEFINITION, not the value). test-workstation has the feature disabled
  # and defines no profile, so enabling it there yields a host whose profile is
  # genuinely unset. `enableSsh = false` keeps it key-free.
  unsetProfileHost =
    mods:
    (self.nixosConfigurations.test-workstation.extendModules {
      modules = [
        {
          myconfig.ai.microvm = {
            enable = true;
            enableSsh = false;
          };
        }
      ]
      ++ mods;
    }).config;
  # The unmodified host must have NO failed assertions — positive control so
  # the rejectsWith checks below can't pass vacuously.
  baselineClean = failedAssertions [ ] == [ ];

  # --- lightweight plan phase 1: the `lite` profile ----------------------
  # A host whose sizing is NOT defined at all, so the profile's own class table
  # is what the pool is built from. test-f13 cannot be used: it defines
  # `resourceClasses` explicitly (a definition can only be outranked by
  # extendModules, never removed), and an explicit table always wins over the
  # profile default. test-workstation has the feature disabled and defines no
  # sizing, so enabling it there yields a genuinely profile-sized pool.
  liteHostWith =
    mods:
    self.nixosConfigurations.test-workstation.extendModules {
      modules = [
        {
          myconfig.ai.microvm = {
            enable = true;
            profile = "lite";
            # Keeps the variant key-free (the pool/store assertions below are
            # independent of the SSH control channel).
            enableSsh = false;
          };
          # The secure `proxy-only` default requires the host LiteLLM backend.
          services.litellm.enable = true;
        }
      ]
      ++ mods;
    };
  liteHost = liteHostWith [ ];
  liteProfile =
    (import ../modules/myconfig.ai/myconfig.ai.microvm/profiles.nix { inherit lib; }).forProfile
      "lite";
  liteSlots = slotLib.mkSlots liteProfile.resourceClasses;
in
{
  # ---------------------------------------------------------------------- #
  # (a) DISABLED (secure default) — module evaluates, zero side effects.    #
  #     Plan §37 (public internet / VM tier off by default) & §38           #
  #     ("test-f13 builds/evaluates with module disabled").                 #
  # ---------------------------------------------------------------------- #
  microvm-eval-disabled = mkEvalCheck "microvm-eval-disabled" [
    {
      assertion = disabledCfg.myconfig.ai.microvm.enable == false;
      message = "feature should be disabled but enable != false";
    }
    {
      # microvm.nix host option exists (module imported unconditionally) but
      # NO declarative VMs are defined while the feature is off.
      assertion = builtins.attrNames disabledCfg.microvm.vms == [ ];
      message = "disabled feature still defines microvm.vms: ${toString (builtins.attrNames disabledCfg.microvm.vms)}";
    }
    {
      # The bridge-only LiteLLM forwarding socket must not exist.
      assertion = !(disabledCfg.systemd.sockets ? agent-litellm-proxy);
      message = "disabled feature still defines systemd.sockets.agent-litellm-proxy";
    }
    {
      # No dedicated agent firewall chain leaked into the ruleset.
      assertion = !(lib.hasInfix "AGENT_MICROVM_FORWARD" disabledCfg.networking.firewall.extraCommands);
      message = "disabled feature still injects AGENT_MICROVM_* firewall rules";
    }
    {
      # microvm.nix host machinery stays neutralised (mkDefault false).
      assertion = disabledCfg.microvm.host.enable == false;
      message = "disabled feature still enables microvm.host";
    }
    {
      # No passwordless-control group leaks while the feature is off, so the
      # scoped NOPASSWD sudo rule cannot exist either.
      assertion = !(disabledCfg.users.groups ? agent-microvm);
      message = "disabled feature still defines the agent-microvm control group";
    }
  ];

  # ---------------------------------------------------------------------- #
  # (b) ENABLED (test-f13) — exactly `slotCount` slots, bridge-only proxy   #
  #     endpoint, terminal FORWARD DROP + 169.254.169.254 metadata drop.    #
  #     Plan §37 (assertions) & §38 (f13/test-f13 evaluate enabled).        #
  # ---------------------------------------------------------------------- #
  microvm-eval-enabled =
    let
      vmNames = lib.sort (a: b: a < b) (builtins.attrNames enabledCfg.microvm.vms);
      expectedNames = lib.sort (a: b: a < b) (map (sl: sl.name) enabledSlots);
      socket = enabledCfg.systemd.sockets.agent-litellm-proxy.socketConfig;
      fw = enabledCfg.networking.firewall.extraCommands;
    in
    mkEvalCheck "microvm-eval-enabled" (
      [
        {
          assertion = enabledCfg.myconfig.ai.microvm.enable == true;
          message = "test-f13 should have the feature enabled";
        }
        {
          assertion = vmNames == expectedNames;
          message = "expected VMs ${toString expectedNames} but got ${toString vmNames}";
        }
        {
          assertion = enabledCfg.microvm.host.enable == true;
          message = "microvm.host.enable should be true when the feature is enabled";
        }
        {
          # Bridge-only: listens on <gateway>:<port>, NOT 0.0.0.0 / the LAN.
          assertion = socket.ListenStream == "${gateway}:${port}";
          message = "agent-litellm-proxy ListenStream is '${toString socket.ListenStream}', expected '${gateway}:${port}'";
        }
        {
          assertion = !(lib.hasInfix "0.0.0.0" (toString socket.ListenStream));
          message = "agent-litellm-proxy must never listen on 0.0.0.0";
        }
        {
          assertion = (socket.BindToDevice or null) == microvmOpts.bridgeName;
          message = "agent-litellm-proxy should BindToDevice the bridge ${microvmOpts.bridgeName}";
        }
        {
          # BOOT ORDERING (the fix for the ~2s worker death): the bridge-only
          # socket uses `BindToDevice` (SO_BINDTODEVICE), which fails with
          # ENODEV if the bridge device does not exist at bind time. The socket
          # is `wantedBy sockets.target` (early), but the bridge is created by
          # `<bridge>-netdev.service` (`wantedBy network.target`, later), so
          # without an explicit `after`/`requires` the socket failed once at
          # boot and never retried — the bridge endpoint had no listener and
          # every guest -> LiteLLM connection was refused. Assert the ORDERING
          # property (After= + requires= the bridge netdev), not a substring,
          # so a future edit that drops either half fails this check.
          assertion =
            builtins.elem "${microvmOpts.bridgeName}-netdev.service" (
              enabledCfg.systemd.sockets.agent-litellm-proxy.after or [ ]
            )
            && builtins.elem "${microvmOpts.bridgeName}-netdev.service" (
              enabledCfg.systemd.sockets.agent-litellm-proxy.requires or [ ]
            );
          message = "agent-litellm-proxy.socket must be ordered after (after= + requires=) ${microvmOpts.bridgeName}-netdev.service so SO_BINDTODEVICE succeeds at boot";
        }
        {
          # Terminal fail-closed FORWARD drop (§13/§14).
          assertion = lib.hasInfix "AGENT_MICROVM_FORWARD -j DROP" fw;
          message = "firewall missing terminal 'AGENT_MICROVM_FORWARD -j DROP'";
        }
        {
          # Cloud-metadata IP explicitly dropped (§13).
          assertion = lib.hasInfix "169.254.169.254" fw;
          message = "firewall missing 169.254.169.254 metadata drop";
        }
        {
          # passwordlessControl is opted-in on f13: the dedicated control group
          # exists and the operator (myconfig.user) is a member.
          assertion =
            (enabledCfg.users.groups ? agent-microvm)
            && builtins.elem "agent-microvm" enabledCfg.users.users.mhuber.extraGroups;
          message = "passwordlessControl should create the agent-microvm group and add mhuber to it";
        }
        {
          # The NOPASSWD+SETENV rule is scoped to EXACTLY the launcher binary
          # for the agent-microvm group — never a blanket ALL command.
          assertion = builtins.any (
            r:
            (builtins.elem "agent-microvm" (r.groups or [ ]))
            && builtins.any (
              c:
              (c.command or "") == "/run/current-system/sw/bin/agent-microvm"
              && builtins.elem "NOPASSWD" (c.options or [ ])
              && builtins.elem "SETENV" (c.options or [ ])
            ) (r.commands or [ ])
          ) enabledCfg.security.sudo.extraRules;
          message = "passwordlessControl should grant a scoped NOPASSWD+SETENV sudo rule for agent-microvm";
        }
        {
          # The dedicated private key stays root:root 0400 (OpenSSH rejects a
          # group/world-readable private key) — it must NOT be widened even
          # under passwordlessControl.
          assertion =
            enabledCfg.myconfig.secrets."dedicated-agent-vm-key".group == "root"
            && enabledCfg.myconfig.secrets."dedicated-agent-vm-key".permissions == "0400";
          message = "dedicated-agent-vm-key must stay root:root 0400 (got group '${
            enabledCfg.myconfig.secrets."dedicated-agent-vm-key".group
          }' perms '${enabledCfg.myconfig.secrets."dedicated-agent-vm-key".permissions}')";
        }
        {
          # passwordlessControl authorises the host operator's OWN public keys
          # on the guest `agent` user (so non-root `agent-microvm ssh` works
          # with the operator's default identity), in ADDITION to the dedicated
          # key file. The operator has >=1 declared key, so this is non-empty
          # and matches the host user's keys exactly.
          assertion =
            let
              guestKeys = guest0Cfg.users.users.agent.openssh.authorizedKeys.keys;
              hostKeys = enabledCfg.users.users.mhuber.openssh.authorizedKeys.keys;
            in
            hostKeys != [ ] && guestKeys == hostKeys;
          message = "passwordlessControl should authorise the host operator's public keys on the guest agent user";
        }
        {
          # BATCH WORKER ENDPOINT ENV (the fix for the ~2s worker death):
          # `agent-job-worker@<agent>.service` is a NON-LOGIN systemd oneshot,
          # so it does NOT source /etc/set-environment (where NixOS puts
          # `environment.variables`). Without an explicit `environment=` on the
          # unit the worker inherited only PATH and had no model endpoint —
          # pi/codex/hermes batch jobs died within seconds. Assert the worker
          # unit's OWN environment carries the loopback LiteLLM endpoint (and
          # the registry's per-agent plumbing, e.g. hermes' OPENROUTER_BASE_URL),
          # matching what the login shell gets. `guest0Cfg` is the reference
          # guest; the template is identical for every slot.
          assertion =
            let
              we = guest0Cfg.systemd.services."agent-job-worker@".environment or { };
              want = "http://127.0.0.1:${port}/v1";
            in
            we ? OPENAI_BASE_URL
            && we.OPENAI_BASE_URL == want
            && we ? OPENROUTER_BASE_URL
            && we.OPENROUTER_BASE_URL == want
            && we ? ANTHROPIC_BASE_URL
            && we.ANTHROPIC_BASE_URL == "http://127.0.0.1:${port}"
            && we ? PATH;
          message = "agent-job-worker@.environment must carry the model-endpoint vars (OPENAI/OPENROUTER/ANTHROPIC_BASE_URL) the non-login batch worker cannot get from a login profile";
        }
      ]
      # --- per-TAP L2 isolation (ticket 3 A) -------------------------------
      # Every slot's attach oneshot must BOTH enslave the TAP to the bridge and
      # mark that bridge port `isolated`, so guest<->guest frames are dropped by
      # the bridge itself (ARP / IPv6 ND / any EtherType included) and not only
      # by the IPv4 FORWARD rule. Asserted per slot so a partially-isolated pool
      # cannot pass.
      ++ lib.concatMap (
        slot:
        let
          execStart = toString (
            enabledCfg.systemd.services."agent-microvm-attach-${slot.name}".serviceConfig.ExecStart
          );
        in
        [
          {
            assertion = lib.hasInfix "link set ${slot.tap} master ${microvmOpts.bridgeName}" execStart;
            message = "slot ${slot.name}: attach unit does not enslave ${slot.tap} to ${microvmOpts.bridgeName}";
          }
          {
            assertion = lib.hasInfix "link set dev ${slot.tap} isolated on" execStart;
            message = "slot ${slot.name}: attach unit does not mark ${slot.tap} as an isolated bridge port (L2 guest-to-guest isolation)";
          }
        ]
      ) enabledSlots
    );

  # ---------------------------------------------------------------------- #
  # (r) LIGHTWEIGHT PROFILE (lightweight plan phase 1): `profile = "lite"`   #
  #     is a COMPATIBILITY BOUNDARY, not a behaviour change for existing     #
  #     hosts. Assert both halves: the reference host stays on `full` with    #
  #     its own sizing, and a host that opts into `lite` (and says nothing    #
  #     about sizing) gets EXACTLY one 2 vCPU / 4 GiB slot with a pinned,     #
  #     optimized EROFS guest store.                                         #
  # ---------------------------------------------------------------------- #
  microvm-eval-lite-profile =
    let
      liteCfg = liteHost.config;
      liteVmNames = lib.sort (a: b: a < b) (builtins.attrNames liteCfg.microvm.vms);
      liteSlotNames = lib.sort (a: b: a < b) (map (sl: sl.name) liteSlots);
      liteGuest = liteCfg.microvm.vms.${(lib.head liteSlots).name}.config.config;
      liteFailed = map (a: a.message) (builtins.filter (a: !a.assertion) liteCfg.assertions);
    in
    mkEvalCheck "microvm-eval-lite-profile" [
      {
        # Existing hosts keep the full-featured tier: the profile default must
        # never flip under them.
        assertion = microvmOpts.profile == "full";
        message = "the profile default must stay 'full' (reference host reports '${microvmOpts.profile}')";
      }
      {
        # ... and the full profile must NOT impose the lite class table: the
        # reference host's own pool is untouched.
        assertion = lib.attrNames resourceClasses != [ "lite" ];
        message = "the full profile must not replace the host's resource classes";
      }
      {
        assertion = liteFailed == [ ];
        message = "a plain `profile = \"lite\"` host must evaluate cleanly, got: ${toString liteFailed}";
      }
      {
        assertion = liteCfg.myconfig.ai.microvm.profile == "lite";
        message = "the lite variant host does not report profile = lite";
      }
      {
        # ONE prebuilt slot unless overridden (plan phase 1 acceptance).
        assertion = lib.length liteSlots == 1;
        message = "the lite profile must declare exactly ONE slot, got ${toString (lib.length liteSlots)}";
      }
      {
        assertion = liteVmNames == liteSlotNames;
        message = "lite host VMs ${toString liteVmNames} do not match the profile pool ${toString liteSlotNames}";
      }
      {
        assertion = liteGuest.microvm.vcpu == 2 && liteGuest.microvm.mem == 4096;
        message = "lite guest must be 2 vCPU / 4096 MiB, got ${toString liteGuest.microvm.vcpu} vCPU / ${toString liteGuest.microvm.mem} MiB";
      }
      {
        # Pinned rather than inherited: a microvm.nix release cannot silently
        # give the lightweight guest a bigger/slower store image.
        assertion = liteGuest.microvm.storeDiskType == "erofs";
        message = "lite guest storeDiskType must be pinned to erofs, got '${toString liteGuest.microvm.storeDiskType}'";
      }
      {
        assertion = liteGuest.microvm.optimize.enable == true;
        message = "lite guest must pin microvm.optimize.enable = true";
      }
      {
        # The profile default is a DEFAULT: an explicit table still wins.
        assertion =
          let
            overridden =
              (liteHostWith [
                {
                  myconfig.ai.microvm.resourceClasses = lib.mkForce {
                    normal = {
                      count = 2;
                      vcpu = 1;
                      memoryMiB = 1024;
                    };
                  };
                }
              ]).config;
          in
          lib.sort (a: b: a < b) (builtins.attrNames overridden.microvm.vms) == [
            "agent-normal-0"
            "agent-normal-1"
          ];
        message = "an explicit resourceClasses table must outrank the lite profile's default pool";
      }
      {
        # ... but mixing the profile table with the DEPRECATED slot options is
        # ambiguous and must be rejected, never silently resolved.
        assertion =
          let
            msgs = map (a: a.message) (
              builtins.filter (a: !a.assertion)
                (liteHostWith [ { myconfig.ai.microvm.slotCount = 3; } ]).config.assertions
            );
          in
          builtins.any (m: lib.hasInfix "carries its own resource-class table" m) msgs;
        message = "profile = lite together with the deprecated slotCount must be rejected";
      }
    ];

  # ---------------------------------------------------------------------- #
  # (c) PURE-EVAL slot pool: unique + well-formed IPs/MACs, contiguous      #
  #     names, across a range of slot counts. Encodes §37 duplicate         #
  #     detection as an executable test against the real slots.nix.         #
  # ---------------------------------------------------------------------- #
  microvm-slot-uniqueness = mkEvalCheck "microvm-slot-uniqueness" (
    lib.concatMap slotPoolChecks slotPoolsUnderTest
  );

  # ---------------------------------------------------------------------- #
  # (f) NEGATIVE eval tests: the module's §37 assertions must REJECT        #
  #     invalid config (inspected via config.assertions, see helper above).  #
  #     This locks down the OTHER half of §37 (rejecting                     #
  #     bad input) — including the module's own IP/MAC uniqueness guards     #
  #     (default.nix:258/262) and the slotCount bound / enableSsh-key /      #
  #     insecure-network-acknowledgement assertions. Without these the       #
  #     guards would be dead code from the suite's perspective.              #
  # ---------------------------------------------------------------------- #
  microvm-eval-rejects-invalid = mkEvalCheck "microvm-eval-rejects-invalid" [
    {
      # Positive control: the unmodified enabled host trips NO assertion,
      # otherwise the rejectsWith checks below would pass vacuously.
      assertion = baselineClean;
      message = "positive control: unmodified test-f13 must have no failed assertions, got: ${
        toString (failedAssertions [ ])
      }";
    }
    {
      # count=0 is rejected at the option-TYPE level (`positive integer`), a
      # strictly stronger guard than any assertion.
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.resourceClasses = lib.mkForce {
            normal.count = 0;
            normal.vcpu = 2;
            normal.memoryMiB = 1024;
          };
        }
      ] "resource-class pool is empty";
      message = "a resource class with count=0 must be rejected (type `positive integer`)";
    }
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.resourceClasses = lib.mkForce {
            normal.count = slotLib.maxSlotCount + 1;
            normal.vcpu = 2;
            normal.memoryMiB = 1024;
          };
        }
      ] "total number of slots";
      message = "a pool larger than maxSlotCount (${toString slotLib.maxSlotCount}) must be rejected";
    }
    {
      # A class name long enough to overflow the 15-char interface-name limit.
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.resourceClasses = lib.mkForce {
            "ludicrously-large".count = 1;
            "ludicrously-large".vcpu = 2;
            "ludicrously-large".memoryMiB = 1024;
          };
        }
      ] "generated TAP names must be";
      message = "a class name that overflows the TAP name limit must be rejected";
    }
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.resourceClasses = lib.mkForce {
            "Bad_Name".count = 1;
            "Bad_Name".vcpu = 2;
            "Bad_Name".memoryMiB = 1024;
          };
        }
      ] "class names must match";
      message = "an invalid class name must be rejected";
    }
    {
      # The litellm-capable profiles (proxy-only/package-access/internet)
      # assume the host's `services.litellm` backend exists: the guest-side
      # forwarder hands connections to 127.0.0.1:<litellmPort>, which only
      # exists when litellm is enabled. A host that enables a litellm-capable
      # sandbox WITHOUT the backend must fail at EVAL (not 2s into every
      # batch job). `mkForce` wins over the host's own `enable = true`.
      assertion = rejectsWith [
        { services.litellm.enable = lib.mkForce false; }
      ] "lets guests reach the model API";
      message = "a litellm-capable profile with services.litellm.enable=false must be rejected at eval";
    }
    {
      # Setting BOTH spellings would silently drop one of them.
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.resourceClasses = lib.mkForce {
            normal.count = 1;
            normal.vcpu = 2;
            normal.memoryMiB = 1024;
          };
          myconfig.ai.microvm.slotCount = lib.mkForce 3;
        }
      ] "ambiguous slot configuration";
      message = "resourceClasses together with the deprecated slotCount must be rejected";
    }
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.enableSsh = lib.mkForce true;
          myconfig.ai.microvm.sshPublicKeyFile = lib.mkForce null;
        }
      ] "enableSsh requires an explicit sshPublicKeyFile";
      message = "enableSsh without sshPublicKeyFile must be rejected";
    }
    {
      # On a host whose profile is genuinely unset, the deprecated boolean
      # translates to the `internet` profile — which is insecure and therefore
      # still needs the explicit acknowledgement.
      assertion =
        let
          cfg = unsetProfileHost [ { myconfig.ai.microvm.allowPublicInternet = true; } ];
          msgs = map (a: a.message) (builtins.filter (a: !a.assertion) cfg.assertions);
        in
        builtins.any (m: lib.hasInfix "INSECURE profile" m) msgs;
      message = "allowPublicInternet (-> internet profile) without acknowledgeInsecureNetwork must be rejected";
    }
    # --- ticket 3 C: profile model + legacy-boolean migration -------------
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.networkProfile = "internet";
          myconfig.ai.microvm.acknowledgeInsecureNetwork = lib.mkForce false;
        }
      ] "is an\nINSECURE profile";
      message = "networkProfile = internet without acknowledgeInsecureNetwork must be rejected";
    }
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.networkProfile = "package-access";
          myconfig.ai.microvm.packageProxyPort = 3128;
          myconfig.ai.microvm.acknowledgeInsecureNetwork = lib.mkForce false;
        }
      ] "is an\nINSECURE profile";
      message = "networkProfile = package-access without acknowledgeInsecureNetwork must be rejected";
    }
    {
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.networkProfile = "package-access";
          myconfig.ai.microvm.acknowledgeInsecureNetwork = true;
        }
      ] "requires\n`packageProxyPort`";
      message = "networkProfile = package-access without packageProxyPort must be rejected";
    }
    {
      # Ambiguity must never be resolved silently in either direction.
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.allowPublicInternet = lib.mkForce true;
          myconfig.ai.microvm.networkProfile = "proxy-only";
          myconfig.ai.microvm.acknowledgeInsecureNetwork = true;
        }
      ] "ambiguous network configuration";
      message = "allowPublicInternet together with an explicit, different networkProfile must be rejected";
    }
    {
      assertion = rejectsWith [
        { myconfig.ai.microvm.allowInterVmTraffic = lib.mkForce true; }
      ] "`allowInterVmTraffic` has been REMOVED";
      message = "allowInterVmTraffic = true must be rejected (guest isolation is unconditional)";
    }
    {
      assertion = rejectsWith [
        { myconfig.ai.microvm.allowPrivateNetworks = lib.mkForce true; }
      ] "`allowPrivateNetworks` has been REMOVED";
      message = "allowPrivateNetworks = true must be rejected (no profile grants private-range access)";
    }
    {
      # A host that still defines a deprecated boolean must be WARNED, not
      # silently migrated.
      assertion = builtins.any (
        w: lib.hasInfix "network booleans" w && lib.hasInfix "deprecated" w
      ) (warningsOf [ { myconfig.ai.microvm.allowPrivateNetworks = false; } ]);
      message = "defining a deprecated network boolean must emit a deprecation warning";
    }
    {
      # ... and a translated boolean must say so explicitly (again on a host
      # whose profile is genuinely unset).
      assertion =
        let
          cfg = unsetProfileHost [
            {
              myconfig.ai.microvm.allowPublicInternet = true;
              myconfig.ai.microvm.acknowledgeInsecureNetwork = true;
            }
          ];
        in
        builtins.any (w: lib.hasInfix "translated the deprecated" w) cfg.warnings;
      message = "translating allowPublicInternet -> networkProfile = internet must warn";
    }
  ];

  # ---------------------------------------------------------------------- #
  # (g) GUEST SHARES: prove the guest declares EXACTLY TWO virtiofs shares  #
  #     — the WRITABLE `/workspace` (host `source` == the launcher's         #
  #     bind-mount target `${stateRoot}/<slot>/workspace`) and the           #
  #     READ-ONLY per-slot SSH host-key share (ticket 3 B). This locks down  #
  #     plan §10/§11 crit. 12 (the workspace share can never silently       #
  #     vanish, be renamed, be made read-only or be repointed) AND the       #
  #     ticket-3 amendment: the hostkey share must stay read-only, per-slot  #
  #     and nothing else may be shared (no /nix, /home, host sockets).       #
  # ---------------------------------------------------------------------- #
  microvm-eval-workspace-share =
    let
      shares = guest0Cfg.microvm.shares;
      virtiofsShares = builtins.filter (s: s.proto == "virtiofs") shares;
      wsShares = builtins.filter (s: s.mountPoint == "/workspace") shares;
      ws = if wsShares == [ ] then null else builtins.head wsShares;
      expectedSource = "${microvmOpts.stateRoot}/${refSlot.name}/workspace";
      hkShares = builtins.filter (s: s.mountPoint == hostKeys.guestMountPoint) shares;
      hk = if hkShares == [ ] then null else builtins.head hkShares;
      expectedHkSource = hostKeys.slotDir refSlot.name;
      jobShares = builtins.filter (s: s.mountPoint == jobs.guestMountPoint) shares;
      jobShare = if jobShares == [ ] then null else builtins.head jobShares;
      expectedJobSource = jobs.slotDir refSlot.name;
      stShares = builtins.filter (s: s.mountPoint == agentStatePaths.guestMountPoint) shares;
      stShare = if stShares == [ ] then null else builtins.head stShares;
      expectedStSource = agentStatePaths.slotDir refSlot.name;
    in
    mkEvalCheck "microvm-eval-workspace-share" [
      {
        # Exactly four shares: writable workspace, read-only hostkey, the
        # per-slot batch job directory and the per-slot agent-state directory.
        assertion = builtins.length shares == 4;
        message = "guest ${refSlot.name} must declare exactly FOUR shares (workspace + hostkey + job + agent-state); got ${toString (builtins.length shares)}: ${
          toString (map (s: s.mountPoint or "?") shares)
        }";
      }
      {
        assertion =
          stShare != null && stShare.proto == "virtiofs" && stShare.tag == agentStatePaths.guestTag;
        message = "guest ${refSlot.name} must declare a virtiofs share tagged '${agentStatePaths.guestTag}' at ${agentStatePaths.guestMountPoint}";
      }
      {
        assertion = stShare != null && stShare.source == expectedStSource;
        message = "agent-state share source is '${
          toString (stShare.source or "<none>")
        }', expected the PER-SLOT dir '${expectedStSource}' (the launcher binds the per-TASK dir onto it)";
      }
      {
        assertion = jobShare != null && jobShare.proto == "virtiofs" && jobShare.tag == jobs.guestTag;
        message = "guest ${refSlot.name} must declare a virtiofs share tagged '${jobs.guestTag}' at ${jobs.guestMountPoint}";
      }
      {
        assertion = jobShare != null && jobShare.source == expectedJobSource;
        message = "job share source is '${
          toString (jobShare.source or "<none>")
        }', expected the PER-SLOT dir '${expectedJobSource}' (must match job.nix)";
      }
      {
        # Read-WRITE on purpose (the guest controller writes
        # controller/result.json and the worker its logs). WHO may write WHAT
        # inside the share is enforced by ownership/modes, not by this flag:
        # input/ and controller/ are root-owned (controller/ additionally 0700),
        # only worker/ belongs to the unprivileged guest agent.
        assertion = jobShare != null && (jobShare.readOnly or false) == false;
        message = "job share must be read-write so the guest controller can write its result";
      }
      {
        assertion = hk != null && hk.proto == "virtiofs" && hk.tag == hostKeys.guestTag;
        message = "guest ${refSlot.name} must declare a virtiofs share tagged '${hostKeys.guestTag}' at ${hostKeys.guestMountPoint}";
      }
      {
        assertion = hk != null && hk.source == expectedHkSource;
        message = "hostkey share source is '${
          toString (hk.source or "<none>")
        }', expected the PER-SLOT dir '${expectedHkSource}' (must match hostkeys.nix)";
      }
      {
        # Read-only is load-bearing: the guest must not be able to replace its
        # own host identity, and only guest root may read the 0400 private key.
        assertion = hk != null && (hk.readOnly or false) == true;
        message = "hostkey share MUST be readOnly (the guest must not be able to rewrite its host identity)";
      }
      {
        assertion = ws != null;
        message = "guest ${refSlot.name} has no share mounted at /workspace (mountPoints: ${
          toString (map (s: s.mountPoint or "?") shares)
        })";
      }
      {
        assertion = ws != null && ws.proto == "virtiofs";
        message = "/workspace share must be proto=virtiofs, got '${toString (ws.proto or "<none>")}'";
      }
      {
        assertion = ws != null && ws.tag == "workspace";
        message = "/workspace share must have tag=workspace, got '${toString (ws.tag or "<none>")}'";
      }
      {
        assertion = ws != null && ws.source == expectedSource;
        message = "/workspace share source is '${
          toString (ws.source or "<none>")
        }', expected '${expectedSource}' (must match launcher.nix mount_point())";
      }
      {
        # Must be read-write so agent-run's `test -w /workspace` can pass.
        assertion = ws != null && (ws.readOnly or false) == false;
        message = "/workspace share must be read-write (readOnly=false)";
      }
      {
        # Defence in depth: the ONLY virtiofs shares are the workspace and the
        # hostkey dir — no /nix, /home or host-socket share leaked in.
        assertion = builtins.length virtiofsShares == 4;
        message = "expected exactly four virtiofs shares (workspace + hostkey + job + agent-state); got ${toString (builtins.length virtiofsShares)}";
      }
      {
        assertion = builtins.all (
          s:
          builtins.elem s.mountPoint [
            "/workspace"
            hostKeys.guestMountPoint
            jobs.guestMountPoint
            agentStatePaths.guestMountPoint
          ]
        ) shares;
        message = "unexpected share mountPoint(s): ${toString (map (s: s.mountPoint or "?") shares)}";
      }
    ];

  # ---------------------------------------------------------------------- #
  # (h) AGENT REGISTRY (ticket 1): prove that agents.nix really is the ONE  #
  #     source of truth — the guest closure, the workmux registrations, the #
  #     host launcher's `--agent` validation/help and the guest `agent-run` #
  #     dispatch are all derived from it. Eval part: registry well-formed,  #
  #     workmux keys == registry workmuxNames, every agent package in the   #
  #     guest closure. Build part: grep the BUILT launcher scripts for each #
  #     registry agent, so a registry entry that never reaches the shell    #
  #     code fails the check.                                              #
  # ---------------------------------------------------------------------- #
  microvm-agent-registry =
    let
      registryAgents = lib.attrValues agentRegistry.agents;
      workmuxAgents = enabledCfg.myconfig.ai.workmux.agents;
      guestPkgPaths = map (p: p.outPath) guest0Cfg.environment.systemPackages;
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      guestAgentRun = findPkg guest0Cfg.environment.systemPackages "agent-run";
      evalMarker = mkEvalCheck "microvm-agent-registry-eval" (
        [
          {
            assertion = agentRegistry.errors == [ ];
            message = "agent registry is malformed: ${toString agentRegistry.errors}";
          }
          {
            assertion = agentRegistry.names != [ ];
            message = "agent registry declares no agents";
          }
          {
            # The workmux registry contains EXACTLY the registry's microvm-*
            # agents — no hand-written extra, none missing.
            assertion =
              lib.sort (a: b: a < b) (
                builtins.filter (lib.hasPrefix "microvm-") (builtins.attrNames workmuxAgents)
              ) == lib.sort (a: b: a < b) (map (a: a.workmuxName) registryAgents);
            message = "workmux microvm-* agents do not match the registry: ${toString (builtins.filter (lib.hasPrefix "microvm-") (builtins.attrNames workmuxAgents))} vs ${
              toString (map (a: a.workmuxName) registryAgents)
            }";
          }
        ]
        ++ map (a: {
          assertion = builtins.elem a.package.outPath guestPkgPaths;
          message = "agent '${a.name}': package ${a.package.outPath} is not in the guest closure";
        }) registryAgents
        ++ map (a: {
          assertion = (workmuxAgents.${a.workmuxName}.type or null) == a.workmuxType;
          message = "workmux agent '${a.workmuxName}' type is '${
            toString (workmuxAgents.${a.workmuxName}.type or null)
          }', expected '${a.workmuxType}'";
        }) registryAgents
      );
    in
    pkgs.runCommand "microvm-agent-registry"
      {
        inherit evalMarker;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        agentRunBin = "${guestAgentRun}/bin/agent-run";
        agentNames = lib.concatStringsSep " " agentRegistry.names;
      }
      ''
        for n in $agentNames; do
          # Host-side: the generated `validate_agent_name` case pattern and
          # the generated help listing both mention every registry agent.
          grep -qE "(^| |\()$n( |\||\))" "$launcherBin" \
            || { echo "agent '$n' missing from host launcher $launcherBin" >&2; exit 1; }
          # Guest-side: the generated `agent-run` dispatch table has a case arm.
          grep -q "$n) exec " "$agentRunBin" \
            || { echo "agent '$n' missing from guest agent-run dispatch" >&2; exit 1; }
        done
        {
          echo "microvm-agent-registry: agents.nix is the single source of truth for:"
          echo "  agents        : $agentNames"
          echo "  host launcher : $launcherBin"
          echo "  guest dispatch: $agentRunBin"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (p) OBSERVABILITY (ticket 6 B): every lifecycle transition is emitted as  #
  #     ONE structured JSON record, to stderr + the journal (tag             #
  #     `agent-microvm`) + a BOUNDED per-task log; the guest emits its own    #
  #     transitions; and nothing secret is ever logged.                       #
  # ---------------------------------------------------------------------- #
  microvm-observability =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      # The guest-side emitter is now the TRUSTED controller (the untrusted
      # worker deliberately emits no lifecycle events — nothing it says could be
      # trusted as a transition).
      guestJob = findPkg guest0Cfg.environment.systemPackages "agent-job-controller";
      evalMarker = mkEvalCheck "microvm-observability-eval" [
        {
          assertion = microvmOpts.taskLogMaxBytes > 0;
          message = "per-task logs must be bounded (taskLogMaxBytes > 0)";
        }
      ];
    in
    pkgs.runCommand "microvm-observability"
      {
        inherit evalMarker;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        guestJobBin = "${guestJob}/bin/agent-job-controller";
        # The transitions ticket 6 B requires from the HOST side (including
        # `result-rejected`, the ticket-7 transition for a guest document that
        # does not belong to the active allocation).
        # `mount-leak` is the ticket-8 transition: a bind mount that survived its
        # unmount (the launcher no longer accepts a lazy unmount as success).
        hostEvents = "task-submitted slot-allocated workspace-created vm-start-requested vm-ready agent-started agent-finished timeout cancellation result-rejected vm-stopped cleanup-completed recovery-action mount-leak";
        # ... and from the guest side (the controller also reports the
        # cancellations it performed itself).
        guestEvents = "agent-started agent-finished timeout cancellation";
      }
      ''
        for e in $hostEvents; do
          grep -q "emit_event $e" "$launcherBin" \
            || { echo "host launcher never emits the '$e' lifecycle event" >&2; exit 1; }
        done
        for e in $guestEvents; do
          grep -q "emit_event $e" "$guestJobBin" \
            || { echo "guest runner never emits the '$e' lifecycle event" >&2; exit 1; }
        done
        # Records must carry the identifying fields.
        for f in task slot agent resource_class state exit_code; do
          grep -q "$f:" "$launcherBin" \
            || { echo "lifecycle records lack the '$f' field" >&2; exit 1; }
        done
        # Discoverable in the journal, and bounded on disk.
        grep -q 'logger --tag "$PROG"' "$launcherBin" \
          || { echo "lifecycle events are not sent to the journal" >&2; exit 1; }
        grep -q "LOG_MAX_BYTES" "$launcherBin" \
          || { echo "per-task logs are not bounded" >&2; exit 1; }
        # The prompt CONTENT must never be logged: the launcher may reference the
        # prompt file's path/size, but must not cat/read it into a log line.
        if grep -qE 'emit_event[^\n]*\$\(cat' "$launcherBin"; then
          echo "launcher logs prompt content" >&2; exit 1
        fi
        {
          echo "microvm-observability:"
          echo "  host events : $hostEvents"
          echo "  guest events: $guestEvents"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (o) LIMITS + WORKSPACE SAFETY (ticket 5 C/D): per-class guest limits, the #
  #     host-side hypervisor-unit limits (never BELOW guest RAM + overhead),  #
  #     the retained-usage report, and a re-verification of the seven         #
  #     workspace-safety properties in the BUILT launcher — including that it #
  #     never evaluates anything the repository provides.                     #
  # ---------------------------------------------------------------------- #
  microvm-limits-and-workspace-safety =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      evalMarker = mkEvalCheck "microvm-limits-eval" (
        lib.concatMap (
          slot:
          let
            guest = enabledCfg.microvm.vms.${slot.name}.config.config;
            # The per-class cgroup limits live on the WORKER unit (the untrusted
            # half): the trusted controller must not be able to eat the class's
            # whole budget, and the agent must be the one that gets OOM-killed.
            job = guest.systemd.services."${jobs.workerUnitTemplate}".serviceConfig;
            host = enabledCfg.systemd.services."microvm@${slot.name}".serviceConfig;
            cls = resourceClasses.${slot.class};
            # "5120M" -> 5120
            mib = v: lib.toInt (lib.removeSuffix "M" v);
          in
          [
            {
              assertion = job.CPUQuota == "${toString (cls.vcpu * 100)}%";
              message = "${slot.name}: guest job worker CPUQuota must match its class (${toString cls.vcpu} vCPU)";
            }
            {
              assertion = mib job.MemoryMax < cls.memoryMiB && mib job.MemoryMax >= cls.memoryMiB / 2;
              message = "${slot.name}: guest job MemoryMax must leave headroom but at least half the class memory";
            }
            {
              assertion = (job.TasksMax or 0) > 0;
              message = "${slot.name}: guest job must bound TasksMax";
            }
            {
              # The load-bearing one: a host limit BELOW guest RAM + overhead
              # would OOM-kill a well-behaved VM.
              assertion = mib host.MemoryMax >= cls.memoryMiB + microvmOpts.hypervisorMemoryOverheadMiB;
              message = "${slot.name}: host MemoryMax (${toString host.MemoryMax}) must be >= class memory + hypervisor overhead";
            }
            {
              assertion = (host.TasksMax or 0) > 0 && (host.CPUWeight or 0) > 0 && (host.IOWeight or 0) > 0;
              message = "${slot.name}: host hypervisor unit must set TasksMax/CPUWeight/IOWeight";
            }
          ]
        ) enabledSlots
      );
    in
    pkgs.runCommand "microvm-limits-and-workspace-safety"
      {
        inherit evalMarker;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
      }
      ''
        # --- ticket 5 D: the seven workspace-safety properties -------------
        # 1+2: .git and the git COMMON dir must resolve inside the workspace.
        grep -q "git-dir escapes the workspace" "$launcherBin" \
          || { echo "missing git-dir escape check" >&2; exit 1; }
        grep -q "git-common-dir escapes the workspace" "$launcherBin" \
          || { echo "missing git-common-dir escape check" >&2; exit 1; }
        # 3+4: every caller-supplied path is canonicalised with realpath, so a
        # symlink cannot smuggle an escape.
        grep -q "realpath -e --" "$launcherBin" \
          || { echo "paths are not canonicalised with realpath" >&2; exit 1; }
        # 5: cleanup/removal targets stay under the configured roots.
        grep -q 'rm -rf -- "$clone"' "$launcherBin" \
          || { echo "workspace removal is not scoped to the clone" >&2; exit 1; }
        grep -q "refusing a repository that is itself an agent workspace" "$launcherBin" \
          || { echo "missing workspace-root repository guard" >&2; exit 1; }
        # 6: standalone clones only.
        grep -q "clone --no-local" "$launcherBin" \
          || { echo "clones are not created with --no-local" >&2; exit 1; }
        # 7: the host launcher must never EVALUATE anything the repository
        # provides. Guard against the obvious ways that could creep in.
        for forbidden in "nix-build" "nix build" "nix-shell" "direnv" "npm " "yarn " \
                         "pnpm " "cargo " "make " "core.hooksPath" "run-hooks"; do
          if grep -qF -- "$forbidden" "$launcherBin"; then
            echo "launcher must not invoke repository-provided tooling: found '$forbidden'" >&2
            exit 1
          fi
        done
        # The retained-usage report must exist and point at the pruning command.
        grep -q "cmd_usage()" "$launcherBin" \
          || { echo "launcher has no retained-usage report" >&2; exit 1; }
        grep -q "workspace-remove <task>" "$launcherBin" \
          || { echo "usage report does not point at the pruning command" >&2; exit 1; }
        {
          echo "microvm-limits-and-workspace-safety:"
          echo "  launcher: $launcherBin"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (n) AGENT STATE (ticket 5 B): persistence is OPT-IN and TASK-SCOPED.     #
  #     Only registry-DECLARED directories are ever exposed, each slot has   #
  #     its own share source (so the launcher can bind a per-task dir onto   #
  #     it), the guest linker is registry-driven, and nothing about the host  #
  #     home / sockets / other tasks is reachable.                           #
  # ---------------------------------------------------------------------- #
  microvm-agent-state =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      linker = agentStatePaths.linker;
      declared = agentStatePaths.declaredDirs;
      registryDeclared = lib.unique (
        lib.concatMap (a: a.persistentState.directories) (lib.attrValues agentRegistry.agents)
      );
      unit = guest0Cfg.systemd.services.agent-state-link;
      evalMarker = mkEvalCheck "microvm-agent-state-eval" (
        [
          {
            # The linker's directory list IS the registry's, never a copy.
            assertion = lib.sort (a: b: a < b) declared == lib.sort (a: b: a < b) registryDeclared;
            message = "the guest linker's directory list must equal the registry's declared state dirs";
          }
          {
            assertion = declared != [ ];
            message = "at least one agent must declare persistent state (hermes today)";
          }
          {
            # DISPOSABLE by default: no agent may opt itself in.
            assertion = builtins.all (a: a.persistentState.enabledByDefault == false) (
              lib.attrValues agentRegistry.agents
            );
            message = "no agent may enable persistence by default — the guest home stays disposable";
          }
          {
            # Only relative, traversal-free paths inside the guest home.
            assertion = builtins.all (d: !lib.hasPrefix "/" d && !lib.hasInfix ".." d && d != "") declared;
            message = "declared state dirs must be relative, non-empty and '..'-free";
          }
          {
            assertion = (unit.serviceConfig.Type or "") == "oneshot";
            message = "agent-state-link must be a oneshot";
          }
          {
            assertion = builtins.elem jobs.controllerUnit (unit.before or [ ]);
            message = "agent-state-link must run BEFORE the batch job controller (a batch job must see its state)";
          }
          {
            assertion = (unit.unitConfig.RequiresMountsFor or "") == agentStatePaths.guestMountPoint;
            message = "agent-state-link must wait for the agent-state mount";
          }
          {
            # The share source is per SLOT (the pool is prebuilt), and the
            # per-task dirs live in a directory the guest never sees.
            assertion =
              lib.hasPrefix "${microvmOpts.runtimeRoot}/" agentStatePaths.tasksRoot
              && !lib.hasPrefix agentStatePaths.slotsRoot agentStatePaths.tasksRoot;
            message = "per-task state must live outside the per-slot share sources";
          }
        ]
        ++ map (slot: {
          assertion = builtins.elem ("d ${agentStatePaths.slotDir slot.name} 0755 ${toString microvmOpts.guestAgentUid} ${toString microvmOpts.guestAgentGid} - -") enabledCfg.systemd.tmpfiles.rules;
          message = "missing tmpfiles rule for the agent-state share source of ${slot.name}";
        }) enabledSlots
      );
    in
    pkgs.runCommand "microvm-agent-state"
      {
        inherit evalMarker;
        linkerBin = "${linker}/bin/agent-state-link";
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        declaredDirs = lib.concatStringsSep " " declared;
      }
      ''
        for d in $declaredDirs; do
          grep -q "link_one $d" "$linkerBin" \
            || { echo "declared state dir '$d' missing from the guest linker" >&2; exit 1; }
        done
        # Persistence must be explicitly requested, task-scoped, and refuse
        # agents that declare nothing.
        grep -q -- "--persist-agent-state" "$launcherBin" \
          || { echo "launcher has no --persist-agent-state flag" >&2; exit 1; }
        grep -q "declares no persistent state directories" "$launcherBin" \
          || { echo "launcher does not refuse persistence for agents without declared dirs" >&2; exit 1; }
        grep -q "clear_agent_state_slot()" "$launcherBin" \
          || { echo "launcher does not clear the per-slot state share for disposable runs" >&2; exit 1; }
        # The linker must never clobber real data in the home.
        grep -q "refusing to replace non-empty" "$linkerBin" \
          || { echo "guest linker would clobber existing home data" >&2; exit 1; }
        {
          echo "microvm-agent-state: opt-in, task-scoped agent state"
          echo "  declared dirs : $declaredDirs"
          echo "  guest linker  : $linkerBin"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (m) RESOURCE CLASSES (ticket 5 A): the pool is grouped into fixed,       #
  #     PREBUILT classes; every slot keeps a unique network/control identity #
  #     AND its own host-side directories; the guest's sizing comes from its #
  #     class; and the host launcher can only allocate from the REQUESTED    #
  #     class (never a substitute).                                          #
  # ---------------------------------------------------------------------- #
  microvm-resource-classes =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      classNames = lib.attrNames resourceClasses;
      # Per-slot host-side directories must be pairwise distinct, otherwise two
      # slots (possibly of different classes) would share state.
      dirsOf = f: map f enabledSlots;
      allDistinct = xs: lib.length (lib.unique xs) == lib.length xs;
      evalMarker = mkEvalCheck "microvm-resource-classes-eval" (
        [
          {
            assertion = classNames != [ ] && enabledSlots != [ ];
            message = "the reference host must declare at least one resource class with slots";
          }
          {
            # No per-job Nix evaluation: one prebuilt VM per slot, all declared.
            assertion =
              lib.sort (a: b: a < b) (builtins.attrNames enabledCfg.microvm.vms)
              == lib.sort (a: b: a < b) (map (sl: sl.name) enabledSlots);
            message = "microvm.vms must be exactly the prebuilt slot pool";
          }
          {
            assertion = allDistinct (dirsOf (sl: sl.tap));
            message = "slot TAP names must be unique across classes";
          }
          {
            assertion = allDistinct (dirsOf (sl: sl.mac));
            message = "slot MACs must be unique across classes";
          }
          {
            assertion = allDistinct (dirsOf (sl: sl.ip));
            message = "slot IPs must be unique across classes";
          }
          {
            assertion = allDistinct (dirsOf (sl: sl.cid));
            message = "slot VSOCK CIDs must be unique across classes";
          }
          {
            assertion = allDistinct (dirsOf (sl: hostKeys.slotDir sl.name));
            message = "each slot must have its OWN SSH host-key directory";
          }
          {
            assertion = allDistinct (dirsOf (sl: jobs.slotDir sl.name));
            message = "each slot must have its OWN job directory";
          }
          {
            assertion = allDistinct (dirsOf (sl: "${microvmOpts.stateRoot}/${sl.name}/workspace"));
            message = "each slot must have its OWN workspace bind-mount target";
          }
        ]
        # Every prebuilt guest must be sized by ITS class.
        ++ map (sl: {
          assertion =
            let
              g = enabledCfg.microvm.vms.${sl.name}.config.config;
            in
            g.microvm.vcpu == resourceClasses.${sl.class}.vcpu
            && g.microvm.mem == resourceClasses.${sl.class}.memoryMiB;
          message = "guest ${sl.name} is not sized by its class '${sl.class}'";
        }) enabledSlots
      );
    in
    pkgs.runCommand "microvm-resource-classes"
      {
        inherit evalMarker;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        classNames = lib.concatStringsSep " " classNames;
        slotNames = lib.concatStringsSep " " (map (sl: sl.name) enabledSlots);
      }
      ''
        for c in $classNames; do
          grep -q "$c" "$launcherBin" \
            || { echo "resource class '$c' missing from the host launcher" >&2; exit 1; }
        done
        for sl in $slotNames; do
          grep -q "$sl" "$launcherBin" \
            || { echo "slot '$sl' missing from the launcher slot table" >&2; exit 1; }
        done
        # The allocator must filter by class and must offer a bounded wait
        # instead of silently substituting another class.
        grep -q "validate_resource_class()" "$launcherBin" \
          || { echo "launcher cannot validate --resource-class" >&2; exit 1; }
        grep -q 'no free slot in resource class' "$launcherBin" \
          || { echo "launcher does not fail loudly when the requested class is full" >&2; exit 1; }
        grep -q "validate_wait()" "$launcherBin" \
          || { echo "launcher has no bounded --wait" >&2; exit 1; }
        {
          echo "microvm-resource-classes: prebuilt pool"
          echo "  classes: $classNames"
          echo "  slots  : $slotNames"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l) BATCH JOBS (ticket 4 A, split into controller + worker in ticket 7): #
  #     Eval part: the CONTROLLER unit is inert without a job, waits for     #
  #     BOTH mounts and runs as guest root; the WORKER template runs as the   #
  #     unprivileged agent in /workspace, carries the required hardening and  #
  #     a STATIC timeout ceiling; the registry's batch metadata is            #
  #     consistent; every slot's job directory tree is pre-created            #
  #     (virtiofsd needs the share source to exist). Build part: controller,  #
  #     worker and verifier build (shellcheck) and the worker's generated      #
  #     dispatch has an arm for every batch-capable agent.                    #
  # ---------------------------------------------------------------------- #
  microvm-batch-jobs =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      ctrlUnit = guest0Cfg.systemd.services.agent-job-controller;
      ctrl = ctrlUnit.serviceConfig;
      workerUnit = guest0Cfg.systemd.services."${jobs.workerUnitTemplate}";
      worker = workerUnit.serviceConfig;
      controllerBin = findPkg guest0Cfg.environment.systemPackages "agent-job-controller";
      workerBin = findPkg guest0Cfg.environment.systemPackages "agent-job-worker";
      batchAgents = builtins.filter (a: a.batchArgs != null) (lib.attrValues agentRegistry.agents);
      tmpfiles = enabledCfg.systemd.tmpfiles.rules;
      evalMarker = mkEvalCheck "microvm-batch-jobs-eval" (
        [
          {
            # Inert unless the host placed a spec in the share: an interactive
            # slot must never accidentally run a batch job.
            assertion = (ctrlUnit.unitConfig.ConditionPathExists or null) == jobs.guestSpec;
            message = "agent-job-controller must be conditional on ${jobs.guestSpec}";
          }
          {
            assertion =
              let
                m = ctrlUnit.unitConfig.RequiresMountsFor or "";
              in
              lib.hasInfix "/workspace" m && lib.hasInfix jobs.guestMountPoint m;
            message = "agent-job-controller must wait for the workspace AND job mounts (RequiresMountsFor)";
          }
          {
            # The controller is the TRUSTED half: it must NOT run as the
            # untrusted agent user, because it owns the result channel and
            # starts the worker under a different uid.
            assertion = (ctrl.User or "root") == "root";
            message = "agent-job-controller must run as guest root (it owns the authoritative result channel)";
          }
          {
            assertion = worker.User == "agent" && worker.Group == "users";
            message = "the batch WORKER must run as the unprivileged guest agent user";
          }
          {
            assertion = worker.WorkingDirectory == "/workspace";
            message = "the batch worker must run with /workspace as its working directory";
          }
          {
            assertion = ctrl.Type == "oneshot" && worker.Type == "oneshot";
            message = "controller and worker must be oneshots";
          }
          {
            # RemainAfterExit is what keeps ExecMainStatus readable, i.e. it is
            # how the controller learns the worker's real exit status.
            assertion = (worker.RemainAfterExit or false) == true;
            message = "the worker unit must RemainAfterExit so the controller can read its exit status";
          }
          {
            # A static ceiling ON TOP of the controller's own deadline, so even
            # a worker nobody supervises cannot run forever. Type=oneshot
            # ignores RuntimeMaxSec, hence TimeoutStartSec.
            assertion =
              worker.TimeoutStartSec == microvmOpts.job.maxTimeoutSeconds + microvmOpts.job.gracePeriodSeconds;
            message = "the worker unit must carry a static TimeoutStartSec ceiling derived from job.maxTimeoutSeconds";
          }
          {
            assertion = microvmOpts.job.defaultTimeoutSeconds <= microvmOpts.job.maxTimeoutSeconds;
            message = "the default job timeout must not exceed the maximum";
          }
          {
            # The host archives finished results OUTSIDE any guest share, so a
            # guest can neither read nor forge another task's outcome.
            assertion =
              lib.hasPrefix "${microvmOpts.runtimeRoot}/" jobs.resultsDir
              && !lib.hasPrefix jobs.root jobs.resultsDir;
            message = "the host result archive must live under the runtime root and OUTSIDE the shared job dirs";
          }
          {
            # 0700: an archived result carries the allocation token of its run.
            assertion = builtins.elem "d ${jobs.resultsDir} 0700 root root - -" tmpfiles;
            message = "missing (or not root-only) tmpfiles rule for the host result archive";
          }
          {
            # Registry consistency: batch metadata must be usable.
            assertion = agentRegistry.batchNames != [ ] && agentRegistry.errors == [ ];
            message = "the registry must declare at least one batch-capable agent and be well-formed";
          }
          {
            # Every agent name must satisfy the STRICTER pattern the host result
            # verifier enforces for `--agent` (`^[a-z][a-z0-9-]{0,32}$`).
            # Without this, a 33+ character registry entry would evaluate fine
            # and then break every submit at RUNTIME with a usage error.
            assertion = builtins.all (n: builtins.match "[a-z][a-z0-9-]{0,32}" n != null) agentRegistry.names;
            message = "every registry agent name must match [a-z][a-z0-9-]{0,32} (the batch result verifier rejects anything else): ${toString agentRegistry.names}";
          }
          {
            assertion = builtins.all (a: a.batchStdin || builtins.elem "%PROMPT%" a.batchArgs) batchAgents;
            message = "every batch agent must either take the prompt on stdin or contain the %PROMPT% placeholder";
          }
          {
            # The WORKER (not the controller) needs the agent binaries on its
            # unit PATH: the controller must never be able to exec an agent.
            assertion = builtins.all (
              a: builtins.elem a.package.outPath (map (p: p.outPath) workerUnit.path)
            ) batchAgents;
            message = "the worker unit's PATH must contain every batch agent package";
          }
          {
            # The controller must never be able to exec a coding agent itself:
            # its unit PATH (NixOS adds a small default toolchain) carries none
            # of the registry packages.
            assertion =
              let
                ctrlPaths = map (p: p.outPath) (ctrlUnit.path or [ ]);
              in
              !builtins.any (a: builtins.elem a.package.outPath ctrlPaths) batchAgents;
            message = "the controller unit must NOT carry the agent packages on its PATH";
          }
          {
            # No upstream provider credential may reach a batch job: the guest
            # env only ever carries placeholders + endpoint URLs.
            assertion =
              let
                env = guest0Cfg.environment.variables;
              in
              (env.OPENAI_API_KEY or "") == "not-needed" && (env.ANTHROPIC_API_KEY or "") == "not-needed";
            message = "the guest must carry only placeholder API keys, never an upstream credential";
          }
        ]
        # Every slot's job dir (and its input/controller/worker subdirs) must be
        # pre-created, otherwise virtiofsd refuses to start for that slot.
        ++ lib.concatMap (slot: [
          {
            assertion = builtins.elem "d ${jobs.slotDir slot.name} 0755 root root - -" tmpfiles;
            message = "missing tmpfiles rule for the job dir of ${slot.name}";
          }
          {
            assertion = builtins.elem "d ${jobs.hostInputDir slot.name} ${jobs.inputDirMode} root root - -" tmpfiles;
            message = "missing tmpfiles rule for the immutable input dir of ${slot.name}";
          }
          {
            assertion = builtins.elem "d ${jobs.hostControllerDir slot.name} ${jobs.controllerDirMode} root root - -" tmpfiles;
            message = "missing tmpfiles rule for the controller-only dir of ${slot.name}";
          }
          {
            assertion = builtins.elem "d ${jobs.hostWorkerDir slot.name} ${jobs.workerDirMode} ${toString jobs.workerUid} ${toString jobs.workerGid} - -" tmpfiles;
            message = "missing tmpfiles rule for the worker-writable dir of ${slot.name}";
          }
        ]) enabledSlots
      );
    in
    pkgs.runCommand "microvm-batch-jobs"
      {
        inherit evalMarker;
        controllerBin = "${controllerBin}/bin/agent-job-controller";
        workerBin = "${workerBin}/bin/agent-job-worker";
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        batchNames = lib.concatStringsSep " " agentRegistry.batchNames;
        # Agents that exist but cannot run unattended — empty today, but the
        # guard stays honest if one is added later.
        nonBatchNames = lib.concatStringsSep " " (
          lib.subtractLists agentRegistry.batchNames agentRegistry.names
        );
        jobSpecPath = jobs.guestSpec;
        jobPromptPath = jobs.guestPrompt;
      }
      ''
        for n in $batchNames; do
          grep -q "$n) run_agent" "$workerBin" \
            || { echo "batch agent '$n' missing from the generated worker dispatch" >&2; exit 1; }
        done
        # --- host lifecycle surface (ticket 4 B) --------------------------
        for c in cmd_submit cmd_cancel cmd_recover validate_batch_agent_name \
                 validate_timeout prepare_job clear_job cleanup_slot_owned \
                 write_session_marker allocate_slot owner_alive proc_start_time; do
          grep -q "$c()" "$launcherBin" \
            || { echo "host launcher is missing the '$c' helper" >&2; exit 1; }
        done
        # The spec must name the guest-side prompt path the guest validates by
        # exact match, so host and guest cannot drift.
        grep -q "$jobPromptPath" "$launcherBin" \
          || { echo "launcher does not reference the guest prompt path" >&2; exit 1; }
        # Cancellation must be token-guarded, not slot-name-guarded.
        grep -q "allocation token changed" "$launcherBin" \
          || { echo "launcher does not guard destructive ops with the allocation token" >&2; exit 1; }
        # recover --dry-run must exist and must never delete a clone.
        grep -q -- "--dry-run" "$launcherBin" \
          || { echo "launcher has no recover --dry-run" >&2; exit 1; }
        grep -q "keeping the workspace clone" "$launcherBin" \
          || { echo "recover does not promise to keep workspace clones" >&2; exit 1; }
        # The controller must reject a spec that names an executable, and must
        # validate the schema version and the prompt path.
        grep -q "spec must not contain an executable path" "$controllerBin" \
          || { echo "the controller does not reject executable paths in the spec" >&2; exit 1; }
        grep -q "unsupported spec version" "$controllerBin" \
          || { echo "the controller does not validate the spec version" >&2; exit 1; }
        {
          echo "microvm-batch-jobs: guest controller $controllerBin"
          echo "  guest worker: $workerBin"
          echo "  host launcher: $launcherBin"
          echo "  batch agents: $batchNames"
          echo "  non-batch agents: ''${nonBatchNames:-<none>}"
          echo "  job spec (guest): $jobSpecPath"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l2) BATCH RESULT INTEGRITY (ticket 7): the authoritative result channel  #
  #      must be writable ONLY by a trusted guest-side controller, and the    #
  #      host must accept a result only for the ACTIVE allocation.            #
  #                                                                          #
  #      Eval part: controller and worker run under SEPARATE identities; the  #
  #      controller directory is root-only and outside every worker-writable  #
  #      path; the worker unit cannot even see it; the spec is root-only (it  #
  #      carries the allocation token); the launcher reads the result only    #
  #      through the ONE verifier.                                           #
  #      EXECUTED part: tests/microvm-batch-result-integrity.sh really runs   #
  #      the verifier and the guest-side permission assertions against        #
  #      forged / stale / malformed / symlinked / world-writable fixtures     #
  #      (see the honesty note in that script: fakeroot fakes metadata, so    #
  #      the KERNEL-level write denial is a real-KVM property covered by      #
  #      runtime-validation.sh --section forgery).                            #
  # ---------------------------------------------------------------------- #
  microvm-batch-result-integrity =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      ctrlUnit = guest0Cfg.systemd.services.agent-job-controller;
      ctrl = ctrlUnit.serviceConfig;
      workerUnit = guest0Cfg.systemd.services."${jobs.workerUnitTemplate}";
      worker = workerUnit.serviceConfig;
      tmpfiles = enabledCfg.systemd.tmpfiles.rules;
      # Every path the UNTRUSTED worker can write, per the layout.
      workerWritable = [
        (jobs.hostWorkerDir refSlot.name)
        "${microvmOpts.stateRoot}/${refSlot.name}/workspace"
      ];
      evalMarker = mkEvalCheck "microvm-batch-result-integrity-eval" [
        {
          # THE trust split: the writer of the authoritative result is not the
          # identity that runs the coding agent.
          assertion = (ctrl.User or "root") != (worker.User or "root");
          message = "the job controller and the job worker must run under DIFFERENT guest identities";
        }
        {
          assertion = (worker.User or "") == "agent" && jobs.workerUid != 0;
          message = "the batch worker must run as the unprivileged guest agent (uid != 0)";
        }
        {
          # The result lives in the controller area, never in the worker area.
          assertion =
            lib.hasPrefix "${jobs.guestControllerDir}/" jobs.guestResult
            && !lib.hasPrefix jobs.guestWorkerDir jobs.guestResult;
          message = "the authoritative result must live inside the controller directory";
        }
        {
          assertion = !builtins.any (d: lib.hasPrefix "${d}/" (jobs.hostResult refSlot.name)) workerWritable;
          message = "the host-side result path must not sit inside a worker-writable directory";
        }
        {
          # 0700 root:root is what makes the channel unwritable AND unreadable
          # for the worker (the token in the spec must not leak either).
          assertion =
            jobs.controllerDirMode == "0700"
            && builtins.elem "d ${jobs.hostControllerDir refSlot.name} 0700 root root - -" tmpfiles;
          message = "the controller directory must be created root:root 0700";
        }
        {
          assertion = jobs.specMode == "0400";
          message = "the job spec must be root-only 0400 (it carries the allocation token)";
        }
        {
          assertion = jobs.promptMode == "0444";
          message = "the prompt must be world-readable 0444 (the worker reads it) and writable by nobody";
        }
        {
          # Belt and braces on top of the mode bits: the worker's mount
          # namespace does not even contain the controller directory.
          assertion = builtins.elem "-${jobs.guestControllerDir}" (worker.InaccessiblePaths or [ ]);
          message = "the worker unit must mask the controller directory (InaccessiblePaths)";
        }
        {
          assertion = builtins.elem "-${jobs.guestInputDir}" (worker.ReadOnlyPaths or [ ]);
          message = "the worker unit must see the immutable input read-only";
        }
        {
          assertion = builtins.elem "-${jobs.guestControllerDir}" (ctrl.ReadWritePaths or [ ]);
          message = "the controller unit must be able to write its own directory";
        }
        {
          assertion = builtins.elem "-${jobs.guestInputDir}" (ctrl.ReadOnlyPaths or [ ]);
          message = "the controller unit must see the immutable input read-only";
        }
        {
          # The controller must not get broad write access to the workspace:
          # that is the worker's job.
          assertion = !builtins.elem "-/workspace" (ctrl.ReadWritePaths or [ ]);
          message = "the controller must NOT have /workspace in its ReadWritePaths";
        }
        {
          # Timeout/cancellation kill a CGROUP, not a pid, so a double-forked
          # repository process cannot outlive the job. Asserted EXPLICITLY (a
          # `worker.KillMode or "control-group"` default would also pass if the
          # setting were deleted, because that is systemd's default).
          assertion = (worker ? KillMode) && worker.KillMode == "control-group";
          message = "the worker unit must explicitly set KillMode=control-group";
        }
        {
          # BLOCKER regression guard (token secrecy, layer 3): /proc/<pid>/cmdline
          # is world-readable 0444 and the worker shares the guest PID
          # namespace, so every process it does not own must be hidden from it.
          assertion = (worker.ProtectProc or "") == "invisible";
          message = "the worker unit must set ProtectProc=invisible (foreign /proc entries would otherwise expose a root process's argv)";
        }
        {
          # The worker's stdout/stderr are opened by the guest's systemd AS
          # ROOT, following symlinks, so they must NOT live in the
          # worker-writable directory (whose owner could rename it and plant a
          # symlink); they live next to it, root-owned.
          assertion =
            lib.hasPrefix "${jobs.guestWorkerLogsDir}/" jobs.guestWorkerStdout
            && !lib.hasPrefix "${jobs.guestWorkerDir}/" jobs.guestWorkerStdout
            && !lib.hasPrefix "${jobs.guestWorkerDir}/" jobs.guestWorkerStderr;
          message = "the worker's log files must live in the ROOT-owned worker-logs directory, not inside the worker-writable one";
        }
        {
          assertion = builtins.elem "d ${jobs.hostWorkerLogsDir refSlot.name} ${jobs.workerLogsDirMode} root root - -" tmpfiles;
          message = "the worker log directory must be pre-created root:root ${jobs.workerLogsDirMode}";
        }
        {
          # An archived result carries the run's allocation token, so the
          # archive is root-only — not world-readable 0755/0644.
          assertion = builtins.elem "d ${jobs.resultsDir} 0700 root root - -" tmpfiles;
          message = "the host result archive must be root-only 0700 (an archived result carries the allocation token)";
        }
        {
          # The CONTROLLER needs an effective runtime ceiling of its own.
          # `Type=oneshot` ignores RuntimeMaxSec and defaults TimeoutStartSec to
          # infinity, so the ceiling must be TimeoutStartSec.
          assertion =
            ctrl.Type == "oneshot"
            && !(ctrl ? RuntimeMaxSec)
            &&
              ctrl.TimeoutStartSec == microvmOpts.job.maxTimeoutSeconds + 2 * microvmOpts.job.gracePeriodSeconds;
          message = "the controller must carry its ceiling as TimeoutStartSec (Type=oneshot ignores RuntimeMaxSec), sized above the worker's own ceiling";
        }

        {
          assertion = (worker.TimeoutStopSec or 0) == jobs.workerKillGraceSeconds;
          message = "the worker unit must give the same SIGTERM grace the controller uses";
        }
        {
          # The worker template must not be startable by anything but the
          # controller (no wantedBy/requiredBy/upholds).
          assertion =
            (workerUnit.wantedBy or [ ]) == [ ]
            && (workerUnit.requiredBy or [ ]) == [ ]
            && (workerUnit.upheldBy or [ ]) == [ ];
          message = "the worker template must never be pulled in by a target — only the controller starts it";
        }
        {
          # The instance name is the AGENT name (registry-constrained), never a
          # caller-supplied task id.
          assertion = builtins.all (
            n: jobs.workerUnit n == "agent-job-worker@${n}.service"
          ) agentRegistry.batchNames;
          message = "the worker unit instance must be the registry agent name";
        }
        {
          # Untrusted worker output goes to the log files systemd opens for it,
          # inside the ROOT-owned log directory of the job share (never into the
          # controller channel, and never into a directory the worker could
          # rename — see the guestWorkerLogsDir assertion below).
          assertion =
            (worker.StandardOutput or "") == "append:${jobs.guestWorkerStdout}"
            && (worker.StandardError or "") == "append:${jobs.guestWorkerStderr}"
            && lib.hasPrefix "${jobs.guestWorkerLogsDir}/" jobs.guestWorkerStdout
            && !lib.hasPrefix "${jobs.guestControllerDir}/" jobs.guestWorkerStdout;
          message = "worker stdout/stderr must be written into the root-owned worker log directory of the job share";
        }
        {
          # The schema was bumped together with the layout change; there is no
          # compatibility mode for the old, forgeable v1 result.
          assertion = jobs.specVersion >= 2;
          message = "the batch schema version must have been bumped for the controller/worker split";
        }
        {
          assertion = builtins.elem "R ${jobs.slotDir refSlot.name}/out - - - - -" tmpfiles;
          message = "the legacy guest-writable out/ directory must be removed by tmpfiles";
        }
        {
          # The old single-identity unit must be gone: it was the forgery hole.
          assertion = !(guest0Cfg.systemd.services ? agent-job);
          message = "the old combined agent-job unit must no longer exist";
        }
      ];
    in
    pkgs.runCommand "microvm-batch-result-integrity"
      {
        inherit evalMarker;
        nativeBuildInputs = [
          pkgs.fakeroot
          pkgs.jq
          pkgs.coreutils
        ];
        harness = ./microvm-batch-result-integrity.sh;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        controllerBin = "${jobs.controller}/bin/agent-job-controller";
        workerBin = "${jobs.worker}/bin/agent-job-worker";
        # --- environment of the EXECUTED harness ---------------------------
        VERIFIER = lib.getExe jobs.resultVerifier;
        ASSERT_PATHS = lib.getExe jobs.assertPaths;
        SPEC_VERSION = toString jobs.specVersion;
        CONTROLLER_VERSION = toString jobs.controllerVersion;
        INPUT_SUBDIR = jobs.inputSubdir;
        CONTROLLER_SUBDIR = jobs.controllerSubdir;
        WORKER_SUBDIR = jobs.workerSubdir;
        WORKER_LOGS_SUBDIR = jobs.workerLogsSubdir;
        SPEC_NAME = jobs.specName;
        # Guest paths the worker must never mention (checked by grep below).
        CONTROLLER_DIR = jobs.guestControllerDir;
        RESULT_PATH = jobs.guestResult;
        SPEC_PATH = jobs.guestSpec;
        PROMPT_NAME = jobs.promptName;
        CANCEL_NAME = jobs.cancelName;
        RESULT_NAME = jobs.resultName;
        STATE_NAME = jobs.controllerStateName;
        WORKER_STDOUT_NAME = jobs.workerStdoutName;
        WORKER_STDERR_NAME = jobs.workerStderrName;
        WORKER_UID = toString jobs.workerUid;
        SLOT = refSlot.name;
        TASK = "integrity-task";
        AGENT = lib.head agentRegistry.batchNames;
        SPEC_MODE = jobs.specMode;
        PROMPT_MODE = jobs.promptMode;
        INPUT_DIR_MODE = jobs.inputDirMode;
        CONTROLLER_DIR_MODE = jobs.controllerDirMode;
        WORKER_DIR_MODE = jobs.workerDirMode;
        WORKER_LOGS_DIR_MODE = jobs.workerLogsDirMode;
      }
      ''
        # --- (1) the host launcher must read results ONLY via the verifier ---
        grep -q "$VERIFIER" "$launcherBin" \
          || { echo "the launcher does not use the result verifier" >&2; exit 1; }
        for h in verify_job_document verify_job_result job_phase host_result_json \
                 archive_controller_result request_guest_cancel job_controller_dir \
                 job_worker_dir; do
          grep -q "$h()" "$launcherBin" \
            || { echo "the launcher is missing the '$h' helper" >&2; exit 1; }
        done
        # It must never parse or copy the guest-written documents by hand (that
        # was the old `jq -r .state out/result.json` + `cp result.json` path):
        # the ONLY consumer of those paths is the verifier.
        if grep -nE "(jq|cat|cp|read|source|eval)[^#]*job_(result|ctrl_state)" "$launcherBin" \
             | grep -q .; then
          echo "the launcher reads/copies a guest document outside the verifier:" >&2
          grep -nE "(jq|cat|cp|read|source|eval)[^#]*job_(result|ctrl_state)" "$launcherBin" >&2
          exit 1
        fi
        # The authoritative path must be the controller one, and the legacy
        # guest-writable out/ path must be gone.
        grep -q "JOB_CONTROLLER_SUBDIR" "$launcherBin" \
          || { echo "the launcher does not know the controller subdir" >&2; exit 1; }
        # 256-bit allocation tokens from the kernel CSPRNG.
        grep -q "od -An -tx1 -N32 /dev/urandom" "$launcherBin" \
          || { echo "the launcher does not mint 256-bit allocation tokens" >&2; exit 1; }

        # --- (2) the controller is the only writer of the result -------------
        grep -q "agent-job-assert-paths" "$controllerBin" \
          || { echo "the controller does not assert its trust boundary" >&2; exit 1; }
        grep -q "allocationToken" "$controllerBin" \
          || { echo "the controller does not record the allocation token" >&2; exit 1; }
        grep -q "kill-whom=all" "$controllerBin" \
          || { echo "the controller does not force-kill the whole worker cgroup" >&2; exit 1; }
        # --- (2b) the allocation token never travels in an ARGUMENT VECTOR ---
        # /proc/<pid>/cmdline is world-readable (0444) while /proc/<pid>/environ
        # is 0400, so `--arg allocationToken <token>` would publish the ACTIVE
        # token to every local process — inside the guest, to the untrusted
        # worker. (The EXECUTED proof that no jq the controller runs ever sees
        # the token in its argv lives in microvm-batch-controller-smoke; this is
        # the cheap structural guard against a regression.)
        for b in "$controllerBin" "$launcherBin"; do
          if grep -q -- "--arg allocationToken" "$b"; then
            echo "$b passes the allocation token as a jq ARGUMENT (readable via /proc/<pid>/cmdline)" >&2
            exit 1
          fi
          grep -q 'ALLOC_TOKEN=' "$b" \
            || { echo "$b does not hand the allocation token to jq in the environment" >&2; exit 1; }
        done
        if grep -q -- "--token" "$launcherBin"; then
          echo "the launcher still passes --token to the verifier (world-readable argv)" >&2
          exit 1
        fi
        grep -q "AGENT_JOB_EXPECTED_TOKEN" "$launcherBin" \
          || { echo "the launcher does not pass the expected token in the environment" >&2; exit 1; }
        # --- (2c) the deadline is measured on a CLOCK, not in poll iterations -
        grep -q "SECONDS >= timeout_s" "$controllerBin" \
          || { echo "the controller does not measure its deadline against the wall clock" >&2; exit 1; }
        if grep -q "waited >= timeout_s" "$controllerBin"; then
          echo "the controller still counts poll iterations as its deadline" >&2; exit 1
        fi
        # The worker must not know anything about the result channel or the
        # spec: it only ever reads the prompt and runs one registry agent.
        for forbidden in "$CONTROLLER_DIR" "$RESULT_PATH" "$SPEC_PATH"; do
          if grep -q -- "$forbidden" "$workerBin"; then
            echo "the worker references $forbidden" >&2; exit 1
          fi
        done

        # --- (3) EXECUTED regression harness --------------------------------
        mkdir -p work && cd work
        # fakeroot lets the fixtures carry the real ownership split
        # (root-owned controller vs agent-owned worker) inside the build sandbox.
        fakeroot -- bash "$harness" > report.txt || {
          echo "--- batch result integrity harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-batch-result-integrity"
          echo "  launcher:   $launcherBin"
          echo "  controller: $controllerBin"
          echo "  worker:     $workerBin"
          echo "  verifier:   $VERIFIER"
          echo
          cat report.txt
          echo
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l3) BATCH CONTROLLER SMOKE TEST (ticket 7): actually RUN the trusted     #
  #      guest-side controller. `bwrap` supplies the paths it hard-codes and   #
  #      the guest hostname, `systemctl` is stubbed by bind-mounting a script  #
  #      over the exact store path the controller resolves, and `fakeroot`     #
  #      gives the fixture the real ownership split (a single-uid namespace    #
  #      cannot express two owners). Then the HOST verifier is run against     #
  #      the documents the controller produced.                               #
  #                                                                          #
  #      This is NOT a KVM test: it proves that host and guest agree on the    #
  #      protocol and that the controller's validation / deadline /            #
  #      cancellation / result-writing logic works, not that the guest kernel  #
  #      denies the worker access (see runtime-validation.sh --section        #
  #      forgery). It SKIPS honestly if the sandbox forbids user namespaces.   #
  # ---------------------------------------------------------------------- #
  microvm-batch-controller-smoke =
    pkgs.runCommand "microvm-batch-controller-smoke"
      {
        nativeBuildInputs = [
          pkgs.bubblewrap
          pkgs.fakeroot
          pkgs.jq
          pkgs.coreutils
        ];
        harness = ./microvm-batch-controller-smoke.sh;
        BWRAP = lib.getExe pkgs.bubblewrap;
        FAKEROOT = "${pkgs.fakeroot}/bin/fakeroot";
        BASH_BIN = "${pkgs.bash}/bin/bash";
        CONTROLLER = lib.getExe jobs.controller;
        VERIFIER = lib.getExe jobs.resultVerifier;
        # The exact `systemctl` the controller resolves from its own PATH; the
        # harness bind-mounts its stub over this path, so the script under test
        # stays byte-identical to the one in the guest closure.
        SYSTEMCTL_TARGET = "${pkgs.systemd}/bin/systemctl";
        # The exact `jq` the controller resolves. The harness bind-mounts an
        # ARGV RECORDER over it, so it can PROVE (by execution, not by reading
        # the source) that the allocation token never reaches a process's
        # world-readable /proc/<pid>/cmdline.
        JQ_TARGET = "${pkgs.jq}/bin/jq";
        SPEC_VERSION = toString jobs.specVersion;
        INPUT_SUBDIR = jobs.inputSubdir;
        CONTROLLER_SUBDIR = jobs.controllerSubdir;
        WORKER_SUBDIR = jobs.workerSubdir;
        WORKER_LOGS_SUBDIR = jobs.workerLogsSubdir;
        SPEC_NAME = jobs.specName;
        PROMPT_NAME = jobs.promptName;
        CANCEL_NAME = jobs.cancelName;
        RESULT_NAME = jobs.resultName;
        STATE_NAME = jobs.controllerStateName;
        WORKER_STDOUT_NAME = jobs.workerStdoutName;
        WORKER_UID = toString jobs.workerUid;
        SLOT = refSlot.name;
        TASK = "smoke-task";
        AGENT = lib.head agentRegistry.batchNames;
      }
      ''
        mkdir -p work && cd work
        bash "$harness" > report.txt 2>&1 || {
          echo "--- batch controller smoke test FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-batch-controller-smoke"
          echo "  controller: $CONTROLLER"
          echo "  verifier:   $VERIFIER"
          echo
          cat report.txt
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l4) HOST SUBMIT PATH (ticket 7): actually RUN `agent-microvm submit`.     #
  #      `bwrap` (tmpfs root) + `fakeroot` give the launcher the absolute      #
  #      roots and the uid 0 it needs; `systemctl`, `mount`, `umount` and      #
  #      `findmnt` are stubbed by bind-mounting over the exact store paths it   #
  #      resolves, and the `systemctl start microvm@<slot>` stub PLAYS THE      #
  #      GUEST: it records the effective ownership/modes of the job share the   #
  #      launcher just created and plants the scenario's "result".             #
  #                                                                          #
  #      So this really exercises: the layout (input 0755/spec 0400/controller  #
  #      0700/worker agent-owned), the 256-bit allocation token in the spec,    #
  #      and that ONLY a controller-authenticated result for the ACTIVE         #
  #      allocation is accepted — a foreign token, a foreign slot, a v1         #
  #      document, a malformed one, worker-written fakes and silence all become #
  #      exit 70. It SKIPS honestly if the sandbox forbids user namespaces.     #
  # ---------------------------------------------------------------------- #
  microvm-batch-launcher-submit =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
    in
    pkgs.runCommand "microvm-batch-launcher-submit"
      {
        nativeBuildInputs = [
          pkgs.bubblewrap
          pkgs.fakeroot
          pkgs.jq
          pkgs.git
          pkgs.coreutils
        ];
        harness = ./microvm-batch-launcher-submit.sh;
        LAUNCHER = "${hostLauncher}/bin/agent-microvm";
        BWRAP = lib.getExe pkgs.bubblewrap;
        FAKEROOT = "${pkgs.fakeroot}/bin/fakeroot";
        BASH_BIN = "${pkgs.bash}/bin/bash";
        # The EXACT binaries the launcher resolves from its own runtimeInputs;
        # the harness bind-mounts its stubs over these, so the launcher under
        # test stays byte-identical to the installed one.
        SYSTEMCTL_TARGET = "${pkgs.systemd}/bin/systemctl";
        # `util-linux`'s bin/mount and bin/umount are SYMLINKS into its
        # separate `mount` output; bwrap resolves the destination, so bind over
        # the real files.
        MOUNT_TARGET = "${pkgs.util-linux.mount}/bin/mount";
        UMOUNT_TARGET = "${pkgs.util-linux.mount}/bin/umount";
        FINDMNT_TARGET = "${pkgs.util-linux.bin}/bin/findmnt";
        # The exact `jq` the launcher resolves: the harness binds an ARGV
        # RECORDER over it to PROVE, by execution, that the allocation token
        # never lands in a world-readable /proc/<pid>/cmdline.
        JQ_TARGET = "${pkgs.jq}/bin/jq";
        # The exact `curl` the launcher resolves (from its runtimeInputs): the
        # harness binds a stub over it for the endpoint-preflight test, so the
        # launcher under test stays byte-identical to the installed one.
        CURL_TARGET = "${pkgs.curl}/bin/curl";
        RUNTIME_ROOT = microvmOpts.runtimeRoot;
        STATE_ROOT = microvmOpts.stateRoot;
        INPUT_SUBDIR = jobs.inputSubdir;
        CONTROLLER_SUBDIR = jobs.controllerSubdir;
        WORKER_SUBDIR = jobs.workerSubdir;
        WORKER_LOGS_SUBDIR = jobs.workerLogsSubdir;
        WORKER_STDERR_NAME = jobs.workerStderrName;
        SPEC_NAME = jobs.specName;
        PROMPT_NAME = jobs.promptName;
        RESULT_NAME = jobs.resultName;
        SPEC_VERSION = toString jobs.specVersion;
        CONTROLLER_VERSION = toString jobs.controllerVersion;
        WORKER_UID = toString jobs.workerUid;
        AGENT = lib.head agentRegistry.batchNames;
      }
      ''
        mkdir -p work && cd work
        bash "$harness" > report.txt 2>&1 || {
          echo "--- host submit harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-batch-launcher-submit"
          echo "  launcher: $LAUNCHER"
          echo
          cat report.txt
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l5) RECOVERY PATH: actually RUN `agent-microvm recover` against a stale   #
  #      workspace bind mount whose holder (the slot's virtiofsd) only lets    #
  #      go once its unit is stopped, and against one that never lets go.      #
  #      Same stubbing technique as (l4). This is a BEHAVIOURAL test of the     #
  #      "never accept a lazy unmount" rule: the `umount` stub refuses `-l`     #
  #      outright and returns EBUSY until `microvm-virtiofsd@<slot>` has been   #
  #      stopped, and `findmnt` keeps reporting the mount until it is really    #
  #      gone — so a launcher that lazily unmounted (or ignored the failure)    #
  #      fails this check instead of printing "unmounting …" and exiting 0.    #
  # ---------------------------------------------------------------------- #
  microvm-launcher-recover =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
    in
    pkgs.runCommand "microvm-launcher-recover"
      {
        nativeBuildInputs = [
          pkgs.bubblewrap
          pkgs.fakeroot
          pkgs.jq
          pkgs.coreutils
        ];
        harness = ./microvm-launcher-recover.sh;
        LAUNCHER = "${hostLauncher}/bin/agent-microvm";
        BWRAP = lib.getExe pkgs.bubblewrap;
        FAKEROOT = "${pkgs.fakeroot}/bin/fakeroot";
        BASH_BIN = "${pkgs.bash}/bin/bash";
        SYSTEMCTL_TARGET = "${pkgs.systemd}/bin/systemctl";
        MOUNT_TARGET = "${pkgs.util-linux.mount}/bin/mount";
        UMOUNT_TARGET = "${pkgs.util-linux.mount}/bin/umount";
        FINDMNT_TARGET = "${pkgs.util-linux.bin}/bin/findmnt";
        RUNTIME_ROOT = microvmOpts.runtimeRoot;
        STATE_ROOT = microvmOpts.stateRoot;
        SLOT = refSlot.name;
        # A slot name from the PREVIOUS naming scheme (`agent-<i>`, before the
        # per-class `agent-<class>-<i>` rename). Asserted by the harness not to
        # be in the current pool, so the "foreign per-slot state" scenarios
        # cannot silently degrade into "current slot" ones.
        FOREIGN_SLOT = "agent-0";
      }
      ''
        mkdir -p work && cd work
        bash "$harness" > report.txt 2>&1 || {
          echo "--- recover harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-launcher-recover"
          echo "  launcher: $LAUNCHER"
          echo
          cat report.txt
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l6) The GUEST COMMAND TRANSPORT of the real-KVM suite. That suite needs   #
  #      /dev/kvm and root, so CI can never run it — but the mechanism that    #
  #      decides whether ANY of its guest-side denials mean anything (does the  #
  #      command reach the guest as written, through OpenSSH's argv flattening  #
  #      and the agent's fish login shell?) can be executed here, against a     #
  #      stub that reproduces exactly that path. Includes a NEGATIVE CONTROL:   #
  #      the previous, unquoted transport must FAIL this check.                 #
  # ---------------------------------------------------------------------- #
  microvm-rtv-transport =
    pkgs.runCommand "microvm-rtv-transport"
      {
        nativeBuildInputs = [
          pkgs.coreutils
          pkgs.fish
        ];
        harness = ./microvm-rtv-transport.sh;
        # The suite under test, and the very shell guest.nix gives the agent
        # user (`users.users.agent.shell = pkgs.fish`) — the re-parsing side of
        # the transport, so the stub is not a guess about which shell runs.
        SUITE = ../modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh;
        FISH = lib.getExe pkgs.fish;
      }
      ''
        mkdir -p work && cd work
        export HOME=$PWD
        bash "$harness" > report.txt 2>&1 || {
          echo "--- runtime-validation transport harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-rtv-transport"
          echo "  suite: $SUITE"
          echo
          cat report.txt
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (rtv-dispatch) the real-KVM suite's SECTION DISPATCH (Bug 2 / Gap 3).    #
  #     The suite needs /dev/kvm + root, so CI cannot run its section         #
  #     bodies. But CI CAN run the part that decides WHICH sections run      #
  #     and which are SKIPPED: the endpoint preflight + section planning.    #
  #     Bug 2: under `--section all`, an unreachable endpoint used to abort    #
  #     the ENTIRE run (so the operator validated nothing, incl. forgery).    #
  #     The fix runs the five endpoint-independent sections and skips only    #
  #     net+forgery with a loud reason; hard-abort only for `--section         #
  #     net`/`--section forgery` alone. This harness sources the dispatch      #
  #     block verbatim with stubbed section bodies + a stub curl, asserting: #
  #       * all+up -> all 7 run, exit 0, plan + per-section tallies + summary; #
  #       * all+down -> 5 run, net+forgery SKIPPED (loud, counted, with the    #
  #       doctor hint), exit non-zero (so the skipped security-critical        #
  #       forgery section cannot pass silently);                              #
  #       * net+down / forgery+down -> hard-abort;                            #
  #       * boot+down -> runs (endpoint not needed);                          #
  #       * an unknown section is rejected, not silently dropped.             #
  # ---------------------------------------------------------------------- #
  microvm-rtv-dispatch =
    pkgs.runCommand "microvm-rtv-dispatch"
      {
        nativeBuildInputs = [
          pkgs.coreutils
          pkgs.bash
        ];
        harness = ./microvm-rtv-dispatch.sh;
        SUITE = ../modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh;
      }
      ''
        mkdir -p work && cd work
        export HOME=$PWD
        bash "$harness" > report.txt 2>&1 || {
          echo "--- runtime-validation dispatch harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-rtv-dispatch"
          echo "  suite: $SUITE"
          echo
          cat report.txt
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (l5-doctor) `agent-microvm doctor` against STUBBED host state. Bug 1:    #
  #     `doctor` false-failed the LiteLLM ACCEPT rule because it grepped     #
  #     `iptables -S`'s PRINTED form for `-d <addr> <space> ...`, but       #
  #     `iptables -S` canonicalises the address (`-d 192.168.83.1` ->        #
  #     `-d 192.168.83.1/32`), so the pattern's required space never          #
  #     matched and `doctor` ALWAYS exited non-zero on a healthy host.       #
  #     The fix tests the rule with `iptables -C <spec>` (exit 0 = exists),  #
  #     built from the SAME variables network.nix installs the rule with.    #
  #     This harness drives the REAL `cmd_doctor` through stubs (systemctl,  #
  #     curl, ip, iptables) the way tests/microvm-batch-*.sh drive the      #
  #     launcher: a stubbed HEALTHY host must report OK (exit 0), and a      #
  #     stubbed BROKEN one (the rule genuinely absent) must report non-OK   #
  #     (exit non-zero) and name the broken check — a negative control that #
  #     the check was not loosened into something that passes vacuously.     #
  # ---------------------------------------------------------------------- #
  microvm-doctor =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
    in
    pkgs.runCommand "microvm-doctor"
      {
        nativeBuildInputs = [
          pkgs.bubblewrap
          pkgs.fakeroot
          pkgs.coreutils
        ];
        harness = ./microvm-doctor.sh;
        LAUNCHER = "${hostLauncher}/bin/agent-microvm";
        BWRAP = lib.getExe pkgs.bubblewrap;
        FAKEROOT = "${pkgs.fakeroot}/bin/fakeroot";
        BASH_BIN = "${pkgs.bash}/bin/bash";
        # The EXACT binaries the launcher resolves from its own runtimeInputs;
        # the harness bind-mounts its stubs over these, so the launcher under
        # test stays byte-identical to the installed one.
        SYSTEMCTL_TARGET = "${pkgs.systemd}/bin/systemctl";
        IP_TARGET = "${pkgs.iproute2}/bin/ip";
        IPTABLES_TARGET = "${pkgs.iptables}/bin/iptables";
        CURL_TARGET = "${pkgs.curl}/bin/curl";
        RUNTIME_ROOT = microvmOpts.runtimeRoot;
        # The SAME variables the launcher bakes in (and network.nix installs
        # the rule with), so the harness can assert the doctor's `iptables -C`
        # spec matches the rule exactly.
        GATEWAY = microvmOpts.gatewayAddress;
        SUBNET = microvmOpts.subnet;
        LITELLM_PORT = toString microvmOpts.litellmPort;
        BRIDGE = microvmOpts.bridgeName;
        SLOT_NAMES = lib.concatStringsSep " " (map (s: s.name) enabledSlots);
      }
      ''
        mkdir -p work && cd work
        bash "$harness" > report.txt 2>&1 || {
          echo "--- doctor harness FAILED ---" >&2
          cat report.txt >&2
          exit 1
        }
        {
          echo "microvm-doctor"
          echo "  launcher: $LAUNCHER"
          echo
          cat report.txt
        } > "$out"
      '';
  # ---------------------------------------------------------------------- #
  # (k) NETWORK PROFILES (ticket 3 C): render all four profiles and assert   #
  #     the rules each one must and must NOT contain, plus the guest-side    #
  #     configuration derived from the SAME decision (LiteLLM forwarder,     #
  #     http_proxy, resolvers). This is what keeps `internet` from being a   #
  #     mere firewall verdict (it must also carry NAT + a DNS policy) and    #
  #     `package-access` from silently becoming general egress.              #
  # ---------------------------------------------------------------------- #
  microvm-network-profiles =
    let
      # One variant host per profile; only the firewall strings and a few
      # guest attrs are forced, never a toplevel (see the note on
      # `failedAssertions` above about eval cost).
      variant =
        profile:
        (self.nixosConfigurations.test-f13.extendModules {
          modules = [
            {
              myconfig.ai.microvm = {
                networkProfile = lib.mkForce profile;
                acknowledgeInsecureNetwork = true;
                packageProxyPort = 3128;
              };
            }
          ];
        });
      fwOf = profile: (variant profile).config.networking.firewall.extraCommands;
      guestOf = profile: (variant profile).config.microvm.vms.${refSlot.name}.config.config;
      subnet = microvmOpts.subnet;
      litellmAccept = "AGENT_MICROVM_INPUT -s ${subnet} -d ${gateway} -p tcp --dport ${port} -j ACCEPT";
      proxyAccept = "AGENT_MICROVM_INPUT -s ${subnet} -d ${gateway} -p tcp --dport 3128 -j ACCEPT";
      internetAccept = "AGENT_MICROVM_FORWARD -s ${subnet} -j ACCEPT";
      has = profile: needle: lib.hasInfix needle (fwOf profile);
      # Invariants that must hold in EVERY profile.
      invariants = profile: [
        {
          assertion = has profile "AGENT_MICROVM_INPUT -d 169.254.169.254 -j DROP";
          message = "${profile}: cloud-metadata must be dropped in INPUT";
        }
        {
          assertion = has profile "AGENT_MICROVM_FORWARD -d 169.254.169.254 -j DROP";
          message = "${profile}: cloud-metadata must be dropped in FORWARD";
        }
        {
          assertion = has profile "AGENT_MICROVM_FORWARD -s ${subnet} -d ${subnet} -j DROP";
          message = "${profile}: guest-to-guest traffic must be dropped";
        }
        {
          assertion = has profile "AGENT_MICROVM_FORWARD -s ${subnet} -d 10.0.0.0/8 -j DROP";
          message = "${profile}: private ranges must be dropped";
        }
        {
          assertion =
            has profile "AGENT_MICROVM_INPUT -j DROP" && has profile "AGENT_MICROVM_FORWARD -j DROP";
          message = "${profile}: INPUT and FORWARD must end in a terminal DROP";
        }
        {
          # Host -> guest control traffic must survive in every profile.
          assertion = has profile "AGENT_MICROVM_OUTPUT -j ACCEPT";
          message = "${profile}: host-originated control traffic must be allowed";
        }
      ];
    in
    mkEvalCheck "microvm-network-profiles" (
      lib.concatMap invariants [
        "offline"
        "proxy-only"
        "package-access"
        "internet"
      ]
      ++ [
        # --- offline ----------------------------------------------------
        {
          assertion = !(has "offline" litellmAccept);
          message = "offline must NOT allow the LiteLLM endpoint";
        }
        {
          assertion = !(has "offline" "MASQUERADE") && !(has "offline" internetAccept);
          message = "offline must NOT allow routing/NAT";
        }
        {
          assertion = !(has "offline" "--dport 53");
          message = "offline must NOT allow DNS";
        }
        {
          # No conntrack ACCEPT in FORWARD: nothing is routed at all.
          assertion = !(lib.hasInfix "AGENT_MICROVM_FORWARD -m state" (fwOf "offline"));
          message = "offline must not allow ESTABLISHED forwarding";
        }
        {
          assertion = !((guestOf "offline").systemd.sockets ? litellm-forwarder);
          message = "offline guest must not run the loopback LiteLLM forwarder";
        }
        # --- proxy-only (the secure default) ----------------------------
        {
          assertion = microvmOpts.networkProfile == "proxy-only";
          message = "proxy-only must remain the default profile on the reference host";
        }
        {
          assertion = has "proxy-only" litellmAccept;
          message = "proxy-only must allow the LiteLLM endpoint";
        }
        {
          assertion =
            !(has "proxy-only" internetAccept)
            && !(has "proxy-only" "MASQUERADE")
            && !(has "proxy-only" "--dport 53")
            && !(has "proxy-only" proxyAccept);
          message = "proxy-only must allow NOTHING besides LiteLLM (no internet, NAT, DNS or package proxy)";
        }
        {
          assertion = (guestOf "proxy-only").systemd.sockets ? litellm-forwarder;
          message = "proxy-only guest must run the loopback LiteLLM forwarder";
        }
        # --- package-access ---------------------------------------------
        {
          assertion = has "package-access" litellmAccept && has "package-access" proxyAccept;
          message = "package-access must allow LiteLLM and the explicit host proxy port";
        }
        {
          # Explicitly NOT general egress.
          assertion =
            !(has "package-access" internetAccept)
            && !(has "package-access" "MASQUERADE")
            && !(has "package-access" "--dport 53");
          message = "package-access must NOT provide routing, NAT or DNS (it is not unrestricted internet)";
        }
        {
          assertion =
            (guestOf "package-access").environment.variables.http_proxy or null == "http://${gateway}:3128";
          message = "package-access guest must point http_proxy at the host package proxy";
        }
        # --- internet ---------------------------------------------------
        {
          assertion = has "internet" internetAccept;
          message = "internet must allow egress from the guest subnet";
        }
        {
          # Functional egress, not just a verdict.
          assertion = lib.hasInfix "AGENT_MICROVM_NAT -s ${subnet} ! -d ${subnet} -j MASQUERADE" (
            fwOf "internet"
          );
          message = "internet must masquerade the guest subnet (functional egress, not just a firewall verdict)";
        }
        {
          assertion = lib.hasInfix "AGENT_MICROVM_NAT" (
            (variant "internet").config.networking.firewall.extraStopCommands
          );
          message = "the NAT chain must be torn down symmetrically";
        }
        {
          # Explicit DNS policy: allow the configured resolver, drop the rest.
          assertion =
            has "internet" "-d ${gateway} -p udp --dport 53 -j ACCEPT"
            && has "internet" "AGENT_MICROVM_FORWARD -s ${subnet} -p udp --dport 53 -j DROP";
          message = "internet must allow ONLY the configured resolvers on port 53 and drop other DNS";
        }
        {
          assertion = (guestOf "internet").networking.nameservers == [ gateway ];
          message = "internet guest must be configured with exactly the allowed resolvers";
        }
        {
          assertion = has "internet" "--log-prefix \"agent-microvm-drop: \"";
          message = "internet must log dropped guest packets (rate-limited audit trail)";
        }
        {
          # Even with full egress, the host LAN stays unreachable.
          assertion = has "internet" "AGENT_MICROVM_FORWARD -s ${subnet} -d 192.168.0.0/16 -j DROP";
          message = "internet must still block private ranges";
        }
      ]
    );

  # ---------------------------------------------------------------------- #
  # (j) HOST IDENTITY / AUTHENTICATED CONTROL CHANNEL (ticket 3 B):         #
  #     prove that (1) every slot gets its OWN deterministic ed25519 host   #
  #     key from a read-only per-slot share, (2) the guest does NOT         #
  #     generate throwaway keys, (3) the host provisioning unit exists, and #
  #     (4) the launcher verifies guests STRICTLY against the generated     #
  #     known_hosts file — with no `StrictHostKeyChecking=no` /             #
  #     `UserKnownHostsFile=/dev/null` left anywhere in the script.         #
  # ---------------------------------------------------------------------- #
  microvm-host-identity =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      guestSsh = guest0Cfg.services.openssh;
      # Per-slot key dirs must be pairwise distinct (no shared private key).
      slotKeyDirs = map (slot: hostKeys.slotDir slot.name) enabledSlots;
      evalMarker = mkEvalCheck "microvm-host-identity-eval" [
        {
          assertion = guestSsh.generateHostKeys == false;
          message = "guest must NOT generate its own throwaway host keys (generateHostKeys must be false)";
        }
        {
          assertion = map (k: k.path) guestSsh.hostKeys == [ hostKeys.guestKeyPath ];
          message = "guest sshd must use exactly the provisioned key ${hostKeys.guestKeyPath}, got ${
            toString (map (k: k.path) guestSsh.hostKeys)
          }";
        }
        {
          assertion = builtins.all (k: k.type == "ed25519") guestSsh.hostKeys;
          message = "guest host key must be ed25519";
        }
        {
          assertion = enabledCfg.systemd.services ? agent-microvm-hostkeys;
          message = "host must provision per-slot host keys via agent-microvm-hostkeys.service";
        }
        {
          assertion = lib.length (lib.unique slotKeyDirs) == lib.length slotKeyDirs;
          message = "each slot must have its OWN host-key directory (no shared private key): ${toString slotKeyDirs}";
        }
        {
          # The known_hosts file must live under the runtime root, not in the
          # world-writable /tmp or inside a guest-writable path.
          assertion = lib.hasPrefix "${microvmOpts.runtimeRoot}/" hostKeys.knownHosts;
          message = "known_hosts must live under the runtime root, got ${hostKeys.knownHosts}";
        }
      ];
    in
    pkgs.runCommand "microvm-host-identity"
      {
        inherit evalMarker;
        launcherBin = "${hostLauncher}/bin/agent-microvm";
        knownHosts = hostKeys.knownHosts;
      }
      ''
        grep -q "StrictHostKeyChecking=yes" "$launcherBin" \
          || { echo "launcher does not use StrictHostKeyChecking=yes" >&2; exit 1; }
        grep -q "UserKnownHostsFile=\"\$KNOWN_HOSTS\"" "$launcherBin" \
          || { echo "launcher does not pin UserKnownHostsFile to the generated known_hosts" >&2; exit 1; }
        grep -q "KNOWN_HOSTS=$knownHosts" "$launcherBin" \
          || { echo "launcher does not reference $knownHosts" >&2; exit 1; }
        # Fail if ANY unauthenticated ssh invocation survives.
        if grep -n "StrictHostKeyChecking=no" "$launcherBin" | grep -v "^[0-9]*:#"; then
          echo "launcher still contains StrictHostKeyChecking=no" >&2; exit 1
        fi
        if grep -n "UserKnownHostsFile=/dev/null" "$launcherBin" | grep -v "^[0-9]*:#"; then
          echo "launcher still contains UserKnownHostsFile=/dev/null" >&2; exit 1
        fi
        {
          echo "microvm-host-identity: authenticated control channel"
          echo "  launcher    : $launcherBin"
          echo "  known_hosts : $knownHosts"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (i) AGENT EXECUTABLES (ticket 2): actually BUILD every registry agent   #
  #     package and prove it ships the declared `executable`. This is the   #
  #     `command -v <agent>` acceptance criterion turned into a build       #
  #     check: a registry entry whose package/executable pair is wrong (a   #
  #     renamed CLI, a wrong package attr) fails HERE instead of inside a   #
  #     booted guest. The same packages are part of the guest closure the   #
  #     host toplevel already needs, so this adds no new heavy build.       #
  # ---------------------------------------------------------------------- #
  microvm-agent-executables =
    let
      registryAgents = lib.attrValues agentRegistry.agents;
    in
    pkgs.runCommand "microvm-agent-executables"
      {
        # "<name> <store-path> <executable>" per agent.
        agentTriples = lib.concatMapStringsSep "\n" (
          a: "${a.name} ${a.package} ${a.executable}"
        ) registryAgents;
      }
      ''
        printf '%s\n' "$agentTriples" | while read -r name path exe; do
          [ -n "$name" ] || continue
          test -x "$path/bin/$exe" \
            || { echo "agent '$name': $path/bin/$exe is missing or not executable" >&2; exit 1; }
          echo "agent $name -> $path/bin/$exe"
        done > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (d) GUEST EVALUATES: prove the agent-0 guest closure resolves to a      #
  #     realisable derivation. Plan §38 ("guest config builds").            #
  #                                                                         #
  #     HONESTY NOTE: the name is `microvm-guest-evaluates` (not `-builds`) #
  #     on purpose. To keep `nix flake check` affordable this is an         #
  #     EVAL-DEPTH check — it forces the guest's `system.build.toplevel`    #
  #     and the Cloud Hypervisor `declaredRunner` down to their .drvPath    #
  #     (which fully type-checks the guest module graph and catches any     #
  #     guest eval error) and records those paths in the marker, but does   #
  #     NOT realise the (very large) guest closure in CI. To ACTUALLY build #
  #     the guest closure, run:                                             #
  #       nix build .#nixosConfigurations.test-f13.config.microvm.vms.\     #
  #         agent-0.config.config.microvm.declaredRunner                    #
  # ---------------------------------------------------------------------- #
  microvm-guest-evaluates =
    let
      toplevelDrv = guest0Cfg.system.build.toplevel.drvPath;
      runnerDrv = guest0Cfg.microvm.declaredRunner.drvPath;
    in
    pkgs.runCommand "microvm-guest-evaluates"
      {
        # Referencing the .drvPath strings forces the guest module graph to
        # evaluate; a broken guest fails HERE, at eval, before any build.
        inherit toplevelDrv runnerDrv;
      }
      ''
        {
          echo "microvm-guest-evaluates: EVAL-DEPTH check (see comment in tests/microvm.nix)"
          echo "reference guest toplevel drv: $toplevelDrv"
          echo "reference guest CH runner drv: $runnerDrv"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (e) LAUNCHER SHELLCHECK: build the host `agent-microvm` launcher, the   #
  #     guest `agent-run` entry point, and the workmux per-agent launchers. #
  #     Every one of these is a `pkgs.writeShellApplication`, which runs    #
  #     `shellcheck` in its checkPhase at BUILD time. Therefore a check      #
  #     that merely forces these derivations to build IS the shellcheck     #
  #     gate (plan §38: "shell scripts pass shellcheck"). This is a real    #
  #     build (not eval-only): shellcheck actually executes.                #
  # ---------------------------------------------------------------------- #
  microvm-launcher-shellcheck =
    let
      hostLauncher = findPkg enabledCfg.environment.systemPackages "agent-microvm";
      guestAgentRun = findPkg guest0Cfg.environment.systemPackages "agent-run";
      # The per-slot SSH host-key provisioner (hostkeys.nix) is also a
      # writeShellApplication; pull it in via the unit's ExecStart so its
      # shellcheck gate runs too.
      hostKeyProvisioner = toString enabledCfg.systemd.services.agent-microvm-hostkeys.serviceConfig.ExecStart;
      # The batch trust split (job.nix): the trusted controller, the untrusted
      # worker, the guest-side permission assertions and the HOST-side result
      # verifier are all writeShellApplications, so building them runs their
      # shellcheck gate.
      guestJobController = findPkg guest0Cfg.environment.systemPackages "agent-job-controller";
      guestJobWorker = findPkg guest0Cfg.environment.systemPackages "agent-job-worker";
      guestJobAssertPaths = findPkg guest0Cfg.environment.systemPackages "agent-job-assert-paths";
      hostResultVerifier = jobs.resultVerifier;
      # The workmux agent `command`s are `lib.getExe <launcher>` strings whose
      # string context references the launcher derivation, so building against
      # them pulls the writeShellApplication (and its shellcheck) in.
      workmuxAgents = enabledCfg.myconfig.ai.workmux.agents;
      workmuxLauncherCmds = map (a: workmuxAgents.${a.workmuxName}.command) (
        lib.attrValues agentRegistry.agents
      );
    in
    pkgs.runCommand "microvm-launcher-shellcheck"
      {
        # Build dependencies: forcing these to build runs their shellcheck.
        launchers = [
          hostLauncher
          guestAgentRun
          guestJobController
          guestJobWorker
          guestJobAssertPaths
          hostResultVerifier
        ];
        # Pull in the workmux launcher drvs via their exe-path string context.
        workmuxCmds = workmuxLauncherCmds;
        inherit hostKeyProvisioner;
      }
      ''
        {
          echo "microvm-launcher-shellcheck: the following writeShellApplication"
          echo "derivations built successfully, so their shellcheck gate passed:"
          echo "  host launcher : ${hostLauncher}"
          echo "  guest agent-run: ${guestAgentRun}"
          echo "  hostkey provisioner: $hostKeyProvisioner"
          echo "  guest job controller: ${guestJobController}"
          echo "  guest job worker: ${guestJobWorker}"
          echo "  guest path assertions: ${guestJobAssertPaths}"
          echo "  host result verifier: ${hostResultVerifier}"
          for c in $workmuxCmds; do echo "  workmux launcher: $c"; done
        } > "$out"
      '';
}
