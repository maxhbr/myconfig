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
#                                        config (pool bounds, class names,
#                                        enableSsh key, insecure-network
#                                        acknowledgement, missing litellm)
#   microvm-eval-guest-shape  phases 1+8 — the guest is the lightweight shape:
#                                        pinned EROFS store, bash login shell,
#                                        the documented minimal toolset
#   microvm-eval-enabled-agents phase 2 — the agent SELECTION is applied once
#   microvm-config-seed       phase 3 — launch-time, allowlisted config staging
#   microvm-session-tree      phase 4 — ONE writable + ONE read-only share, the
#                                        layout table and its trust policy
#   microvm-capabilities      phase 5 — interactive and batch are independently
#                                        selectable: the default keeps both, and
#                                        each narrowing REMOVES the other half's
#                                        units/paths/packages/subcommands (the
#                                        narrowed guest closures are BUILT)
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
  # The EFFECTIVE resource-class table of the reference host (ticket 5 A).
  resourceClasses = self.nixosConfigurations.test-f13._module.args.agentResourceClasses;
  # The reference slot every guest-level check inspects: the first slot of the
  # first class, taken from the generated pool rather than hardcoded.
  refSlot = lib.head enabledSlots;
  gateway = microvmOpts.gatewayAddress; # 192.168.83.1
  port = toString microvmOpts.litellmPort; # 4000

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
        message = "pool=${toString n}: IPv4 addresses not unique (${toString ips})";
      }
      {
        assertion = lib.length (lib.unique macs) == n;
        message = "pool=${toString n}: MAC addresses not unique (${toString macs})";
      }
      {
        assertion = ipsWellFormed;
        message = "pool=${toString n}: an IPv4 address is malformed (${toString ips})";
      }
      {
        assertion = macsWellFormed;
        message = "pool=${toString n}: a MAC address is malformed (${toString macs})";
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
        message = "pool=${toString n}: slot .index values not contiguous 0..${toString (n - 1)}";
      }
      {
        # ticket 3 B: every concurrently runnable slot needs a UNIQUE VSOCK
        # control-channel identity ...
        assertion = lib.length (lib.unique cids) == n;
        message = "pool=${toString n}: VSOCK CIDs not unique (${toString cids})";
      }
      {
        # ... that avoids the reserved CIDs 0 (hypervisor), 1 (loopback),
        # 2 (host) and VMADDR_CID_ANY (0xffffffff).
        assertion = builtins.all (c: c > 2 && c < 4294967295) cids;
        message = "pool=${toString n}: VSOCK CIDs must avoid reserved values (${toString cids})";
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
  # (e.g. a class `count = 0` violates the `positive integer` type, so forcing the
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
  # The POSITIVE counterpart: `mods` evaluates AND no failed assertion mentions
  # `needle`. Used where the point of a phase is that a guard must STOP firing
  # for a legitimate configuration (phase 5: the batch-capable-agent assertion
  # on a host that does not select the `batch` capability).
  # It asserts NO failed assertion at all, not merely the absence of `needle`:
  # the point of such a call site is that the whole configuration is legitimate,
  # so an UNRELATED guard firing on it (a future assertion that does not cope with
  # a narrowed capability set, say) must fail the check instead of being masked by
  # a needle that happens not to match.
  acceptsWithout =
    mods: needle:
    let
      r = builtins.tryEval (failedAssertions mods);
    in
    r.success && r.value == [ ] && !(builtins.any (m: lib.hasInfix needle m) r.value);
  # The unmodified host must have NO failed assertions — positive control so
  # the rejectsWith checks below can't pass vacuously.
  baselineClean = failedAssertions [ ] == [ ];

  # --- a single-slot VARIANT host ----------------------------------------
  # Some checks need an option value the reference host defines itself (a
  # definition can only be OUTRANKED by `extendModules`, never removed) — in
  # particular a narrower `enabledAgents` and a guest without the SSH control
  # channel. test-workstation has the feature disabled and defines nothing about
  # it, so enabling it there yields a host whose only non-default microvm
  # options are the ones the check sets. The pool is forced to ONE small slot so
  # these variants stay cheap to evaluate.
  variantHostWith =
    mods:
    self.nixosConfigurations.test-workstation.extendModules {
      modules = [
        {
          myconfig.ai.microvm = {
            enable = true;
            # Keeps the variant key-free; the checks that need sshd use the
            # reference host, which has the control channel.
            enableSsh = false;
            resourceClasses = lib.mkForce {
              lite = {
                count = 1;
                vcpu = 2;
                memoryMiB = 4096;
              };
            };
          };
          # The secure `proxy-only` default requires the host LiteLLM backend.
          services.litellm.enable = true;
        }
      ]
      ++ mods;
    };
  # The FILTERED registry instance of a variant host (`nixosSystem` — and hence
  # `extendModules` — exposes module args on the top-level attrset).
  variantRegistryOf = mods: (variantHostWith mods)._module.args.agentRegistry;
  variantSlot = lib.head (
    slotLib.mkSlots {
      lite = {
        count = 1;
        vcpu = 2;
        memoryMiB = 4096;
      };
    }
  );
  # THE reference variant of the SELECTION checks: one agent only (the plan's
  # reference agent), which is what a host that wants the smallest guest closure
  # would configure.
  codexHost = variantHostWith [ { myconfig.ai.microvm.enabledAgents = [ "codex" ]; } ];

  # --- lightweight plan phase 5: the NARROWED capability variants ----------
  # ONE agent keeps these cheap to BUILD (the capability check builds their
  # closures, not only their drvPaths), and `codex` is stdin-driven, so the
  # batch variant also exercises ../modules/myconfig.ai/myconfig.ai.microvm/
  # job.nix's `promptUnusedSuppression` — the exact shape whose SC2034 broke the
  # lite guest BUILD in phase 4 while every eval-depth check stayed green.
  capabilityHostWith =
    caps: extra:
    variantHostWith [
      {
        myconfig.ai.microvm = {
          capabilities = caps;
          enabledAgents = [ "codex" ];
        }
        // extra;
      }
    ];
  # Interactive-only, WITH the SSH control channel (`variantHostWith` turns it
  # off, so this outranks it and supplies the required public key): the point of
  # the variant is that sshd and the host-key tree are PRESENT while the batch
  # half is gone.
  interactiveOnlyHost = capabilityHostWith [ "interactive" ] {
    enableSsh = lib.mkForce true;
    sshPublicKeyFile = ../hosts/host.f13/dedicated-agent-vm-key.pub;
  };
  # Batch-only: `enableSsh` is already false on the variant host, which is what
  # the enableSsh/capability reconciliation REQUIRES (see default.nix).
  batchOnlyHost = capabilityHostWith [ "batch" ] { };
  # Batch + VSOCK (lightweight plan phase 6): a batch-only host that ALSO
  # selects `vsock`, so it has no TCP sshd but DOES have the VSOCK
  # `sshd-vsock@` control channel. Needs the dedicated public key (the VSOCK
  # sshd authorises the same key the interactive one does). `enableSsh` stays
  # false (from `variantHostWith`), which is exactly the batch+vsock shape.
  batchVsockHost = capabilityHostWith [ "batch" "vsock" ] {
    sshPublicKeyFile = ../hosts/host.f13/dedicated-agent-vm-key.pub;
  };

  # --- lightweight plan phase 6 (the LITERAL objective): the VSOCK MODEL ----
  # TRANSPORT variants. `vsock` + the closed `proxy-only` profile REPLACES the
  # guest network: no TAP, no bridge, no static IP, no guest networkd, no host
  # firewall chain. `batchVsockHost` above is already exactly that shape (its
  # profile is the module default `proxy-only`), so these add the two shapes it
  # does not cover:
  #   * an INTERACTIVE guest without a network interface — the plan's own
  #     "definition of done" shape (one command, one slot, no ordinary network
  #     interface), whose TCP sshd is unreachable and therefore masked;
  #   * a TWO-SLOT pool, which is what proves the host runs ONE forwarder
  #     listener per VM rather than a shared one.
  interactiveVsockHost = capabilityHostWith [ "interactive" "batch" "vsock" ] {
    enableSsh = lib.mkForce true;
    sshPublicKeyFile = ../hosts/host.f13/dedicated-agent-vm-key.pub;
  };
  # `mkOverride 40` outranks `variantHostWith`'s own `mkForce` (50) — two
  # `mkForce`s would conflict.
  twoSlotVsockClasses = {
    lite = {
      count = 2;
      vcpu = 2;
      memoryMiB = 4096;
    };
  };
  twoSlotVsockHost = capabilityHostWith [ "batch" "vsock" ] {
    sshPublicKeyFile = ../hosts/host.f13/dedicated-agent-vm-key.pub;
    resourceClasses = lib.mkOverride 40 twoSlotVsockClasses;
  };
  twoSlotVsockSlots = slotLib.mkSlots twoSlotVsockClasses;

  capGuestOf = h: h.config.microvm.vms.${variantSlot.name}.config.config;
  capSessionOf = h: h._module.args.agentSession;
  pkgNamesOf = ps: map (p: p.pname or p.name or "") ps;
  capGuestPkgNames = h: pkgNamesOf (capGuestOf h).environment.systemPackages;
  # Every ExecStart of a HOST unit, flattened — used to prove that no unit of a
  # batch-only host still generates SSH host keys / known_hosts.
  hostExecStartsOf =
    h:
    lib.concatMap (
      s:
      let
        e = s.serviceConfig.ExecStart or "";
      in
      if builtins.isList e then map toString e else [ (toString e) ]
    ) (lib.attrValues h.config.systemd.services);

  # The module's own path/layout definitions of the REFERENCE host, so no check
  # carries a second copy of them.
  session = self.nixosConfigurations.test-f13._module.args.agentSession;
  seed = self.nixosConfigurations.test-f13._module.args.agentConfigSeed;
  launcherPkg = findPkg enabledCfg.environment.systemPackages "agent-microvm";
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
  # (b) ENABLED (test-f13) — exactly the pool's slots, bridge-only proxy    #
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
  # (r) GUEST SHAPE (lightweight plan phases 1 + 8): the module has exactly  #
  #     ONE shape, and it is the lightweight one — a pinned, optimized EROFS #
  #     guest store, a plain bash login shell and the documented minimal CLI #
  #     toolset with NixOS' `defaultPackages` convenience set dropped. Every  #
  #     assertion is against the REFERENCE host, i.e. against what f13        #
  #     actually deploys.                                                    #
  # ---------------------------------------------------------------------- #
  microvm-eval-guest-shape =
    let
      guestPkgPaths = map (p: p.outPath) guest0Cfg.environment.systemPackages;
    in
    mkEvalCheck "microvm-eval-guest-shape" [
      {
        assertion = baselineClean;
        message = "positive control: the reference host must evaluate cleanly, got ${
          toString (failedAssertions [ ])
        }";
      }
      {
        # Pinned rather than inherited: a microvm.nix release cannot silently
        # give the guest a bigger/slower store image.
        assertion = guest0Cfg.microvm.storeDiskType == "erofs";
        message = "the guest storeDiskType must be pinned to erofs, got '${toString guest0Cfg.microvm.storeDiskType}'";
      }
      {
        assertion = guest0Cfg.microvm.optimize.enable == true;
        message = "the guest must pin microvm.optimize.enable = true";
      }
      # --- lightweight plan phase 8: minimized guest closure --------------
      {
        # A plain bash login shell: no fish closure, no `programs.fish`
        # machinery, and nothing in the guest configures fish any more.
        assertion =
          guest0Cfg.users.users.agent.shell.outPath == pkgs.bashInteractive.outPath
          && !guest0Cfg.programs.fish.enable
          && !(builtins.elem pkgs.fish.outPath guestPkgPaths);
        message = "the guest must use a plain bash login shell and contain no fish";
      }
      {
        # Every tool `guestCommonPackages` documents a consumer for is present.
        assertion = builtins.all (p: builtins.elem p.outPath guestPkgPaths) (
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
        );
        message = "the guest is missing a package from the documented guestCommonPackages set";
      }
      {
        # ... and nothing else this module used to add.
        # NOTE: NixOS' own `requiredPackages` (coreutils-full, curl, openssh,
        # which, ...) is load-bearing for a bootable system and deliberately
        # NOT asserted absent — only the module's discretionary additions are.
        assertion = builtins.all (p: !(builtins.elem p.outPath guestPkgPaths)) (
          with pkgs;
          [
            fd
            file
            gnumake
            tree
            unzip
          ]
        );
        message = "the guest still carries a discretionary package with no documented in-guest consumer";
      }
      {
        # NixOS' `environment.defaultPackages` (perl, rsync, strace) has no
        # in-guest consumer in a single-purpose sandbox image.
        assertion = guest0Cfg.environment.defaultPackages == [ ];
        message = "the guest must drop NixOS' defaultPackages convenience set";
      }
      {
        # Sizing is the HOST's decision: an explicit `resourceClasses` table is
        # what the pool is built from (the reference host defines two classes).
        assertion =
          lib.sort (a: b: a < b) (builtins.attrNames enabledCfg.microvm.vms)
          == lib.sort (a: b: a < b) (map (sl: sl.name) enabledSlots)
          && lib.length (lib.attrNames resourceClasses) == 2;
        message = "the reference host's VMs must be exactly the pool of its own two resource classes, got ${toString (builtins.attrNames enabledCfg.microvm.vms)}";
      }
    ];

  # ---------------------------------------------------------------------- #
  # (s) SELECTED AGENTS (lightweight plan phase 2): the registry selection is  #
  #     applied ONCE, in agents.nix, so a deselected agent is ABSENT from the  #
  #     guest closure rather than merely hidden — while the module-wide        #
  #     DEFAULT (no `enabledAgents` at all) still keeps EVERY declared agent,  #
  #     so no host silently loses a workmux pane or a `submit --agent <name>`. #
  # ---------------------------------------------------------------------- #
  microvm-eval-enabled-agents =
    let
      codexReg = codexHost._module.args.agentRegistry;
      codexGuest = (codexHost.config.microvm.vms.${variantSlot.name}).config.config;
      codexGuestPaths = map (p: p.outPath) codexGuest.environment.systemPackages;
      # The packages of the agents the codex-only host does NOT select, taken
      # from the reference host's registry rather than a second list.
      deselected = lib.filter (a: !(builtins.elem a.name codexReg.names)) (
        lib.attrValues agentRegistry.agents
      );
      codexWorkmux = builtins.filter (lib.hasPrefix "microvm-") (
        builtins.attrNames codexHost.config.myconfig.ai.workmux.agents
      );
    in
    mkEvalCheck "microvm-eval-enabled-agents" (
      [
        {
          # The module-wide default: a host that says NOTHING keeps every
          # declared agent.
          assertion = (variantRegistryOf [ ]).names == agentRegistry.declaredNames;
          message = "the module default must keep every declared agent, got ${
            toString (variantRegistryOf [ ]).names
          }";
        }
        {
          # The reference host states its selection EXPLICITLY (its workmux
          # panes and `submit --agent` tokens depend on it) and currently keeps
          # all of them.
          assertion =
            microvmOpts.enabledAgents != null
            && lib.sort (a: b: a < b) microvmOpts.enabledAgents == agentRegistry.declaredNames
            && agentRegistry.names == agentRegistry.declaredNames;
          message = "the reference host must select its agents explicitly (got ${toString microvmOpts.enabledAgents} of ${toString agentRegistry.declaredNames})";
        }
        {
          assertion = codexReg.names == [ "codex" ];
          message = "an explicit `enabledAgents = [ \"codex\" ]` must select codex only, got ${toString codexReg.names}";
        }
        {
          # ... while still DECLARING all of them (the selection filters, it
          # does not delete registry entries).
          assertion = codexReg.declaredNames == agentRegistry.declaredNames;
          message = "the selection must not change the set of DECLARED agents";
        }
        {
          assertion = codexReg.unknownEnabled == [ ];
          message = "the codex-only selection must be valid";
        }
        {
          # Every DERIVED list follows the selection — not just the packages.
          assertion = codexWorkmux == [ "microvm-codex" ];
          message = "workmux registrations must follow the selection, got ${toString codexWorkmux}";
        }
        {
          assertion = codexReg.batchNames == [ "codex" ] && codexReg.namesAlternation == "codex";
          message = "the generated batch/help fragments must follow the selection";
        }
        {
          assertion = deselected != [ ];
          message = "positive control: the codex-only selection must actually exclude some declared agent";
        }
        {
          # The SELECTED agent is in the guest closure.
          assertion = builtins.elem (lib.head (lib.attrValues codexReg.agents)).package.outPath codexGuestPaths;
          message = "the selected agent's package is missing from the guest closure";
        }
        {
          # Unknown token: a typo must fail at EVAL, naming the valid tokens.
          assertion =
            let
              msgs = map (a: a.message) (
                builtins.filter (a: !a.assertion)
                  (variantHostWith [
                    {
                      myconfig.ai.microvm.enabledAgents = [
                        "codex"
                        "nope"
                      ];
                    }
                  ]).config.assertions
              );
            in
            builtins.any (m: lib.hasInfix "unknown agent(s) nope" m) msgs;
          message = "an unknown enabledAgents entry must be rejected at eval";
        }
        {
          # An empty selection would build a guest that can run nothing.
          assertion =
            let
              msgs = map (a: a.message) (
                builtins.filter (a: !a.assertion)
                  (variantHostWith [ { myconfig.ai.microvm.enabledAgents = [ ]; } ]).config.assertions
              );
            in
            builtins.any (m: lib.hasInfix "selects no agent" m) msgs;
          message = "an empty enabledAgents selection must be rejected at eval";
        }
      ]
      # ... and the DESELECTED agents' runtimes are ABSENT from the guest
      # closure (the point of the whole phase).
      ++ map (a: {
        assertion = !(builtins.elem a.package.outPath codexGuestPaths);
        message = "deselected agent '${a.name}' is still in the codex-only guest closure (${a.package.outPath})";
      }) deselected
    );

  # ---------------------------------------------------------------------- #
  # (t) RUNTIME CONFIG STAGING (lightweight plan phase 3): the guest home is #
  #     provisioned at LAUNCH time from an ALLOWLISTED, root-owned staged     #
  #     copy of the host configuration, and NO guest home-manager activation  #
  #     exists at all. Locks down: the mechanism is active + correctly         #
  #     ordered, the share is per-slot/read-only/root-owned, the manifest is   #
  #     outside every share, the allowlist follows `enabledAgents`, and        #
  #     escaping / credential-shaped entries are REJECTED at eval. The build   #
  #     part proves the generated stager really enforces the allowlist and     #
  #     cleans its destination.                                               #
  # ---------------------------------------------------------------------- #
  microvm-config-seed =
    let
      codexSeed = codexHost._module.args.agentConfigSeed;
      codexReg = codexHost._module.args.agentRegistry;

      # The share through which the guest sees the staged tree: the ONE
      # READ-ONLY share, whose source CONTAINS the staged payload. Found by
      # asking which share covers the guest mount point, so this check follows
      # the layout instead of pinning a path.
      seedShares = builtins.filter (
        s: s.mountPoint == seed.guestMountPoint || lib.hasPrefix "${s.mountPoint}/" seed.guestMountPoint
      ) guest0Cfg.microvm.shares;
      # True when `path` is inside (or is) the source of share `s`.
      sourceCovers = s: path: path == s.source || lib.hasPrefix "${s.source}/" path;
      # A tmpfiles `d` rule for `path` with `mode`, owned by root. tmpfiles
      # accepts the owner both by name and numerically; ./session.nix renders
      # the numeric form (it derives the ids from its layout table), the
      # host-only manifest directories render the name.
      hasRootDirRule =
        rules: path: mode:
        builtins.any (r: r == "d ${path} ${mode} root root - -" || r == "d ${path} ${mode} 0 0 - -") rules;
      seedUnit = guest0Cfg.systemd.services.agent-config-seed;
      # Every unit the seeding oneshot must precede, from the SAME sources the
      # module orders against.
      seedBeforeUnits = [
        "sshd.service"
        jobs.controllerUnit
        "agent-state-link.service"
        "agent-model-config.service"
      ];
      hmServices =
        cfg: builtins.filter (n: lib.hasPrefix "home-manager" n) (builtins.attrNames cfg.systemd.services);
      # `configPaths` of the agents a codex-only host does NOT select.
      deselectedPaths = lib.unique (
        lib.concatMap (a: a.configPaths) (
          lib.filter (a: !(builtins.elem a.name codexReg.names)) (lib.attrValues agentRegistry.agents)
        )
      );
      seedRejects =
        infix: mods:
        let
          msgs = map (a: a.message) (
            builtins.filter (a: !a.assertion) (variantHostWith mods).config.assertions
          );
        in
        builtins.any (m: lib.hasInfix infix m) msgs;

      evalMarker = mkEvalCheck "microvm-config-seed-eval" [
        # --- THE acceptance criterion of phase 3 --------------------------
        {
          # No guest home-manager activation exists any more — anywhere.
          assertion = hmServices guest0Cfg == [ ];
          message = "the guest must NOT run home-manager activation, got ${toString (hmServices guest0Cfg)}";
        }
        {
          # Positive control for the assertion above: the guest DOES have a
          # provisioning unit, it is just not home-manager's.
          assertion = guest0Cfg.systemd.services ? agent-config-seed;
          message = "positive control: the guest must carry the config-seed provisioning unit";
        }
        # --- the share: per-slot, READ-ONLY, root-owned source -------------
        {
          assertion = lib.length seedShares == 1;
          message = "the guest must reach the staged tree through exactly one share, got ${toString (lib.length seedShares)}";
        }
        {
          assertion =
            let
              s = lib.head seedShares;
            in
            sourceCovers s (seed.hostPayloadDir refSlot.name)
            && lib.hasPrefix s.mountPoint seed.guestMountPoint
            && s.proto == "virtiofs"
            && (s.readOnly or false);
          message = "the staged configuration must reach the guest through a per-slot READ-ONLY share covering ${seed.hostPayloadDir refSlot.name} at ${seed.guestMountPoint}";
        }
        {
          # The manifest names the host home and every SKIPPED,
          # credential-shaped host file name. It must therefore stay OUTSIDE
          # everything the untrusted guest can see: not the share source, and
          # not below it.
          assertion =
            let
              manifest = seed.hostManifest refSlot.name;
            in
            builtins.all (
              s: manifest != s.source && !(lib.hasPrefix "${s.source}/" manifest)
            ) guest0Cfg.microvm.shares;
          message = "the staging manifest ${seed.hostManifest refSlot.name} must not be inside any guest share";
        }
        {
          assertion = seed.manifestMode == "0400";
          message = "the staging manifest must be root-only (0400), got ${seed.manifestMode}";
        }
        {
          # No host home (or any other broad host directory) is ever mounted:
          # every share source stays under the module's own roots.
          assertion = builtins.all (
            s: lib.hasPrefix microvmOpts.runtimeRoot s.source || lib.hasPrefix microvmOpts.stateRoot s.source
          ) guest0Cfg.microvm.shares;
          message = "a guest share escapes the module's runtime/state roots: ${
            toString (map (s: s.source) guest0Cfg.microvm.shares)
          }";
        }
        {
          # The staging directory is root-owned and NOT writable by the guest
          # agent (virtiofsd passes ownership through unchanged).
          assertion =
            hasRootDirRule enabledCfg.systemd.tmpfiles.rules (seed.slotDir refSlot.name) seed.slotDirMode
            && hasRootDirRule enabledCfg.systemd.tmpfiles.rules (seed.hostPayloadDir refSlot.name) seed.dirMode;
          message = "the per-slot staging directories must be pre-created root-owned";
        }
        {
          # ... as are the host-only manifest directories, which no guest ever
          # sees.
          assertion =
            hasRootDirRule enabledCfg.systemd.tmpfiles.rules seed.manifestRoot seed.manifestRootMode
            &&
              hasRootDirRule enabledCfg.systemd.tmpfiles.rules (seed.manifestDir refSlot.name)
                seed.manifestDirMode;
          message = "the host-only manifest directories must be pre-created root-owned";
        }
        {
          # ... and root-ONLY: no group/other bit anywhere in the staged tree,
          # so neither the guest agent nor another unprivileged HOST user can
          # read the operator's staged configuration.
          assertion = builtins.all (m: lib.hasSuffix "00" m) [
            seed.rootMode
            seed.slotDirMode
            seed.dirMode
            seed.fileMode
            seed.manifestMode
            seed.manifestRootMode
            seed.manifestDirMode
          ];
          message = "the staged tree must be root-only, got ${
            toString [
              seed.rootMode
              seed.slotDirMode
              seed.dirMode
              seed.fileMode
              seed.manifestMode
            ]
          }";
        }
        # --- the guest oneshot: root, before every agent entry point -------
        {
          assertion =
            seedUnit.serviceConfig.Type == "oneshot" && seedUnit.wantedBy == [ "multi-user.target" ];
          message = "the guest seeding unit must be a oneshot wanted by multi-user.target";
        }
        {
          # Ordered before EVERY way an agent process can come into existence:
          # the interactive control channel, the trusted batch controller (which
          # starts the untrusted worker), the agent-state linker (whose symlinks
          # the copy must not write through) and the boot-time model discovery
          # (which writes into the same home).
          assertion = builtins.all (u: builtins.elem u seedUnit.before) seedBeforeUnits;
          message = "the guest seeding unit must be ordered before ${toString seedBeforeUnits}, got ${toString seedUnit.before}";
        }
        {
          # ... and those really are UNITS of the guest, not strings nothing
          # resolves. The reference host HAS the SSH control channel, so this is
          # an ordering against a real sshd.
          assertion =
            microvmOpts.enableSsh
            && (guest0Cfg.systemd.services ? sshd || guest0Cfg.services.openssh.enable)
            && guest0Cfg.systemd.services ? agent-state-link
            && guest0Cfg.systemd.services ? agent-model-config
            && guest0Cfg.systemd.services ? "${lib.removeSuffix ".service" jobs.controllerUnit}";
          message = "the units the seeding oneshot is ordered before must exist in the guest";
        }
        {
          # The model-config unit writes INTO the same home the seeder populates
          # with `cp -R`/`chown -R`/`chmod -R`, so it must be ordered after the
          # SEEDER (there is no home-manager unit to order against any more).
          assertion =
            let
              a = guest0Cfg.systemd.services.agent-model-config.after;
            in
            builtins.elem seed.guestUnit a && !(builtins.elem "home-manager-agent.service" a);
          message = "the guest's agent-model-config must be ordered after ${seed.guestUnit}, got ${toString guest0Cfg.systemd.services.agent-model-config.after}";
        }
        {
          assertion = seedUnit.unitConfig.RequiresMountsFor == seed.guestMountPoint;
          message = "the guest seeding unit must require the config-seed mount";
        }
        {
          # It may only ever READ the staged tree.
          assertion = seedUnit.serviceConfig.ReadOnlyPaths == [ "-${seed.guestMountPoint}" ];
          message = "the guest seeding unit must treat the staged tree as read-only";
        }
        # --- the allowlist follows the agent selection ---------------------
        {
          assertion =
            seed.allowedPaths == lib.unique (
              lib.sort (a: b: a < b) (
                lib.concatMap (a: a.configPaths) (lib.attrValues agentRegistry.agents)
                ++ microvmOpts.configSeed.extraPaths
              )
            );
          message = "the staging allowlist must be the SELECTED agents' configPaths plus extraPaths, got ${toString seed.allowedPaths}";
        }
        {
          assertion =
            codexSeed.allowedPaths == lib.unique (
              lib.sort (a: b: a < b) (
                (lib.head (lib.attrValues codexReg.agents)).configPaths
                ++ codexHost.config.myconfig.ai.microvm.configSeed.extraPaths
              )
            );
          message = "a codex-only host must stage only codex's configPaths plus extraPaths, got ${toString codexSeed.allowedPaths}";
        }
        {
          assertion =
            deselectedPaths != [ ]
            && builtins.any (p: !(builtins.elem p codexSeed.allowedPaths)) deselectedPaths;
          message = "positive control: a deselected agent must contribute paths the codex-only allowlist does not carry";
        }
        {
          assertion =
            builtins.elem ".pi/agent/prompts" seed.allowedPaths
            && !(builtins.elem ".pi/agent/prompts" codexSeed.allowedPaths);
          message = "the staged allowlist must follow enabledAgents (pi's paths must appear only when pi is selected)";
        }
        {
          # Registry hygiene: no declared agent may allowlist a credential-
          # shaped or escaping path (the module's own assertions would fail the
          # build, but that would only be noticed once that agent is selected).
          assertion = builtins.all (
            p:
            !(lib.hasPrefix "/" p)
            && !(lib.hasInfix ".." p)
            && !(lib.hasInfix "auth" p)
            && !(lib.hasInfix "credential" p)
            && !(lib.hasInfix "token" p)
            && !(lib.hasSuffix ".pem" p)
            && !(lib.hasSuffix ".key" p)
          ) (lib.concatMap (a: a.configPaths) (lib.attrValues agentRegistry.agents));
          message = "an agent in the registry allowlists an escaping or credential-shaped configuration path";
        }
        # --- NEGATIVE: invalid allowlist entries are rejected at EVAL -------
        {
          assertion = seedRejects "not plain, relative" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ "../../etc/shadow" ]; }
          ];
          message = "a `..` escape in the staging allowlist must be rejected at eval";
        }
        {
          assertion = seedRejects "not plain, relative" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ "/etc/passwd" ]; }
          ];
          message = "an absolute path in the staging allowlist must be rejected at eval";
        }
        {
          assertion = seedRejects "not plain, relative" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ ".config/../../root" ]; }
          ];
          message = "a nested `..` escape in the staging allowlist must be rejected at eval";
        }
        {
          assertion = seedRejects "CREDENTIAL material" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ ".codex/auth.json" ]; }
          ];
          message = "a credential-shaped allowlist entry must be rejected at eval";
        }
        {
          assertion = seedRejects "CREDENTIAL material" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ ".ssh/config" ]; }
          ];
          message = "an allowlist entry under ~/.ssh must be rejected at eval";
        }
        {
          assertion = seedRejects "CREDENTIAL material" [
            { myconfig.ai.microvm.configSeed.extraPaths = [ ".config/agent/api-token.txt" ]; }
          ];
          message = "an allowlist entry whose name contains a credential word must be rejected at eval";
        }
        {
          assertion = seedRejects "absolute path" [
            { myconfig.ai.microvm.configSeed.hostHome = "relative/home"; }
          ];
          message = "a relative configSeed.hostHome must be rejected at eval";
        }
        {
          # Staging a path that is also a PERSISTED agent-state directory would
          # leave the state linker refusing to replace a non-empty directory,
          # i.e. persistence silently off.
          assertion = seedRejects "overlaps the" [
            {
              myconfig.ai.microvm = {
                enabledAgents = [
                  "codex"
                  "hermes"
                ];
                configSeed.extraPaths = [ ".hermes/config.yaml" ];
              };
            }
          ];
          message = "an allowlist entry inside a persisted agent-state directory must be rejected at eval";
        }
        {
          # Positive control for the test above: `.hermes` really IS a declared
          # persistent-state directory of the selected registry there.
          assertion = builtins.elem ".hermes" (
            (variantHostWith [
              {
                myconfig.ai.microvm.enabledAgents = [
                  "codex"
                  "hermes"
                ];
              }
            ])._module.args.agentState.declaredDirs
          );
          message = "positive control: selecting hermes must declare the '.hermes' persistent-state directory";
        }
        {
          # ... and the reference host is free of such an overlap (the rejection
          # above cannot be masking a permanently failing host).
          assertion = seed.allowedPaths != [ ] && enabledCfg.assertions != [ ] && baselineClean;
          message = "positive control: the reference host must have a non-empty allowlist, real assertions and trip none of them";
        }
        {
          # Positive control: the unmodified VARIANT host trips no assertion
          # either, so the rejections above cannot pass vacuously.
          assertion = builtins.filter (a: !a.assertion) (variantHostWith [ ]).config.assertions == [ ];
          message = "positive control: a plain variant host must evaluate cleanly";
        }
      ];
    in
    pkgs.runCommand "microvm-config-seed"
      {
        inherit evalMarker;
        # Building these runs their writeShellApplication shellcheck gate; the
        # greps below then inspect the GENERATED code.
        stagerBin = "${seed.stager}/bin/agent-microvm-stage-config";
        seederBin = "${seed.seeder}/bin/agent-config-seed-apply";
        launcherBin = "${launcherPkg}/bin/agent-microvm";
        # Modes come from the module, so this check follows a policy change
        # instead of pinning yesterday's numbers.
        FILE_MODE = seed.fileMode;
        DIR_MODE = seed.dirMode;
        allowlist = lib.concatStringsSep "\n" seed.allowedPaths;
      }
      ''
        # --- the stager ENFORCES the allowlist ---------------------------
        # Exactly the evaluated allowlist is baked in — no other path can be
        # staged, and the list is not assembled at runtime.
        grep -q 'readonly ALLOWLIST=(' "$stagerBin" \
          || { echo "the stager has no baked allowlist" >&2; exit 1; }
        while IFS= read -r p; do
          [ -n "$p" ] || continue
          grep -qF -- "$p" "$stagerBin" \
            || { echo "allowlisted path '$p' is missing from the stager" >&2; exit 1; }
        done <<< "$allowlist"
        # ... and the credential DENYLIST is applied as defence in depth, to
        # every path component (not only to the allowlist entries).
        grep -q 'path_is_denied' "$stagerBin" \
          || { echo "the stager applies no credential denylist" >&2; exit 1; }
        for pat in auth.json credentials.json id_ed25519 .pem .netrc; do
          grep -qF -- "$pat" "$stagerBin" \
            || { echo "the credential denylist lost '$pat'" >&2; exit 1; }
        done
        # Escapes are refused: a path that resolves outside the configured host
        # home is never copied (only /nix/store dereferencing is allowed).
        grep -q 'resolves outside the host home' "$stagerBin" \
          || { echo "the stager does not reject paths escaping the host home" >&2; exit 1; }
        grep -q 'resolved_is_allowed' "$stagerBin" \
          || { echo "the stager has no resolved-path policy" >&2; exit 1; }
        # ... and the denylist is applied to the RESOLVED TARGET too, not only
        # to the name a path is reached under: otherwise ONE benignly named
        # symlink in the host home (`.codex/config.toml` -> `.codex/auth.json`,
        # `.agents/skills/x` -> `~/.ssh`) stages a credential.
        grep -q 'resolved_is_denied' "$stagerBin" \
          || { echo "the stager does not apply the denylist to resolved targets" >&2; exit 1; }
        test "$(grep -c 'resolved_is_denied "' "$stagerBin")" -ge 3 \
          || { echo "the resolved-target denylist must guard entries, files AND subdirectories" >&2; exit 1; }
        # Only regular files and directories, never setuid/setgid.
        grep -q 'setuid/setgid file' "$stagerBin" \
          || { echo "the stager does not skip setuid/setgid files" >&2; exit 1; }
        grep -q -- '-type d -o -type f' "$stagerBin" \
          || { echo "the stager does not restrict the walk to files/directories" >&2; exit 1; }
        # The destination is CLEANED before every launch (the payload tree AND
        # the previous manifest, whatever they are named).
        grep -qE 'rm -rf --.*\$PAYLOAD' "$stagerBin" \
          || { echo "the stager does not clean its destination before staging" >&2; exit 1; }
        grep -qE 'rm -rf --.*(\$PAYLOAD.*\$MANIFEST|\$MANIFEST)' "$stagerBin" \
          || { echo "the stager does not remove the previous manifest" >&2; exit 1; }
        # Everything it writes is root-owned and NOT readable by the guest agent
        # (or by any other unprivileged user on the host).
        grep -q -- '-o root -g root' "$stagerBin" \
          || { echo "the stager does not stage root-owned files" >&2; exit 1; }
        grep -qE "^readonly FILE_MODE=.?$FILE_MODE" "$stagerBin" \
          || { echo "staged files must be mode $FILE_MODE" >&2; exit 1; }
        grep -qE "^readonly DIR_MODE=.?$DIR_MODE" "$stagerBin" \
          || { echo "staged directories must be mode $DIR_MODE" >&2; exit 1; }
        for m in "$FILE_MODE" "$DIR_MODE"; do
          case "$m" in
            *00) ;;
            *) echo "the staged tree must not be group/other-accessible ($m)" >&2; exit 1 ;;
          esac
        done
        # A manifest records what was staged.
        grep -q 'manifest' "$stagerBin" \
          || { echo "the stager writes no manifest" >&2; exit 1; }
        # NEGATIVE: it must never copy a whole host directory wholesale — the
        # host home is only ever JOINED with an allowlisted relative path.
        if grep -nE '(cp|rsync|install|tar)[^#]*\$HOST_HOME"' "$stagerBin"; then
          echo "the stager must never copy the host home wholesale" >&2
          exit 1
        fi
        if grep -nE '\$HOST_HOME/\$\{?[a-z]' "$stagerBin" | grep -qv 'rel'; then
          echo "the host home may only be joined with an allowlisted relative path" >&2
          exit 1
        fi

        # --- the guest seeder --------------------------------------------
        # It refuses a staged tree that is not root-owned / is agent-writable,
        # and hands the COPY (never the original) to the agent.
        grep -q 'is not root-owned' "$seederBin" \
          || { echo "the guest seeder does not verify the staged tree is root-owned" >&2; exit 1; }
        grep -q 'group/other-writable' "$seederBin" \
          || { echo "the guest seeder does not verify the staged tree is not agent-writable" >&2; exit 1; }
        grep -q 'chown -R' "$seederBin" \
          || { echo "the guest seeder does not hand the copy to the agent" >&2; exit 1; }

        # --- the launcher stages once per launch, and cleans up ------------
        grep -q 'stage_config_seed' "$launcherBin" \
          || { echo "the launcher does not stage the host configuration" >&2; exit 1; }
        grep -q 'clear_config_seed' "$launcherBin" \
          || { echo "the launcher does not clear the staged configuration" >&2; exit 1; }
        grep -qF -- "$stagerBin" "$launcherBin" \
          || { echo "the launcher does not call the generated stager" >&2; exit 1; }
        # Both entry points stage BEFORE the VM is started.
        test "$(grep -c 'stage_config_seed "\$slot"' "$launcherBin")" -ge 2 \
          || { echo "both run and submit must stage the host configuration" >&2; exit 1; }

        {
          echo "microvm-config-seed:"
          echo "  stager        : $stagerBin"
          echo "  guest seeder  : $seederBin"
          echo "  launcher      : $launcherBin"
          echo "  allowlist     :"
          printf '    %s\n' $allowlist
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (u) THE SESSION TREE (lightweight plan phase 4): every guest gets ONE    #
  #     writable virtiofs share (the per-session tree) plus ONE read-only    #
  #     share (host identity + staged configuration). Locks down: the share  #
  #     set, the ownership/mode table as the ONE source of truth every       #
  #     consumer derives from, the guest mount/unit ordering, the layout     #
  #     POLICY (fed a deliberately broken table it must complain about the   #
  #     SPECIFIC rule) and the generated launcher code that prepares,        #
  #     verifies and REMOVES the tree. It also forces the guest closure to   #
  #     EVALUATE to a realisable derivation — a `.drvPath` string only,      #
  #     NOTHING is built here (see the file header and plan §38), so a guest #
  #     that evaluates but does not BUILD still passes CI.                   #
  # ---------------------------------------------------------------------- #
  microvm-session-tree =
    let
      # A guest WITHOUT the SSH control channel: the read-only share carries the
      # staged configuration too, so it must exist there as well (and the share
      # count must not change).
      noSshGuest = (variantHostWith [ ]).config.microvm.vms.${variantSlot.name}.config.config;

      shares = guest0Cfg.microvm.shares;
      writableShares = builtins.filter (s: !(s.readOnly or false)) shares;
      roShares = builtins.filter (s: s.readOnly or false) shares;
      sessionShare = if writableShares == [ ] then null else lib.head writableShares;
      roShare = if roShares == [ ] then null else lib.head roShares;

      # The expectations of EVERY session directory come from the module's own
      # table, so this check follows a layout change instead of pinning
      # yesterday's paths and numbers.
      pathOf = base: e: if e.rel == "" then base else "${base}/${e.rel}";
      expectedRules =
        map (
          e: "d ${pathOf (session.slotDir refSlot.name) e} ${e.mode} ${toString e.uid} ${toString e.gid} - -"
        ) session.layout
        ++ map (
          e:
          "d ${pathOf (session.roSlotDir refSlot.name) e} ${e.mode} ${toString e.uid} ${toString e.gid} - -"
        ) session.roLayout;
      # The TRUST-POLICY half of this check binds to the FULL tables
      # (`fullLayout`/`fullRoLayout`), never to the capability-FILTERED ones
      # (lightweight plan phase 5). The filtered tables of `test-f13` happen to be
      # the full ones only because the reference host selects both capabilities:
      # every rule below would silently shrink — and the `input/` / `controller/` /
      # `worker-logs/` negative cases would silently VANISH — the day that host
      # narrows. The filtered tables are used only where the point IS the
      # filtering (`expectedRules`, i.e. what the host actually creates, and the
      # verifier greps at the bottom).
      entry = rel: lib.findFirst (e: e.rel == rel) (throw "no layout entry '${rel}'") session.fullLayout;
      roEntry =
        rel:
        lib.findFirst (e: e.rel == rel) (throw "no read-only layout entry '${rel}'") session.fullRoLayout;

      # The REAL tables, as the module asserts them.
      realPolicyInput = {
        writableRoot = session.root;
        readOnlyRoot = session.roRoot;
        writable = session.fullLayout;
        readOnly = session.fullRoLayout;
        hostKeyDir = session.hostHostkeysDir refSlot.name;
        configSeedDir = session.hostConfigSeedDir refSlot.name;
      };
      # ... and DOCTORED variants: the policy is only worth anything if it
      # complains about a table that breaks the trust boundary.
      #
      # `doctored <expected message infix> <patch>` asserts that the SPECIFIC
      # rule fires, never merely that "some" violation was reported: a refactor
      # that drops rule (2) but keeps rule (1) would otherwise leave every
      # negative case below green. `violationsOf` returns the violations as
      # TEXT precisely so this is checkable.
      violationsWith = f: session.violationsOf (realPolicyInput // f realPolicyInput);
      doctored = want: f: lib.any (v: lib.hasInfix want v) (violationsWith f);
      # The path a violation message names for a writable-tree entry (the
      # policy is a pure function of the TABLE, so the paths carry no slot).
      wrel = rel: "${session.root}/${rel}";
      withRel = rel: patch: input: {
        writable = map (e: if e.rel == rel then e // patch else e) input.writable;
      };

      # Every unit that must be ordered against the mounts, and the mount each
      # of them needs.
      requiresMountsFor = unit: (unit.unitConfig or { }).RequiresMountsFor or null;

      evalMarker = mkEvalCheck "microvm-session-tree-eval" [
        {
          # THE acceptance criterion of phase 4.
          assertion = builtins.length writableShares == 1 && builtins.length roShares == 1;
          message = "the guest must declare ONE writable share plus ONE read-only share, got ${
            toString (map (s: "${s.tag}${lib.optionalString (s.readOnly or false) " (ro)"}") shares)
          }";
        }
        {
          assertion =
            sessionShare != null
            && sessionShare.proto == "virtiofs"
            && sessionShare.tag == session.guestTag
            && sessionShare.source == session.slotDir refSlot.name
            && sessionShare.mountPoint == session.guestMountPoint;
          message = "the writable share must be the per-slot session tree ${session.slotDir refSlot.name} mounted at ${session.guestMountPoint}, got ${toString sessionShare}";
        }
        {
          assertion =
            roShare != null
            && roShare.proto == "virtiofs"
            && roShare.tag == session.guestRoTag
            && roShare.source == session.roSlotDir refSlot.name
            && roShare.mountPoint == session.guestRoMountPoint
            && roShare.readOnly;
          message = "the read-only share must be the per-slot read-only tree ${session.roSlotDir refSlot.name} mounted READ-ONLY at ${session.guestRoMountPoint}";
        }
        {
          # Defence in depth: NOTHING else is shared — no /nix, no /home, no
          # host socket, no second writable tree.
          assertion =
            builtins.length shares == 2
            && builtins.all (s: s.proto == "virtiofs") shares
            &&
              lib.sort (a: b: a < b) (map (s: s.mountPoint) shares) == lib.sort (a: b: a < b) [
                session.guestMountPoint
                session.guestRoMountPoint
              ];
          message = "unexpected share(s): ${toString (map (s: "${s.tag}@${s.mountPoint}") shares)}";
        }
        {
          # A guest WITHOUT the SSH control channel still has exactly the same
          # two shares: the read-only one also carries the staged configuration.
          assertion =
            builtins.length noSshGuest.microvm.shares == 2
            && builtins.length (builtins.filter (s: s.readOnly or false) noSshGuest.microvm.shares) == 1;
          message = "a guest without SSH must still declare one writable + one read-only share, got ${
            toString (map (s: s.tag) noSshGuest.microvm.shares)
          }";
        }
        {
          # No host home, and nothing outside the module's own roots.
          assertion = builtins.all (s: lib.hasPrefix "${microvmOpts.runtimeRoot}/" s.source) shares;
          message = "a guest share escapes the module's runtime root: ${toString (map (s: s.source) shares)}";
        }
        {
          # The two trees must not nest, or the read-only payloads would be
          # reachable through the writable share.
          assertion =
            !(lib.hasPrefix "${session.root}/" session.roRoot)
            && !(lib.hasPrefix "${session.roRoot}/" session.root);
          message = "the writable and read-only session trees must be disjoint (${session.root} / ${session.roRoot})";
        }
        # --- the ownership/mode table is the ONE source of truth -----------
        {
          # Asserted FROM the table (not against a second hardcoded copy):
          # every entry has a matching host tmpfiles rule, so the tree really
          # is created with the owners and modes the trust split needs.
          assertion = builtins.all (r: builtins.elem r enabledCfg.systemd.tmpfiles.rules) expectedRules;
          message = "the session tree is not pre-created exactly as its layout table says; missing: ${
            toString (builtins.filter (r: !(builtins.elem r enabledCfg.systemd.tmpfiles.rules)) expectedRules)
          }";
        }
        {
          assertion =
            session.tmpfilesRules == builtins.filter (
              r: lib.hasInfix "${microvmOpts.runtimeRoot}/sessions" r
            ) enabledCfg.systemd.tmpfiles.rules;
          message = "the host must create exactly the session directories the table declares";
        }
        {
          # The trust boundary itself, read off the table: the control
          # directories are root-owned, the agent's are not.
          assertion =
            builtins.all (rel: (entry rel).uid == 0) [
              ""
              session.subdirs.input
              session.subdirs.controller
              session.subdirs.workerLogs
            ]
            && builtins.all (rel: (entry rel).uid == microvmOpts.guestAgentUid) [
              session.subdirs.workspace
              session.subdirs.worker
              session.subdirs.state
            ];
          message = "the session layout does not put the control directories under root and the agent's under uid ${toString microvmOpts.guestAgentUid}";
        }
        {
          # `controller/` carries the allocation token: root-ONLY.
          assertion = (entry session.subdirs.controller).mode == "0700";
          message = "the controller directory must stay root-only 0700, got ${(entry session.subdirs.controller).mode}";
        }
        {
          # The whole read-only tree is root-owned and denies group/other.
          assertion = builtins.all (e: e.uid == 0 && lib.hasSuffix "00" e.mode) session.fullRoLayout;
          message = "the read-only tree must be root-owned and root-only, got ${
            toString (map (e: "${e.rel}:${e.mode}:${toString e.uid}") session.fullRoLayout)
          }";
        }
        # --- every consumer DERIVES from the table ------------------------
        {
          assertion =
            jobs.slotDir refSlot.name == session.slotDir refSlot.name
            && jobs.guestMountPoint == session.guestMountPoint
            && jobs.inputSubdir == session.subdirs.input
            && jobs.controllerSubdir == session.subdirs.controller
            && jobs.workerSubdir == session.subdirs.worker
            && jobs.workerLogsSubdir == session.subdirs.workerLogs
            && jobs.controllerDirMode == (entry session.subdirs.controller).mode
            && jobs.inputDirMode == (entry session.subdirs.input).mode;
          message = "job.nix does not derive its paths/modes from the session layout";
        }
        {
          assertion =
            agentStatePaths.slotDir refSlot.name == session.hostStateDir refSlot.name
            && agentStatePaths.guestMountPoint == session.guestStateDir;
          message = "state.nix does not derive its per-slot paths from the session layout";
        }
        {
          assertion =
            seed.hostPayloadDir refSlot.name == session.hostConfigSeedDir refSlot.name
            && seed.guestMountPoint == session.guestConfigSeedDir
            && seed.dirMode == (roEntry session.roSubdirs.configSeed).mode;
          message = "config-seed.nix does not derive its staged payload path/mode from the session layout";
        }
        {
          assertion =
            hostKeys.slotDir refSlot.name == session.hostHostkeysDir refSlot.name
            && hostKeys.guestMountPoint == session.guestHostkeysDir
            && lib.hasPrefix "${session.guestHostkeysDir}/" hostKeys.guestKeyPath;
          message = "hostkeys.nix does not derive its per-slot paths from the session layout";
        }
        {
          # The SSH private host key must never be inside the WRITABLE tree.
          assertion =
            !(lib.hasPrefix "${session.slotDir refSlot.name}/" (hostKeys.slotDir refSlot.name))
            && lib.hasPrefix "${session.roSlotDir refSlot.name}/" (hostKeys.slotDir refSlot.name);
          message = "the SSH host-key directory must live in the READ-ONLY tree, is ${hostKeys.slotDir refSlot.name}";
        }
        {
          # ... and neither the staged configuration nor its manifest may be
          # writable by (or, for the manifest, visible to) the guest.
          assertion =
            !(lib.hasPrefix "${session.slotDir refSlot.name}/" (seed.hostPayloadDir refSlot.name))
            && builtins.all (
              s:
              seed.hostManifest refSlot.name != s.source
              && !(lib.hasPrefix "${s.source}/" (seed.hostManifest refSlot.name))
            ) shares;
          message = "the staged configuration must stay read-only and its manifest outside every share (manifest: ${seed.hostManifest refSlot.name})";
        }
        # --- the guest surfaces /workspace and orders its units -----------
        {
          # `agent-run`'s findmnt + writability checks, and every agent's
          # expectation of /workspace, keep working through a bind mount.
          assertion =
            let
              ws = guest0Cfg.fileSystems."/workspace";
            in
            ws.device == session.guestWorkspaceSource
            && builtins.elem "bind" ws.options
            && builtins.elem "x-systemd.requires-mounts-for=${session.guestMountPoint}" ws.options;
          message = "the guest must bind-mount ${session.guestWorkspaceSource} to /workspace after the session mount, got ${
            toString guest0Cfg.fileSystems."/workspace".options
          }";
        }
        {
          assertion = session.guestWorkspace == "/workspace";
          message = "the session layout must surface the workspace at /workspace";
        }
        {
          # The batch controller and the (untrusted) worker template must not
          # start before the session mount and the /workspace bind exist.
          assertion =
            let
              want = "/workspace ${session.guestMountPoint}";
            in
            requiresMountsFor guest0Cfg.systemd.services.agent-job-controller == want
            && requiresMountsFor guest0Cfg.systemd.services."${jobs.workerUnitTemplate}" == want;
          message = "the batch controller/worker must require /workspace and ${session.guestMountPoint}, got ${toString (requiresMountsFor guest0Cfg.systemd.services.agent-job-controller)}";
        }
        {
          assertion = requiresMountsFor guest0Cfg.systemd.services.agent-state-link == session.guestStateDir;
          message = "the agent-state linker must require ${session.guestStateDir}, got ${toString (requiresMountsFor guest0Cfg.systemd.services.agent-state-link)}";
        }
        {
          assertion =
            requiresMountsFor guest0Cfg.systemd.services.agent-config-seed == session.guestConfigSeedDir
            &&
              guest0Cfg.systemd.services.agent-config-seed.serviceConfig.ReadOnlyPaths == [
                "-${session.guestConfigSeedDir}"
              ];
          message = "the config-seed seeder must require (and only read) ${session.guestConfigSeedDir}";
        }
        {
          # sshd reads its host key from the read-only mount, so it must be
          # ordered against it.
          assertion =
            microvmOpts.enableSsh
            && guest0Cfg.services.openssh.enable
            && requiresMountsFor guest0Cfg.systemd.services.sshd == session.guestHostkeysDir
            && lib.hasPrefix "${session.guestHostkeysDir}/" (lib.head guest0Cfg.services.openssh.hostKeys).path;
          message = "the guest must order sshd after the read-only host-key mount";
        }
        # --- the layout POLICY rejects a weakened table -------------------
        {
          # Positive control: the REAL table satisfies the policy the module
          # asserts (so the negative cases below cannot pass vacuously).
          assertion = session.violationsOf realPolicyInput == [ ];
          message = "the real session layout must satisfy its own policy, got ${toString (session.violationsOf realPolicyInput)}";
        }
        {
          # NON-VACUITY of binding the policy to the FULL tables: they must
          # really carry BOTH capabilities' entries, whatever this host selects.
          # Without this, a future narrowing of the reference host would leave
          # every `input/` / `controller/` / `worker-logs/` / `hostkeys/` rule
          # below inspecting a table that no longer has the entry.
          assertion =
            lib.all (rel: lib.elem rel (map (e: e.rel) session.fullLayout)) [
              ""
              session.subdirs.workspace
              session.subdirs.input
              session.subdirs.controller
              session.subdirs.worker
              session.subdirs.workerLogs
              session.subdirs.state
            ]
            && lib.all (rel: lib.elem rel (map (e: e.rel) session.fullRoLayout)) [
              ""
              session.roSubdirs.hostkeys
              session.roSubdirs.configSeed
            ];
          message = "the FULL layout tables must contain every capability's entries, got ${
            toString (map (e: e.rel) session.fullLayout)
          } / ${toString (map (e: e.rel) session.fullRoLayout)}";
        }
        {
          # ... and the FILTERED tables are exactly a subset of them (the
          # capability selector may only REMOVE entries, never invent or alter
          # one, which is what makes `modeOf` on the full table safe).
          assertion =
            lib.all (e: lib.elem e session.fullLayout) session.layout
            && lib.all (e: lib.elem e session.fullRoLayout) session.roLayout;
          message = "the capability-filtered layout must be a subset of the full one";
        }
        {
          assertion = doctored "'${wrel session.subdirs.input}' must be root-owned" (
            withRel session.subdirs.input { owner = "agent"; }
          );
          message = "an AGENT-owned input/ must be rejected (the guest could rewrite its own job)";
        }
        {
          assertion = doctored "'${wrel session.subdirs.input}' is group/other-writable" (
            withRel session.subdirs.input { mode = "0777"; }
          );
          message = "a world-writable input/ must be rejected";
        }
        {
          assertion = doctored "'${wrel session.subdirs.controller}' grants group/other access" (
            withRel session.subdirs.controller { mode = "0755"; }
          );
          message = "a group/other-readable controller/ must be rejected (it carries the allocation token)";
        }
        {
          assertion = doctored "'${wrel session.subdirs.controller}' must be root-owned" (
            withRel session.subdirs.controller { owner = "agent"; }
          );
          message = "an AGENT-owned controller/ must be rejected (the guest could forge a result)";
        }
        {
          assertion = doctored "'${wrel session.subdirs.workerLogs}' must be root-owned" (
            withRel session.subdirs.workerLogs { owner = "agent"; }
          );
          message = "an AGENT-owned worker-logs/ must be rejected (guest systemd opens those files as root)";
        }
        {
          assertion =
            doctored "read-only '${session.roRoot}/${session.roSubdirs.configSeed}' is group/other-writable"
              (_: {
                readOnly = map (
                  e: if e.rel == session.roSubdirs.configSeed then e // { mode = "0777"; } else e
                ) session.fullRoLayout;
              });
          message = "a group/other-writable staged configuration must be rejected";
        }
        {
          assertion = doctored "host-key directory" (input: {
            hostKeyDir = "${input.writableRoot}/${refSlot.name}/${session.roSubdirs.hostkeys}";
          });
          message = "SSH host keys inside the WRITABLE session tree must be rejected";
        }
        {
          assertion = doctored "is inside the WRITABLE session tree" (input: {
            configSeedDir = "${input.writableRoot}/${refSlot.name}/${session.roSubdirs.configSeed}";
          });
          message = "a staged configuration inside the WRITABLE session tree must be rejected";
        }
        {
          assertion = doctored "overlap" (input: {
            readOnlyRoot = "${input.writableRoot}/ro";
          });
          message = "a read-only tree nested inside the writable one must be rejected";
        }
        {
          # ... and the POSITIVE control for the mechanism above: an infix that
          # nothing produces must NOT match, or every `doctored` line would be
          # satisfied by any message at all.
          assertion =
            !(doctored "this infix never appears" (withRel session.subdirs.input { owner = "agent"; }));
          message = "positive control: `doctored` must not match an arbitrary infix";
        }
        # --- NEGATIVE config: the guest agent must stay unprivileged -------
        {
          # With uid 0 the `agent`-owned directories of the table would BE
          # root-owned and the whole split would collapse.
          # `guestAgentUid = 0` is rejected by the option TYPE (positive
          # integer) before the assertion can even run, so `tryEval` counts as
          # a rejection here exactly as in `rejectsWith` above.
          assertion = rejectsWith [ { myconfig.ai.microvm.guestAgentUid = lib.mkForce 0; } ] "unprivileged";
          message = "a privileged guest agent uid must be rejected at eval";
        }
      ];
    in
    pkgs.runCommand "microvm-session-tree"
      {
        inherit evalMarker;
        # Building these runs their writeShellApplication shellcheck gate; the
        # greps below then inspect the GENERATED code.
        verifierBin = "${session.verifier}/bin/agent-microvm-verify-session";
        # EVAL-DEPTH ONLY (a `.drvPath` string, nothing is built): a guest that
        # does not even evaluate to a realisable derivation must fail CI, and
        # this check is the one that owns the guest SHAPE.
        guestToplevelDrv = guest0Cfg.system.build.toplevel.drvPath;
        guestRunnerDrv = guest0Cfg.microvm.declaredRunner.drvPath;
        noSshGuestToplevelDrv = noSshGuest.system.build.toplevel.drvPath;
        launcherBin = "${launcherPkg}/bin/agent-microvm";
        sessionRoot = session.root;
        sessionRoRoot = session.roRoot;
        # "<rel> <mode-without-leading-zero> <uid>" per entry, from the module's
        # table — so the greps below follow a layout change too.
        writableEntries = lib.concatMapStringsSep "\n" (
          e: "${if e.rel == "" then "." else e.rel} ${lib.removePrefix "0" e.mode} ${toString e.uid}"
        ) (builtins.filter (e: e.strictMode) session.layout);
        roEntries = lib.concatMapStringsSep "\n" (
          e: "${if e.rel == "" then "." else e.rel} ${lib.removePrefix "0" e.mode} ${toString e.uid}"
        ) session.roLayout;
      }
      ''
        # --- the PRE-LAUNCH verifier -------------------------------------
        # Every directory of the table is verified, with its exact expected
        # owner and (where the host controls it) mode.
        # The generated call for one entry is
        #   verify_dir "<path>" '<label>' <uid> <mode> <private>
        # so the last three fields carry the expectations this table declares.
        check_entry() {
          local var="$1" rel="$2" mode="$3" uid="$4" path line got
          if [ "$rel" = "." ]; then path="\"\$$var\""; else path="\"\$$var/$rel\""; fi
          line="$(grep -F -- "verify_dir $path " "$verifierBin" | head -n 1)"
          [ -n "$line" ] \
            || { echo "the verifier does not check '$rel' of \$$var" >&2; exit 1; }
          got="$(printf '%s' "$line" | awk '{print $(NF-2), $(NF-1)}')"
          [ "$got" = "$uid $mode" ] \
            || { echo "the verifier demands '$got' for '$rel', expected '$uid $mode'" >&2; exit 1; }
        }
        while IFS=' ' read -r rel mode uid; do
          [ -n "$rel" ] || continue
          check_entry SESSION_DIR "$rel" "$mode" "$uid"
        done <<< "$writableEntries"
        while IFS=' ' read -r rel mode uid; do
          [ -n "$rel" ] || continue
          check_entry SESSION_RO_DIR "$rel" "$mode" "$uid"
        done <<< "$roEntries"
        # It refuses a symlinked component (path traversal / escape), a
        # non-root parent directory and any group/other-writable directory.
        for pat in 'is a SYMLINK' 'group/other-writable' 'must be owned by uid' \
                   'a parent directory of the session tree is missing'; do
          grep -qF -- "$pat" "$verifierBin" \
            || { echo "the verifier lost its '$pat' check" >&2; exit 1; }
        done
        # ... and it enforces that no SSH host-key material is in the writable
        # tree (the plan's own rule).
        grep -qF -- 'SSH host-key material found in the WRITABLE session tree' "$verifierBin" \
          || { echo "the verifier does not keep SSH host keys out of the writable tree" >&2; exit 1; }
        # The slot name it is given must come from the prebuilt pool.
        grep -qF -- "unknown slot" "$verifierBin" \
          || { echo "the verifier accepts an arbitrary slot argument" >&2; exit 1; }
        # ... and NOTHING the table does not declare may be in either tree: the
        # per-directory checks alone say nothing about UNDECLARED entries, which
        # is how a stale batch subdirectory of a previous capability selection
        # would end up exported to the guest unverified.
        for t in '"$SESSION_DIR"' '"$SESSION_RO_DIR"'; do
          grep -qF -- "assert_no_extras $t" "$verifierBin" \
            || { echo "the verifier does not reject undeclared entries of $t" >&2; exit 1; }
        done
        grep -qF -- 'the session layout table does not declare for this host' "$verifierBin" \
          || { echo "the verifier's undeclared-entry refusal lost its message" >&2; exit 1; }
        # The launcher SWEEPS them before every launch, so the fail-closed check
        # above cannot brick a slot after a capability change.
        grep -qF -- 'session_sweep_extras()' "$launcherBin" \
          || { echo "the launcher has no session_sweep_extras" >&2; exit 1; }
        test "$(grep -c 'session_sweep_extras "\$dir"' "$launcherBin")" -eq 2 \
          || { echo "prepare_session must sweep BOTH trees" >&2; exit 1; }
        grep -qF -- 'while a mount survives under it' "$launcherBin" \
          || { echo "the sweep would rm -rf through a surviving mount" >&2; exit 1; }

        # --- the launcher prepares / verifies / REMOVES the whole tree ------
        for fn in prepare_session verify_session clear_session; do
          grep -qF -- "$fn()" "$launcherBin" \
            || { echo "the launcher has no $fn" >&2; exit 1; }
        done
        grep -qF -- "$verifierBin" "$launcherBin" \
          || { echo "the launcher does not call the generated session verifier" >&2; exit 1; }
        # The verification runs BEFORE the VM is started.
        grep -qF -- 'verify_session "$slot"' "$launcherBin" \
          || { echo "the launcher never verifies a session before launch" >&2; exit 1; }
        test "$(grep -c 'verify_session "\$slot"' "$launcherBin")" -ge 2 \
          || { echo "both run and submit must verify the session tree" >&2; exit 1; }
        test "$(grep -c 'prepare_session "\$slot"' "$launcherBin")" -ge 2 \
          || { echo "both run and submit must prepare the session tree" >&2; exit 1; }
        # Cleanup removes the COMPLETE tree, and refuses to do so through a
        # surviving bind mount (which would delete the clone / persisted state).
        grep -qE 'rm -rf -- "\$\{dir:\?\}"' "$launcherBin" \
          || { echo "the launcher does not remove the complete session tree" >&2; exit 1; }
        grep -qF -- 'refusing to remove the session tree' "$launcherBin" \
          || { echo "the launcher removes the session tree without proving the binds are gone" >&2; exit 1; }
        grep -qF -- 'still exists after removing it' "$launcherBin" \
          || { echo "the launcher does not prove the session tree is gone" >&2; exit 1; }
        grep -qF -- 'clear_session "$slot" || leaked=1' "$launcherBin" \
          || { echo "the teardown does not clear the session tree (or swallows its failure)" >&2; exit 1; }
        # Both bind-mount targets are inside the session tree, and there is
        # exactly ONE definition of each.
        grep -qF -- 'mount_point()  { printf '"'"'%s'"'"' "$SESSION_ROOT/$1/$SESSION_WORKSPACE_SUBDIR"; }' "$launcherBin" \
          || { echo "the launcher does not put the workspace bind inside the session tree" >&2; exit 1; }
        grep -qF -- 'state_slot_dir() { printf '"'"'%s'"'"' "$SESSION_ROOT/$1/$SESSION_STATE_SUBDIR"; }' "$launcherBin" \
          || { echo "the launcher does not put the agent-state bind inside the session tree" >&2; exit 1; }
        test "$(grep -c 'mount_point()' "$launcherBin")" -eq 1 \
          || { echo "mount_point must have exactly one definition" >&2; exit 1; }
        test "$(grep -c 'state_slot_dir()' "$launcherBin")" -eq 1 \
          || { echo "state_slot_dir must have exactly one definition" >&2; exit 1; }
        # There is only ONE share layout left: no code may still branch on the
        # historical four-share paths.
        for pat in '/run/agent-job' '/var/lib/agent-state' '/var/lib/agent-hostkey' \
                   '$STATE_ROOT/$1/workspace'; do
          if grep -qF -- "$pat" "$launcherBin"; then
            echo "the launcher still references the historical four-share path $pat" >&2
            exit 1
          fi
        done

        # The guest (and its runner) must EVALUATE to a realisable derivation.
        # Referencing the drvPaths here is what forces that evaluation in CI.
        for drv in "$guestToplevelDrv" "$guestRunnerDrv" "$noSshGuestToplevelDrv"; do
          case "$drv" in
            /nix/store/*.drv) ;;
            *) echo "a guest did not evaluate to a derivation: '$drv'" >&2; exit 1 ;;
          esac
        done

        {
          echo "microvm-session-tree:"
          echo "  writable tree : $sessionRoot/<slot>"
          echo "  guest         : $guestToplevelDrv"
          echo "  runner        : $guestRunnerDrv"
          printf '    %s\n' "$writableEntries"
          echo "  read-only tree: $sessionRoRoot/<slot>"
          printf '    %s\n' "$roEntries"
          echo "  verifier      : $verifierBin"
          echo "  launcher      : $launcherBin"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # (b2) phase 5 — INTERACTIVE and BATCH are independently selectable.      #
  #      The DEFAULT host must still carry BOTH halves (regression guard    #
  #      bound to the real evaluated config of test-f13), an               #
  #      interactive-only host must have NO batch unit / path / package and #
  #      no `submit`, and a batch-only host must have NO sshd, NO host-key  #
  #      tree and no `run`/`ssh`. Every absence is read off the EVALUATED    #
  #      config (units, tmpfiles rules, package names, layout table) or off  #
  #      the BUILT launcher, never off intent.                              #
  # ---------------------------------------------------------------------- #
  microvm-capabilities =
    let
      # ---- the DEFAULT (both capabilities) --------------------------------
      defaultCaps = microvmOpts.capabilities;
      defaultGuestServices = builtins.attrNames guest0Cfg.systemd.services;
      defaultGuestPkgNames = pkgNamesOf guest0Cfg.environment.systemPackages;
      hostRules = enabledCfg.systemd.tmpfiles.rules;
      hostServiceNames = builtins.attrNames enabledCfg.systemd.services;
      workmuxNames = builtins.attrNames enabledCfg.myconfig.ai.workmux.agents;

      controllerService = lib.removeSuffix ".service" jobs.controllerUnit;
      workerService = jobs.workerUnitTemplate;
      # The three job-protocol programs, by NAME (the derivations themselves are
      # per-host, so a name is what compares across variants).
      jobProgramNames = [
        "agent-job-controller"
        "agent-job-worker"
        "agent-job-assert-paths"
      ];
      batchSubdirs = [
        session.subdirs.input
        session.subdirs.controller
        session.subdirs.worker
        session.subdirs.workerLogs
      ];

      # ---- the two narrowed hosts -----------------------------------------
      iGuest = capGuestOf interactiveOnlyHost;
      iSession = capSessionOf interactiveOnlyHost;
      iRules = interactiveOnlyHost.config.systemd.tmpfiles.rules;
      iHostKeys = interactiveOnlyHost._module.args.agentHostKeys;
      iSlotDir = iSession.slotDir variantSlot.name;
      iRoSlotDir = iSession.roSlotDir variantSlot.name;

      bGuest = capGuestOf batchOnlyHost;
      bSession = capSessionOf batchOnlyHost;
      bJobs = batchOnlyHost._module.args.agentJobs;
      bRules = batchOnlyHost.config.systemd.tmpfiles.rules;
      bRoSlotDir = bSession.roSlotDir variantSlot.name;
      bController = bGuest.systemd.services.${controllerService};
      bWorker = bGuest.systemd.services.${workerService};

      # The batch + VSOCK variant (lightweight plan phase 6): a batch-only host
      # that ALSO selects `vsock`, so it has the VSOCK `sshd-vsock@` control
      # channel instead of a TCP sshd. The transport it provides is what lets
      # the runtime-validation suite reach a batch-only guest at all.
      bvGuest = capGuestOf batchVsockHost;
      bvSession = capSessionOf batchVsockHost;
      bvRules = batchVsockHost.config.systemd.tmpfiles.rules;
      bvRoSlotDir = bvSession.roSlotDir variantSlot.name;

      rels = l: map (e: e.rel) l;
      mentions = rules: infix: lib.any (r: lib.hasInfix infix r) rules;

      # --- the WORKLOAD-vs-TRANSPORT capability matrix --------------------
      # `interactive` and `batch` are WORKLOAD capabilities (what the guest can
      # be asked to do); `vsock` is a TRANSPORT (how the host reaches it) and
      # declares no workload, so it must never be selectable alone. Every cell
      # of the matrix is exercised below against the REAL reference host, so a
      # future token cannot quietly re-open the transport-only hole.
      #
      # `enableSsh` is forced per cell to satisfy the INDEPENDENT
      # enableSsh/`interactive` reconciliation: a cell without `interactive`
      # must turn the SSH server off, or that other assertion would fire and
      # make an `acceptsWithout` cell fail for the wrong reason.
      capabilityCell = caps: [
        {
          myconfig.ai.microvm.capabilities = lib.mkForce caps;
          myconfig.ai.microvm.enableSsh = lib.mkForce (lib.elem "interactive" caps);
        }
      ];
      noWorkloadNeedle = "no WORKLOAD capability";
      # The messages of the transport-only rejection, so the test can assert the
      # message actually NAMES the three tokens an operator has to reason about
      # (an "invalid capabilities" message would be useless here).
      transportOnlyMessages = lib.filter (m: lib.hasInfix noWorkloadNeedle m) (
        failedAssertions (capabilityCell [ "vsock" ])
      );
      # The generated allowlist fragments of a layout table: the launcher's
      # `session_sweep_extras` argument list and the verifier's
      # `SESSION_*_ENTRIES` array, rendered with the SAME quoting the module uses
      # so the greps below compare against the exact generated text.
      sweepListOf =
        l: lib.concatMapStringsSep " " (e: lib.escapeShellArg e.rel) (lib.filter (e: e.rel != "") l);
      entryArrayOf = sweepListOf;

      evalMarker = mkEvalCheck "microvm-capabilities-eval" ([
        # --- the DEFAULT selects BOTH, i.e. today's behaviour -------------
        {
          assertion =
            lib.sort (a: b: a < b) defaultCaps == [
              "batch"
              "interactive"
            ];
          message = "the module default must select BOTH capabilities, got ${toString defaultCaps}";
        }
        {
          # The batch half of the reference host, from its real units.
          assertion =
            lib.elem controllerService defaultGuestServices && lib.elem workerService defaultGuestServices;
          message = "the default guest must carry the batch controller + worker template";
        }
        {
          assertion = lib.all (n: lib.elem n defaultGuestPkgNames) jobProgramNames;
          message = "the default guest must carry the job-protocol programs (${toString jobProgramNames})";
        }
        {
          # The interactive half of the reference host.
          assertion =
            lib.elem "sshd" defaultGuestServices
            && guest0Cfg.services.openssh.enable
            && lib.elem "agent-run" defaultGuestPkgNames;
          message = "the default guest must carry sshd and the interactive `agent-run` entry point";
        }
        {
          assertion = lib.elem "agent-microvm-hostkeys" hostServiceNames;
          message = "the default host must provision per-slot SSH host keys";
        }
        {
          assertion = lib.all (sub: mentions hostRules "${session.slotDir refSlot.name}/${sub}") batchSubdirs;
          message = "the default host must create every batch subdirectory of the session tree";
        }
        {
          assertion =
            mentions hostRules "${session.hostHostkeysDir refSlot.name}" && mentions hostRules jobs.resultsDir;
          message = "the default host must create the host-key tree and the batch result archive";
        }
        {
          assertion = lib.any (n: lib.hasPrefix "microvm-" n) workmuxNames;
          message = "the default host must register the interactive workmux panes";
        }

        # --- INTERACTIVE-ONLY: the batch half is GONE ---------------------
        {
          assertion =
            !(iGuest.systemd.services ? ${controllerService}) && !(iGuest.systemd.services ? ${workerService});
          message = "an interactive-only guest must have NO batch controller and NO worker template, got ${
            toString (lib.filter (n: lib.hasPrefix "agent-job" n) (builtins.attrNames iGuest.systemd.services))
          }";
        }
        {
          assertion = !(lib.any (n: lib.elem n (capGuestPkgNames interactiveOnlyHost)) jobProgramNames);
          message = "an interactive-only guest must not carry any job-protocol program";
        }
        {
          assertion =
            rels iSession.layout == [
              ""
              session.subdirs.workspace
              session.subdirs.state
            ];
          message = "an interactive-only session tree must contain only the root, the workspace and the state bind point, got ${toString (rels iSession.layout)}";
        }
        {
          assertion = !(lib.any (sub: mentions iRules "${iSlotDir}/${sub}") batchSubdirs);
          message = "an interactive-only host must emit NO tmpfiles rule for a batch subdirectory";
        }
        {
          assertion = !(mentions iRules bJobs.resultsDir);
          message = "an interactive-only host must not create the batch result archive (${bJobs.resultsDir})";
        }
        {
          # ... and the interactive half is intact, including the read-only
          # host-key subdirectory and its tmpfiles rule.
          assertion =
            iGuest.services.openssh.enable
            && iGuest.systemd.services ? sshd
            && lib.elem "agent-run" (capGuestPkgNames interactiveOnlyHost);
          message = "an interactive-only guest must still run sshd and carry `agent-run`";
        }
        {
          assertion =
            iGuest.services.openssh.hostKeys == [
              {
                type = "ed25519";
                path = iHostKeys.guestKeyPath;
              }
            ];
          message = "an interactive-only guest must use exactly the per-slot host key from the read-only share";
        }
        {
          assertion =
            lib.elem session.roSubdirs.hostkeys (rels iSession.roLayout)
            && mentions iRules "${iRoSlotDir}/${session.roSubdirs.hostkeys}";
          message = "an interactive-only host must keep the read-only host-key subdirectory";
        }
        {
          assertion = lib.elem "agent-microvm-hostkeys" (
            builtins.attrNames interactiveOnlyHost.config.systemd.services
          );
          message = "an interactive-only host must still provision its per-slot SSH host keys";
        }

        # --- BATCH-ONLY: the interactive half is GONE ---------------------
        {
          assertion = !(bGuest.systemd.services ? sshd) && !bGuest.services.openssh.enable;
          message = "a batch-only guest must have NO sshd";
        }
        {
          assertion = !(lib.elem "agent-run" (capGuestPkgNames batchOnlyHost));
          message = "a batch-only guest must not carry the interactive `agent-run` entry point";
        }
        {
          assertion = !(lib.elem session.roSubdirs.hostkeys (rels bSession.roLayout));
          message = "a batch-only read-only tree must have NO host-key subdirectory, got ${toString (rels bSession.roLayout)}";
        }
        {
          assertion = !(mentions bRules "${bRoSlotDir}/${session.roSubdirs.hostkeys}");
          message = "a batch-only host must emit no tmpfiles rule for a host-key directory";
        }
        {
          assertion =
            !(lib.elem "agent-microvm-hostkeys" (builtins.attrNames batchOnlyHost.config.systemd.services));
          message = "a batch-only host must not provision SSH host keys";
        }
        {
          # No known_hosts generation either: the provisioning program is the
          # ONLY thing that writes it, so no host unit may reference it.
          assertion =
            !(lib.any (e: lib.hasInfix "agent-microvm-provision-hostkeys" e) (hostExecStartsOf batchOnlyHost));
          message = "a batch-only host must not run the host-key / known_hosts generator";
        }
        {
          # POSITIVE control for the check above: the reference host DOES.
          assertion = lib.any (e: lib.hasInfix "agent-microvm-provision-hostkeys" e) (
            hostExecStartsOf self.nixosConfigurations.test-f13
          );
          message = "positive control: the default host must run the host-key generator";
        }
        {
          assertion =
            !(lib.any (n: lib.hasPrefix "microvm-" n) (
              builtins.attrNames batchOnlyHost.config.myconfig.ai.workmux.agents
            ));
          message = "a batch-only host must register no interactive workmux pane";
        }
        {
          # ... and the batch half is intact, with its trust split.
          assertion =
            bGuest.systemd.services ? ${controllerService} && bGuest.systemd.services ? ${workerService};
          message = "a batch-only guest must carry the controller and the worker template";
        }
        {
          assertion = lib.all (n: lib.elem n (capGuestPkgNames batchOnlyHost)) jobProgramNames;
          message = "a batch-only guest must carry the job-protocol programs";
        }
        {
          # The TRUSTED controller runs as guest root (no User=), the
          # UNTRUSTED worker as the unprivileged agent, and the worker cannot
          # even see the controller's channel.
          assertion = !(bController.serviceConfig ? User);
          message = "the batch controller must run as guest root";
        }
        {
          assertion =
            bWorker.serviceConfig.User == bJobs.workerUser
            && bWorker.serviceConfig.ProtectProc == "invisible"
            && bWorker.serviceConfig.InaccessiblePaths == [ "-${bJobs.guestControllerDir}" ];
          message = "the batch worker must stay unprivileged and be denied the controller channel";
        }
        {
          assertion = bController.unitConfig.ConditionPathExists == bJobs.guestSpec;
          message = "the batch controller must stay inert without a job spec";
        }
        {
          assertion = lib.all (
            sub: mentions bRules "${bSession.slotDir variantSlot.name}/${sub}"
          ) batchSubdirs;
          message = "a batch-only host must create every batch subdirectory of the session tree";
        }

        # --- NEGATIVE eval tests, pinned to the SPECIFIC assertion --------
        {
          assertion = baselineClean;
          message = "positive control: the unmodified reference host must have no failed assertion";
        }
        {
          assertion = rejectsWith [
            { myconfig.ai.microvm.capabilities = lib.mkForce [ ]; }
          ] "selects no capability";
          message = "an EMPTY capability set must be rejected";
        }

        # --- the FULL workload/transport capability matrix -----------------
        # VALID cells: every selection that carries at least one WORKLOAD
        # capability must evaluate with no failed assertion at all.
        {
          assertion = acceptsWithout (capabilityCell [ "interactive" ]) noWorkloadNeedle;
          message = "capabilities = [ interactive ] must be accepted";
        }
        {
          assertion = acceptsWithout (capabilityCell [ "batch" ]) noWorkloadNeedle;
          message = "capabilities = [ batch ] must be accepted";
        }
        {
          assertion = acceptsWithout (capabilityCell [
            "interactive"
            "batch"
          ]) noWorkloadNeedle;
          message = "capabilities = [ interactive batch ] (the default) must be accepted";
        }
        {
          assertion = acceptsWithout (capabilityCell [
            "interactive"
            "vsock"
          ]) noWorkloadNeedle;
          message = "capabilities = [ interactive vsock ] must be accepted (a workload + the transport)";
        }
        {
          assertion = acceptsWithout (capabilityCell [
            "batch"
            "vsock"
          ]) noWorkloadNeedle;
          message = "capabilities = [ batch vsock ] must be accepted (the phase-6 batch+vsock shape)";
        }
        {
          assertion = acceptsWithout (capabilityCell [
            "interactive"
            "batch"
            "vsock"
          ]) noWorkloadNeedle;
          message = "capabilities = [ interactive batch vsock ] must be accepted";
        }
        # INVALID cells: no workload capability at all.
        {
          # THE hole this assertion closes: `vsock` alone enabled a VSOCK SSH
          # transport while declaring NEITHER execution capability, i.e. an
          # undocumented interactive mode reachable over VSOCK.
          assertion = rejectsWith (capabilityCell [ "vsock" ]) noWorkloadNeedle;
          message = "a TRANSPORT-ONLY capability set ([ vsock ]) must be rejected";
        }
        {
          # ... and it is rejected for the WORKLOAD reason even on a host that
          # leaves `enableSsh` on (where the reconciliation assertion also
          # fires): the two guards are independent.
          assertion = rejectsWith [
            { myconfig.ai.microvm.capabilities = lib.mkForce [ "vsock" ]; }
          ] noWorkloadNeedle;
          message = "[ vsock ] must be rejected for the missing WORKLOAD capability regardless of enableSsh";
        }
        {
          # EXACTLY ONE guard fires for the transport-only set, and its message
          # names all three tokens the operator must reason about.
          assertion =
            lib.length transportOnlyMessages == 1
            && lib.all (w: lib.hasInfix w (lib.head transportOnlyMessages)) [
              "interactive"
              "batch"
              "vsock"
            ];
          message = "the transport-only rejection must fire exactly once and name interactive, batch and vsock, got ${toString transportOnlyMessages}";
        }
        {
          # The EMPTY set trips the pre-existing "selects no capability" guard,
          # and the new workload guard as well — both are true statements about
          # it, so this only pins that the new guard did not REPLACE the old one.
          assertion =
            rejectsWith (capabilityCell [ ]) "selects no capability"
            && rejectsWith (capabilityCell [ ]) noWorkloadNeedle;
          message = "the empty capability set must still trip the pre-existing empty-selection guard";
        }
        {
          # NEGATIVE control: the workload guard must NOT fire on any valid
          # cell (it would mask the accept cells above if it always fired).
          assertion =
            !(lib.any (caps: rejectsWith (capabilityCell caps) noWorkloadNeedle) [
              [ "interactive" ]
              [ "batch" ]
              [
                "interactive"
                "batch"
              ]
              [
                "interactive"
                "vsock"
              ]
              [
                "batch"
                "vsock"
              ]
              [
                "interactive"
                "batch"
                "vsock"
              ]
            ]);
          message = "the WORKLOAD-capability guard must not fire on a selection that has one";
        }

        {
          assertion = rejectsWith [
            { myconfig.ai.microvm.capabilities = lib.mkForce [ "interactve" ]; }
          ] "unknown capability";
          message = "an UNKNOWN capability token must be rejected";
        }
        {
          # The reconciliation: `enableSsh` (true on the reference host) is
          # MEANINGLESS without the interactive capability and is rejected
          # rather than silently ignored.
          assertion = rejectsWith [
            { myconfig.ai.microvm.capabilities = lib.mkForce [ "batch" ]; }
          ] "has no meaning without the";
          message = "enableSsh without the `interactive` capability must be rejected";
        }
        {
          # The batch-capable-agent assertion must still fire for a host that
          # selects `batch` with a selection that has no batch-capable agent.
          # Every DECLARED agent is batch-capable today, so the conflict is
          # produced by substituting a registry whose batch subset is empty —
          # the same shape a future non-batch agent would create.
          assertion = rejectsWith [
            { _module.args.agentRegistry = lib.mkForce (agentRegistry // { batchNames = [ ]; }); }
          ] "selects no agent that can run";
          message = "a batch host with no batch-capable agent must be rejected";
        }
        {
          # ... and it must NOT fire once `batch` is deselected — that is the
          # recorded phase-5 requirement.
          assertion = acceptsWithout [
            {
              _module.args.agentRegistry = lib.mkForce (agentRegistry // { batchNames = [ ]; });
              myconfig.ai.microvm.capabilities = lib.mkForce [ "interactive" ];
            }
          ] "selects no agent that can run";
          message = "the batch-capable-agent assertion must NOT fire on an interactive-only host";
        }

        # --- BATCH + VSOCK (lightweight plan phase 6): the VSOCK control ----
        # channel is wired, the TCP sshd is suppressed, and the host-key tree
        # is provisioned for the VSOCK sshd exactly as for the interactive one.
        {
          assertion = bvGuest.microvm.vsock.cid == variantSlot.cid;
          message = "a batch+vsock guest must set microvm.vsock.cid to the slot's deterministic CID (${toString variantSlot.cid}), got ${toString bvGuest.microvm.vsock.cid}";
        }
        {
          assertion = bvGuest.services.openssh.enable;
          message = "a batch+vsock guest must enable services.openssh (the sshd-vsock@ unit + its NixOS dropins are gated on it)";
        }
        {
          # The TCP sshd is SUPPRESSED (the recorded deviation from phase 5):
          # `sshd.service` is not installed, so no TCP listener ever starts —
          # only the VSOCK `sshd-vsock@`.
          assertion = !(bvGuest.systemd.services.sshd.enable or true);
          message = "a batch+vsock guest must NOT install the TCP sshd.service (systemd.services.sshd.enable must be false); only the VSOCK sshd-vsock@ should run";
        }
        {
          # The TCP sshd is masked AND its TAP firewall opening is closed: the
          # VSOCK sshd is host-only (CID 2 -> vsock::22) and never needs a TAP
          # rule, so 22 must be absent from the guest's allowed TCP ports. This
          # pins the "no TCP/network SSH daemon" invariant's FIREWALL half —
          # without it the openssh module's default `openFirewall = true` would
          # silently open 22 on a guest whose sshd is masked (a regression
          # vector if the masking ever dropped).
          assertion =
            !(bvGuest.services.openssh.openFirewall or true)
            && !(lib.elem 22 bvGuest.networking.firewall.allowedTCPPorts);
          message = "a batch+vsock guest must close the TAP firewall for the TCP sshd (services.openssh.openFirewall = false and 22 absent from networking.firewall.allowedTCPPorts); the VSOCK sshd is host-only and needs no TAP rule";
        }
        {
          assertion = bvGuest.systemd.sockets ? "sshd-vsock" && !(bvGuest.systemd.sockets ? "sshd");
          message = "a batch+vsock guest must declare the sshd-vsock socket and NO TCP sshd socket";
        }
        {
          # N1: the live VSOCK control channel is the `sshd-vsock@` socket, so
          # the host-key mount ordering must live on THAT socket (not on the
          # masked `sshd.service`). The socket is wantedBy `sockets.target`
          # (early), so `RequiresMountsFor` makes it wait for the read-only
          # host-key share before it listens — eliminating the boot race in
          # which the VSOCK sshd accepts before the share is up. nixpkgs defines
          # the socket with `overrideStrategy = "asDropin"`, so this is a dropin
          # on the generator-created unit.
          assertion =
            (bvGuest.systemd.sockets.sshd-vsock.unitConfig.RequiresMountsFor or null)
            == bvSession.guestHostkeysDir;
          message = "a batch+vsock guest must order the sshd-vsock socket after the read-only host-key mount (RequiresMountsFor = the guest hostkeys dir); the VSOCK sshd reads its key from that share";
        }
        {
          assertion = lib.elem session.roSubdirs.hostkeys (rels bvSession.roLayout);
          message = "a batch+vsock read-only tree must contain the host-key subdirectory (the VSOCK sshd needs the per-slot host identity)";
        }
        {
          assertion = mentions bvRules "${bvRoSlotDir}/${session.roSubdirs.hostkeys}";
          message = "a batch+vsock host must emit the tmpfiles rule for the host-key directory";
        }
        {
          assertion = lib.elem "agent-microvm-hostkeys" (
            builtins.attrNames batchVsockHost.config.systemd.services
          );
          message = "a batch+vsock host must provision per-slot SSH host keys (the VSOCK channel is host-key-verified, like the TAP one)";
        }
        {
          assertion = bvGuest.users.users.agent.openssh.authorizedKeys.keyFiles or [ ] != [ ];
          message = "a batch+vsock guest must authorise the dedicated SSH key for the VSOCK sshd";
        }
        {
          assertion = !(lib.elem "agent-run" (capGuestPkgNames batchVsockHost));
          message = "a batch+vsock guest must not carry the interactive `agent-run` entry point (vsock adds a control channel, not the interactive capability)";
        }
        {
          # POSITIVE control: the batch-only (no vsock) variant has NONE of the
          # VSOCK plumbing — vsock is OFF by default, so a default/batch host is
          # byte-for-byte without it. It also has NO TCP sshd firewall opening
          # (no openssh at all), the positive control for the S1 fix below.
          assertion =
            bGuest.microvm.vsock.cid == null
            && !(bGuest.systemd.sockets ? "sshd-vsock")
            && !(lib.elem 22 bGuest.networking.firewall.allowedTCPPorts);
          message = "a batch-only (no vsock) guest must have no VSOCK device, no sshd-vsock socket and no TCP sshd firewall opening";
        }
        {
          # POSITIVE control for the S1 fix: the DEFAULT guest (enableSsh =
          # true, the `interactive` capability) DOES open 22 on its TAP firewall,
          # because its TCP sshd actually runs. This proves the firewall
          # suppression above is scoped to the batch+vsock shape and does not
          # over-close the firewall on a host that legitimately runs the TCP
          # sshd (`guest0Cfg` is the reference test-f13 guest).
          assertion =
            lib.elem 22 guest0Cfg.networking.firewall.allowedTCPPorts
            && guest0Cfg.services.openssh.openFirewall;
          message = "the default (interactive) guest must open TCP 22 on its firewall (its TCP sshd runs); the S1 firewall suppression is scoped to the batch+vsock shape";
        }

        # --- NEGATIVE: vsock requires a key + the closed network profiles ---
        {
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              myconfig.ai.microvm.sshPublicKeyFile = lib.mkForce null;
            }
          ] "an SSH control channel";
          message = "vsock without an sshPublicKeyFile must be rejected (the VSOCK sshd needs an authorising key)";
        }
        {
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              myconfig.ai.microvm.networkProfile = lib.mkForce "internet";
              myconfig.ai.microvm.acknowledgeInsecureNetwork = true;
            }
          ] "closed network profiles";
          message = "vsock with networkProfile = \"internet\" must be rejected";
        }
        {
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              myconfig.ai.microvm.networkProfile = lib.mkForce "package-access";
              myconfig.ai.microvm.packageProxyPort = 3128;
              myconfig.ai.microvm.acknowledgeInsecureNetwork = true;
            }
          ] "closed network profiles";
          message = "vsock with networkProfile = \"package-access\" must be rejected";
        }
        {
          # S2: the launcher's VSOCK `ssh` runs `ssh vsock-mux/<path>`, a hostname
          # that only resolves through the `Host vsock-mux/*` ProxyCommand
          # `20-systemd-ssh-proxy.conf` supplies when
          # `programs.ssh.systemd-ssh-proxy.enable` is true. A host that selects
          # `vsock` AND disables that nixpkgs option must fail at EVAL with the
          # pinned message, not at runtime with an opaque DNS-resolution error.
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              myconfig.ai.microvm.sshPublicKeyFile = lib.mkForce ../hosts/host.f13/dedicated-agent-vm-key.pub;
              programs.ssh.systemd-ssh-proxy.enable = lib.mkForce false;
            }
          ] "systemd-ssh-proxy";
          message = "vsock with programs.ssh.systemd-ssh-proxy.enable = false must be rejected (the VSOCK ssh hostname needs the ProxyCommand that option supplies)";
        }
        {
          # N2: `stateRoot` (the launcher's VSOCK ssh target + the per-slot
          # `known_hosts` key) MUST equal `microvm.stateDir` (where microvm.nix
          # backs the VSOCK device). A host that diverges them must fail at EVAL,
          # not at runtime with an unresolvable VSOCK mux socket. Forced on the
          # reference host (default `stateRoot` = `microvm.stateDir` =
          # `/var/lib/microvms`) by overriding ONLY `stateRoot`.
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.stateRoot = lib.mkForce "/var/lib/microvms-other";
            }
          ] "must equal";
          message = "a stateRoot that differs from microvm.stateDir must be rejected (the VSOCK ssh target / known_hosts key would not resolve to the socket microvm created)";
        }
      ]);
    in
    pkgs.runCommand "microvm-capabilities"
      {
        inherit evalMarker;
        # BUILT, not merely instantiated: the narrowed guests are exactly where a
        # `writeShellApplication` shellcheck failure hides (phase 4's SC2034 in
        # the batch worker survived every drvPath-only check).
        defaultGuest = guest0Cfg.system.build.toplevel;
        defaultRunner = guest0Cfg.microvm.declaredRunner;
        interactiveGuest = iGuest.system.build.toplevel;
        interactiveRunner = iGuest.microvm.declaredRunner;
        batchGuest = bGuest.system.build.toplevel;
        batchRunner = bGuest.microvm.declaredRunner;
        # The batch+vsock guest, BUILT (lightweight plan phase 6): the narrowed
        # closure where the VSOCK sshd + the suppressed TCP sshd live is exactly
        # where a `writeShellApplication` shellcheck failure or a VSOCK-wiring
        # eval error would hide, so it is forced like the other two.
        batchVsockGuest = bvGuest.system.build.toplevel;
        batchVsockRunner = bvGuest.microvm.declaredRunner;
        # The three launchers, BUILT (their own shellcheck gate) and then
        # inspected: the narrowed ones must refuse the subcommands whose
        # machinery they do not have, the default one must contain no guard at
        # all (so this phase left it untouched).
        defaultLauncher = "${launcherPkg}/bin/agent-microvm";
        interactiveLauncher = "${findPkg interactiveOnlyHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
        batchLauncher = "${findPkg batchOnlyHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
        # The batch+vsock launcher: it must ALLOW `ssh` (over VSOCK) and report
        # the vsock capability, while still refusing `run` (interactive-only).
        batchVsockLauncher = "${findPkg batchVsockHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
        # The generated pre-launch verifiers: they are rendered FROM the layout
        # table, so a narrowed tree must not even mention the other half's
        # directories.
        interactiveVerifier = "${iSession.verifier}/bin/agent-microvm-verify-session";
        batchVerifier = "${bSession.verifier}/bin/agent-microvm-verify-session";
        # The EXACT generated `verify_dir` argument of each directory (a bare
        # subdirectory name would also match the verifier's own prose).
        controllerVerifyArg = "\"$SESSION_DIR/${session.subdirs.controller}\"";
        hostkeysVerifyArg = "\"$SESSION_RO_DIR/${session.roSubdirs.hostkeys}\"";
        # The allowlists the two generated artefacts render from the FILTERED
        # table: the verifier's undeclared-entry check and the launcher's sweep
        # must both follow the narrowing, or a stale batch subdirectory would
        # either be exported unverified or refuse every launch. Rendered HERE
        # with the same `escapeShellArg` the module uses, so these are the exact
        # generated fragments.
        controllerRel = lib.escapeShellArg session.subdirs.controller;
        hostkeysRel = lib.escapeShellArg session.roSubdirs.hostkeys;
        iSweepList = sweepListOf iSession.layout;
        bSweepList = sweepListOf bSession.layout;
        iRoEntryList = entryArrayOf iSession.roLayout;
        bRoEntryList = entryArrayOf bSession.roLayout;
      }
      ''
        # --- the narrowed launchers refuse the missing capability ----------
        for pair in "submit batch" "cancel batch"; do
          grep -qF -- "require_capability $pair" "$interactiveLauncher" \
            || { echo "the interactive-only launcher does not refuse '$pair'" >&2; exit 1; }
        done
        # `run` needs `interactive` (single capability). (`ssh` needs `interactive`
        # OR `vsock` — phase 6 — so its refusal is `require_capability_any ssh
        # interactive vsock`, asserted explicitly below, not the single-capability
        # `require_capability ssh interactive` form this loop checks.)
        for pair in "run interactive"; do
          grep -qF -- "require_capability $pair" "$batchLauncher" \
            || { echo "the batch-only launcher does not refuse '$pair'" >&2; exit 1; }
        done
        # ... and neither refuses what it CAN do.
        if grep -qF -- 'require_capability run interactive' "$interactiveLauncher"; then
          echo "the interactive-only launcher refuses its own 'run'" >&2; exit 1
        fi
        if grep -qF -- 'require_capability submit batch' "$batchLauncher"; then
          echo "the batch-only launcher refuses its own 'submit'" >&2; exit 1
        fi
        # The refusal names the OPTION to change, not just "unsupported".
        for l in "$interactiveLauncher" "$batchLauncher"; do
          grep -qF -- 'myconfig.ai.microvm.capabilities' "$l" \
            || { echo "a narrowed launcher's refusal does not name the option" >&2; exit 1; }
        done
        # POSITIVE control: the DEFAULT launcher carries no REFUSAL at all (a
        # host with both capabilities has nothing to refuse, so the guard would
        # be unreachable code).
        if grep -qF -- 'require_capability' "$defaultLauncher"; then
          echo "the default launcher must contain no capability guard" >&2; exit 1
        fi

        # --- every launcher can be ASKED what it selects --------------------
        # The capability SET is unconditional configuration and machine-readable
        # on EVERY host: runtime-validation.sh must never have to infer it from an
        # English refusal message (a detection that failed OPEN, i.e. defaulted to
        # "this host has everything", the moment the message was reworded).
        for l in "$defaultLauncher" "$interactiveLauncher" "$batchLauncher" "$batchVsockLauncher"; do
          grep -qF -- 'cmd_capabilities()' "$l" \
            || { echo "a launcher has no 'capabilities' subcommand" >&2; exit 1; }
          grep -qF -- 'capabilities)     cmd_capabilities' "$l" \
            || { echo "a launcher does not dispatch 'capabilities'" >&2; exit 1; }
          grep -qE "^ *readonly DECLARED_CAPABILITIES=" "$l" \
            || { echo "a launcher does not render the DECLARED capability set" >&2; exit 1; }
        done
        grep -qF -- "readonly SELECTED_CAPABILITIES=${lib.escapeShellArg "interactive batch"}" "$defaultLauncher" \
          || { echo "the default launcher does not report BOTH capabilities" >&2; exit 1; }
        grep -qF -- "readonly SELECTED_CAPABILITIES=${lib.escapeShellArg "interactive"}" "$interactiveLauncher" \
          || { echo "the interactive-only launcher does not report its capability" >&2; exit 1; }
        grep -qF -- "readonly SELECTED_CAPABILITIES=${lib.escapeShellArg "batch"}" "$batchLauncher" \
          || { echo "the batch-only launcher does not report its capability" >&2; exit 1; }
        grep -qF -- "readonly SELECTED_CAPABILITIES=${lib.escapeShellArg "batch vsock"}" "$batchVsockLauncher" \
          || { echo "the batch+vsock launcher does not report its capability set (batch vsock)" >&2; exit 1; }

        # --- the batch+vsock launcher: `ssh` is ALLOWED over VSOCK, `run` refused ---
        # `ssh` needs `interactive` OR `vsock` (phase 6): a batch+vsock host has
        # vsock, so `ssh` is NOT refused — the VSOCK control channel replaces the
        # TCP sshd. `run` still needs `interactive`, so it IS refused. The launcher
        # also renders `VSOCK_ENABLED=1`, the flag its `ssh`/readiness path branches on.
        if grep -qF -- 'require_capability_any ssh' "$batchVsockLauncher"; then
          echo "the batch+vsock launcher refuses 'ssh' despite selecting vsock" >&2; exit 1
        fi
        grep -qF -- "require_capability run interactive" "$batchVsockLauncher" \
          || { echo "the batch+vsock launcher does not refuse 'run' (it has no interactive capability)" >&2; exit 1; }
        grep -qF -- 'readonly VSOCK_ENABLED=1' "$batchVsockLauncher" \
          || { echo "the batch+vsock launcher does not render VSOCK_ENABLED=1" >&2; exit 1; }
        # The batch-only (no vsock) launcher, by contrast, refuses `ssh` (no control
        # channel at all) and renders VSOCK_ENABLED=0.
        grep -qF -- 'require_capability_any ssh interactive vsock' "$batchLauncher" \
          || { echo "the batch-only (no vsock) launcher does not refuse 'ssh' (it has no control channel)" >&2; exit 1; }
        grep -qF -- 'readonly VSOCK_ENABLED=0' "$batchLauncher" \
          || { echo "the batch-only (no vsock) launcher does not render VSOCK_ENABLED=0" >&2; exit 1; }

        # --- `usage` reports no directory the narrowing removed -------------
        # `$RESULTS_DIR` is not created without the `batch` capability, so a
        # '0B' line for it is the same "reports something absent" defect the
        # honest `doctor` host-key section removed.
        for l in "$defaultLauncher" "$batchLauncher"; do
          grep -qF -- 'job results:' "$l" \
            || { echo "a batch host's usage report lost the result archive" >&2; exit 1; }
        done
        if grep -qF -- 'job results:' "$interactiveLauncher"; then
          echo "the interactive-only usage report still claims a result archive" >&2; exit 1
        fi
        grep -qF -- 'does not select the "batch" capability' "$interactiveLauncher" \
          || { echo "the interactive-only usage report does not name the missing capability" >&2; exit 1; }

        # --- the sweep + the undeclared-entry check follow the narrowing -----
        # Both are rendered from the FILTERED table, so a narrowed host REMOVES
        # the other capability's stale subdirectories before a launch and REFUSES
        # the launch while one is still present. Without this pair, narrowing
        # would silently drop the only coverage those directories ever had.
        grep -qF -- "session tree\" $iSweepList" "$interactiveLauncher" \
          || { echo "the interactive-only launcher does not sweep its session tree to the narrowed table" >&2; exit 1; }
        grep -qF -- "session tree\" $bSweepList" "$batchLauncher" \
          || { echo "the batch-only launcher does not sweep its session tree to the narrowed table" >&2; exit 1; }
        if grep -F -- 'session_sweep_extras "$dir" "session tree"' "$interactiveLauncher" \
             | grep -qF -- "$controllerRel"; then
          echo "the interactive-only launcher still allows a batch subdirectory in its session tree" >&2; exit 1
        fi
        grep -qF -- "readonly SESSION_RO_ENTRIES=($iRoEntryList)" "$interactiveVerifier" \
          || { echo "the interactive verifier's allowlist does not match its layout table" >&2; exit 1; }
        grep -qF -- "readonly SESSION_RO_ENTRIES=($bRoEntryList)" "$batchVerifier" \
          || { echo "the batch verifier's allowlist does not match its layout table" >&2; exit 1; }
        if grep -F -- 'readonly SESSION_RO_ENTRIES=' "$batchVerifier" | grep -qF -- "$hostkeysRel"; then
          echo "the batch-only verifier still allows a host-key directory" >&2; exit 1
        fi
        grep -F -- 'readonly SESSION_RO_ENTRIES=' "$interactiveVerifier" | grep -qF -- "$hostkeysRel" \
          || { echo "the interactive verifier does not allow the host-key directory" >&2; exit 1; }
        for v in "$interactiveVerifier" "$batchVerifier"; do
          grep -qF -- 'assert_no_extras "$SESSION_DIR"' "$v" \
            || { echo "a narrowed verifier does not reject undeclared session entries" >&2; exit 1; }
          grep -qF -- 'assert_no_extras "$SESSION_RO_DIR"' "$v" \
            || { echo "a narrowed verifier does not reject undeclared read-only entries" >&2; exit 1; }
        done

        # --- `doctor` stays HONEST about the host-key section ---------------
        # A batch-only host provisions no key material, so a "every slot has a
        # host-key directory" check would be vacuous; it must report the
        # capability instead.
        for l in "$defaultLauncher" "$interactiveLauncher"; do
          grep -qF -- 'lack a host-key directory' "$l" \
            || { echo "doctor lost its host-key check on an interactive host" >&2; exit 1; }
        done
        if grep -qF -- 'lack a host-key directory' "$batchLauncher"; then
          echo "the batch-only doctor still checks for host-key directories" >&2; exit 1
        fi
        grep -qF -- 'no per-slot SSH host keys are expected' "$batchLauncher" \
          || { echo "the batch-only doctor does not report the missing capability" >&2; exit 1; }

        # --- the generated verifiers follow the narrowed layout table -------
        grep -qF -- "$controllerVerifyArg" "$batchVerifier" \
          || { echo "the batch verifier does not verify the controller directory" >&2; exit 1; }
        if grep -qF -- "$controllerVerifyArg" "$interactiveVerifier"; then
          echo "the interactive-only verifier still verifies a batch directory" >&2; exit 1
        fi
        grep -qF -- "$hostkeysVerifyArg" "$interactiveVerifier" \
          || { echo "the interactive verifier does not verify the host-key directory" >&2; exit 1; }
        if grep -qF -- "$hostkeysVerifyArg" "$batchVerifier"; then
          echo "the batch-only verifier still verifies a host-key directory" >&2; exit 1
        fi

        {
          echo "microvm-capabilities:"
          echo "  default  guest : $defaultGuest"
          echo "  default  runner: $defaultRunner"
          echo "  interactive-only guest : $interactiveGuest"
          echo "  interactive-only runner: $interactiveRunner"
          echo "  batch-only       guest : $batchGuest"
          echo "  batch-only       runner: $batchRunner"
          echo "  batch+vsock      guest : $batchVsockGuest"
          echo "  batch+vsock      runner: $batchVsockRunner"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # phase 6, the LITERAL objective: the VSOCK MODEL TRANSPORT.               #
  #   `vsock` + `networkProfile = "proxy-only"` REPLACES the guest network:   #
  #   the guest has NO interface (loopback only), the model API travels        #
  #   guest 127.0.0.1:<litellmPort> -> AF_VSOCK -> a PER-VM host forwarder ->  #
  #   127.0.0.1:<litellmPort>, and the host builds no bridge, no TAP, no       #
  #   AGENT_MICROVM_* chain and no bridge-only socket. Everything below is     #
  #   asserted from the EVALUATED config (or from the BUILT artefacts), and    #
  #   every removal has a POSITIVE control on the `tap` reference host, so no  #
  #   check can pass because the attribute it inspects does not exist.         #
  # ---------------------------------------------------------------------- #
  microvm-vsock-transport =
    let
      vsockPort = port; # the guest-visible AND the AF_VSOCK port (one number)

      # The `vsock`-transport hosts under test.
      bvHost = batchVsockHost;
      bvGuest = capGuestOf bvHost;
      bvNet = bvHost._module.args.agentNetwork;
      ivHost = interactiveVsockHost;
      ivGuest = capGuestOf ivHost;

      # ... and the `tap` reference (the enabled f13 host), for the positive
      # controls.
      tapNet = self.nixosConfigurations.test-f13._module.args.agentNetwork;

      forwarderUnit = slotName: "agent-litellm-vsock-${slotName}";
      forwarderPath = slotName: "${microvmOpts.stateRoot}/${slotName}/notify.vsock_${vsockPort}";
      vsockSocketsOf =
        h:
        lib.filter (n: lib.hasPrefix "agent-litellm-vsock-" n) (
          builtins.attrNames h.config.systemd.sockets
        );

      bvSocket = bvHost.config.systemd.sockets.${forwarderUnit variantSlot.name};
      bvService = bvHost.config.systemd.services.${forwarderUnit variantSlot.name};

      # The guest-side bridge: ONE socket (the historical loopback endpoint) and
      # a per-connection socat instance to AF_VSOCK CID 2.
      bvGuestSocket = bvGuest.systemd.sockets.litellm-forwarder.socketConfig;
      bvGuestBridge = bvGuest.systemd.services.litellm-forwarder;
      bvGuestBridgeExec = toString bvGuestBridge.serviceConfig.ExecStart;

      evalMarker = mkEvalCheck "microvm-vsock-transport-eval" [
        # --- the transport is RESOLVED, once, from profile + capability -----
        {
          assertion = bvNet.transport == "vsock" && bvNet.transportCaps.vsockLitellm;
          message = "a `vsock` + proxy-only host must resolve the `vsock` model transport, got ${bvNet.transport}";
        }
        {
          assertion = tapNet.transport == "tap" && tapNet.transportCaps.guestInterface;
          message = "positive control: the reference host (no `vsock` capability) must keep the `tap` transport, got ${tapNet.transport}";
        }
        {
          # The narrow condition: `vsock` alone is NOT enough, the profile must
          # be the closed `proxy-only` one. `offline` has no model API to carry.
          assertion =
            (capabilityHostWith [ "batch" "vsock" ] {
              sshPublicKeyFile = ../hosts/host.f13/dedicated-agent-vm-key.pub;
              networkProfile = lib.mkForce "offline";
            })._module.args.agentNetwork.transport == "tap";
          message = "the `vsock` model transport must be scoped to `proxy-only` (an `offline` host has no model API to carry over it)";
        }

        # --- THE GUEST HAS NO NETWORK INTERFACE ----------------------------
        {
          assertion = bvGuest.microvm.interfaces == [ ];
          message = "a vsock+proxy-only guest must declare NO microvm.interfaces, got ${
            toString (map (i: i.id) bvGuest.microvm.interfaces)
          }";
        }
        {
          assertion = ivGuest.microvm.interfaces == [ ];
          message = "an INTERACTIVE vsock+proxy-only guest must declare NO microvm.interfaces either (the plan's definition-of-done shape)";
        }
        {
          assertion =
            lib.length guest0Cfg.microvm.interfaces == 1
            && (lib.head guest0Cfg.microvm.interfaces).type == "tap";
          message = "positive control: the reference (tap) guest must still declare exactly one TAP interface";
        }
        {
          assertion = !bvGuest.systemd.network.enable && !bvGuest.networking.useNetworkd;
          message = "a vsock+proxy-only guest must run NO systemd-networkd (microvm.nix defaults it on, so it has to be turned off explicitly)";
        }
        {
          assertion = bvGuest.systemd.network.networks == { };
          message = "a vsock+proxy-only guest must declare no networkd .network unit (no static IP, no default route), got ${toString (builtins.attrNames bvGuest.systemd.network.networks)}";
        }
        {
          assertion = !bvGuest.networking.dhcpcd.enable && !bvGuest.networking.useDHCP;
          message = "a vsock+proxy-only guest must run no dhcpcd either (turning networkd off must not fall back to the scripted DHCP client)";
        }
        {
          assertion =
            bvGuest.networking.nameservers == [ ] && bvGuest.networking.firewall.allowedTCPPorts == [ ];
          message = "a vsock+proxy-only guest must have no resolver and no open TCP port";
        }
        {
          # Several guest units still `want` network-online.target
          # unconditionally. That is only harmless because NOTHING in this guest
          # PROVIDES it: with networkd and dhcpcd off there is no
          # `*-wait-online` unit, so the target activates trivially instead of
          # stalling the boot for the default 120 s. Pinned, because the day a
          # unit starts providing it the guest would hang on every boot.
          assertion =
            !(lib.any (n: lib.hasSuffix "-wait-online" n) (builtins.attrNames bvGuest.systemd.services));
          message = "a vsock+proxy-only guest must contain NO *-wait-online unit (the units that want network-online.target would otherwise stall its boot for 120 s), got ${
            toString (
              lib.filter (n: lib.hasSuffix "-wait-online" n) (builtins.attrNames bvGuest.systemd.services)
            )
          }";
        }
        {
          assertion = lib.any (n: lib.hasSuffix "-wait-online" n) (
            builtins.attrNames guest0Cfg.systemd.services
          );
          message = "positive control: the reference (tap) guest DOES have a *-wait-online unit, which is what makes the assertion above meaningful";
        }
        {
          assertion =
            guest0Cfg.systemd.network.enable
            && guest0Cfg.systemd.network.networks ? "10-agent"
            && (lib.head guest0Cfg.systemd.network.networks."10-agent".address) == "${refSlot.ip}/24";
          message = "positive control: the reference (tap) guest must still get its static IPv4 through networkd";
        }
        {
          # The VSOCK device IS there (it is the model path AND the control
          # channel), keyed to the slot's deterministic CID.
          assertion =
            bvGuest.microvm.vsock.cid == variantSlot.cid && ivGuest.microvm.vsock.cid == variantSlot.cid;
          message = "a vsock guest must keep its deterministic VSOCK CID (${toString variantSlot.cid})";
        }

        # --- THE GUEST ENDPOINT IS UNCHANGED (no agent reconfiguration) -----
        {
          assertion =
            bvGuest.environment.variables.OPENAI_BASE_URL == "http://127.0.0.1:${vsockPort}/v1"
            && bvGuest.environment.variables.ANTHROPIC_BASE_URL == "http://127.0.0.1:${vsockPort}"
            && bvGuest.environment.variables.OPENAI_BASE_URL == guest0Cfg.environment.variables.OPENAI_BASE_URL;
          message = "a vsock guest's model endpoint must stay the loopback address the host-provisioned agent configuration already uses, so nothing has to be reconfigured";
        }
        {
          # ONE shape under both transports: `Accept = no`, so ONE long-running
          # forwarder multiplexes every connection off the listening fd. A
          # per-connection (`Accept = yes`) template would have capped concurrent
          # model streams at systemd's default MaxConnections=64.
          assertion = bvGuestSocket.ListenStream == "127.0.0.1:${vsockPort}" && !bvGuestSocket.Accept;
          message = "the guest bridge must listen on the historical loopback endpoint with Accept=no (one long-running multiplexer, no per-connection cap), got ${toString bvGuestSocket.ListenStream}";
        }
        {
          # ... which is only possible because socat takes the LISTENING fd
          # systemd passes as fd 3 and forks per connection itself.
          assertion = lib.hasInfix "ACCEPT-FD:3,fork" bvGuestBridgeExec;
          message = "the guest bridge must accept off the socket-activated listening fd and fork per connection, got ${bvGuestBridgeExec}";
        }
        {
          # NO INACTIVITY TIMEOUT. socat's `-T` is BIDIRECTIONAL: it tears a
          # connection down mid-transfer after N idle seconds, which for a model
          # API silently kills a long prefill, a cold LiteLLM or a slow tool-call
          # turn. The tap path (systemd-socket-proxyd) has no such timeout, and
          # the whole point of this unit is that the agent behaves identically on
          # both transports. (`-t`, the half-close DRAIN timeout, is fine and is
          # deliberately raised past socat's 0.5 s default.)
          assertion = !(lib.hasInfix " -T" bvGuestBridgeExec) && lib.hasInfix " -t 120 " bvGuestBridgeExec;
          message = "the guest bridge must carry NO socat -T inactivity timeout (it would kill in-flight model requests) and a generous -t half-close drain, got ${bvGuestBridgeExec}";
        }
        {
          # No interface means no `*-wait-online` unit exists, so ordering
          # against `network-online.target` would depend on a target that only
          # ever activates trivially today and would BLOCK the guest's boot for
          # 120 s the day anything provides it.
          assertion =
            !(lib.elem "network-online.target" bvGuestBridge.wants)
            && !(lib.elem "network-online.target" bvGuestBridge.after);
          message = "a vsock guest's forwarder must not want/order after network-online.target (there is no network stack to wait for)";
        }
        {
          assertion =
            lib.elem "network-online.target" guest0Cfg.systemd.services.litellm-forwarder.wants
            && lib.elem "network-online.target" guest0Cfg.systemd.services.litellm-forwarder.after;
          message = "positive control: the reference (tap) forwarder dials a bridge address and must still wait for network-online.target";
        }
        {
          # DESTINATION-FIXED in the unit: CID 2 (the host) and the model port,
          # baked in by Nix. The guest cannot ask for another CID or port, and
          # there is no CONNECT protocol to abuse.
          assertion = lib.hasInfix "VSOCK-CONNECT:2:${vsockPort}" bvGuestBridgeExec;
          message = "the guest bridge must dial AF_VSOCK CID 2 port ${vsockPort} with a FIXED destination, got ${bvGuestBridgeExec}";
        }
        {
          # ONE unit name, ONE forwarder: the socket-proxyd-to-the-gateway
          # ExecStart is GONE (it would dial a gateway address that no longer
          # exists) and there is no per-connection template either.
          assertion =
            !(lib.hasInfix "systemd-socket-proxyd" bvGuestBridgeExec)
            && !(lib.hasInfix gateway bvGuestBridgeExec)
            && !(bvGuest.systemd.services ? "litellm-forwarder@");
          message = "a vsock guest's forwarder must not be the socket-proxyd-to-the-bridge one, and no per-connection template may exist, got ${bvGuestBridgeExec}";
        }
        {
          assertion =
            guest0Cfg.systemd.services ? "litellm-forwarder"
            && !(guest0Cfg.systemd.services ? "litellm-forwarder@")
            && !(guest0Cfg.systemd.sockets.litellm-forwarder.socketConfig.Accept)
            &&
              toString guest0Cfg.systemd.services.litellm-forwarder.serviceConfig.ExecStart
              == "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd ${gateway}:${port}";
          message = "positive control: the reference (tap) guest must keep exactly the socket-proxyd forwarder to the bridge gateway";
        }

        # --- THE HOST HAS NO BRIDGE, NO TAP AND NO FIREWALL CHAIN ----------
        {
          assertion = bvHost.config.networking.bridges == { };
          message = "a vsock+proxy-only host must create NO bridge, got ${toString (builtins.attrNames bvHost.config.networking.bridges)}";
        }
        {
          assertion = !(bvHost.config.networking.interfaces ? ${microvmOpts.bridgeName});
          message = "a vsock+proxy-only host must not address the agent bridge";
        }
        {
          assertion = !(lib.hasInfix "AGENT_MICROVM_INPUT" bvHost.config.networking.firewall.extraCommands);
          message = "a vsock+proxy-only host must install NO AGENT_MICROVM_* firewall chain (there is no interface to filter)";
        }
        {
          assertion = !(lib.hasInfix "AGENT_MICROVM" bvHost.config.networking.firewall.extraStopCommands);
          message = "a vsock+proxy-only host must have no AGENT_MICROVM_* teardown either";
        }
        {
          assertion = lib.hasInfix "AGENT_MICROVM_INPUT -s ${microvmOpts.subnet} -d ${gateway} -p tcp --dport ${port} -j ACCEPT" enabledCfg.networking.firewall.extraCommands;
          message = "positive control: the reference (tap) host must still ACCEPT the bridge LiteLLM endpoint in its own chain";
        }
        {
          assertion =
            !(bvHost.config.systemd.sockets ? "agent-litellm-proxy")
            && !(bvHost.config.systemd.services ? "agent-litellm-proxy");
          message = "a vsock+proxy-only host must not create the bridge-only LiteLLM socket/service";
        }
        {
          assertion =
            !(lib.any (n: lib.hasPrefix "agent-microvm-attach-" n) (
              builtins.attrNames bvHost.config.systemd.services
            ));
          message = "a vsock+proxy-only host must not create a TAP attach/L2-isolate unit (there is no TAP)";
        }
        {
          assertion =
            !(lib.any (
              u: lib.hasInfix microvmOpts.bridgeName u
            ) bvHost.config.networking.networkmanager.unmanaged);
          message = "a vsock+proxy-only host must not declare bridge/TAP interfaces unmanaged in NetworkManager (they do not exist)";
        }
        {
          assertion =
            enabledCfg.systemd.sockets ? "agent-litellm-proxy"
            && lib.any (n: lib.hasPrefix "agent-microvm-attach-" n) (
              builtins.attrNames enabledCfg.systemd.services
            )
            && lib.any (
              u: lib.hasInfix microvmOpts.bridgeName u
            ) enabledCfg.networking.networkmanager.unmanaged;
          message = "positive control: the reference (tap) host must keep the bridge socket, the TAP attach units and the NetworkManager exclusions";
        }

        # --- ONE HOST FORWARDER PER VM, DESTINATION-FIXED ------------------
        {
          assertion = vsockSocketsOf bvHost == [ (forwarderUnit variantSlot.name) ];
          message = "a vsock+proxy-only host must declare exactly one AF_VSOCK forwarder socket per slot, got ${toString (vsockSocketsOf bvHost)}";
        }
        {
          assertion = bvSocket.socketConfig.ListenStream == forwarderPath variantSlot.name;
          message = "the per-VM forwarder must listen on cloud-hypervisor's per-VM VSOCK socket ${forwarderPath variantSlot.name}, got ${toString bvSocket.socketConfig.ListenStream}";
        }
        {
          # Reachable by the VMM (`microvm:kvm`) and root only \u2014 never by an
          # unprivileged host user, and never over the network.
          assertion =
            bvSocket.socketConfig.SocketUser == "root"
            && bvSocket.socketConfig.SocketGroup == "kvm"
            && bvSocket.socketConfig.SocketMode == "0660";
          message = "the per-VM forwarder socket must be root:kvm 0660 — the VMM's group, so only root and the VMM (plus any host user already in `kvm`) can connect";
        }
        {
          assertion =
            toString bvService.serviceConfig.ExecStart
            == "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${port}";
          message = "the per-VM forwarder must be DESTINATION-FIXED to the loopback LiteLLM proxy, got ${toString bvService.serviceConfig.ExecStart}";
        }
        {
          # HOST-TCP-ONLY, enforced on the forwarder process itself.
          assertion =
            bvService.serviceConfig.IPAddressAllow == "localhost"
            && bvService.serviceConfig.IPAddressDeny == "any"
            && bvService.serviceConfig.DynamicUser
            && bvService.serviceConfig.ProtectSystem == "strict";
          message = "the per-VM forwarder must be confined to the host loopback (IPAddressAllow=localhost + IPAddressDeny=any) and unprivileged";
        }
        {
          # ONE LISTENER PER SLOT, on DISTINCT paths: slot A's guest cannot
          # address slot B's forwarder at all.
          assertion =
            let
              names = map (s: forwarderUnit s.name) twoSlotVsockSlots;
              paths = map (
                s: twoSlotVsockHost.config.systemd.sockets.${forwarderUnit s.name}.socketConfig.ListenStream
              ) twoSlotVsockSlots;
            in
            lib.length twoSlotVsockSlots == 2
            && lib.sort (a: b: a < b) (vsockSocketsOf twoSlotVsockHost) == lib.sort (a: b: a < b) names
            && lib.length (lib.unique paths) == 2;
          message = "a two-slot vsock host must declare one forwarder per slot, each on its OWN socket path";
        }
        {
          assertion = vsockSocketsOf self.nixosConfigurations.test-f13 == [ ];
          message = "positive control: the reference (tap) host must declare no AF_VSOCK forwarder at all";
        }

        # --- THE CONTROL CHANNEL FOLLOWS THE TRANSPORT ---------------------
        {
          # An interactive guest under the vsock transport has `enableSsh = true`
          # but NO interface, so the TCP sshd could never be reached: it is
          # masked and the VSOCK `sshd-vsock@` is the ONE control channel.
          assertion =
            ivHost.config.myconfig.ai.microvm.enableSsh
            && !ivHost._module.args.agentNetwork.tapSshUsable
            && !(ivGuest.systemd.services.sshd.enable or true)
            && ivGuest.systemd.sockets ? "sshd-vsock"
            && !(ivGuest.services.openssh.openFirewall or true);
          message = "an interactive vsock+proxy-only guest must mask its unreachable TCP sshd, keep the VSOCK sshd and close the firewall for 22";
        }
        {
          assertion =
            ivGuest.services.openssh.hostKeys == [
              {
                type = "ed25519";
                path = (capSessionOf ivHost).guestHostkeysDir + "/ssh_host_ed25519_key";
              }
            ]
            &&
              (ivGuest.systemd.sockets.sshd-vsock.unitConfig.RequiresMountsFor or null) == (capSessionOf ivHost)
              .guestHostkeysDir;
          message = "an interactive vsock guest's VSOCK sshd must use the per-slot host key and wait for its read-only mount";
        }
        {
          # ... and the interactive half is otherwise intact.
          assertion = lib.elem "agent-run" (capGuestPkgNames ivHost);
          message = "an interactive vsock guest must still carry the `agent-run` entry point";
        }
        {
          assertion =
            enabledCfg.myconfig.ai.microvm.enableSsh
            && tapNet.tapSshUsable
            && (guest0Cfg.systemd.services.sshd.enable or true)
            && guest0Cfg.services.openssh.openFirewall;
          message = "positive control: the reference (tap) guest must keep its TCP sshd and its firewall opening";
        }

        # --- NEGATIVE eval tests, pinned to the SPECIFIC assertion ---------
        {
          assertion = baselineClean;
          message = "positive control: the unmodified reference host must have no failed assertion";
        }
        {
          # microvm.nix reserves AF_VSOCK port 8888 (`notify.vsock_8888`) for the
          # guest's systemd notify socket, so the model forwarder must not claim
          # it \u2014 the two would fight over the same Unix socket path.
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              myconfig.ai.microvm.litellmPort = lib.mkForce 8888;
              services.litellm.port = lib.mkForce 8888;
            }
          ] "must not be 8888";
          message = "the VSOCK model transport must reject litellmPort = 8888 (microvm.nix's reserved notify port)";
        }
        {
          # The same litellm-backend guard as on a TAP host, pinned in the vsock
          # SHAPE: the per-VM forwarder's only destination is the loopback proxy,
          # so a host without it builds a model path that ends nowhere.
          assertion = rejectsWith [
            {
              myconfig.ai.microvm.capabilities = lib.mkForce [
                "batch"
                "vsock"
              ];
              myconfig.ai.microvm.enableSsh = lib.mkForce false;
              services.litellm.enable = lib.mkForce false;
            }
          ] "lets guests reach the model API";
          message = "a vsock host without the loopback LiteLLM backend must be rejected (the per-VM forwarder would forward into nothing)";
        }
      ];
    in
    pkgs.runCommand "microvm-vsock-transport"
      {
        inherit evalMarker;
        # BUILT: the INTERACTIVE vsock guest is a shape no other check builds
        # (masked TCP sshd + zero interfaces + the socat bridge unit), and the
        # runner is where "no TAP" becomes observable as a missing script.
        interactiveVsockGuest = ivGuest.system.build.toplevel;
        interactiveVsockRunner = ivGuest.microvm.declaredRunner;
        batchVsockGuest = bvGuest.system.build.toplevel;
        vsockLauncher = "${findPkg ivHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
        tapLauncher = "${launcherPkg}/bin/agent-microvm";
        # The GENERATED host-key provisioner, taken from the unit that runs it
        # (it is not a systemPackages entry), so this greps exactly the script
        # the host would execute.
        vsockHostKeys = toString ivHost.config.systemd.services.agent-microvm-hostkeys.serviceConfig.ExecStart;
        tapHostKeys = toString enabledCfg.systemd.services.agent-microvm-hostkeys.serviceConfig.ExecStart;
        slotIp = variantSlot.ip;
        refIp = refSlot.ip;
      }
      ''
        # --- the RUNNER of a vsock guest has no TAP plumbing ---------------
        # microvm.nix generates `bin/tap-up` / `bin/tap-down` only for a guest
        # that declares a `type = "tap"` interface, so their ABSENCE is the
        # runner-level proof that no TAP device is ever created for this slot.
        for f in tap-up tap-down; do
          if [ -e "$interactiveVsockRunner/bin/$f" ]; then
            echo "the vsock guest's runner still ships bin/$f (a TAP device would be created)" >&2; exit 1
          fi
        done
        grep -q -- '--vsock' "$interactiveVsockRunner/bin/microvm-run" \
          || { echo "the vsock guest's runner does not pass --vsock to cloud-hypervisor" >&2; exit 1; }

        # --- the LAUNCHER follows the transport ----------------------------
        grep -qF -- "readonly NETWORK_TRANSPORT=vsock" "$vsockLauncher" \
          || { echo "the vsock launcher does not render NETWORK_TRANSPORT=vsock" >&2; exit 1; }
        grep -qF -- "readonly GUEST_INTERFACE=0" "$vsockLauncher" \
          || { echo "the vsock launcher does not record that the guest has no interface" >&2; exit 1; }
        # `enableSsh = true` on that host, but there is no interface to reach the
        # TCP sshd on, so the launcher must take the VSOCK control channel.
        grep -qF -- "readonly SSH_ENABLED=0" "$vsockLauncher" \
          || { echo "the vsock launcher still claims a usable TCP ssh channel" >&2; exit 1; }
        grep -qF -- "readonly VSOCK_ENABLED=1" "$vsockLauncher" \
          || { echo "the vsock launcher does not render VSOCK_ENABLED=1" >&2; exit 1; }
        grep -qF -- 'network-transport: %s' "$vsockLauncher" \
          || { echo "the launcher does not report the transport machine-readably" >&2; exit 1; }
        # `doctor` checks the components that EXIST and says so about the ones
        # that deliberately do not.
        grep -qF -- 'agent-litellm-vsock-$vs.socket is active' "$vsockLauncher" \
          || { echo "the vsock launcher's doctor does not check the per-VM forwarders" >&2; exit 1; }
        for needle in 'AGENT_MICROVM_INPUT chain is installed' 'bridge interface $BRIDGE exists' \
                      'agent-litellm-proxy.socket is active'; do
          if grep -qF -- "$needle" "$vsockLauncher"; then
            echo "the vsock launcher's doctor still checks '$needle', which this host does not build" >&2; exit 1
          fi
        done
        # POSITIVE control: the tap launcher keeps exactly those three checks and
        # has no per-VM forwarder section.
        grep -qF -- "readonly NETWORK_TRANSPORT=tap" "$tapLauncher" \
          || { echo "the tap launcher does not render NETWORK_TRANSPORT=tap" >&2; exit 1; }
        grep -qF -- "readonly SSH_ENABLED=1" "$tapLauncher" \
          || { echo "the tap launcher lost its TCP ssh channel" >&2; exit 1; }
        for needle in 'AGENT_MICROVM_INPUT chain is installed' 'bridge interface $BRIDGE exists' \
                      'agent-litellm-proxy.socket is active'; do
          grep -qF -- "$needle" "$tapLauncher" \
            || { echo "the tap launcher's doctor lost '$needle'" >&2; exit 1; }
        done
        # (the `NETWORK_TRANSPORT` comment block MENTIONS the vsock unit name on
        # every host, so this must pin the doctor CHECK, not the mere name.)
        if grep -qF -- 'agent-litellm-vsock-$vs.socket is active' "$tapLauncher"; then
          echo "the tap launcher's doctor checks a per-VM VSOCK forwarder it does not have" >&2; exit 1
        fi

        # --- known_hosts follows the transport ----------------------------
        # A vsock guest has no IPv4 to pin a key under; the vsock-mux address is
        # the WHOLE database there. Keying it to an address nothing listens on
        # would be misleading dead data.
        grep -qF -- 'vsock-mux/' "$vsockHostKeys" \
          || { echo "the vsock host-key generator writes no vsock-mux known_hosts entry" >&2; exit 1; }
        if grep -qF -- "$slotIp" "$vsockHostKeys"; then
          echo "the vsock host-key generator still pins the key to a guest IPv4 that does not exist" >&2; exit 1
        fi
        grep -qF -- "$refIp" "$tapHostKeys" \
          || { echo "positive control: the tap host-key generator must pin the key to the slot IPv4" >&2; exit 1; }

        {
          echo "microvm-vsock-transport:"
          echo "  interactive vsock guest : $interactiveVsockGuest"
          echo "  interactive vsock runner: $interactiveVsockRunner"
          echo "  batch+vsock guest       : $batchVsockGuest"
          cat "$evalMarker"
        } > "$out"
      '';

  # ---------------------------------------------------------------------- #
  # The launcher is RENDERED from Nix fragments spliced into one indented    #
  # string (../modules/myconfig.ai/myconfig.ai.microvm/launcher.nix          #
  # `mkFragment`/`indentFragment`). The indent argument is the column in the  #
  # GENERATED script, which is NOT the column the `${...}` has in the Nix     #
  # file (Nix strips the string's common indentation first), so passing the   #
  # wrong one silently over-indents every line after the first and leaves     #
  # whitespace-only lines behind. That is invisible to shellcheck and to      #
  # every functional check, so it is pinned HERE, on the BUILT script of      #
  # BOTH transports: no line may end in whitespace, and the blocks that come  #
  # from fragments must sit at the same column as their neighbours.           #
  # ---------------------------------------------------------------------- #
  microvm-launcher-rendering =
    pkgs.runCommand "microvm-launcher-rendering"
      {
        tapLauncher = "${launcherPkg}/bin/agent-microvm";
        vsockLauncher = "${findPkg interactiveVsockHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
        batchLauncher = "${findPkg batchOnlyHost.config.environment.systemPackages "agent-microvm"}/bin/agent-microvm";
      }
      ''
        for l in "$tapLauncher" "$vsockLauncher" "$batchLauncher"; do
          if grep -nP '[ \t]+$' "$l"; then
            echo "$l: the generated launcher has whitespace-only/trailing-whitespace lines" >&2
            echo "(a fragment was spliced with the wrong indent: it must be the column in the" >&2
            echo " GENERATED script, i.e. topIndent/bodyIndent, not the column in launcher.nix)" >&2
            exit 1
          fi
        done

        # A top-level `readonly` from a fragment must sit at column 0 exactly
        # like the hand-written ones around it.
        grep -qx -- 'readonly BRIDGE=agentbr0' "$tapLauncher" \
          || { echo "the tap launcher's BRIDGE constant is not a column-0 statement" >&2; exit 1; }
        grep -qx -- "readonly SELECTED_CAPABILITIES='interactive batch'" "$tapLauncher" \
          || { echo "the tap launcher's capability set is not a column-0 statement" >&2; exit 1; }
        # ... and a fragment inside a function body at column 4.
        grep -qx -- '    section_hdr "private bridge + gateway address"' "$tapLauncher" \
          || { echo "the tap launcher's doctor section is not a column-4 statement" >&2; exit 1; }
        grep -qx -- '    section_hdr "per-VM AF_VSOCK model forwarder (the vsock transport)"' "$vsockLauncher" \
          || { echo "the vsock launcher's doctor section is not a column-4 statement" >&2; exit 1; }
        grep -qx -- '    require_capability run interactive' "$batchLauncher" \
          || { echo "the batch-only launcher's capability guard is not a column-4 statement" >&2; exit 1; }

        {
          echo "microvm-launcher-rendering: no trailing whitespace, fragments at the right column"
          for l in "$tapLauncher" "$vsockLauncher" "$batchLauncher"; do echo "  $l"; done
        } > "$out"
      '';

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
  #     and the pool-bound / class-name / enableSsh-key /                   #
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
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.enableSsh = lib.mkForce true;
          myconfig.ai.microvm.sshPublicKeyFile = lib.mkForce null;
        }
      ] "an SSH control channel";
      message = "enableSsh without sshPublicKeyFile must be rejected";
    }
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
        # 6: standalone clones only. The fast path copies the object store
        # (`--local --no-hardlinks`) and the fallback uses the ordinary git
        # transport (`--no-local`); NEITHER may borrow objects, so `--shared` /
        # `--reference` must not appear at all and the absence of an
        # `objects/info/alternates` file is verified at runtime.
        grep -q "clone --local --no-hardlinks" "$launcherBin" \
          || { echo "clones are not created with --local --no-hardlinks" >&2; exit 1; }
        grep -q "clone --no-local" "$launcherBin" \
          || { echo "missing the --no-local clone fallback" >&2; exit 1; }
        grep -q "objects/info/alternates" "$launcherBin" \
          || { echo "clones are not verified to be free of object alternates" >&2; exit 1; }
        # Match only actual `git … clone …` COMMANDS (no '#' before the `git`),
        # so the explanatory comments naming these flags do not trip the check.
        if grep -nE '^[^#]*git[^#]*clone[^#]*(--shared|--reference)' "$launcherBin"; then
          echo "clones must never borrow objects (--shared / --reference)" >&2
          exit 1
        fi
        # Readiness must not burn a fixed multi-second sleep once the guest is
        # already reachable (lightweight plan phase 7).
        grep -q "READY_POLL_MIN_MS" "$launcherBin" \
          || { echo "guest-readiness polling is not exponential-backoff bounded" >&2; exit 1; }
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
        # pre-created, otherwise virtiofsd refuses to start for that slot. They
        # are part of the ONE writable session share, so ./session.nix creates
        # them from its layout table — which renders the owner NUMERICALLY (it
        # derives the ids from `guestAgentUid`/`guestAgentGid`); tmpfiles accepts
        # either spelling, so `hasDirRule` does too.
        ++ lib.concatMap (
          slot:
          let
            hasDirRule =
              path: mode: user: group:
              builtins.any (r: r == "d ${path} ${mode} ${user} ${group} - -") tmpfiles;
            hasRootDirRule = path: mode: hasDirRule path mode "root" "root" || hasDirRule path mode "0" "0";
          in
          [
            {
              assertion = hasRootDirRule (jobs.slotDir slot.name) "0755";
              message = "missing tmpfiles rule for the job dir of ${slot.name}";
            }
            {
              assertion = hasRootDirRule (jobs.hostInputDir slot.name) jobs.inputDirMode;
              message = "missing tmpfiles rule for the immutable input dir of ${slot.name}";
            }
            {
              assertion = hasRootDirRule (jobs.hostControllerDir slot.name) jobs.controllerDirMode;
              message = "missing tmpfiles rule for the controller-only dir of ${slot.name}";
            }
            {
              assertion = hasDirRule (jobs.hostWorkerDir slot.name) jobs.workerDirMode (toString jobs.workerUid) (
                toString jobs.workerGid
              );
              message = "missing tmpfiles rule for the worker-writable dir of ${slot.name}";
            }
          ]
        ) enabledSlots
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
          # (The rule is rendered by ./session.nix from its layout table, which
          # spells the owner numerically; tmpfiles accepts either form.)
          assertion =
            jobs.controllerDirMode == "0700"
            && builtins.elem "d ${jobs.hostControllerDir refSlot.name} 0700 0 0 - -" tmpfiles;
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
          assertion = builtins.elem "d ${jobs.hostWorkerLogsDir refSlot.name} ${jobs.workerLogsDirMode} 0 0 - -" tmpfiles;
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
          # The v1 guest-writable `out/` result channel never existed inside the
          # session tree (it predates it), and the tree is REMOVED and recreated
          # around every launch, so there is nothing to clean up any more. What
          # must stay true is that no guest-writable directory of the current
          # layout is a result path.
          assertion =
            !(builtins.elem "out" (builtins.attrValues session.subdirs))
            && !builtins.any (d: lib.hasPrefix "${d}/" (jobs.hostResult refSlot.name)) workerWritable;
          message = "the authoritative result must not live in a guest-writable directory of the session tree";
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
        # The guest-side mount point of the job data (the ONE writable session
        # share), from job.nix -> session.nix rather than a literal in the
        # harness.
        GUEST_JOB_DIR = jobs.guestMountPoint;
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
        # The per-slot job data root and the workspace subdirectory of the ONE
        # writable session share, from the module rather than a literal in the
        # harness.
        JOBS_ROOT = jobs.root;
        WORKSPACE_SUBDIR = session.subdirs.workspace;
        # The host home the config stager resolves: the harness creates it
        # (empty) inside the sandbox so the launch-time staging step succeeds.
        HOST_HOME = microvmOpts.configSeed.hostHome;
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
        # The layout the launcher actually uses, from the module (session.nix /
        # state.nix / hostkeys.nix) rather than from literals in the harness.
        SESSION_ROOT = session.root;
        SESSION_RO_ROOT = session.roRoot;
        WORKSPACE_SUBDIR = session.subdirs.workspace;
        STATE_SUBDIR = session.subdirs.state;
        HOSTKEYS_SUBDIR = session.roSubdirs.hostkeys;
        # The PRE-consolidation per-slot state root: nothing creates anything
        # there any more, but `recover` still scans it so a host migrated from
        # the four-share layout gets its residue reported.
        STATE_SLOTS_ROOT = agentStatePaths.slotsRoot;
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
  #      and the agent's login shell?) can be executed here, against a          #
  #      stub that reproduces exactly that path. Includes a NEGATIVE CONTROL:   #
  #      the previous, unquoted transport must FAIL this check.                 #
  # ---------------------------------------------------------------------- #
  microvm-rtv-transport =
    pkgs.runCommand "microvm-rtv-transport"
      {
        nativeBuildInputs = [
          pkgs.coreutils
          pkgs.bashInteractive
        ];
        harness = ./microvm-rtv-transport.sh;
        # The suite under test, and the very shell guest.nix gives the agent user
        # — the re-parsing side of the transport, so the stub is not a guess
        # about which shell runs. Read off the evaluated guest, so it follows a
        # change of the guest login shell instead of pinning one.
        SUITE = ../modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh;
        GUEST_SHELL = "${guest0Cfg.users.users.agent.shell}/bin/bash";
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
        # The per-slot host-key root, from hostkeys.nix (the read-only session
        # tree), so the harness does not carry a second copy of the layout.
        HOSTKEYS_ROOT = hostKeys.root;
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
