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
#   microvm-eval-workspace-share §10/§11 — the agent-0 guest declares EXACTLY
#                                        one virtiofs share: the writable
#                                        /workspace whose source matches the
#                                        launcher bind-mount target (crit. 12)
#   microvm-guest-evaluates   §38     — the agent-0 guest closure evaluates to a
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
  gateway = microvmOpts.gatewayAddress; # 192.168.83.1
  port = toString microvmOpts.litellmPort; # 4000
  slotCount = microvmOpts.slotCount; # 4

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
  guest0Cfg = enabledCfg.microvm.vms."agent-0".config.config;

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

  # Slot counts to exercise. Includes small pools, pools with index >= 10
  # (which exercise 2-hex-digit MAC formatting, e.g. i=10 → ...:1a), and the
  # generator's declared maximum.
  slotCountsUnderTest = [
    1
    2
    4
    8
    16
    slotLib.maxSlotCount
  ];

  ipv4Re = "([0-9]{1,3}\\.){3}[0-9]{1,3}";
  # Derive the expected MAC OUI/prefix from the REAL slots.nix generator
  # (slot 0's MAC) instead of hardcoding it, so this test can't silently
  # drift if slots.nix ever changes the OUI. The trailing `[0-9a-f]{2}`
  # bound is intentional: it would correctly fail if maxSlotCount were ever
  # raised past the single-byte MAC ceiling (255).
  macPrefix = builtins.substring 0 15 (slotLib.mkSlot 0).mac; # "02:00:00:83:00:"
  macRe = "${macPrefix}[0-9a-f]{2}";

  # Per-pool structural assertions for slot count `n`.
  slotPoolChecks =
    n:
    let
      pool = slotLib.mkSlots n;
      ips = map (s: s.ip) pool;
      macs = map (s: s.mac) pool;
      names = map (s: s.name) pool;
      expectedNames = builtins.genList (i: "agent-${toString i}") n;
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
        message = "slotCount=${toString n}: names not contiguous agent-0.. (${toString names})";
      }
      {
        assertion = indicesContiguous;
        message = "slotCount=${toString n}: slot .index values not contiguous 0..${toString (n - 1)}";
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
  # The unmodified host must have NO failed assertions — positive control so
  # the rejectsWith checks below can't pass vacuously.
  baselineClean = failedAssertions [ ] == [ ];
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
      expectedNames = builtins.genList (i: "agent-${toString i}") slotCount;
      socket = enabledCfg.systemd.sockets.agent-litellm-proxy.socketConfig;
      fw = enabledCfg.networking.firewall.extraCommands;
    in
    mkEvalCheck "microvm-eval-enabled" [
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
    ];

  # ---------------------------------------------------------------------- #
  # (c) PURE-EVAL slot pool: unique + well-formed IPs/MACs, contiguous      #
  #     names, across a range of slot counts. Encodes §37 duplicate         #
  #     detection as an executable test against the real slots.nix.         #
  # ---------------------------------------------------------------------- #
  microvm-slot-uniqueness = mkEvalCheck "microvm-slot-uniqueness" (
    lib.concatMap slotPoolChecks slotCountsUnderTest
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
      # slotCount=0 is rejected at the option-TYPE level (`positive integer`),
      # a strictly stronger guard than the slotCount>0 assertion.
      assertion = rejectsWith [
        { myconfig.ai.microvm.slotCount = lib.mkForce 0; }
      ] "slotCount must be > 0";
      message = "slotCount=0 must be rejected (type `positive integer` / slotCount>0 assertion)";
    }
    {
      assertion = rejectsWith [
        { myconfig.ai.microvm.slotCount = lib.mkForce (slotLib.maxSlotCount + 1); }
      ] "slotCount must be <=";
      message = "slotCount>maxSlotCount (${toString slotLib.maxSlotCount}) must be rejected";
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
      assertion = rejectsWith [
        {
          myconfig.ai.microvm.allowPublicInternet = lib.mkForce true;
          myconfig.ai.microvm.acknowledgeInsecureNetwork = lib.mkForce false;
        }
      ] "must remain false for the secure default";
      message = "allowPublicInternet without acknowledgeInsecureNetwork must be rejected";
    }
  ];

  # ---------------------------------------------------------------------- #
  # (g) WORKSPACE SHARE: prove the guest declares EXACTLY ONE virtiofs      #
  #     share — the writable `/workspace` — whose host `source` matches the  #
  #     launcher's bind-mount target `${stateRoot}/agent-0/workspace`.       #
  #     This locks down plan §10/§11 crit. 12 so the previously-missing      #
  #     guest share can never silently reappear as `[]`. It FAILS if the    #
  #     share is removed, renamed, made read-only, or repointed.            #
  # ---------------------------------------------------------------------- #
  microvm-eval-workspace-share =
    let
      shares = guest0Cfg.microvm.shares;
      virtiofsShares = builtins.filter (s: s.proto == "virtiofs") shares;
      wsShares = builtins.filter (s: s.mountPoint == "/workspace") shares;
      ws = if wsShares == [ ] then null else builtins.head wsShares;
      expectedSource = "${microvmOpts.stateRoot}/agent-0/workspace";
    in
    mkEvalCheck "microvm-eval-workspace-share" [
      {
        # There must be exactly one share, and it must be the workspace.
        assertion = builtins.length shares == 1;
        message = "guest agent-0 must declare exactly ONE share (the workspace); got ${toString (builtins.length shares)}: ${
          toString (map (s: s.mountPoint or "?") shares)
        }";
      }
      {
        assertion = ws != null;
        message = "guest agent-0 has no share mounted at /workspace (mountPoints: ${
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
        # Defence in depth: the ONLY virtiofs share is the workspace — no
        # /nix, /home or host-socket share leaked in.
        assertion = builtins.length virtiofsShares == 1;
        message = "expected exactly one virtiofs share (workspace); got ${toString (builtins.length virtiofsShares)}";
      }
    ];

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
          echo "guest agent-0 toplevel drv: $toplevelDrv"
          echo "guest agent-0 CH runner drv: $runnerDrv"
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
      # The workmux agent `command`s are `lib.getExe <launcher>` strings whose
      # string context references the launcher derivation, so building against
      # them pulls the writeShellApplication (and its shellcheck) in.
      workmuxAgents = enabledCfg.myconfig.ai.workmux.agents;
      workmuxLauncherCmds = map (n: workmuxAgents.${n}.command) [
        "microvm-claude"
        "microvm-pi"
        "microvm-codex"
        "microvm-opencode"
      ];
    in
    pkgs.runCommand "microvm-launcher-shellcheck"
      {
        # Build dependencies: forcing these to build runs their shellcheck.
        launchers = [
          hostLauncher
          guestAgentRun
        ];
        # Pull in the workmux launcher drvs via their exe-path string context.
        workmuxCmds = workmuxLauncherCmds;
      }
      ''
        {
          echo "microvm-launcher-shellcheck: the following writeShellApplication"
          echo "derivations built successfully, so their shellcheck gate passed:"
          echo "  host launcher : ${hostLauncher}"
          echo "  guest agent-run: ${guestAgentRun}"
          for c in $workmuxCmds; do echo "  workmux launcher: $c"; done
        } > "$out"
      '';
}
