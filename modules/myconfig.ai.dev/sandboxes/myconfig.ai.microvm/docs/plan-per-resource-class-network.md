# Plan: per-resource-class network profiles

> Historical planning artifact (noted 2026-08-30): the per-class profiles
> sketched here were **not** implemented; superseded by the single global
> `networkProfile` option (later extended with the per-host `vsock`
> transport capability).

Status: revised after code review. **Scope is deliberately narrowed to v1**
(per-class *profile capabilities*, ONE shared *transport* per host); per-class
transports are a documented follow-up, because they would rewrite the
launcher's control-channel logic and the runtime-validation suite.

## Problem

`myconfig.ai.microvm.networkProfile` is a **single global option**. It is
resolved exactly once in `default.nix` (`effectiveProfile = cfg.networkProfile`,
`default.nix:197`) and threaded into every consumer — the host firewall in
`network.nix`, every guest's networking in `guest.nix`, the launcher, the bridge
socket, the per-VM VSOCK forwarder — through the shared `agentNetwork` module
argument (`default.nix:214`, `_module.args.agentNetwork` at `default.nix:705`).

Concretely, three things couple ALL slots to the one value today:

1. **The firewall is one set of chains on one bridge.** `network.nix` builds
   `AGENT_MICROVM_INPUT` / `_FORWARD` scoped to the single bridge and the
   single guest subnet (`-s ${subnet}`, `network.nix:131-158`). All slots share
   that bridge and that subnet, so a per-slot verdict needs per-source-IP rules
   (or per-slot bridges/subnets).
2. **NAT is one `POSTROUTING` rule for the whole subnet.** `natSetup`
   (`network.nix:174-180`) masquerades `-s ${subnet}` as a single block; there
   is no per-guest NAT.
3. **The guest config is built per-slot but from the shared flags.**
   `guest.nix:120/129` reads `netCaps = agentNetwork.caps` and
   `netTransport = agentNetwork.transportCaps` — both derived once from
   `effectiveProfile`. `networking.nameservers` (`guest.nix:767`), whether
   `systemd-networkd` runs at all, the LiteLLM forwarder target, etc. are all
   functions of that one resolved value.

The only per-run selector that exists is `--resource-class`, and it carries
no network flag: it only changes vCPU/RAM and which prebuilt slot you land on.
The launcher has no `--network` argument and the job spec's `SPEC_KEYS`
records `resourceClass` but no network field.

So there is currently no mechanism — not per-VM, not per-run — to give one
task internet and another `proxy-only`.

## Goal

Let each **resource class** carry its own network profile, so an operator
defines e.g. a `normal` class on `proxy-only` and an `inet` class on
`internet`, and `--resource-class inet` is the (already-existing) per-run
selector that picks a slot with that egress. No new launcher flag, no per-job
Nix evaluation, no change to the deterministic slot pool model.

### Non-goals

- **Per-run `--network` overrides.** That would require staging per-launch
  firewall/NAT changes against a firewall that is currently static at
  activation time. Out of scope; `--resource-class` already exists.
- **Per-VM (separate bridges/subnets) isolation.** Cleanest but the most
  restructuring (new slot identities, new bridges). Out of scope.
- **Changing the secure default.** A class with no explicit profile keeps
  `proxy-only`, exactly as a host does today.
- **v1 non-goal: per-class TRANSPORT.** All classes on a host must resolve to
  the SAME transport (`tap` or `vsock`); this is enforced by an assertion (§7).
  Rationale in §8 ("Why the transport stays global in v1") — mixing transports
  would turn the launcher's global shell constants and the runtime-validation
  suite's single `network-transport:` contract into per-slot lookups, which is
  more work than the whole firewall change and cannot be validated without
  booting both shapes on KVM.

  Practically this is not very restrictive: `resolveTransport`
  (`network-profiles.nix`) only returns `vsock` for `proxy-only`, so v1 covers
  (a) a `tap` host mixing `proxy-only` / `package-access` / `internet` classes
  freely, and (b) an all-`proxy-only` `vsock` host. It rejects only "vsock for
  the closed class AND tap for the open class on the same host".

## Design

### The principle: resolve once per class, not once globally

`default.nix` already resolves `effectiveProfile` once. The change is to
resolve it **per resource class**, producing a per-class `agentNetwork`-like
record, and hand each consumer the record for the slot it is building for.

The module's existing structure makes this natural:

- slots are deterministic per-class (`slots.nix` walks `resourceClasses` in
  alphabetical order and assigns each slot a fixed `class`, MAC, IPv4, CID);
- every slot already has a fixed `ip`, so per-class firewall verdicts can be
  scoped by source IP instead of by the whole subnet;
- `--resource-class` is already the per-run selector and the allocator only
  ever considers the requested class.

So "pick a class with `--resource-class`" stays the ONLY operator knob, and
the class now also implies the network profile.

### 1. Option: a per-class `networkProfile`

In `default.nix`, extend the `resourceClasses` submodule (`default.nix:367`)
with an optional `networkProfile`:

```nix
resourceClasses = mkOption {
  type = types.attrsOf (types.submodule {
    options = {
      count     = ...;   # unchanged
      vcpu      = ...;   # unchanged
      memoryMiB = ...;   # unchanged
      networkProfile = mkOption {
        type = types.nullOr (types.enum profileLib.names);
        default = null;   # null => inherit the host-wide networkProfile
        description = ''
          Network profile for every slot in this class. `null` (the default)
          inherits the host-wide `myconfig.ai.microvm.networkProfile`.
        '';
      };
    };
  });
  ...
};
```

`null` = inherit keeps the secure default and keeps every existing host
byte-identical: a class with no `networkProfile` resolves to exactly today's
`effectiveProfile`.

### 2. Per-class resolution in `default.nix`

Replace the single `effectiveProfile` / `agentNetwork` with a per-class map.
Note the **explicit `null` test**: with a `nullOr` option the attribute always
exists, so `cls.networkProfile or globalProfile` would evaluate to `null` and
silently break the inherit path.

```nix
globalProfile = cfg.networkProfile;

# Resolve the effective profile + transport + caps per class. `null` inherits
# the host-wide profile, so a class that sets nothing is byte-identical to today.
classNetwork = lib.mapAttrs (
  name: cls:
  let
    profile = if cls.networkProfile == null then globalProfile else cls.networkProfile;
    transport = profileLib.resolveTransport {
      inherit profile;
      vsockCapability = agentCapabilities.vsock;
    };
    transportCaps = profileLib.forTransport transport;
  in
  {
    inherit profile transport transportCaps;
    caps = profileLib.forProfile profile;
    tapSshUsable = cfg.enableSsh && transportCaps.tapSsh;
    inherit vsockLitellmPort;
    dnsServers = if cfg.dnsServers == [ ] then [ cfg.gatewayAddress ] else cfg.dnsServers;
  }
) cfg.resourceClasses;

# v1: all classes share ONE transport (asserted in §7), so the host-level
# transport facts stay a single value and every host-shaped consumer
# (launcher.nix, session.nix, hostkeys.nix, network.nix's transport gating)
# keeps reading exactly one decision.
hostTransport = (lib.head (lib.attrValues classNetwork)).transport;
```

`agentNetwork` is kept as the module argument and gains the map, so no
consumer loses its current field:

```nix
agentNetwork = {
  # per-class records; the ONLY thing profile-derived consumers should read
  byClass = classNetwork;
  # host-wide transport facts (v1: identical across classes, asserted)
  transport = hostTransport;
  transportCaps = profileLib.forTransport hostTransport;
  tapSshUsable = cfg.enableSsh && (profileLib.forTransport hostTransport).tapSsh;
  inherit vsockLitellmPort;
  dnsServers = ...;                     # unchanged (global in v1, §Open questions)
  # union over classes, for host-wide artefacts that must exist if ANY class
  # needs them (e.g. the single bridge LiteLLM socket, the litellm assertion)
  anyCaps = { litellm = lib.any (c: c.caps.litellm) (lib.attrValues classNetwork); ... };
  # LEGACY single-value fields (`profile`, `caps`) are REMOVED so no consumer
  # can keep reading a host-wide profile by accident; the compiler (eval) then
  # points at every site that must become per-class.
};
```

### 3. `network.nix`: per-source-IP firewall + per-source NAT

This is the bulk of the work. Today every ACCEPT/DROP/NAT line is
`-s ${subnet}`. It must become per-class, keyed on the deterministic slot IPs.

**Scope by individual slot IP, not by a per-class CIDR.** A class occupies a
contiguous run of `192.168.83.${10 + globalIndex}`, but that run is NOT
CIDR-aligned (e.g. counts 1 + 4 give `.10` and `.11-.14`), and `iptables -s`
accepts only an address or a CIDR. So the rules are rendered per slot:

```nix
classSlots = className: slotLib.slotsOfClass slots className;
srcIPsWithCap = capName:
  map (s: s.ip) (lib.filter (s: agentNetwork.byClass.${s.class}.caps.${capName}) slots);
```

Rule count is bounded by the pool size (`slotLib.maxSlotCount = 200`, real
pools are single digits), so a handful of extra lines per capability is fine;
`-m iprange --src-range` or an ipset per class are available later if a pool
ever grows large enough to matter.

Concretely:

- `inputAllowLines` (`network.nix:131-135`) — **must become per-source too.**
  Leaving the LiteLLM / package-proxy / host-DNS INPUT ACCEPTs at
  `-s ${subnet}` would let a slot of an `offline` class reach the model API,
  which contradicts per-class resolution. Render one line per slot IP whose
  class has the corresponding capability.
- `dnsForwardLines` / `dnsDenyLines`: `-s <slot.ip>` for the slots of classes
  with `caps.dns` / `caps.internetEgress`.
- `privateRuleLines`: keep as `-s ${subnet}`. They DENY, so a wider source
  only denies more — safe, and it keeps the invariant global.
- `internetLines` and `natSetup`: per source IP. Only slots whose class
  resolves to `internet` get an egress ACCEPT and a MASQUERADE:

  ```
  iptables -A AGENT_MICROVM_FORWARD -s <slot.ip> -j ACCEPT
  iptables -t nat -A AGENT_MICROVM_NAT -s <slot.ip> ! -d ${subnet} -j MASQUERADE
  ```

  The `POSTROUTING -s ${subnet} -j AGENT_MICROVM_NAT` hook and the chain
  create/flush/teardown stay subnet-wide (a jump into a chain that may contain
  no MASQUERADE is a no-op), and the chain is only created when at least one
  class has `caps.nat`.
- `logLines`, the metadata DROP, the inter-VM DROP and the terminal DROP stay
  unconditional and global (catch-alls). `logDrops` becomes "any class wants
  it".
- The rendered header comment `# network profile: ${profile}` becomes a
  per-class listing (`# network profiles: normal=proxy-only inet=internet`).

Per-slot host artefacts in this file must also be filtered by class once
per-class transports land, and are called out here so they are not forgotten:
`tapAttachServices` (`network.nix:498`), `networking.networkmanager.unmanaged`
tap list (`network.nix:515`), `vsockForwarderSockets` / `Services`
(`network.nix:490`). In v1 the transport is host-wide, so they stay as-is.

**Invariant preserved:** the metadata DROP, the private-range DROPs, the
inter-VM DROP and the terminal DROP all stay unconditional and global. Only
the positive ACCEPTs and the NAT become per-source. So a `proxy-only` slot's
egress verdict is unchanged: it still falls through to the terminal DROP.

### 4. `guest.nix`: per-slot guest network config from the class

`guest.nix:120/129` must read the slot's class record instead of the global
value:

```nix
netCaps      = agentNetwork.byClass.${slot.class}.caps;
netTransport = agentNetwork.byClass.${slot.class}.transportCaps;  # v1: same for all
```

Everything downstream (`networking.nameservers` at `guest.nix:767`, whether
`systemd-networkd` runs, the LiteLLM forwarder target, the `vsockLitellm`
branch) is already written against these two values, so it adapts with no new
branches — only the source changes from "global" to "this slot's class".

### 5. Other per-slot consumers of the profile

The review found three more consumers that read the host-wide record and are
built per slot; they must switch to `byClass.${slot.class}`:

| site | reads | change |
| --- | --- | --- |
| `guest-model-config.nix:244` | `agentNetwork.caps.litellm` | per slot's class — a class with no `litellm` must not get a model config |
| `hostkeys.nix:310` | `transportCaps.guestInterface` | v1: unchanged (host-wide transport); flagged for the per-class-transport follow-up |
| `session.nix:713` | `tapSshUsable` | v1: unchanged (host-wide transport); flagged for the follow-up |

### 6. The transport decision (v1: resolved per class, asserted equal)

`resolveTransport` is called per class in `classNetwork` so the *resolution* is
already per-class, and §7's assertion then requires all classes to agree. This
keeps the eventual per-class-transport step a pure removal of one assertion
plus the per-slot rework in §5 / §3's artefact list, with no re-plumbing.

### 7. Assertions

Changes to `default.nix`'s assertion block:

- **`acknowledgeInsecureNetwork` (`default.nix:948`)**: re-check per class —
  if ANY class resolves to a profile in `profileLib.insecureProfiles`, require
  the opt-in. The opt-in stays a single global flag (it acknowledges "this host
  runs insecure profiles at all"), which matches the existing intent; the
  message must name the offending class(es).
- **`package-access` needs `packageProxyPort` (`default.nix:957`)**: per class.
- **`litellm` backend (`default.nix:980`)**: use `anyCaps.litellm`.
- **`vsock` capability vs. profile (`default.nix:825`)**: today this asserts
  `!vsock || effectiveProfile == "proxy-only" || "offline"`. It must become
  per-class: reject only when the `vsock` capability is selected AND some class
  resolves to an insecure profile. Its message, the `capabilities` option
  description (`default.nix:~360`) and `docs/agent-microvm-security-model.md`
  all state the pairing rule as a host-wide fact and must be reworded.
- **NEW (v1 boundary)**: all classes must resolve to the same transport.

  ```nix
  {
    assertion = lib.length (lib.unique (map (c: c.transport) (lib.attrValues classNetwork))) <= 1;
    message = ''
      myconfig.ai.microvm: all resource classes must resolve to the same model
      transport, but this host mixes <...>. Per-class transports are not
      supported yet (the launcher's control-channel selection and
      `agent-microvm capabilities` are host-wide). Either drop the `vsock`
      capability so every class uses `tap`, or give every class a closed
      profile.
    '';
  }
  ```

### 8. Launcher: display only (and why the transport stays global in v1)

`cmd_run` / `cmd_submit` already take `--resource-class <name>` and the
allocator only considers slots of that class. No new flag.

What the launcher DOES need:

- `agent-microvm capabilities` and `agent-microvm doctor` grow a per-class
  network line (`network-profile-<class>: <profile>`), so an operator can see
  which class grants what. Append-only, keyed lines — the existing
  `network-transport:` line stays exactly one value.
- `readonly NETWORK_PROFILE` (`launcher.nix:1032`) becomes a slot→profile
  lookup table plus a per-run resolved `$NETWORK_PROFILE` for the log/status
  output, and `LITELLM_CAPABLE` (`launcher.nix:1031`) becomes the same lookup.
  The two `bridgeLitellm` diagnostics at `launcher.nix:555-563` print the
  profile, so they read the resolved value.

Why the transport must stay host-wide in v1: `launcher.nix` bakes the resolved
transport as GLOBAL shell constants — `SSH_ENABLED` (1058),
`NETWORK_TRANSPORT` (1068), `GUEST_INTERFACE` (1069), plus `tapNetworkConfig`
(503) and the `bridgeLitellm` branches (525/535/555/580) — and the
control-channel selection branches on them at 368-376, 2170-2205
(`wait_for_ready`), 2578, 3424-3432, 3507 and 3559-3563 (`ssh` / `status`).
`runtime-validation.sh:2209-2240` parses ONE `network-transport:` line from
`capabilities` and `die`s on a value it does not know. Making those per-slot is
a larger change than this whole plan and needs a real-KVM run of both shapes,
so it is a separate ticket.

## Files touched

| file | change |
| --- | --- |
| `default.nix` | per-class `networkProfile` option; `classNetwork` map + `agentNetwork.byClass` / `anyCaps` (legacy `profile`/`caps` removed); per-class assertions incl. the new same-transport assertion; reword the `vsock`-vs-profile assertion and the `capabilities` option doc. |
| `network.nix` | render `inputAllowLines`, DNS lines, `internetLines` and the NAT MASQUERADE per slot IP instead of `-s ${subnet}`; keep every DROP and the POSTROUTING hook global; `logDrops`/chain creation gated on "any class"; per-class header comment. |
| `guest.nix` | `netCaps`/`netTransport` from `agentNetwork.byClass.${slot.class}`. |
| `guest-model-config.nix` | `caps.litellm` per slot's class. |
| `hostkeys.nix` | v1: none (host-wide transport) — add a NOTE marking `:310` as a per-class-transport follow-up site. |
| `session.nix` | v1: none — same NOTE for `tapSshUsable` at `:713`. |
| `launcher.nix` | slot→profile / slot→litellm-capable lookup for `NETWORK_PROFILE` / `LITELLM_CAPABLE`; per-class `network-profile-<class>:` lines in `capabilities` + `doctor`. |
| `job.nix` | record the RESOLVED profile in the job spec (`SPEC_KEYS`) for post-hoc audit — observability only, not control. |
| `runtime-validation.sh` | confirm the `capabilities` parser tolerates the new `network-profile-<class>:` keys; assert per-class egress where a mixed host is under test. |
| `config-seed.nix` | none (config staging is not network-derived). |
| docs | `docs/agent-microvm.md` (option doc), `network-profiles.nix` header ("single global option" framing), `docs/agent-microvm-security-model.md` + `docs/agent-microvm-operator-guide.md` (the guarantees become per-class), and this plan's status. |

## Verification (per AGENTS.md snapshot/diff workflow)

This is a behaviour-adding change, so a byte-identical diff is the goal for
the **unchanged** hosts and an expected, localised diff for the **new**
behaviour.

1. **Coarse baseline** — the strongest single check for the unchanged host
   (`f13` is the only host that sets `resourceClasses` / `networkProfile`, in
   `hosts/host.f13/ai.f13.nix:78-95`):

   ```bash
   nix eval --raw .#nixosConfigurations.f13.config.system.build.toplevel.drvPath
   ```

   With every class on `networkProfile = null` this MUST be unchanged.

2. **Localising slice**, for when (1) differs. Note the correct attribute path:
   guests are built as `microvm.vms.<name>` (`guest.nix:941`) — there is no
   `microvm.declaredRunners`.

   ```bash
   mkdir -p /tmp/opencode/per-class-network
   nix eval --impure --raw --expr '
   let
     flake = builtins.getFlake ("git+file://" + toString /home/mhuber/myconfig/myconfig);
     cfg = flake.nixosConfigurations."f13";
     c = cfg.config;
   in builtins.toJSON {
     fwStart   = c.networking.firewall.extraCommands;
     fwStop    = c.networking.firewall.extraStopCommands;
     bridges   = c.networking.bridges;
     ifaces    = builtins.attrNames c.networking.interfaces;
     nmUnmanaged = c.networking.networkmanager.unmanaged;
     sysctl    = c.boot.kernel.sysctl;
     units     = builtins.attrNames c.systemd.services;
     sockets   = builtins.attrNames c.systemd.sockets;
     tmpfiles  = c.systemd.tmpfiles.rules;
     vms = builtins.mapAttrs (_: vm: {
       toplevel = vm.config.system.build.toplevel.drvPath;
       runner   = vm.config.microvm.declaredRunner.drvPath;
       shares   = vm.config.microvm.shares;
       nameservers = vm.config.networking.nameservers;
       networkd = vm.config.systemd.network.enable or false;
     }) c.microvm.vms;
   }' > /tmp/opencode/per-class-network/before.json
   ```

   `git add` every new/renamed file before the "after" eval, then
   `diff before.json after.json` → must be empty.

3. **New behaviour**: add a second class with `networkProfile = "internet"`
   plus `acknowledgeInsecureNetwork = true` and confirm from the same slice
   that
   - the per-source ACCEPT and the MASQUERADE appear ONLY for that class's slot
     IPs, and the LiteLLM/DNS INPUT ACCEPTs are per slot IP;
   - the `proxy-only` slots' firewall verdict is unchanged (no ACCEPT matches;
     they fall to the terminal DROP);
   - that slot's guest gains `networking.nameservers` while the `proxy-only`
     guests do not.

4. **Assertion tests** (eval-only, no build): a class with `internet` and no
   `acknowledgeInsecureNetwork` fails; a `package-access` class without
   `packageProxyPort` fails; `vsock` capability + an `internet` class fails the
   new same-transport assertion with the v1 message.

## Risks and mitigations

- **A `proxy-only` slot silently getting egress.** Positive ACCEPTs are
  strictly per-source-IP and the terminal DROP stays global; a `proxy-only`
  slot matches no ACCEPT and falls to DROP, as today. Verified by step 3.
- **A closed class keeping model access via a subnet-wide INPUT rule.** This is
  the real trap the review found: `inputAllowLines` must be per-source, not
  left at `-s ${subnet}`. Verified by step 3.
- **Rule-count growth.** Bounded by the pool size (≤ `maxSlotCount = 200`, real
  pools are single digits). If it ever matters, switch to
  `-m iprange --src-range` or one ipset per class — a rendering change only.
- **Renumbering.** Adding a class re-numbers MAC/IP/CID of later classes
  (`slots.nix` header). Since firewall rules are now keyed on slot IPs, a class
  addition rewrites more of the firewall than before — cosmetic, but expect a
  large step-2 diff whenever the class set changes.
- **NAT ordering vs. the host's own NAT.** Unchanged: a dedicated
  `AGENT_MICROVM_NAT` chain hooked from `POSTROUTING`, never
  `networking.nat`; now with per-source MASQUERADE entries inside it.
- **Mixed transports.** Rejected at eval in v1 (§7), so no half-built host.

## Open questions

- Should `dnsServers` and `packageProxyPort` become per-class too, or stay
  global (one resolver set / one proxy port for the whole host)? Recommendation:
  keep global for v1, revisit if a real host needs two proxies.
- Should `capabilities`/`doctor` warn when two classes resolve to the same
  profile (nothing distinguishes them by network then)? Low priority.
- Follow-up ticket: per-class TRANSPORT. Requires slot→transport lookups in
  `launcher.nix` (the constants and branch points listed in §8), a per-class
  `network-transport-<class>:` report format, `runtime-validation.sh` support,
  per-class filtering of `tapAttachServices` / NM `unmanaged` /
  `vsockForwarder*`, and the `hostkeys.nix:310` + `session.nix:713` sites.
