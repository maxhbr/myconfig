# Plan: per-resource-class network profiles

## Problem

`myconfig.ai.microvm.networkProfile` is a **single global option**. It is
resolved exactly once in `default.nix` (`effectiveProfile = cfg.networkProfile`)
and threaded into every consumer — the host firewall in `network.nix`, every
guest's networking in `guest.nix`, the launcher, the bridge socket, the
per-VM VSOCK forwarder — through the shared `agentNetwork` module argument.

Concretely, three things couple ALL slots to the one value today:

1. **The firewall is one set of chains on one bridge.** `network.nix` builds
   `AGENT_MICROVM_INPUT` / `_FORWARD` scoped to the single bridge and the
   single guest subnet (`-s ${subnet}`). All slots share that bridge and that
   subnet, so a per-slot verdict would need either per-source-IP rules or
   per-slot bridges/subnets.
2. **NAT is one `POSTROUTING` rule for the whole subnet.** `natSetup`
   masquerades `-s ${subnet}` as a single block; there is no per-guest NAT.
3. **The guest config is built per-slot but from the shared flags.**
   `guest.nix` reads `netCaps = agentNetwork.caps` and
   `netTransport = agentNetwork.transportCaps` — both derived once from
   `effectiveProfile`. `networking.nameservers`, whether `systemd-networkd`
   runs at all, the LiteLLM forwarder target, etc. are all functions of that
   one resolved value.

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
  activation time. Out of scope for this plan; the resource-class axis is
  enough because `--resource-class` already exists.
- **Per-VM (separate bridges/subnets) isolation.** Cleanest but the most
  restructuring (new slot identities, new bridges). Out of scope.
- **Changing the secure default.** A class with no explicit profile keeps
  `proxy-only`, exactly as a host does today.

## Design

### The principle: resolve once per class, not once globally

`default.nix` already resolves `effectiveProfile` once. The change is to
resolve it **per resource class**, producing a per-class `agentNetwork`-like
record, and hand each consumer the record for the slot it is building for.

The module's existing structure makes this natural:

- slots are deterministic per-class (`slots.nix` already walks
  `resourceClasses` in alphabetical order and assigns each slot a fixed
  `class` field, MAC, IPv4, VSOCK CID);
- every slot already has a fixed `ip`, so per-class firewall verdicts can be
  scoped by source IP instead of by the whole subnet;
- `--resource-class` is already the per-run selector and the allocator only
  ever considers the requested class.

So "pick a class with `--resource-class`" stays the ONLY operator knob, and
the class now also implies the network profile.

### 1. Option: a per-class `networkProfile`

In `default.nix`, extend the `resourceClasses` submodule with an optional
`networkProfile`:

```nix
resourceClasses = mkOption {
  type = types.attrsOf (types.submodule {
    options = {
      count     = ...;   # unchanged
      vcpu      = ...;   # unchanged
      memoryMiB = ...;   # unchanged
      networkProfile = mkOption {
        type = types.nullOr (types.enum profileLib.names);
        default = null;   # null => inherit the global default (proxy-only)
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

`null` = inherit the global default keeps the secure default and keeps every
existing host byte-identical: a class with no `networkProfile` resolves to
exactly today's `effectiveProfile`.

### 2. Per-class resolution in `default.nix`

Replace the single `effectiveProfile` / `agentNetwork` with a per-class map:

```nix
globalProfile = cfg.networkProfile;

# Resolve the effective profile + transport + caps per class. `null` inherits
# the global default, so a class that sets nothing is byte-identical to today.
classNetwork = lib.mapAttrs (name: cls:
  let
    profile   = cls.networkProfile or globalProfile;
    transport = profileLib.resolveTransport {
      inherit profile;
      vsockCapability = agentCapabilities.vsock;
    };
    transportCaps = profileLib.forTransport transport;
    caps = profileLib.forProfile profile;
  in {
    inherit profile transport transportCaps caps;
    tapSshUsable = cfg.enableSsh && transportCaps.tapSsh;
    dnsServers =
      if cfg.dnsServers == [ ] then [ cfg.gatewayAddress ] else cfg.dnsServers;
    # the host-side per-slot IP this class's slots will source from:
    # derived from slots.nix per class below.
  }
) cfg.resourceClasses;
```

The shared `agentNetwork` argument is then either replaced by `classNetwork`
or, for backward compatibility of the module's own consumers, augmented so
each consumer looks up `classNetwork.${slot.class}`.

### 3. `network.nix`: per-source-IP firewall + per-source NAT

This is the bulk of the work. Today every ACCEPT/DROP/NAT line is
`-s ${subnet}`. It must become per-class, keyed on the deterministic slot
IPs.

Because `slots.nix` already gives each slot a fixed `ip` and a `class`, build
the chains from the slot table rather than from one subnet block:

- For each class that has `caps.litellm`, the INPUT ACCEPT to the bridge
  LiteLLM endpoint is unchanged (the endpoint is shared on the bridge
  address); it is already scoped to `-s ${subnet}` and that is fine.
- For each class:
  - `dnsForwardLines` / `dnsDenyLines`: scope to `-s <class-slot-ip>` (or the
    class's IP range, which `slots.nix` can compute since a class occupies a
    contiguous run of the subnet).
  - `privateRuleLines`: the private-range DROPs are permissive to keep as
    `-s ${subnet}` (they deny, so widening the source only denies more — safe).
  - `internetLines` (the general-egress ACCEPT) and `natSetup` (the
    MASQUERADE): MUST become per-class. Only slots whose class profile is
    `internet` get an ACCEPT from their source IP, and only those get
    masqueraded:

    ```
    iptables -A AGENT_MICROVM_FORWARD -s <slot.ip> -j ACCEPT   # per internet slot
    iptables -t nat -A AGENT_MICROVM_NAT -s <slot.ip> ! -d ${subnet} -j MASQUERADE
    ```

  - `logDrops` and the terminal DROP stay global (they are catch-alls).

The class's IP *range* (rather than enumerating each slot) keeps the rule
count flat as the pool grows. `slots.nix` can expose a per-class
`{ firstIp; lastIp; }` (or a `cidr`) since the global index runs contiguously
across the pool.

**Invariant preserved:** the metadata DROP, the private-range DROPs, the
inter-VM DROP and the terminal DROP all stay unconditional and global. Only
the positive ACCEPTs and the NAT become per-source. So a `proxy-only` slot's
egress verdict is unchanged: it still falls through to the terminal DROP.

### 4. `guest.nix`: per-slot guest network config from the class

`guest.nix` currently reads the single `agentNetwork`. It must read the
slot's class record instead:

```nix
netCaps      = classNetwork.${slot.class}.caps;
netTransport = classNetwork.${slot.class}.transportCaps;
```

Everything downstream (`networking.nameservers`, whether `systemd-networkd`
runs, the LiteLLM forwarder target, the `vsockLitellm` branch) is already
written against these two values, so it adapts with no new branches — only
the source of `netCaps` / `netTransport` changes from "global" to
"this slot's class".

### 5. The transport decision stays per-class

`resolveTransport` is already parameterised by `profile` and
`vsockCapability`. Calling it per class means a `proxy-only` class can be
`vsock` (no interface) while an `internet` class is `tap` (the only
transport that can carry DNS/NAT) — which is exactly the existing rule
(`package-access`/`internet` need `tap`; `proxy-only` can be `vsock`).

A host that mixes a `vsock` `proxy-only` class with an `internet` `tap` class
then builds BOTH halves of the host: the per-VM VSOCK forwarders for the
vsock slots and the bridge + chains for the tap slots. `network.nix` is
already structured as `lib.mkMerge` over `transport.*` flags; the change is
that each transport block is gated per-class rather than globally.

### 6. Assertions: per-class insecure opt-in

The existing global assertions (`acknowledgeInsecureNetwork`,
`packageProxyPort != null`, `packageProxyPort != litellmPort`,
`litellm enable` for litellm-capable profiles) must be re-checked per class:
for every class whose resolved profile is `package-access`/`internet`,
require `cfg.acknowledgeInsecureNetwork`; for every `package-access` class,
require a `packageProxyPort`. The `litellm.enable` check applies to any
class with `caps.litellm`.

`acknowledgeInsecureNetwork` stays a single global opt-in (it acknowledges
"this host runs insecure profiles at all"), not per-class — that matches the
existing intent (the operator is a full sudoer acknowledging the widening).

### 7. Launcher: no change

`cmd_run` / `cmd_submit` already take `--resource-class <name>` and the
allocator only considers slots of that class. No new flag. The selected
slot's class already determines its network shape; the operator just picks a
class whose profile is what they want.

`agent-microvm capabilities` and `agent-microvm doctor` should grow a
per-class network line in their report so the operator can see which class
grants what — a display change, not a control change.

## Files touched

| file | change |
| --- | --- |
| `default.nix` | add per-class `networkProfile` option; replace single `effectiveProfile`/`agentNetwork` with a per-class `classNetwork` map; move assertions to per-class. |
| `slots.nix` | expose per-class IP range (`firstIp`/`lastIp` or a `cidr`) for per-source firewall/NAT rules. |
| `network.nix` | render the positive ACCEPTs and NAT per-source-IP (from the per-class ranges) instead of `-s ${subnet}`; keep the unconditional DROPs global; gate each transport block per-class. |
| `guest.nix` | read `classNetwork.${slot.class}` instead of the global `agentNetwork` for `netCaps`/`netTransport`/`dnsServers`. |
| `launcher.nix` | `capabilities`/`doctor` report per-class network profile (display only). |
| `config-seed.nix` | none expected (config staging is not network-derived). |
| docs | update `agent-microvm.md` (option doc) and this plan's status. |

## Verification (per AGENTS.md snapshot/diff workflow)

This is a behaviour-adding change, so a byte-identical diff is the goal for
the **unchanged** hosts and an expected, localised diff for the **new**
behaviour.

1. **Baseline**: for a host that sets NO per-class `networkProfile` (e.g.
   `test-f13`), snapshot before the change:
   - every VM's `system.build.toplevel.drvPath` and
     `microvm.declaredRunner.drvPath`,
   - every VM's `microvm.shares`,
   - `networking.firewall.extraCommands` (the rendered firewall text),
   - `builtins.attrNames systemd.services`,
   - `systemd.tmpfiles.rules`,
   - `environment.systemPackages` drvPaths.
2. **After**: re-eval the same slice. A class with `networkProfile = null`
   must resolve to today's `effectiveProfile`, so the diff must be **empty**
   (modulo the two git-revision artefacts). This proves the default is
   byte-identical.
3. **New behaviour**: add a second class with
   `networkProfile = "internet"; acknowledgeInsecureNetwork = true;` and
   snapshot its slot's `guest.nix` outputs (`networking.nameservers`,
   `systemd.network` presence, the LiteLLM forwarder `ExecStart`) plus the
   rendered firewall lines for that slot's source IP. Confirm the per-source
   ACCEPT + MASQUERADE appear ONLY for that class's slots and that the
   `proxy-only` slots' firewall verdict is unchanged (still falls to the
   terminal DROP).

```bash
mkdir -p /tmp/opencode/per-class-network
nix eval --impure --raw --expr '
let
  flake = builtins.getFlake ("git+file://" + toString /home/mhuber/myconfig/myconfig);
  cfg = flake.nixosConfigurations."f13";
in builtins.toJSON {
  fw   = cfg.config.networking.firewall.extraCommands;
  toplevels = builtins.mapAttrs (_: vm:
    vm.config.system.build.toplevel.drvPath) cfg.config.microvm.declaredRunners;
  # ... per-VM network slices ...
}' > /tmp/opencode/per-class-network/before.json
```

## Risks and mitigations

- **Per-source-IP rule explosion.** Mitigated by using the per-class IP
  *range* (a contiguous CIDR per class) rather than one line per slot.
- **A `proxy-only` slot silently getting egress.** Mitigated by keeping the
  positive ACCEPTs strictly per-source-IP and leaving the terminal DROP
  global; a `proxy-only` slot matches no ACCEPT and falls to DROP, as today.
- **VSOCK + internet mix.** `resolveTransport` already rejects
  `package-access`/`internet` under `vsock`; per-class resolution keeps that
  rule, so an `internet` class forces `tap`. A host mixing both builds both
  transport halves — already `lib.mkMerge`-structured.
- **NAT ordering vs. the host's own NAT.** Mitigated as today: a dedicated
  `AGENT_MICROVM_NAT` chain hooked from `POSTROUTING`, never
  `networking.nat`; just now with per-source MASQUERADE entries.

## Open questions

- Should `dnsServers` and `packageProxyPort` become per-class too, or stay
  global (one resolver set / one proxy port for the whole host)? Per-class
  would be more flexible; global is simpler and matches the "operator picked
  one set" intent. Recommendation: keep global for v1, revisit if a real host
  needs two proxies.
- Should `capabilities`/`doctor` warn when two classes resolve to the same
  profile (no point distinguishing them by network then)? Low priority.
