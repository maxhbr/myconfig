<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `agent-qemu-herdr` vs. `agent-microvm run --agent herdr`

Both wrappers put the `herdr` agent multiplexer inside a microVM instead of
running it directly on the host. They are **not** the same feature under two
names: they belong to two different tiers of the repo's documented sandboxing
ladder (`modules/myconfig.ai/docs/README.md:8-29`, tiers 3 and 4) and solve
different problems. This document compares them side by side and gives a
concrete recommendation.

## Summary

| | `agent-qemu-herdr` (tier 3) | `agent-microvm run --agent herdr` (tier 4) |
| --- | --- | --- |
| Purpose | ad-hoc, "just run it" interactive sandbox for one project directory | fleet of prebuilt, policy-hardened sandboxes for interactive **and** unattended/batch agent work |
| Hypervisor | QEMU + SLiRP user-mode networking | Cloud Hypervisor, real bridged networking |
| Host `/nix/store` | shared into guest **read-only** via virtiofs | **not shared** — guest has its own self-contained EROFS store disk |
| Guest root/home | ephemeral tmpfs, no store disk needed, fast boot | ephemeral tmpfs by default; opt-in **task-scoped** state persistence for agents that declare it |
| Workspace | host CWD bind-mounted read-write at `/workspace` (same inodes as host) | **standalone git clone** of the repo on a per-session share; host checkout is never touched |
| Network | outbound NAT via SLiRP, no host config, no bridge/firewall changes | dedicated private bridge (`agentbr0`), per-TAP L2 isolation, host-side nftables chains, named `networkProfile`s |
| Model-API credentials | real API keys forwarded over the SSH session **environment** at launch | **never leave the host**: guest only reaches a bridge- (or VSOCK-)restricted LiteLLM proxy forwarder; no upstream key ever reaches the guest |
| Config seeding | union of every registered agent's `configPaths`, copied over SSH after boot | same allowlist/denylist model, staged as a **root-owned, read-only virtiofs share** before boot |
| Activation | always installed whenever any coding agent is enabled (`agenticCodingEnabled`) | explicit per-host opt-in (`myconfig.ai.microvm.enable`), never implied |
| Enabled on | every agentic host (f13, p14, thing — anywhere an agent flag is `true`) | **f13 only** |
| Execution modes | interactive only (one SSH session, one VM, torn down on exit) | interactive (`run --attach`) **and** unattended batch (`submit`), from a pool of prebuilt slots |
| Startup cost | seconds: boot a disposable VM + `nix build --impure` a small wrapper derivation per invocation | slot **pool is prebuilt** at system-build time; a `run`/`submit` allocates an already-built slot — no guest build in the hot path, but the bridge/firewall/pool exist continuously |
| herdr-specific role | **the whole point** — herdr is the only in-guest entry point this tier's herdr variant offers | **one registry agent among several** (`enabledAgents = [ … "herdr" … ]`); interactive-only, no `submit --agent herdr` |

## Purpose / intended use case

- **`agent-qemu-herdr`** (`modules/myconfig.ai/programs.herdr.nix:110-125`) is
  the herdr-flavoured sibling of `agent-qemu-pi`: run it from a project
  subdirectory to get "the same loop as `agent-bubblewrap-pi`, but with a kernel
  boundary" (`modules/myconfig.ai/docs/README.md:144-146`), except instead of
  dropping straight into `pi` it drops into a `herdr` multiplexer session so
  several agents/shells can run side by side in the one VM
  (`modules/myconfig.ai/agent-qemu-herdr.README.md:14-21`). It is a *disposable,
  one-shot* command a developer types ad hoc.
- **`agent-microvm`** (`modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md:8-25`)
  is "a second, stronger isolation tier", the only one "designed for
  unattended, autonomous agent runs"
  (`modules/myconfig.ai/docs/README.md:207-209`). `herdr` is just one of the
  registry's selectable agents there
  (`modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md:765-784`,
  "herdr specifics") — a way to get a multi-agent pane session *inside* one of
  the fleet's slots, mirroring what tier 3 does, not the tier's reason to
  exist.

## Isolation mechanism and security boundary

| Aspect | `agent-qemu-herdr` | `agent-microvm` (`--agent herdr`) |
| --- | --- | --- |
| Hypervisor | QEMU, SLiRP (`flake.agent-qemu.nix:170-204`) | Cloud Hypervisor (`modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md`, guest shape table) |
| Kernel | own guest kernel | own guest kernel |
| Guest store | host `/nix/store` shared **read-only** via virtiofs (`flake.agent-qemu.nix:175-186`) — the guest closure is the *host's* store | self-contained EROFS store disk built into the guest image; host store not reachable from the guest at all (`agent-microvm.md:13-14`) |
| Network boundary | SLiRP NAT (outbound only) + one loopback-forwarded SSH port; no host bridge/firewall (`agent-qemu-herdr.README.md:80-86`) | dedicated bridge `agentbr0`, per-TAP L2 isolation, nftables `AGENT_MICROVM_*` chains, named `networkProfile`s (`offline`/`proxy-only`/`package-access`/`internet`), and — with the `vsock` capability — literally **no network interface at all** (`agent-microvm.md`, "VSOCK versus TAP transport") |
| Credential exposure to guest | real `OPENAI_API_KEY`/`ANTHROPIC_API_KEY`/etc. land in the guest process environment via SSH `SetEnv` (`programs.herdr.nix:236-238`) | upstream keys **never** reach the guest; only a bridge-/VSOCK-restricted forwarding endpoint to the host LiteLLM proxy is visible (`agent-microvm.md:9-25`, "model-API access restricted to the host LiteLLM proxy") |
| What one compromised guest can attack | the host store is read-only, so a guest exploit cannot corrupt it, but it *can read* the entire host store contents (world-readable anyway) and reach outbound network via SLiRP | no store to read, egress is default-deny beyond the proxy port, guest-to-guest traffic is dropped at L2 and L3, and the workspace is a throwaway clone, not the real checkout |

Net: `agent-microvm`'s boundary is **strictly more defensive** on every axis
the two share (store exposure, network egress, credential exposure). Tier 3's
own docs concede this: "the host store is visible read-only; the guest still
shares the host store closure and reaches the network via the host"
(`modules/myconfig.ai/docs/README.md:199-201`).

## What is shared with the host

| | `agent-qemu-herdr` | `agent-microvm` |
| --- | --- | --- |
| Filesystem | CWD read-write at `/workspace`, **same inodes as the host checkout** (`agent-qemu-herdr.README.md:41-55`) — agent edits appear live on the host; host `/nix/store` read-only | a **standalone git clone**, never the host checkout; import the resulting branch afterwards (`agent-microvm.md`, workspace section) |
| Network | outbound NAT through the host, one inbound SSH port on loopback | bridge-restricted; default profile allows only guest → gateway:litellmPort |
| Devices | none beyond the virtio devices QEMU/virtiofs need | none beyond virtio; VSOCK capability removes even the NIC |
| Env vars | 4 LLM credential vars forwarded over SSH `SetEnv`, only if set on host (`programs.herdr.nix:236-238`) | none — model auth is fully proxied through the host LiteLLM service; guest only gets an endpoint URL |
| Secrets | forwarded live over SSH session env, never in the store, never in argv | never leave the host; guest config seeding explicitly denylists credential-shaped files (`agent-microvm.md`, "Runtime configuration staging") |
| Agent home/state | ephemeral guest `/home/agent`, **seeded once at launch** from an SSH-copied allowlist of every registered agent's `configPaths` (`agent-qemu-herdr.README.md:97-127`) | ephemeral guest `/home/agent` by default, seeded from a **root-owned read-only virtiofs share** staged before boot with the same allowlist/denylist model; **opt-in, task-scoped persistence** for agents that declare `persistentState.directories` (only `hermes` today) |

Both tiers use the *same* seeding vocabulary
(`modules/myconfig.ai/fns/seed-agent-config.nix`), so the credential denylist
and allowlist syntax are identical; the transport differs (live SSH copy after
boot vs. a pre-boot root-owned share).

## Startup cost, resource overhead, latency

- **`agent-qemu-herdr`**: no persistent host state. Each invocation runs
  `nix build --impure` for a small per-invocation wrapper derivation (the
  cached guest system closure is reused; only the workspace-path-specific
  derivation rebuilds — "sub-second", per the module comment
  `flake.agent-qemu.nix:27-31`), then boots a disposable QEMU VM and polls
  SSH for up to 120s before failing (`programs.herdr.nix:206-227`). No host
  services, bridges or pools exist between invocations — zero idle overhead,
  all cost is paid at launch and torn down at exit.
- **`agent-microvm`**: resource classes are **fixed, prebuilt** slots
  (`resourceClasses`, e.g. f13's `small`/`normal` pool,
  `hosts/host.f13/ai.f13.nix:64-79`) sized at system-build time; a `run`/
  `submit` allocates an already-built slot rather than building a guest on
  demand, but the bridge, firewall chains, and (if any) idle slots are
  continuous host-side overhead whether or not a session is active. No
  authoritative launch-to-ready latency numbers exist yet: the reference doc
  explicitly marks "launch-to-ready latency, idle RSS per slot, ... warm build
  time" as **"STILL PENDING a real-KVM run (deliberately NOT estimated)"**
  (`modules/myconfig.ai/myconfig.ai.microvm/docs/myconfig-ai-microvm-lightweight-plan.md:8`).
  Uncertainty flagged: this document cannot state whether tier 4 is faster or
  slower to reach an interactive prompt than tier 3's boot+SSH loop — only
  that tier 4 avoids a `nix build` in the hot path while tier 3 does not need
  a prebuilt pool at all.

Both tiers state **neither has been runtime-validated against real KVM** in
this checkout: `agent-qemu-herdr.README.md:187-196` ("A live VM boot has not yet
been exercised here ... no `/dev/kvm` in the build environment") and
`agent-microvm.md`'s own verification-boundary note ("must be measured ... on
real KVM"). Both are eval/build-clean, not empirically measured, as of this
writing.

## Configuration

| | `agent-qemu-herdr` | `agent-microvm` |
| --- | --- | --- |
| Option path | none — it is unconditionally installed whenever any agent flag is enabled (`agenticCodingEnabled`, `modules/myconfig.ai/programs.herdr.nix:23-29`) | `myconfig.ai.microvm.*` (`enable`, `enabledAgents`, `resourceClasses`, `networkProfile`, `capabilities`, …), all under `modules/myconfig.ai/myconfig.ai.microvm/default.nix` |
| Entry point | `agent-qemu-herdr` shell wrapper (`programs.herdr.nix:120-270`), installed via `home.packages` (`programs.herdr.nix:284`) | `agent-microvm run\|submit\|ssh\|...` launcher (`modules/myconfig.ai/myconfig.ai.microvm/launcher.nix`), plus `microvm-<agent>` workmux panes when `interactive` is selected |
| Guest builder | `mkAgentQemuHerdrRunner` in `flake.agent-qemu.nix:470`, a thin wrapper over the shared `mkSandboxedRunner` (same factory `mkAgentQemuPiRunner` uses) | the module's own guest NixOS system (`guest.nix`), driven by the agent registry `agents.nix` |
| Impure evaluation seam | `packages.<system>.agent-qemu-herdr-runner` in `flake.nix:623-643`, built from `AGENT_QEMU_HERDR_*` env vars set by the wrapper (workspace path never lands in a tracked file) | none needed — slots are declared statically per host and prebuilt at system-build time |
| herdr's role in the config | hard-coded as the guest's SSH-exec target; not configurable per invocation | one entry in the agent registry (`../agents.nix`), selected via `enabledAgents` like any other agent |

## Which hosts enable which

```
$ grep -rln "myconfig.ai.microvm" hosts/
hosts/host.f13/ai.f13.nix
```

- `agent-microvm` is enabled on **f13 only** (`hosts/host.f13/ai.f13.nix:46-124`),
  with `enabledAgents` explicitly including `"herdr"`
  (`hosts/host.f13/ai.f13.nix:64-72`).
- `agent-qemu-herdr` has **no dedicated enable option** — it piggybacks on
  `agenticCodingEnabled`, i.e. it is installed on **every** host with at least
  one of `claude-code`, `codex`, `opencode`, `pi-coding-agent`, `qwen-code`, or
  `github-copilot-cli` enabled. That currently includes f13
  (`hosts/host.f13/ai.f13.nix:15-18`), p14 (`hosts/host.p14/ai.p14.nix`) and
  thing (`hosts/host.thing/default.nix`, `hosts/host.thing/programs.opencode.nix`).

So f13 is the only host where both variants coexist; every other agentic host
only has `agent-qemu-herdr`.

## Overlap and duplication

The two are **not** a superset/subset pair — they overlap in *feature* (herdr
inside a VM) but diverge in *mechanism and guarantees*:

- Both reuse the identical config-seeding vocabulary
  (`modules/myconfig.ai/fns/seed-agent-config.nix`) and the identical
  documented rationale for "why herdr" (compare
  `agent-qemu-herdr.README.md:14-21` with `agent-microvm.md`'s "herdr
  specifics" section — near-verbatim phrasing, deliberately kept in sync per
  the module comments).
- `agent-microvm`'s herdr guest is a **strict security superset** of
  `agent-qemu-herdr`'s: no shared store, no real credentials in the guest,
  bridge/VSOCK-restricted network, throwaway clone instead of the live
  checkout, and a batch execution path for the *other* agents (though not for
  herdr itself, since herdr has no batch mode in either tier).
- `agent-qemu-herdr` is **not** a subset of `agent-microvm` operationally: it
  needs zero host configuration (no `myconfig.ai.microvm.enable`, no bridge, no
  prebuilt pool) and shares the *live* working directory (edits are
  immediately visible on the host, no import step) — properties
  `agent-microvm` deliberately does not offer.
- Duplicated logic that could drift: the enabled-agent-package set is computed
  twice with separately-maintained comments pointing at each other
  (`programs.herdr.nix:80-90` mirrors `agentPackagesByFlag` from
  `myconfig.ai.gvisor-agent-sandbox/default.nix`, itself a *third* place this
  set is derived), and the herdr rationale text is duplicated across
  `agent-qemu-herdr.README.md` and `agent-microvm.md` rather than shared from
  one source.

## Recommendation

- **Use `agent-qemu-herdr`** for the common case: quick, ad hoc, "I want a
  kernel boundary around a multi-agent session in this one project
  directory, right now, with no host setup." It is available everywhere
  agentic coding is enabled and needs no `sudo`, no prebuilt pool, no bridge.
- **Use `agent-microvm run --attach --agent herdr` (or `submit` for the other
  agents)** when the task is untrusted enough to want the model API key kept
  off the guest entirely, when you want the workspace fully isolated from the
  live checkout (batch/autonomous runs, parallel agents, results collected
  later), or when you're already on a host that has the tier provisioned
  (currently f13 only).
- **Do not unify or deprecate either.** They intentionally sit at different
  rungs of the documented sandboxing ladder
  (`modules/myconfig.ai/docs/README.md:8-19`): `agent-qemu-herdr` trades
  security depth for zero-config, always-available convenience; `agent-microvm`
  trades convenience (explicit per-host opt-in, a bridge, a prebuilt pool) for
  a materially stronger boundary and unattended execution. Collapsing them
  would either force every agentic host to provision the microVM fleet (bridge,
  firewall chains, resource classes) just to get a disposable interactive
  shell, or weaken `agent-microvm`'s no-credentials-in-guest / no-shared-store
  guarantees down to tier 3's level. Both are explicitly against the repo's own
  "ladder" design (`modules/myconfig.ai/docs/README.md:24-25`: "the tiers are largely orthogonal and
  compose").
- **Do** deduplicate the *incidental* overlap: the herdr-rationale prose is
  copy-pasted across `agent-qemu-herdr.README.md` and `agent-microvm.md`, and
  the "which coding-agent CLIs get baked in" logic is independently
  re-derived in three places (`programs.herdr.nix`, `agents.nix`, and
  `myconfig.ai.gvisor-agent-sandbox/default.nix`'s `agentPackagesByFlag`).
  None of this is a behavior recommendation — it is a documentation/DRY
  cleanup that would not change either tier's isolation properties.

## Uncertainty notes

- No measured launch-to-ready latency or idle resource numbers exist in this
  checkout for either tier on real KVM; both documents mark this explicitly
  (see [Startup cost](#startup-cost-resource-overhead-latency)). Any relative
  speed claim beyond "tier 3 does a `nix build` per invocation, tier 4 does
  not" would be speculation.
- This document reflects the repository state at review time; `enabledAgents`
  defaults to "every declared agent" when unset, so a host that sets
  `myconfig.ai.microvm.enable = true` without pinning `enabledAgents` would
  also get herdr — f13 pins it explicitly instead.
