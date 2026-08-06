# Lightweight `myconfig.ai.microvm` Implementation Plan

## Implementation status

| Phase | Status | Notes |
| --- | --- | --- |
| 0 — baseline and measurement | **done for everything that does not need KVM** | Behaviour-preserving refactors are verified with the repo's `nix eval` snapshot/diff workflow (`AGENTS.md`). The machine-readable benchmark the phase asks for now exists: `tests/measure-boot.sh` emits ONE JSON document with closure sizes (`nix path-info -S` of every slot's `system.build.toplevel` and `microvm.declaredRunner`), generated guest unit/socket counts, virtiofs share counts, guest network-interface counts and host helper units per VM; `--static-only` needs neither root nor `/dev/kvm`. Recorded before/after numbers (baseline `ca309d221b` vs HEAD, and the Codex-only `tap` vs `vsock` shapes) are in [runtime validation → Measurements](./agent-microvm-runtime-validation.md#measurements-plan-phase-0--the-phase-6-acceptance-numbers). STILL PENDING a real-KVM run (deliberately NOT estimated): launch-to-ready latency, idle RSS per slot, running host/guest process counts, warm build time. |
| 1 — opt-in lightweight profile | **done, then COLLAPSED** | Landed as `myconfig.ai.microvm.profile = "full" \| "lite"` (table in the former `../profiles.nix`). The compatibility boundary has since been REMOVED together with the `full` path: the `lite` values (pinned `microvm.optimize.enable` + `storeDiskType = "erofs"`) are now the unconditional behaviour and there is no `profile` option. Test: `checks.microvm-eval-guest-shape`. See [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path). |
| 2 — build only selected agents | **done** | `myconfig.ai.microvm.enabledAgents`; the module-wide default is `null` = EVERY declared agent (the profile-supplied `[ "codex" ]` default died with the profile table — see the collapse section for the closure trade-off). The selection is applied ONCE in `../agents.nix`, so guest closure, `agent-run`, batch dispatch, launcher validation/help, workmux registrations and agent-state paths all follow. Test: `checks.microvm-eval-enabled-agents`. |
| 3 — runtime config staging | **done**, unconditional | `myconfig.ai.microvm.configSeed` (`../config-seed.nix`) stages an ALLOWLISTED, root-owned copy of the host agent configuration per launch; the guest sees it through a per-slot READ-ONLY virtiofs share and a root oneshot copies it into the disposable `/home/agent` before sshd, the batch controller and the agent-state linker. The allowlist is the SELECTED agents' new registry field `configPaths` plus `configSeed.extraPaths`. Guest home-manager activation is gone (the module no longer contains it). The credential denylist is applied to a path's own name AND to its RESOLVED target, the staged tree is root-only (0500/0400), the manifest stays outside the share, and the staged paths must be disjoint from the persisted agent-state directories. Tests: `checks.microvm-config-seed` (eval/build) plus `runtime-validation.sh --section seed` (root, enforcement). |
| 4 — consolidate writable shares | **done**, unconditional | `myconfig.ai.microvm.session` (`../session.nix`) is the ONE source of truth for the per-session tree: `<runtimeRoot>/sessions/<slot>/` is ONE writable virtiofs share (`workspace/`, `input/`, `controller/`, `worker/`, `worker-logs/`, `state/`) and `<runtimeRoot>/sessions-ro/<slot>/` ONE read-only share (`hostkeys/`, `config-seed/`). `../job.nix`, `../state.nix`, `../config-seed.nix`, `../hostkeys.nix`, the host tmpfiles rules, the generated pre-launch verifier `agent-microvm-verify-session` and `../launcher.nix` all DERIVE from its layout table — there is exactly one implementation. Trust boundaries are unchanged (ownership + modes, passed through by virtiofsd); the guest gets `/workspace` as a bind mount of the session tree. Test: `checks.microvm-session-tree`. |
| 5 — split interactive/batch | **done**, opt-in narrowing | `myconfig.ai.microvm.capabilities` (a SET, default `[ "interactive" "batch" ]` = today's behaviour) selects which halves a host's guests carry. The decision lives in `../session.nix`'s layout table (per-capability entries) plus one resolved module arg (`agentCapabilities`); `job.nix`, `hostkeys.nix`, `guest.nix`, `workmux.nix` and the launcher each apply it in the ONE place that already owns the concern. A narrowing REMOVES units, guest programs, session subdirectories, tmpfiles rules and launcher subcommands. `enableSsh` is rejected without `interactive`, and `agent-microvm capabilities` reports the set machine-readably on every host. Test: `checks.microvm-capabilities` (eval + BUILD of the default, interactive-only and batch-only guest closures and runners). NOT done: `mode`-specific closure MEASUREMENTS (same tier as phase 0), rendering the launcher's batch CODE out of an interactive-only host, and RUNTIME validation of a batch-only host beyond `--section seed` (its transport is ssh; phase 6 is the enabler) — see the deviations. |
| 6 — VSOCK transport | **done, including the LITERAL proxy transport** | `myconfig.ai.microvm.capabilities` gains a third token `vsock` (default OFF, so a default host's guest closure is byte-for-byte unchanged — verified with the AGENTS.md snapshot/diff). It buys TWO things over the ONE guest shape, both resolved from ONE decision and neither a fork. (1) A VSOCK CONTROL channel (the `sshd-vsock@` unit, REUSED from upstream `microvm.vsock.cid` + NixOS' systemd-ssh-generator), which closes the phase-5 coverage hole: a batch-only host has no TCP sshd but can still be driven by `agent-microvm ssh` and the runtime-validation suite. (2) Together with `networkProfile = "proxy-only"`, the VSOCK MODEL TRANSPORT the plan literally sketched: the guest has **no network interface at all** (`microvm.interfaces = [ ]`, no static IP, no default route, no networkd, no dhcpcd), the model API travels `guest 127.0.0.1:<litellmPort>` → AF_VSOCK CID 2 → a PER-VM host forwarder → `127.0.0.1:<litellmPort>`, and the host builds no bridge, no TAP, no `AGENT_MICROVM_*` chain and no bridge-only socket. The transport decision lives in `../network-profiles.nix` (a transport table of positive capability flags + `resolveTransport`), is resolved ONCE in `../default.nix` (`agentNetwork.transport` / `.transportCaps` / `.tapSshUsable`) and is applied by each module in the ONE place that already owns the concern — `network.nix` (bridge/firewall/socket vs. per-VM forwarder), `guest.nix` (interfaces, networkd, the guest bridge, the unreachable TCP sshd), `session.nix` (which sshd the host-key mount ordering belongs to), `hostkeys.nix` (which `known_hosts` addresses exist) and `launcher.nix` (`SSH_ENABLED`, the doctor sections, the preflight endpoint). No consumer tests a transport NAME. SECURITY: strictly stronger than `tap` + `proxy-only` — invariant 6 becomes an ABSENT DEVICE instead of a firewall verdict, the host loses the bridge/TAP/NAT surface, the forwarder is one destination-fixed listener per VM (`root:kvm 0660` inside that VM's state dir, `IPAddressAllow=localhost` + `IPAddressDeny=any`, `DynamicUser`), and the layer-2 attack class (ARP spoofing, IP impersonation, co-resident scanning) ceases to exist. Tests: `checks.microvm-vsock-transport` (40 eval assertions with a positive control for every removal, plus a BUILT interactive-vsock guest closure/runner and greps of the built runner/launcher/host-key generator) and `checks.microvm-capabilities` (the batch+vsock shape); `microvm-rtv-dispatch` proves the suite READS the transport and hard-aborts on an absent/unknown one. NOT done: a real-KVM run of the AF_VSOCK model path (see phase 0's pending numbers and the recorded deviations). |
| 7 — clone/startup optimisation | partially done | Clone: `git clone --local --no-hardlinks` with a `--no-local` fallback plus an explicit `objects/info/alternates` check (≈10× faster on this repo: 0.6 s vs 5 s, measured by hand — both variants produce a fully independent clone). Readiness: exponential-backoff SSH polling (250 ms → 2 s) under the unchanged 90 s ceiling, replacing the fixed 3 s interval. NOT done: readiness as a positive protocol signal (needs phases 3/6) and the install-unit generation guard (see deviations). |
| 8 — minimize guest closure | **done**, unconditional | Every guest builds the documented minimal CLI toolset, a plain bash login shell (no fish) and drops NixOS' `environment.defaultPackages`; per-agent `extraPackages` in the registry keeps agent-specific runtimes tied to the selection. Test: `checks.microvm-eval-guest-shape`. |
| 9 — testing | incremental | Each landed phase adds eval checks to `tests/microvm.nix`; the VM/adversarial tiers of this phase remain out of CI (see `docs/agent-microvm-runtime-validation.md`). |
| 10 — documentation and rollout | incremental | `docs/agent-microvm.md` documents every landed option. |

### Collapsing the two profiles into one path

Phases 1–4 and 8 were landed *alongside* the historical behaviour, gated behind
`profile = "lite"`, so each phase could be reviewed against a byte-identical
`full` host. That was the right way to land them and the wrong way to leave them:
carrying both shapes meant two share layouts, two provisioning mechanisms, two
guest toolsets, ~20 `if session.enable then … else …` sites, two launcher
spellings and a test suite that asserted both. The compatibility layer has now
been **deleted**, which is what phase 4's own task list demanded.

What was removed:

- `../profiles.nix`, the `myconfig.ai.microvm.profile` option, the `agentProfile`
  module argument and every consumer branch on it. The `lite` values are the
  unconditional behaviour: pinned `microvm.optimize.enable = true` +
  `microvm.storeDiskType = "erofs"`, the minimal guest toolset with a bash login
  shell and `environment.defaultPackages = [ ]`, config-seed staging, and the
  consolidated session share.
- `../guest-home.nix` in full (guest home-manager provisioning), the
  `guestDotfiles.*` options, the `mkGuestHome` module argument, the
  home-manager import in `../guest.nix` and its guest tmpfiles
  profile-directory workaround. It is superseded by config-seed staging.
- `guestFullPackages` and the fish login-shell path in `../guest.nix`.
- The `session.enable` and `configSeed.enable` options: both mechanisms are
  unconditional now, so the options could only ever have selected a shape that
  no longer exists (with `guest-home.nix` gone, `configSeed.enable = false`
  would mean "a guest with no configuration at all").
- The four-share layout: every non-session share declaration plus the per-layout
  branches in `../job.nix`, `../state.nix`, `../hostkeys.nix`,
  `../config-seed.nix`, `../launcher.nix` (including the conditional-fragment
  machinery that existed only to keep the `full` launcher byte-identical) and
  the `LAYOUT` detection block of `../runtime-validation.sh`.
- The tests that only described `full`: `checks.microvm-eval-workspace-share`
  (the four-share regression guard), the "the profile default stays full"
  assertions, and every `full`-side half of the phase 3/4/8 checks.
  `checks.microvm-eval-lite-profile` became `checks.microvm-eval-guest-shape`
  and now asserts the shape of the REFERENCE host's guest.

What was deliberately KEPT:

- the launcher's scan for per-slot residue under the PRE-consolidation roots
  (`<stateRoot>/<slot>/workspace`, `<runtimeRoot>/jobs`, `.../hostkeys`,
  `.../state/slots`): those branches find nothing on a fresh host, but f13 is
  migrating FROM that layout and reporting its residue is exactly what
  `recover --prune-foreign` is for;
- the fish quoting dialect of `../runtime-validation.sh`' guest command
  transport: the dialect is DETECTED per slot (today's guest answers `posix`),
  and the detection is what keeps a mangled transport from turning guest-side
  denials into vacuous passes;
- every security invariant. Nothing in the collapse weakens one: the removed code
  was the *weaker* of the two paths in every case where they differed (four
  writable shares instead of one, a build-time guest home instead of a staged
  one, a larger guest closure).

The module-wide `enabledAgents` default was decided deliberately: `null` = EVERY
declared agent, i.e. the historical behaviour, because the selection is
operator-visible (`myconfig.ai.workmux` registers one `microvm-<agent>` pane per
selected agent, and `agent-microvm run|submit --agent <name>` only accepts a
selected one). The trade-off is the guest closure: every declared agent's runtime
is baked into every slot image, so a host that wants the small closure the plan
aims at must name its agents. `hosts/host.f13/ai.f13.nix` therefore states its
five agents explicitly rather than inheriting the default, so trimming the list
later is a visible, reviewable host change.

**Verification.** The collapse is a deliberate behaviour change for f13, so
byte-identity with the *default* f13 was not the goal. What was verified is:
evaluated `test-f13` AFTER the collapse == evaluated `test-f13` BEFORE it with
`profile = mkForce "lite"` plus the explicit `enabledAgents` the host now
carries. The compared slice is every VM's `system.build.toplevel` and
`microvm.declaredRunner` drvPath, every VM's full `microvm.shares` list, the
guest `fileSystems` entries, `networking.firewall.extraCommands`,
`builtins.attrNames systemd.services`, `systemd.tmpfiles.rules` and
`environment.systemPackages` drvPaths. The diff is empty except the two
git-revision artefacts (`nixos-version`, the `myconfig-commit` tmpfiles link)
and the two host shell scripts (`agent-microvm`, `agent-microvm-stage-config`),
whose only differences are COMMENT text — verified by diffing the generated
script bodies. The guest closures are byte-identical.

### Follow-up: the deprecated migration shims are gone too

A separate commit removed the option shims that the collapse left without a
consumer: `slotCount` / `defaultVcpu` / `defaultMemoryMiB` (the pre-`resourceClasses`
single-class spelling, its synthesized class table, its ambiguity assertion and
its deprecation warning) and `allowPublicInternet` / `allowPrivateNetworks` /
`allowInterVmTraffic` (the pre-`networkProfile` booleans, their translate /
reject / warn migration and the `options`-based "was this explicitly defined?"
machinery). No host in this repository sets any of them, so a host that still
does now gets an **unknown option** error naming the option — strictly louder
than the warning or assertion it replaces, and impossible to mistake for a
capability that exists. The evaluated-slice diff for `test-f13` is byte-identical
across that commit (its `resourceClasses` is explicit, so the synthesized table
was never used).

### Recorded deviations

- **Phase 4, the read-only share is NOT folded into the writable tree**: the
  plan's target tree puts `config-seed/` inside the ONE writable session share
  at mode `0555`. NOT done. The staged configuration is host-decided input the
  guest must not be able to modify (invariant 7) and must not be able to read
  beyond the copy the guest root seeder hands it (invariant 8, phase 3's
  recorded deviation), so it stays in a share virtiofsd mounts `--readonly`,
  root-owned `0500`/`0400` — strictly stronger than a mode inside a writable
  share. The SSH private host keys stay out of the writable tree for the same
  reason (the plan says so itself). Since BOTH read-only payloads are
  root-owned, single-purpose and per-slot, they share ONE read-only share (two
  subdirectories) rather than one share each, so the acceptance criterion "one
  writable share plus AT MOST ONE read-only share" holds exactly. A pre-launch
  check (`agent-microvm-verify-session`) additionally REFUSES to start a slot
  whose writable tree contains SSH host-key material, so a future refactor
  cannot regress this silently.
- **Phase 4, the staging manifest moved**: the read-only share source is the
  per-slot READ-ONLY directory (it has to cover both `hostkeys/` and
  `config-seed/`), so the manifest can no longer be the payload's sibling
  without becoming visible to the guest. It now lives in the host-only
  `<runtimeRoot>/config-seed/<slot>/manifest.json`, outside every share — the
  invariant from phase 3 ("the manifest names the host home and every skipped,
  credential-shaped host file name") is preserved, its location is not.
- **Phase 4, per-directory MODES are unchanged from the four-share layout**:
  the plan's tree sketches `input/` and `config-seed/` as `0555` and
  `worker/`/`state/` as `0700`. NOT adopted. The effective modes stay exactly
  what `job.nix`/`state.nix`/`config-seed.nix` already used and what the guest's
  own `agent-job-assert-paths` validates (`input/` `0755` root, `controller/`
  `0700` root, `worker/`/`state/` `0755` agent, `worker-logs/` `0755` root,
  staged tree `0500`/`0400` root), because this phase must not change WHO may
  do WHAT — only how many shares carry it. Every mode is asserted
  group/other-non-writable (and root-only where the data is confidential) by a
  policy function the module applies to its own table at eval time, and the
  same table is what the tests check.
- **Phase 4, `/workspace` is a guest BIND MOUNT, not a symlink**: the plan
  allows "bind mount or symlink". A symlink would make `agent-run`'s
  `findmnt -n /workspace` check fail (it is a mount-point check, deliberately),
  and a symlink into a virtiofs share is also weaker evidence that the share is
  actually mounted. The guest therefore declares a `bind` mount with
  `x-systemd.requires-mounts-for=/run/agent-session`.
- **Phase 4, the batch/worker paths did NOT need updating**: the plan asks to
  "update batch controller and worker paths". They are all derived from
  `job.nix`'s single `paths` attrset, which now takes its root and its
  subdirectory names from the session layout — so the guest controller, the
  worker unit, the assertion helper and the host launcher followed without a
  second implementation. The only genuinely dual-path code this phase added was
  per-profile PATH/MODE resolution (`if session.enable then ... else ...`) plus
  the conditionally rendered launcher fragments that kept the `full` launcher
  byte-identical. SUPERSEDED: all of it was deleted with the `full` path (see
  [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path)).
- **Phase 4, virtiofsd process-count measurement**: recorded STRUCTURALLY (the
  eval check asserts the lite guest declares exactly one writable and at most
  one read-only share, i.e. two virtiofsd instances instead of four or five)
  rather than as a measured process count, for the same reason phase 0's
  benchmark is deferred: counting host processes needs a booted guest on a KVM
  host.
- **Phase 4, the `full` profile kept four shares — RESOLVED**: when the phase
  landed, no compatibility decision had been taken to consolidate the existing
  behaviour, so the consolidated layout was `lite`-only and gated by
  `session.enable`, with the evaluated-slice diff from `AGENTS.md` proving the
  `full` host byte-identical. That was explicitly a *temporary* state, and the
  four-share path has since been deleted together with the rest of the
  compatibility layer — see
  [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path).
- **Phase 4 review follow-up, the pre-launch host-key sweep PRUNES the two
  bind-mount points**: `agent-microvm-verify-session` refuses to launch a slot
  whose WRITABLE tree contains `ssh_host_*`, but it no longer descends into
  `workspace/` and `state/`. Those two are the user's own git clone and the
  task's persisted agent state — content the host never writes key material
  into and the guest agent may create freely. Without the prune an ordinary
  repository file (`somedir/ssh_host_ed25519_key.pub` is entirely plausible in a
  NixOS/agenix clone, and a `.pub` is not secret) would refuse the launch, and a
  hostile agent could deny every future launch of its slot by creating one. All
  root-owned directories — the only places host-written key material can land —
  are still swept, and the eval-time policy independently forbids a host-key
  directory inside the writable tree.
- **Phase 4 review follow-up, teardown additionally proves the whole subtree is
  mount-free**: `clear_session` unmounts and verifies the two binds it knows
  about and then refuses the `rm -rf` if `findmnt` still reports ANY target at
  or below the session root. `rm --one-file-system` is no protection here (a
  bind of a same-filesystem directory shares `st_dev`), and a `rm -rf` that
  descends through an unexpected mount would delete the user's clone. This is
  stronger than the plan's “cleanup removes the complete per-session tree” and
  is kept.
- **Phase 4 review follow-up, `prepare_session` never `die`s**: it returns
  non-zero and the call sites decide. It is reachable from `clear_session`,
  which runs inside the launcher's EXIT trap; an `exit` there would abort the
  trap before the allocation marker is removed and reserve the slot forever.
- **Phase 4 review follow-up, the real-KVM suite detects the share layout**:
  `../runtime-validation.sh` derived its job/bind/staging paths from the
  four-share layout only. Since almost every check there asserts “nothing
  forbidden exists under `$X`”, running it on a `lite` host made the `seed`
  section's seven enforcement checks and the `lifecycle` stale-bind detector
  pass VACUOUSLY. The suite gained per-layout constants plus a detection block;
  with the `full` path deleted the detection is gone again and the constants are
  simply the session layout, but the `seed` section still hard-`FAIL`s if the
  staged payload does not exist after staging — that guard is what makes the
  vacuity structurally impossible.
- **Phase 4 review follow-up, `agent-job-worker` on a stdin-only agent set**:
  the worker reads the prompt TEXT into `prompt` for the registry entries that
  take `%PROMPT%` as an argv token. `lite` selects only the stdin-driven
  `codex`, so the generated dispatch reads it nowhere and
  `writeShellApplication`'s shellcheck gate failed the LITE guest build with
  SC2034 — undetected because no check forced the lite guest's toplevel. The
  suppression is now emitted only on hosts where the variable is genuinely
  unused (`agentRegistry.batchUsesPromptText`) — which today means "on a host
  whose selection happens to be stdin-only" — and `microvm-session-tree` forces
  the guest's `system.build.toplevel` and `microvm.declaredRunner`.

- **Phase 3, `rsync`**: the plan suggests `rsync --archive --copy-links` on both
  sides. NOT used. The HOST stager walks the allowlist with `find -L` and copies
  file-by-file with `install`, because every file needs INDIVIDUAL decisions
  (denylist per path component, per-file symlink-escape check, setuid/type/size
  checks) that an rsync invocation cannot express — and because a per-file loop
  fails closed while an rsync filter ruleset fails open. The GUEST seeder uses
  `cp -R --dereference` + `chown`/`chmod` rather than rsync so the phase-8
  minimal guest closure does not have to regain rsync.
- **Phase 3, share placement**: the plan's target tree puts `config-seed/` in
  the ONE writable session share (that consolidation is phase 4). Until then it
  is its OWN per-slot share, mounted READ-ONLY — strictly stronger than the
  plan's `0555` directory inside a writable share, and modelled on the existing
  read-only host-key share. Phase 4 must keep the read-only property when it
  consolidates.
- **Phase 3, staging was rendered CONDITIONALLY into the launcher —
  RESOLVED**: the host-side staging code existed in `agent-microvm` only when
  `configSeed.enable` was on, so that a `full`-profile host kept a byte-identical
  launcher derivation (the launcher is a host `environment.systemPackages` entry
  and is covered by the evaluated-slice diff from `AGENTS.md`). With the `full`
  path deleted the staging code is unconditional; the fragment-rendering helper
  remains only as a way to define long blocks next to the data they come from.
- **Phase 3, `hermes` stages nothing**: the plan asks for per-agent
  `configPaths`. Hermes keeps `config.yaml`, `.env`, `auth.json`, `state.db` and
  `sessions/` in ONE root (`~/.hermes`) and even `config.yaml` carries a
  provider key, so no path of it is safe to stage. Its `configPaths` is
  deliberately empty and the guest gets its endpoint from `guestEnvironment`
  instead. Same reasoning trimmed the other agents to exact files/directories
  (never `.codex/`, `.pi/` or `.config/opencode/` as a whole).
- **Phase 3, readiness/boot measurement**: the acceptance criterion "boot and
  closure measurements show the effect of removing guest Home Manager" is
  recorded STRUCTURALLY (the check asserts the lite guest has no
  `home-manager-agent.service` and none of home-manager's guest tmpfiles rules)
  for the same reason phase 0's benchmark is deferred: a byte/latency budget
  needs a KVM host and belongs to the out-of-CI runtime-validation tier.
- **Phase 3, "editing a host file affects the next launch"**: proven only
  STRUCTURALLY in CI (the stager reads the live host home at launch time). Its
  actual behaviour — allowlisted file staged, benignly NAMED symlink onto a
  credential refused, denylisted name inside an allowlisted directory skipped,
  symlink outside the host home rejected, FIFO/setuid skipped, budgets and
  cleanup enforced — is now a repeatable, root-run section of the
  runtime-validation tier (`--section seed`, `../runtime-validation.sh`), not a
  hand-run experiment. A Nix-sandbox functional test remains impossible: the
  stager writes a root-owned tree. An end-to-end proof (guest home actually
  seeded) still needs a booted guest, i.e. the same tier.
- **Phase 3, manifest placement and modes**: the plan does not say where the
  manifest lives. It is the payload directory's host-side SIBLING and the share
  source is the payload only, so the manifest — which names the host home and
  every skipped, credential-shaped host file name — is never visible to the
  untrusted guest. The staged tree is `0500`/`0400` rather than the plan's
  `0555`: virtiofsd and the guest seeder both run as root, so nothing
  unprivileged needs read access on either side, and other local host users
  cannot read the operator's staged configuration.
- **Phase 3, denylist on RESOLVED targets**: the plan only asks for a
  credential denylist. Applying it to the path a file is reached UNDER is not
  enough — one benignly named symlink in the host home
  (`.codex/config.toml` -> `.codex/auth.json`, `.agents/skills/x` -> `~/.ssh`)
  would otherwise stage exactly what invariant 8 forbids. The stager therefore
  also denies a resolved target whose real name matches, for entries, files and
  subdirectories alike. What is deliberately NOT covered (documented in
  `agent-microvm-security-model.md`): hardlinks and TOCTOU, both of which need
  pre-existing write access to the TRUSTED host home.

- **Phase 1, `persistentAgentState.enable = false`**: there is no such option.
  Agent-state persistence is already opt-in *per run*
  (`agent-microvm run|submit --persist-agent-state`, see `../state.nix`), i.e.
  the plan's intended lite default is the existing behaviour of every profile.
  Nothing to configure.
- **Phase 1, `networkProfile = "proxy-only"`**: already the module-wide secure
  default, so the `lite` profile does not restate it.
- **Phase 2, per-agent registry restructuring**: the registry already has the
  shape the plan asks for (`package`, `executable`, `interactiveArgs`,
  `batchArgs`, `guestEnvironment`, `persistentState`), so only the *selection*
  was added. `configPaths` / `statePaths` / `extraPackages` are deferred to the
  phases that need them (3 and 8).
- **Phase 2, closure measurement**: recorded as a *structural* check
  (`microvm-eval-enabled-agents` asserts the deselected agents' store paths are
  absent from the guest `environment.systemPackages`) rather than as a byte
  count, for the same reason phase 0's benchmark is deferred.
- **Phase 8, "every package has a documented consumer"**: documented as a
  per-package rationale comment above `guestCommonPackages` in `../guest.nix`,
  and locked down by an eval check listing the same set. NixOS' own
  `requiredPackages` (coreutils-full, curl, openssh, which, …) is load-bearing
  for a bootable system and is therefore neither removed nor asserted absent;
  only the module's discretionary additions and
  `environment.defaultPackages` are minimized.
- **Phase 8, closure-size regression check**: expressed structurally (asserted
  package membership) rather than as a byte budget — a size assertion would
  need a KVM/build tier and would churn with every nixpkgs bump.
- **Phase 7, install-unit restart guard**: NOT implemented. Comparing
  `<stateRoot>/<slot>/current` with the expected runner path would require the
  launcher (a host `systemPackages` entry, reachable from the host home-manager
  config) to reference every guest's `declaredRunner`, and the guest config in
  turn copies already-evaluated host home-manager file entries
  (the removed `guest-home.nix`) — a plausible infinite-recursion loop. The
  current unconditional `systemctl restart install-microvm-<slot>.service` is an
  idempotent symlink relink costing milliseconds, so the guard was deferred until
  phase 3 removed the host→guest home-manager coupling.
  UPDATE (phase 3 landed, and the `full` path has since been deleted): the
  coupling is gone for EVERY guest, so nothing can recurse any more and the guard
  is now genuinely implementable. Still NOT implemented after phase 5 either:
  that phase reshapes the guest unit set by REMOVING units, not by changing what
  the install unit installs, so the guard is unrelated work and keeps its own
  commit.
- **Phase 7, readiness definition**: the extended readiness criteria
  (config staging finished, proxy forwarding healthy, agent executable present)
  presuppose phases 3 and 6; only the polling *strategy* was changed here.
- **Phase 5, the selector is a SET named `capabilities`, not a 3-valued `mode`**
  (RESOLVED — this entry previously recorded the requirement, written while
  `profile` still existed). Implemented as
  `myconfig.ai.microvm.capabilities = [ "interactive" "batch" ]` (the default),
  because a `combined` enum value would be a compatibility profile in disguise:
  every consumer would branch on three values instead of testing two
  independent capabilities, which is exactly the cross-product the collapse
  removed. The three consequences that entry predicted were all hit and are
  all resolved:
  1. the per-mode decision lives IN `../session.nix`'s layout table (each entry
     declares the capabilities that need it) — no consumer gained an `if`; the
     tmpfiles rules, the verifier, the launcher's tree preparation, the guest
     mounts and the tests follow the filtered table;
  2. the batch-capable-agent assertion in `../default.nix` is now
     `!batch || batchNames != [ ]`, so an interactive-only host may select an
     interactive-only agent set;
  3. `enableSsh` vs the capability set is reconciled as
     **meaningless-and-rejected**: `enableSsh` stays the authoritative switch
     for the SSH server *within* the `interactive` capability (an interactive
     host may run console-only, which the test variants exercise), and
     `enableSsh = true` without `interactive` is an eval error naming both
     options. It is deliberately NOT silently forced to `false`: the server, the
     host identity, the `known_hosts` database and the launcher's
     `ssh`/`run --attach` paths would then all disappear behind an option that
     still read `true`.
- **Phase 5, the LAUNCHER keeps one shape — its batch code is refused, not
  removed**: a narrowed host gets a `require_capability` membership check at the
  top of the four capability-specific subcommands (and an honest `doctor`
  host-key section), so `agent-microvm submit` on an interactive-only host fails
  immediately with a message naming `myconfig.ai.microvm.capabilities`. What is
  NOT done is rendering `cmd_submit`, `cmd_cancel`, `prepare_job`, the result
  verifier reference and the archive helpers OUT of the script: that would mean
  ~15 conditionally rendered fragments and two launcher shapes — precisely the
  dual-path machinery the collapse deleted. The consequence is explicit and
  accepted: an interactive-only host still has the (unused) result-verifier
  store path in its launcher's closure, while all host STATE of the capability
  (the `<runtimeRoot>/results` archive, the batch subdirectories of every
  session tree) and every guest-side component are genuinely gone.
- **Phase 5, the REFUSAL fragments are rendered only where they can fire, but the
  capability SET is unconditional** (REVISED by the phase-5 review). The
  `require_capability` guard and the batch-only `doctor`/`usage` variants are
  still emitted only on a host that actually lacks a capability — on a host with
  both they would be unreachable code, the same rule `../job.nix` applies to its
  `promptUnusedSuppression`. What is now rendered on EVERY host is
  `SELECTED_CAPABILITIES` / `DECLARED_CAPABILITIES` plus an
  `agent-microvm capabilities` subcommand that prints them machine-readably.
  CONSEQUENCE, accepted deliberately: the launcher derivation of a default host is
  **no longer byte-identical** to the pre-phase-5 one, so the evaluated-slice diff
  of `test-f13` now shows the launcher (and every host path that references it:
  `environment.systemPackages`, the workmux commands, `security.sudo` rules)
  changing, in addition to the two git-revision artefacts. That was the price of
  the alternative: `../runtime-validation.sh` used to DETECT the set by grepping
  the launcher's English refusal message and defaulted to "this host has
  everything", i.e. it failed OPEN — rewording that `die`, a `require_root`
  failure or a phase-6 transport change would silently restore the vacuous passes
  the whole dispatch exists to prevent. A machine-readable, always-present answer
  that the suite HARD-ABORTS on when it cannot parse it is worth more than the
  byte-identity proof of one derivation, and the *guest* closures, shares,
  fileSystems, units and tmpfiles rules of a default host are still unchanged.
- **Phase 5, a batch-only host cannot be RUNTIME-validated beyond `seed`**: seven
  of the eight sections of `../runtime-validation.sh` need the `interactive`
  capability, because the suite's guest transport IS `agent-microvm ssh`. A
  `capabilities = [ "batch" ]` host therefore runs exactly one section. This is
  honest (the alternative is vacuous passes) but it is a real coverage hole in the
  configuration with the largest untrusted-workload surface, and it is recorded
  here rather than left implicit. Mitigation today: the eval/build tier
  (`checks.microvm-capabilities` builds that guest's closure and runner and
  asserts every removal against the evaluated config) plus `--section seed`.
  ENABLER: phase 6 (VSOCK) — a console/vsock `guest` transport that does not
  require sshd is what would let `boot`, `l2`, `creds`, `malrepo` and the
  lifecycle sections run on such a host, and that is now one of phase 6's
  motivations.
  UPDATE (phase 6 landed): CLOSED. A `capabilities = [ "batch" "vsock" ]` host
  drives the guest over the VSOCK `sshd-vsock@` control channel and runs ALL
  eight sections; only a batch-only-WITHOUT-`vsock` host still narrows to
  `--section seed`. See the phase-6 section and its recorded deviations. UPDATE 2
  (the literal model transport landed): the suite additionally READS the resolved
  model transport and, on a `vsock`-transport host, replaces the TAP-specific
  parts of `net`/`l2` with positive assertions that the guest has no interface —
  so closing the hole did not open a vacuity one.
- **Phase 5, `capabilities = null` in the layout table means "unconditional"**
  (review follow-up). The first implementation spelled it
  `bothCapabilities = agentCapabilities.declared`, i.e. "any declared
  capability" — correct only while exactly two tokens exist. A third token (a
  phase-6 `vsock`, say) would have silently turned every entry that meant
  "interactive AND batch need this" into "a vsock-only host gets it too". The
  table now uses `null` for unconditional entries and strictly ENUMERATIVE lists
  otherwise (a list never refers to `declared`), and an assertion rejects an empty
  list or an undeclared token in one.
- **Phase 5, the per-capability booleans are GENERATED from the token list**
  (review follow-up): `agentCapabilities = lib.genAttrs declaredCapabilities …`
  instead of a second hand-written attrset. The two lists could previously
  disagree, and `../launcher.nix`'s `missingCapabilities` filter would THROW on a
  token that had no matching attribute — the exact failure a phase-6 capability
  would have hit first.
- **Phase 5, stale subdirectories of a PREVIOUS selection are swept and refused**
  (review follow-up). Narrowing removes the tmpfiles rules and the verifier lines
  for the other capability's subdirectories, but nothing removed the
  DIRECTORIES: after an unclean shutdown followed by a narrowing rebuild, a
  root-owned `input/` and `worker-logs/` survived inside the ONE writable share,
  exported to the guest, with no rule covering them. The launcher's
  `prepare_session` now sweeps every top-level entry the (filtered) table does not
  declare — refusing to remove one with a live mount underneath instead of
  `rm -rf`ing through it — and the generated verifier `die`s on any that survives.
  Fail-closed and self-healing, and it also closes the pre-existing "operator left
  something in the tree" case.
- **Phase 5, a batch-only guest has no SSH DAEMON, but still ships the openssh
  BINARIES**: `cfg.enableSsh` no longer adds `openssh` to
  `environment.systemPackages` (`../guest.nix`), and the units, the host identity
  and `services.openssh.enable` are all gone — but NixOS'
  `environment.requiredPackages` still provides `ssh`/`scp`, and that set is
  load-bearing for a bootable system (see the phase-8 deviation above). The
  acceptance criterion "a batch-mode guest has no SSH daemon" is therefore a
  unit/config claim, NOT a closure-size claim.
- **Phase 5, `Before=` on absent units is left alone**: `../config-seed.nix`,
  `../state.nix` and `../guest-model-config.nix` order themselves before
  `sshd.service` / `agent-job-controller.service` unconditionally. On a narrowed
  host one of those units does not exist, which systemd treats as a no-op for a
  pure ordering dependency. Making the lists conditional would have added three
  more capability-aware sites for zero behavioural difference; the tests
  therefore assert the absence of the UNITS and of the guest PROGRAMS, never of
  an ordering string.
- **Phase 5, `session.modeOf` reads the FULL layout table**: a directory's
  owner/mode is a policy fact of the layout, independent of whether this host
  creates it, and `../job.nix` bakes those modes into the guest-side
  `agent-job-assert-paths` and the launcher's constants. Only what gets CREATED,
  VERIFIED and MOUNTED comes from the filtered table. The trust POLICY
  (`violationsOf`) is likewise applied to the FULL tables, so weakening an entry
  a host happens not to select still fails evaluation.
- **Phase 5, the registry-dependent assertions read the MODULE ARGUMENT**:
  `../default.nix` now checks `config._module.args.agentRegistry` (which IS the
  registry it defines) instead of its local `let` binding. Behaviour is
  unchanged for every real host — the evaluated-slice diff is empty — and it
  buys two things: the assertions check the instance the guest closure, the
  launcher and the workmux panes are actually built from, and the
  batch-capable-agent guard becomes EXERCISABLE. Every declared agent has
  `batchArgs` today, so the conflict cannot be produced through options at all;
  `tests/microvm.nix` substitutes a registry whose batch subset is empty and
  requires the guard to fire for a `batch` host and to stay silent for an
  interactive-only one.
- **Phase 5, no per-capability closure MEASUREMENT**: the plan's phase-8
  criterion "interactive and batch closures differ appropriately" is recorded
  STRUCTURALLY (asserted unit/package/path absence, plus a real BUILD of both
  narrowed guests) rather than as a byte count, for the same reason phase 0's
  benchmark is deferred.
- **Phase 5, `doctor`'s host-key check still tests the read-only SLOT directory**
  (`$HOSTKEYS_ROOT/<slot>`, i.e. `<runtimeRoot>/sessions-ro/<slot>`) rather than
  its `hostkeys/` subdirectory — a leftover imprecision from phase 4's path
  consolidation. NOT fixed here: it would change the launcher of a default host
  and therefore the evaluated-slice proof of this phase. The batch-only variant
  of that section is honest (it reports the missing capability instead of
  checking anything), and tightening the interactive one belongs in its own
  commit.
- **Phase 1, store pinning**: `microvm.optimize.enable` and
  `microvm.storeDiskType` currently *default* to `true` / `erofs` upstream, so
  pinning them is behaviour-preserving today. It is done anyway (and, since the
  collapse, for every guest) so an upstream default change cannot silently
  deoptimise the guest.

- **Phase 6, the literal proxy-forwarder sketch — RESOLVED, it IS implemented**
  (this entry previously recorded it as deferred). When `vsock` first landed it
  was a CONTROL channel only, ADDITIVE to the unchanged TAP/bridge/firewall, on
  the argument that a VSOCK proxy forwarder ALONGSIDE the TAP one would fork the
  ONE forwarder shape. That argument was answered rather than overruled: the two
  forwarders are not two shapes carried at once but ONE resolved TRANSPORT
  (`../network-profiles.nix`'s `transports` table + `resolveTransport`, resolved
  once in `../default.nix`), and every consumer reads the transport's CAPABILITY
  FLAGS instead of testing a name — the same mechanism the network PROFILE
  already used for "what may the guest reach". A host therefore has exactly one
  forwarder, one launcher, one session layout and one guest shape; what the
  transport decides is whether a network interface exists at all. The literal
  acceptance criteria are now met and asserted with a positive control each ("no
  network interface other than loopback", "no TAP device", "no bridge/firewall
  configuration"). Scope kept narrow on purpose: the model transport switches
  ONLY for `vsock` + `proxy-only`; `offline` (no model API to carry) and the
  insecure profiles (which NEED IP networking) keep the TAP shape.
- **Phase 6, `socat` in the guest, `systemd-socket-proxyd` on the host**. The
  plan asks for "a small, auditable tool" on both sides and the module already
  used `systemd-socket-proxyd` for the TAP forwarder — but socket-proxyd can only
  forward to a `HOST:PORT` or UNIX target, never to AF_VSOCK, so the GUEST side
  (the one that must dial CID 2) is `socat -t 120 ACCEPT-FD:3,fork
  VSOCK-CONNECT:2:<port>`. The guest's `litellm-forwarder.socket` keeps
  `Accept = no` under BOTH transports and there is ONE `litellm-forwarder.service`
  whose only transport-dependent field is the `ExecStart`: socat takes the
  LISTENING fd systemd passes as fd 3 and forks per connection itself, exactly as
  `systemd-socket-proxyd` multiplexes on one fd, so concurrent model streams are
  not capped by systemd's default `MaxConnections=64` and no DynamicUser unit is
  spawned per request. There is deliberately **no `-T` inactivity timeout**: `-T`
  is bidirectional and would tear a connection down mid-transfer, silently killing
  a long prefill, a cold LiteLLM or a slow tool-call turn; `-t` (the half-close
  drain timeout, raised from socat's 0.5 s default) is the only timeout present.
  `socat` is referenced by the unit's ExecStart and is
  deliberately NOT added to `environment.systemPackages`: the untrusted agent
  must not gain a general-purpose socket tool in its PATH. Cost: ≈0.9 MB of guest
  closure (measured), against three guest units, two guest sockets and the whole
  host bridge/firewall surface removed.
- **Phase 6, the host side is a UNIX socket, not an AF_VSOCK listener**. The
  plan's security requirement is "the host listener must validate the expected
  guest CID or use one listener per slot", and it reads as though the host binds
  AF_VSOCK. With cloud-hypervisor it cannot: the guest's VSOCK device is
  implemented in the VMM, so a guest-initiated connection to host CID 2 port N is
  delivered to the Unix socket `<vm state>/notify.vsock_<N>` (the convention
  microvm.nix itself uses for the guest's notify socket, `notify.vsock_8888`).
  The module therefore listens there, ONE socket per VM, `root:kvm 0660` inside
  that VM's own state directory. This satisfies the requirement's second branch
  and is strictly stronger than CID validation would be: there is no shared
  namespace and no CID for a guest to spoof. A consequence worth recording: this
  binds the implementation to the cloud-hypervisor/firecracker mux convention —
  which the module already depended on for the VSOCK control channel
  (`ssh vsock-mux/...`) — and to `stateRoot == microvm.stateDir`, which is
  asserted at eval.
- **Phase 6, `litellmPort = 8888` is rejected under the VSOCK transport**. The
  per-VM forwarder's socket path is `notify.vsock_<litellmPort>`, and microvm.nix
  reserves AF_VSOCK port 8888 (`notify.vsock_8888`) for the guest's systemd
  notify socket. A host that set `litellmPort = 8888` would have the model
  forwarder and the notify bridge fight over one socket path, breaking either
  readiness signalling or the model endpoint — both at runtime, both opaque. New
  eval assertion, pinned by a test.
- **Phase 6, the TCP sshd is suppressed for EVERY host whose interface is gone,
  not just a batch-only one**. The first landing masked `sshd.service` when
  `vsock && !enableSsh`. With the model transport, an INTERACTIVE host
  (`enableSsh = true`) can also have no network interface, and then a TCP sshd
  would listen where nothing can connect. The condition is therefore
  `vsock && !tapSshUsable`, where `agentNetwork.tapSshUsable = enableSsh && the
  transport has a guest interface` — resolved once in `../default.nix` and also
  what `../launcher.nix` renders `SSH_ENABLED` from, so `ssh`, `run --attach`,
  the readiness poll and `status` all take the VSOCK channel on such a host.
  `enableSsh` is deliberately still NOT forced to `false` there: it remains the
  switch for "this host has an SSH control channel", and the channel exists — it
  is simply carried by AF_VSOCK.
- **Phase 6, the guest KEEPS its firewall under the `vsock` transport**. Nothing
  can reach a guest with no interface, so `networking.firewall` is pure overhead
  by that argument — but it costs one unit, and keeping it means a future
  regression that gives the guest a link cannot silently also give it an open
  port. `services.openssh.openFirewall = false` keeps 22 out of it, and the tests
  assert `allowedTCPPorts == [ ]`.
- **Phase 6, the real-KVM suite ADAPTS its network sections instead of skipping
  them**. `net` and `l2` are written as "the guest must NOT be able to reach X" —
  which is trivially true for a guest with no interface, i.e. exactly the vacuous
  pass the phase-5 capability dispatch was built to prevent. The suite therefore
  reads the transport from `agent-microvm capabilities`
  (`network-transport: tap|vsock`, HARD-ABORTING on an absent or unknown value)
  and, under `vsock`, asserts the ABSENCE positively (no non-loopback link, no
  IPv4/IPv6 default route, no global address, for both guests in `l2`) while
  explicitly `SKIP`ping the TAP-specific subtests with the transport as the
  reason. No section is gated away, so coverage does not shrink.
- **Phase 6, the option spelling is a capability token, NOT a `transport` enum**.
  The plan sketches `myconfig.ai.microvm.transport = "tap" | "vsock"`. NOT
  adopted: a two-valued transport enum is a fork (two transport shapes), and
  the plan's own phase-5 deviation hardened the layout table for a `vsock`
  capability token specifically ("a third token (a phase-6 `vsock`, say)"). So
  `vsock` is a third entry of `declaredCapabilities` (`[ "interactive" "batch"
  "vsock" ]`), default OFF, resolved ONCE in `../default.nix` and applied through
  the SAME per-capability mechanism (`agentCapabilities.vsock`, the layout
  table's `capabilities = [ "interactive" "vsock" ]` on `hostkeys/`, and the
  runtime-validation `transport` token). No consumer gained an `if transport ==
  ...`; the module stays on ONE code path.
- **Phase 6, `vsock` is restricted to `proxy-only`/`offline`**. The plan says
  "Allow `vsock` only with `networkProfile = "proxy-only"` or a fully offline
  profile", and that is enforced. It is no longer merely the safer choice: with
  the model transport implemented, `package-access`/`internet` would be an
  outright contradiction (they need routing, NAT and DNS, i.e. an interface).
  `offline` + `vsock` is accepted and keeps the TAP shape plus the VSOCK CONTROL
  channel — an `offline` guest has no model API to move off the TAP. An operator
  wanting insecure egress does not select `vsock`.
- **Phase 6, the phase-5 "a batch-mode guest has no SSH daemon" invariant is
  SCOPED, not abandoned**. A `capabilities = [ "batch" "vsock" ]` guest enables
  `services.openssh` (the `sshd-vsock@` unit + its NixOS dropins are gated on
  it) and SUPPRESSES the TCP `sshd.service` (`systemd.services.sshd.enable =
  false`, so no TCP listener ever starts — only the VSOCK `sshd-vsock@`) AND
  closes the TAP firewall for 22 (`services.openssh.openFirewall = false`,
  keeping 22 out of `networking.firewall.allowedTCPPorts` — without it the
  openssh module's default `openFirewall = true` would open 22 on a guest whose
  TCP sshd is masked). The invariant is therefore scoped to "no TCP/network SSH
  daemon" (no `sshd.service` AND no TAP firewall opening for 22): a VSOCK-only
  inetd sshd (reachable solely from the host over AF_VSOCK, never from the TAP)
  is the control channel. It is NOT a closure-size claim — NixOS'
  `environment.requiredPackages` still puts the openssh BINARIES in the closure
  (load-bearing for a bootable system), exactly as phase 5 recorded for the
  batch-only-without-vsock case.
- **Phase 6, the host-side `programs.ssh.systemd-ssh-proxy` dependency is
  asserted at eval**. The launcher's VSOCK `ssh` / readiness poll / `status`
  connect as `ssh vsock-mux/<stateRoot>/<slot>/notify.vsock`, a hostname that
  only resolves because nixpkgs' `programs.ssh.systemd-ssh-proxy.enable` (default
  true) includes `20-systemd-ssh-proxy.conf`, which supplies the `Host vsock-mux/*`
  ProxyCommand (systemd-ssh-proxy -> the guest's vsock::22). A host that selects
  `vsock` AND sets `programs.ssh.systemd-ssh-proxy.enable = false` would get a
  confusing runtime DNS-resolution failure of the control channel; this is now
  rejected at evaluation with a pinned message naming the missing dependency
  (satisfied by the nixpkgs default on every host today, including f13).
- **Phase 6, the `stateRoot` == `microvm.stateDir` assumption is asserted at
  eval**. The launcher's VSOCK ssh target (`vsock-mux/<stateRoot>/<slot>/
  notify.vsock`) and the per-slot `known_hosts` entry both key on
  `myconfig.ai.microvm.stateRoot`, while microvm.nix backs the VSOCK device under
  `config.microvm.stateDir/<slot>`. This was a pre-existing assumption
  (the TAP path also uses `stateRoot`); phase 6 widened its blast radius to the
  VSOCK mux address and the `known_hosts` key. A host that sets one without the
  other would get a runtime mismatch (an unresolvable VSOCK mux socket). It is
  now rejected at evaluation (`stateRoot == toString microvm.stateDir`); both
  default to `/var/lib/microvms`, so every host today satisfies it.
- **Phase 6, the VSOCK `known_hosts` matching is asserted structurally, not
  runtime-verified**. The launcher's VSOCK `ssh` runs with
  `StrictHostKeyChecking=yes` against a `known_hosts` entry keyed by
  `vsock-mux/<stateRoot>/<slot>/notify.vsock`. Whether openssh matches that exact
  host string for a `vsock-mux/` target is runtime behaviour that needs a KVM
  host (no KVM in CI); the entry is GENERATED and the launcher is WIRED to use
  it, so a mismatch fails CLOSED (a connection that cannot be verified aborts
  rather than accepting a key), and the out-of-CI real-KVM tier is what would
  prove the round trip. This is the same tier split phase 5's `seed` enforcement
  and phase 0's benchmark use. The PROOF EVENT is therefore the first real-KVM
  run of `--section creds` and `--section boot` on a `batch`+`vsock` host: those
  sections drive the VSOCK `ssh` round trip (host-key verification + command
  exec), so a green run there is the runtime confirmation the structural
  assertion cannot give from CI.
- **Phase 6, the config-seed is NOT ordered before `sshd-vsock.socket`**. The
  guest's `agent-config-seed` oneshot is `before = [ "sshd.service"
  "agent-job-controller.service" ... ]`; adding `sshd-vsock.socket` to that list
  would change the unit and therefore the default host's guest toplevel drvPath
  (the `sshd-vsock` socket is absent there, so the `before` is a no-op — but the
  unit definition still changes). The `sshd-vsock.socket` is `wantedBy
  sockets.target` (early), so there is a theoretical race in which the VSOCK
  sshd accepts before the home is seeded; in practice the operator (and the
  runtime-validation suite) connect well after boot, by which time the oneshot
  has finished. Kept the safer byte-identity and recorded the race rather than
  adding `sshd-vsock.socket` to the config-seed's `before` list. The DISTINCT
  host-key MOUNT race (the `sshd-vsock@` reads its key from the read-only
  host-key share) IS closed: session.nix adds `RequiresMountsFor=<hostkeysDir>`
  to the `sshd-vsock` socket as a `vsock`-gated dropin (nixpkgs defines that
  socket with `overrideStrategy = "asDropin"`), so the socket waits for the
  host-key share before it listens — a capability-conditional ordering site that
  is ABSENT on a default host (no `vsock`), so it does not perturb byte-identity,
  unlike the config-seed `before` edit above. The remaining open race is solely
  the home-seeding one (benign post-boot).

## Objective

Refactor `modules/myconfig.ai/myconfig.ai.microvm` into a lighter-weight execution environment for untrusted coding agents while preserving the important security properties of the current design:

- separate guest kernel through Cloud Hypervisor/KVM;
- disposable guest root and home;
- no direct access to the host home directory;
- no host Nix daemon socket;
- no Docker, Podman, SSH-agent, GPG-agent, browser, password-manager, or cloud-provider sockets;
- standalone disposable repository clones;
- host-side resource limits;
- proxy-only model access by default;
- no general internet, LAN, VPN, metadata-service, DNS, or inter-VM access in the secure profile;
- host-owned batch control data must remain inaccessible to the unprivileged guest agent.

The intended end state is a small, prebuilt MicroVM profile suitable for interactive YOLO-mode coding-agent sessions.

## Target architecture

Implement a lightweight profile with the following default shape:

```text
Cloud Hypervisor/KVM
├── 1 prebuilt slot
├── 2 vCPU
├── 4 GiB RAM
├── separate EROFS guest Nix store
├── one selected agent package
├── minimal generic CLI toolset
├── disposable root filesystem
├── tmpfs/disposable agent home
├── one writable per-session virtiofs share
│   ├── workspace/
│   ├── input/
│   ├── worker/
│   ├── worker-logs/
│   ├── controller/
│   └── state/            # optional
├── one read-only share: SSH host key + staged config seed
│                          (see the phase-4 deviations: neither may live in
│                           the writable tree)
├── runtime-staged, allowlisted host agent configuration
└── VSOCK-only access to the host LiteLLM proxy
    (phase 6: opt-in per host via `capabilities = [ … "vsock" ]` with
     `networkProfile = "proxy-only"`; the guest then has NO network interface at
     all and the host builds no bridge/TAP/firewall. Without `vsock` the same
     shape uses the TAP + the `AGENT_MICROVM_*` chains.)
```

Do not change the existing full-featured behavior without an explicit compatibility decision. Prefer introducing a new profile or opt-in options first, then changing defaults only after validation.

**Status:** that compatibility decision HAS been taken (see
[Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path)):
phases 1–4 and 8 were first landed behind `profile = "lite"`, reviewed against a
byte-identical `full` host, and the `full` path was then deleted. The tier has one
shape, and it is the one sketched above.

---

## Required security invariants

Treat these as non-negotiable acceptance criteria throughout the work.

1. **No live host-home mount**

   The guest must never receive a mount of `$HOME`, `~/.config`, or another broad host directory. Only explicitly allowlisted configuration paths may be staged.

2. **No host control sockets**

   Never expose:

   - `/nix/var/nix/daemon-socket`;
   - Docker or Podman sockets;
   - `$SSH_AUTH_SOCK`;
   - GPG-agent sockets;
   - password-manager sockets;
   - browser profiles or browser debugging sockets;
   - cloud-provider credential directories;
   - Kubernetes configuration;
   - the host D-Bus session socket.

3. **No host Nix store by default**

   Keep the separate guest store as the secure default. A read-only host-store mode may be added only as an explicitly documented, weaker optional profile.

4. **Disposable mutations**

   Agent writes to its home and configuration must disappear when the session is destroyed unless persistence is explicitly enabled for a narrowly defined state directory.

5. **Independent repository clone**

   The guest workspace must not be a Git worktree linked to the original repository. It must have an independent Git object database and Git configuration.

6. **Proxy-only network remains closed**

   The lightweight secure profile must not accidentally gain ordinary IP networking, DNS, LAN access, VPN access, metadata-service access, or unrestricted internet access.

7. **Guest cannot overwrite trusted control files**

   Batch job specifications, prompts, controller state, and final-result metadata that are trusted by the host must remain root-owned and non-writable by the `agent` user.

8. **No secrets copied accidentally**

   Runtime configuration staging must be based on an explicit allowlist. It must not recursively copy an entire agent configuration directory unless each contained file is known to be safe.

---

## Phase 0 — Baseline and measurement

### Tasks

- Document the current launch path for:
  - interactive sessions;
  - batch sessions;
  - slot installation;
  - repository cloning;
  - guest boot;
  - SSH readiness;
  - model-proxy forwarding;
  - cleanup.
- Record the current values for:
  - VM closure size;
  - store image size;
  - build time from a warm Nix cache;
  - launch-to-agent-ready latency;
  - number of host processes per running slot;
  - idle RSS per running slot;
  - number of virtiofsd processes;
  - number of systemd units generated per slot;
  - number of TAP devices, firewall rules, and proxy processes.
- Add a repeatable benchmark script under the module or repository test tooling.
- Capture one baseline for a single interactive Codex session and one for a batch session.

### Suggested benchmark output

```json
{
  "profile": "current",
  "agent": "codex",
  "closure_bytes": 0,
  "store_image_bytes": 0,
  "warm_build_seconds": 0,
  "launch_to_ready_ms": 0,
  "host_process_count": 0,
  "virtiofsd_count": 0,
  "idle_rss_bytes": 0
}
```

### Acceptance criteria

- The benchmark can be run repeatedly with one command.
- Results are machine-readable.
- Later phases can be compared against the same baseline.

---

## Phase 1 — Add an opt-in lightweight profile — **DONE**

Implemented as `../profiles.nix` (the authoritative profile table) plus the
`myconfig.ai.microvm.profile` option in `../default.nix`, which resolved the
profile ONCE and handed it to `../guest.nix` via `_module.args.agentProfile`.
SUPERSEDED: once phases 1–4 and 8 had been reviewed, the compatibility boundary
was deleted along with the `full` path, so the `lite` values are the module's
only behaviour and there is no `profile` option (see
[Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path)).
What remains locked down by `checks.microvm-eval-guest-shape`
(`tests/microvm.nix`) is the SHAPE: pinned optimized EROFS guest store, minimal
toolset, bash login shell, no `defaultPackages`, an explicit `resourceClasses`
outranking the module default, and the deprecated slot options being rejected
alongside it. Runtime boot requires KVM and is therefore part of the out-of-CI
runtime-validation tier, not of `nix flake check`.

### Goal

Create a stable compatibility boundary before changing implementation details.

### Tasks

Add a profile option similar to:

```nix
myconfig.ai.microvm.profile = "full"; # existing behavior
myconfig.ai.microvm.profile = "lite"; # new behavior
```

The `lite` profile should initially select:

```nix
{
  resourceClasses = {
    lite = {
      count = 1;
      vcpu = 2;
      memoryMiB = 4096;
    };
  };

  networkProfile = "proxy-only";
  persistentAgentState.enable = false;
}
```

Keep explicit user options higher priority than profile defaults.

Pin and document:

```nix
microvm.optimize.enable = true;
microvm.storeDiskType = "erofs";
```

### Files likely affected

- `default.nix`
- option declarations
- slot/resource-class generation
- module documentation

### Acceptance criteria

- Existing configurations retain current behavior under `profile = "full"`.
- `profile = "lite"` generates exactly one slot unless overridden.
- The lite profile boots and runs an agent before any deeper refactoring begins.
- Evaluation includes assertions for incompatible combinations.

---

## Phase 2 — Build only selected agents — **DONE**

Implemented as the `enabledNames` argument of `../agents.nix` (the selection is
applied at the single source of truth, so no consumer carries its own filter)
plus the `myconfig.ai.microvm.enabledAgents` option resolved in
`../default.nix`. Unknown tokens, an empty selection and a selection with no
batch-capable agent are rejected by module assertions. The module-wide default
is `null` = every declared agent (the profile-supplied `[ "codex" ]` default died
with the profile table), so a host that says nothing keeps the historical
behaviour and a host that wants the small guest closure names its agents — f13
does, explicitly.

### Goal

Remove unused agent runtimes and configuration from the guest closure.

### Tasks

Add an option:

```nix
myconfig.ai.microvm.enabledAgents = [ "codex" ];
```

Filter all agent-dependent behavior through this option:

- packages added to `environment.systemPackages`;
- `agent-run` command dispatch;
- generated wrapper scripts;
- guest configuration files;
- Home Manager or config-staging paths;
- Workmux registrations;
- state directories;
- shell completion or environment initialization;
- tests.

Reject unknown agent names during Nix evaluation.

Prefer a registry structure in which each agent defines:

```nix
{
  package = ...;
  executable = "...";
  configPaths = [ ... ];
  statePaths = [ ... ];
  extraPackages = [ ... ];
  environment = { ... };
}
```

### Acceptance criteria

- A Codex-only image does not contain packages for Pi, Qwen, OpenCode, or other disabled agents.
- Disabled agent commands fail with a clear error.
- Closure size is measured before and after this change.
- The full profile can still enable all supported agents.

---

## Phase 3 — Replace guest Home Manager activation with runtime configuration staging — **DONE** (unconditional since the `full` path was deleted)

Implemented as `../config-seed.nix` (the authoritative policy: allowlist,
credential denylist, paths, modes, the host-side stager
`agent-microvm-stage-config`, the guest-side seeder `agent-config-seed-apply`
and the guest unit), the new per-agent `configPaths` field of `../agents.nix`,
the per-slot READ-ONLY share in `../guest.nix` and the staging block of
`../launcher.nix`. There is exactly ONE provisioning path per guest: the
alternative (guest home-manager activation, `guest-home.nix`) was first gated
behind `configSeed.enable = false` and then deleted outright with the `full`
path.

Acceptance criteria are locked down by `checks.microvm-config-seed`
(`tests/microvm.nix`): the guest runs no home-manager activation at all, the
config-seed share is per-slot/read-only/root-owned, the guest oneshot is ordered
before sshd, the batch job controller, the agent-state linker and the boot-time
model discovery (checked on the reference host, which HAS sshd, so the ordering
is against real units), the allowlist
follows `enabledAgents`, escaping, credential-shaped and agent-state-colliding
allowlist entries are rejected at eval, the staged tree is root-only and the
manifest is outside every guest share, and the generated stager really enforces
the allowlist, refuses escapes, applies the denylist to RESOLVED targets, skips
setuid/non-regular files and cleans its destination. Whether the stager
ENFORCES that policy at runtime is decided by the root-only
`runtime-validation.sh --section seed` (the Nix sandbox is not root, so CI can
only prove the policy is baked in). When the phase landed, the `full` profile was
verified unchanged with the evaluated-slice diff from `AGENTS.md`; that profile no
longer exists (see
[Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path)).

### Goal

Use the current host agent configuration at launch time without mounting it live and without rebuilding the VM when instructions or settings change.

### Design

The host launcher copies only allowlisted configuration into a per-session staging directory. The existing writable session share exposes the staged copy to the guest. A guest oneshot service copies it into the disposable agent home before the agent or SSH session starts.

```text
host allowlisted config
        │
        │ copy with symlink dereferencing
        ▼
slot/session/config-seed
        │
        │ virtiofs
        ▼
/run/agent-session/config-seed
        │
        │ root-owned oneshot copy
        ▼
/home/agent
```

### Tasks

Add per-agent config declarations to the registry. Example:

```nix
configPaths = [
  ".codex/config.toml"
  ".codex/skills"
  ".agents/skills"
  ".gitconfig"
];
```

Implement host-side staging with these properties:

- paths are resolved relative to the configured host home;
- missing optional paths are allowed;
- paths cannot escape the host home through `..`;
- symlinks into `/nix/store` are dereferenced;
- symlinks to locations outside approved roots are rejected or copied only under an explicit policy;
- staged files are owned by root;
- no sockets, devices, FIFOs, or setuid files are copied;
- secrets are excluded by default;
- the destination is cleaned before every launch;
- a manifest records which paths were staged.

Use a safe copying mechanism such as `rsync --archive --copy-links` plus explicit validation. Do not shell-expand untrusted paths.

Add a guest service similar to:

```nix
systemd.services.seed-agent-home = {
  wantedBy = [ "multi-user.target" ];
  before = [
    "sshd.service"
    "agent-job-controller.service"
  ];
  after = [ "local-fs.target" ];

  serviceConfig = {
    Type = "oneshot";
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectHome = false;
  };

  script = ''
    install -d -m 0700 -o agent -g users /home/agent

    if test -d /run/agent-session/config-seed; then
      ${pkgs.rsync}/bin/rsync \
        --archive \
        --chmod=Du=rwx,Dgo=,Fu=rw,Fgo= \
        /run/agent-session/config-seed/ \
        /home/agent/

      chown -R agent:users /home/agent
    fi
  '';
};
```

After this works, remove guest Home Manager activation from the lite profile. Retain the old path for the full profile until compatibility is proven.

### Secret-handling policy

Create an explicit denylist for common credential material, including:

```text
auth.json
credentials.json
tokens.json
*.pem
*.key
id_rsa
id_ed25519
cookies*
session*
```

Do not rely only on filename matching. Prefer a positive allowlist of exact files and directories.

Model-provider credentials should remain in the host proxy. The guest should receive only the proxy endpoint and any non-sensitive routing metadata required to use it.

### Acceptance criteria

- Editing an allowlisted host instruction or configuration file affects the next VM launch without rebuilding the guest.
- Agent changes to `/home/agent` do not modify host files.
- The staged configuration contains no sockets, device nodes, or unexpected secrets.
- The guest no longer runs Home Manager activation in the lite profile.
- Boot and closure measurements show the effect of removing guest Home Manager.

---

## Phase 4 — Consolidate writable virtiofs shares — **DONE** (unconditional since the `full` path was deleted)

Implemented as `../session.nix`: the authoritative layout table (paths,
subdirectory names, owners, modes, the trust-boundary policy function), the host
tmpfiles rules generated from it, the generated pre-launch verifier
`agent-microvm-verify-session` and the guest fragment that bind-mounts
`/run/agent-session/workspace` to `/workspace`. It was gated behind
`myconfig.ai.microvm.session.enable` (defaulting to the resolved profile's
`consolidatedSession` field) until the `full` path was deleted; the layout is now
unconditional and that option is gone.

Every other module DERIVES from that table instead of growing a second copy of
the layout: `../job.nix` (its root, subdirectory names and directory modes),
`../state.nix` (the per-slot bind target and the guest mount point),
`../config-seed.nix` (the staged payload directory, its modes and the host-only
manifest location), `../hostkeys.nix` (the per-slot key directory and the guest
key path), `../guest.nix` (the share list, the `/workspace` bind mount, the
sshd mount ordering and three cross-module assertions) and `../launcher.nix`
(`prepare_session`, `verify_session`, `clear_session`, and the two bind-mount
targets).

Acceptance criteria are locked down by `checks.microvm-session-tree`
(`tests/microvm.nix`): the guest declares EXACTLY ONE writable share and ONE
read-only share with the expected tags/mountPoints/sources (and a guest WITHOUT
the SSH control channel still declares exactly those two);
the per-directory ownership/mode expectations are asserted FROM the layout table
(against the host tmpfiles rules and against the generated verifier, never
against a second hardcoded copy); every consumer's paths are asserted to be the
table's; the seeder, the batch controller, the worker template, the agent-state
linker and sshd are ordered against the right mounts; the layout POLICY is fed
deliberately broken tables (agent-owned or world-writable `input/`, a
group/other-readable or agent-owned `controller/`, an agent-owned
`worker-logs/`, a group/other-writable staged tree, SSH host keys or the staged
configuration inside the writable tree, nested trees) and must complain about
each; and the build part greps the generated launcher and verifier for the
pre-launch verification, the complete-tree removal and the refusal to remove a
tree whose bind mounts are still live, and proves the launcher no longer
references any historical four-share path. It also FORCES the guest
`system.build.toplevel` and `microvm.declaredRunner`, so a guest that does not
build fails CI.

### Goal

Reduce virtiofsd process count and simplify mount management.

### Target session tree

```text
<slot-session>/
├── workspace/
├── config-seed/
├── input/
├── controller/
├── worker/
└── state/
```

Expose it as a single writable share:

```text
/run/agent-session
```

Bind mount or symlink:

```text
/run/agent-session/workspace -> /workspace
```

Use permissions to preserve trust boundaries:

```text
slot-session/       root:root  0755
workspace/          agent      0700
config-seed/        root:root  0555
input/              root:root  0555
input/spec          root:root  0400
input/prompt        root:root  0444
controller/         root:root  0700
worker/             agent      0700
state/              agent      0700
```

Keep SSH private host keys in a separate read-only share. Do not place them in the writable session tree.

### Tasks

- Replace separate workspace, job, state, and config shares with one session share in the lite profile.
- Update guest mount declarations.
- Update launcher path handling.
- Update cleanup logic.
- Update batch controller and worker paths.
- Add ownership and mode assertions before VM launch.
- Ensure a malicious workspace symlink cannot redirect writes into `controller/`, `input/`, or `config-seed/`.
- **DONE (separate commit): the four-share (`full`) path was deleted.**
  The consolidated layout was introduced *alongside* the historical one so the
  `full` profile could be proved byte-identical (see the recorded deviations).
  That left the module carrying two layouts, two launcher spellings and ~20
  `if session.enable then … else …` sites, i.e. *more* complex than the plan's
  objective, not less. `session.enable` is now unconditional (the option is
  gone), the `else` branches in `job.nix`, `state.nix`, `hostkeys.nix`,
  `config-seed.nix` and `guest.nix` are gone, the conditional launcher fragments
  in `launcher.nix` are gone, `../runtime-validation.sh` has one layout, and the
  `full`-profile assertions (including the whole
  `checks.microvm-eval-workspace-share`) are gone from
  `../../../tests/microvm.nix`. It was a separate commit on purpose: it CHANGED
  the f13 host, so it carries its own evaluated-slice review (see
  [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path)).

### Acceptance criteria

- Interactive mode uses one writable virtiofs share plus at most one read-only SSH-key share.
- The guest units that need one of the new mounts declare it: the workspace bind
  carries `x-systemd.requires-mounts-for=/run/agent-session`, and `sshd`,
  `agent-config-seed` and the agent-state linker carry `RequiresMountsFor` on
  the SUBPATH they use (`/run/agent-session/state`,
  `/run/agent-session-ro/hostkeys`, `…/config-seed`). A subpath is correct and
  deliberate: systemd resolves `RequiresMountsFor=` into a dependency on the
  mount unit of *every* path prefix, so naming the subdirectory pulls in the
  share's own mount unit and additionally documents which part of the share the
  unit needs. The tests assert the string; this is the semantics behind it.
- Batch trust boundaries remain intact.
- The agent cannot modify root-owned input or controller data.
- Cleanup removes the complete per-session tree reliably.
- No path traversal or symlink escape is possible during host writes.

---

## Phase 5 — Separate interactive and batch capabilities — **DONE**

Implemented as `myconfig.ai.microvm.capabilities`, a SET over the ONE guest
shape:

```nix
myconfig.ai.microvm.capabilities = [ "interactive" "batch" ]; # DEFAULT
myconfig.ai.microvm.capabilities = [ "interactive" ];
myconfig.ai.microvm.capabilities = [ "batch" ];               # + enableSsh = false
```

The plan's sketch proposed a three-valued `mode = interactive|batch|combined`.
NOT adopted: `combined` would be a compatibility profile in disguise and would
re-create the cross-product the `full`/`lite` collapse removed (every consumer
would have to ask "which of the three am I in?" instead of "do I have this
capability?"). A set of two named capabilities, defaulting to BOTH, expresses the
same three configurations without a value that means "the old shape".

The decision is resolved EXACTLY ONCE (in `../default.nix`, handed on as
`_module.args.agentCapabilities`) and applied in the ONE place each concern
already lives:

- `../session.nix` — every layout-table entry declares WHICH capabilities need
  it, so the host tmpfiles rules, the generated pre-launch verifier, the
  launcher's `prepare_session`, the guest mounts and `tests/microvm.nix` all
  follow one filtered table. `input/`, `controller/`, `worker/` and
  `worker-logs/` are `batch`-only; the read-only `hostkeys/` is
  `interactive`-only; `capabilities = null` marks the entries every host has. The FULL table remains the authority over a directory's
  owner/mode (`modeOf`), so `../job.nix` can still bake the guest-side
  permission assertions without knowing whether this host creates the
  directory, and the trust POLICY is asserted against the full table so a
  weakening edit to an unselected entry still fails the build.
- `../job.nix` — `mkGuestModule` (and the worker's endpoint-environment
  fragment, which `../guest.nix` now takes from here rather than defining a
  worker unit behind its back) is EMPTY without `batch`, and the host-side
  result archive is gone with it.
- `../hostkeys.nix` — the key pair + `known_hosts` provisioning unit exists only
  with `interactive` (the `hostkeys/` directory itself is the layout table's
  business).
- `../guest.nix` — the interactive `agent-run` entry point.
- `../workmux.nix` — the `microvm-<agent>` panes (they are `run --attach`).
- `../launcher.nix` — ONE launcher, ONE shape; a narrowed host additionally
  gets a `require_capability` membership check at the top of `run`/`ssh` (needs
  `interactive`) and `submit`/`cancel` (needs `batch`), plus an honest
  `doctor` host-key section and `usage` report. Those fragments are rendered ONLY
  on a host that lacks a capability. `console` is deliberately never gated (it
  reads the HOST's `microvm@<slot>` journal and is the only debugging channel a
  batch-only guest has), the capability set itself is rendered unconditionally,
  and `prepare_session` sweeps undeclared top-level entries of both trees.
- `../default.nix` — the batch-capable-agent assertion became
  capability-conditional, and `enableSsh` is rejected without `interactive`.
- `../runtime-validation.sh` — the section dispatch READS the host's capabilities
  from `agent-microvm capabilities` (hard-aborting if it cannot parse the answer)
  and skips (or hard-aborts on) a section whose capability is missing, instead of
  letting its "the guest must NOT be able to …" checks pass vacuously. Within
  `creds`, the batch-worker-environment subtest is gated on its own, because the
  unit it inspects does not exist without `batch`.

**Verification.** The default (`[ "interactive" "batch" ]`) leaves every GUEST
artefact byte-for-byte as it was: in the evaluated-slice diff from `AGENTS.md` for
`test-f13` (every VM's `system.build.toplevel` + `microvm.declaredRunner` drvPath,
every VM's full `microvm.shares`, the guest `fileSystems`, guest
`systemd.services` names, guest `environment.systemPackages` drvPaths, the host
`systemd.tmpfiles.rules`, `networking.firewall.extraCommands`, host
`systemd.services` names and host `environment.systemPackages` drvPaths) the guest
half is empty apart from the two git-revision artefacts (`nixos-version.drv`, the
`myconfig-commit` tmpfiles link). The HOST half now also shows the launcher
derivation changing, deliberately: the review required the capability set to be
machine-readable on every host (`agent-microvm capabilities`) instead of inferred
from a refusal message, and the sweep of undeclared session entries landed in the
same script — see the two revised deviations above. `checks.microvm-capabilities` asserts the
default host still has both halves, asserts every removal of both narrowings
against the EVALUATED config (units, tmpfiles rules, package names, layout
table, workmux registrations, unit ExecStarts) and against the BUILT launchers
and verifiers, pins the four negative assertions to their specific messages, and
BUILDS the default, interactive-only and batch-only `system.build.toplevel` and
`microvm.declaredRunner` — the narrowed closures, not just their drvPaths,
because that is the only thing that catches a `writeShellApplication`
shellcheck failure (phase 4's SC2034).

### Interactive mode includes

- SSH server and readiness;
- workspace;
- configuration staging;
- optional persistent state;
- agent launcher.

### Interactive mode excludes

- batch controller;
- batch worker protocol;
- batch input/result handling;
- job-specific systemd units.

### Batch mode includes

- workspace;
- configuration staging;
- root-owned job input;
- unprivileged worker;
- trusted result controller;
- deterministic exit/result handling.

### Batch mode excludes

- SSH server;
- SSH host-key share;
- known-hosts generation;
- SSH polling;
- interactive shell support unless explicitly enabled.

### Acceptance criteria — all met

- An interactive-mode guest has no batch services or job protocol files:
  `agent-job-controller`, `agent-job-worker@` and the three job programs are
  absent from the guest's units and closure, `input/`/`controller/`/`worker/`/
  `worker-logs/` are not created (no tmpfiles rules for them), the host result
  archive does not exist, and `submit`/`cancel` are refused by the launcher.
- A batch-mode guest has no SSH daemon, SSH host keys, or SSH readiness polling:
  no `sshd` unit and `services.openssh.enable = false`, no
  `agent-microvm-hostkeys.service` (hence no key pair and no `known_hosts`
  generator anywhere in the host's unit ExecStarts), no `hostkeys/` in the
  read-only tree, no `agent-run`, no workmux panes, and `run`/`ssh` refused (so
  the readiness poll is unreachable).
- Both capabilities together preserve the current behaviour — proved by the
  evaluated-slice diff, not asserted.
- Nix assertions reject the meaningless combinations: an empty capability set,
  an unknown token, `enableSsh` without `interactive`, and a `batch` host whose
  agent selection contains no batch-capable agent (which no longer fires for an
  interactive-only host).

---

## Phase 6 — Add a VSOCK transport for proxy-only mode — **DONE** (control channel AND the literal model transport)

Implemented as a THIRD capability token `vsock` on `myconfig.ai.microvm.capabilities`
(default OFF, so the historical `[ "interactive" "batch" ]` is unchanged and a
default host's guest closure is byte-for-byte identical — verified with the
AGENTS.md evaluated-slice snapshot/diff). `vsock` is a NEW AXIS over the ONE
guest shape, not a fork, and it buys two things:

1. a VSOCK **control** channel — the `sshd-vsock@` unit, REUSED from upstream
   `microvm.vsock.cid` + NixOS' systemd-ssh-generator — which is what closes the
   phase-5 coverage hole (a batch-only host has no TCP sshd, so without it the
   runtime-validation suite could only run `--section seed`);
2. together with `networkProfile = "proxy-only"`, the **model transport** the
   plan literally sketched: AF_VSOCK replaces the guest's network entirely.

### The transport decision (where it lives)

ONE table, ONE resolution, no consumer that tests a transport name:

- `../network-profiles.nix` — the authoritative `transports` table of POSITIVE
  capability flags (`guestInterface`, `hostBridge`, `hostFirewall`,
  `bridgeLitellm`, `vsockLitellm`, `tapSsh`) plus `resolveTransport { profile,
  vsockCapability }`. This is the same file that already owned "what may a guest
  reach"; "how does it get there" belongs next to it.
- `../default.nix` — resolves it EXACTLY ONCE into
  `agentNetwork.transport` / `.transportCaps` / `.tapSshUsable` /
  `.vsockLitellmPort`, handed to the sibling modules through the SAME
  `_module.args.agentNetwork` the profile already travels in.
- `../network.nix` — `hostBridge`/`hostFirewall`/`bridgeLitellm` gate the bridge,
  the TAP enslavement/L2-isolation units, the NetworkManager exclusions,
  `br_netfilter`, the `AGENT_MICROVM_*` chains and the bridge-only socket;
  `vsockLitellm` renders the PER-VM forwarder instead.
- `../guest.nix` — `guestInterface` gates `microvm.interfaces`, the static
  address/route/networkd block and (via the absence of it) `useNetworkd` /
  `useDHCP` / `dhcpcd`; `bridgeLitellm` vs `vsockLitellm` picks which
  `litellm-forwarder` the guest has; `tapSshUsable` decides whether the TCP
  `sshd.service` is a live unit or is suppressed.
- `../session.nix` / `../hostkeys.nix` / `../launcher.nix` — the host-key mount
  ordering belongs to the sshd that is actually live, the IPv4 `known_hosts`
  entry only exists while the guest has an address, and the launcher's
  `SSH_ENABLED`, `doctor` sections, `preflight` endpoint and `status` line follow
  the transport.

### Design (as implemented — the plan's literal sketch)

```text
guest agent
  -> 127.0.0.1:<litellmPort>          (UNCHANGED endpoint: the host-provisioned
                                       agent configuration works verbatim)
  -> guest TCP-to-VSOCK bridge        (litellm-forwarder, ONE unit,
                                       socat ACCEPT-FD:3,fork)
  -> host CID 2, port <litellmPort>
  -> cloud-hypervisor's per-VM mux socket
     <stateRoot>/<slot>/notify.vsock_<litellmPort>
  -> host VSOCK-to-TCP forwarder      (agent-litellm-vsock-<slot>)
  -> host LiteLLM loopback listener   (127.0.0.1:<litellmPort>)
```

The deterministic per-slot VSOCK CID from the slot metadata is used, as the plan
asks. Two implementation notes worth recording:

- **there is no AF_VSOCK listener on the host, by design.** With
  cloud-hypervisor the guest's VSOCK device is implemented in the VMM, not by the
  host kernel's vhost-vsock: a guest-initiated connection to host CID 2 port N is
  delivered to the Unix socket `<vsock socket>_<N>` next to the VM's mux socket
  (the same convention microvm.nix itself relies on for the guest's systemd
  notify socket, `notify.vsock_8888`). Listening on that path IS listening on the
  guest's VSOCK port — and it is STRONGER than an AF_VSOCK listener would be,
  because the socket lives inside one VM's own state directory and cannot be
  addressed by anything except that VM's VMM process (`root:kvm 0660`).
- **`systemd-socket-proxyd` is used on the host, `socat` in the guest.**
  socket-proxyd forwards to `HOST:PORT`/UNIX targets only, so the guest side (the
  one that must DIAL AF_VSOCK) is `socat -t 120 ACCEPT-FD:3,fork
  VSOCK-CONNECT:2:<port>` with the destination baked into the unit. `socat` is
  added by that unit's ExecStart, NOT to `environment.systemPackages`, so the
  untrusted agent does not gain a general-purpose socket tool in its PATH. The
  socket stays `Accept = no` on both transports (ONE long-running multiplexer, no
  `MaxConnections=64` ceiling on concurrent model streams) and carries **no `-T`
  inactivity timeout**, which would otherwise abort in-flight model requests.

### Tasks — status

- a transport option: **adapted, not adopted verbatim.** The selector is the
  capability token `vsock` plus the network profile, not
  `myconfig.ai.microvm.transport = "tap" | "vsock"` (see the recorded deviation:
  a two-valued transport enum is the cross-product the `full`/`lite` collapse
  removed, and the plan's own phase-5 deviation hardened the layout table for a
  `vsock` TOKEN specifically). The resolved value IS a two-valued transport
  internally — it just is not an option a host sets directly.
- `vsock` allowed only with `proxy-only`/`offline`: **done** (eval assertion,
  pinned by a test). The model transport switches only under `proxy-only`;
  `offline` has no model API to carry, so it keeps the TAP shape and gets the
  control channel only.
- host and guest forwarding with a small, auditable tool: **done**
  (`systemd-socket-proxyd` + `socat`, both destination-fixed in their units).
- bind the host TCP side only to the LiteLLM loopback endpoint: **done**, and
  additionally enforced on the forwarder PROCESS (`IPAddressAllow=localhost` +
  `IPAddressDeny=any`, `RestrictAddressFamilies=AF_UNIX AF_INET`, `DynamicUser`,
  `ProtectSystem=strict`), so "some other host port" is unreachable for the
  forwarder itself, not merely unrequestable by the guest.
- the guest cannot select an arbitrary host TCP destination: **done** — there is
  no protocol on the wire at all; the guest gets a plain TCP connection to its
  own loopback and the CID/port are Nix-baked constants.
- remove/skip for VSOCK slots — TAP creation, private bridge membership, IP
  allocation, guest networkd, DNS, iptables/nftables chains, NetworkManager
  unmanaged declarations, bridge listener services, NAT and forwarding rules:
  **all done** (asserted individually, each with a positive control on the `tap`
  reference host, in `checks.microvm-vsock-transport`). `microvm-tap-interfaces@<slot>`
  is still DECLARED by upstream microvm.nix for every VM, but its
  `ConditionPathExists=<state>/current/bin/tap-up` fails for a guest with no
  interfaces (the runner ships no `tap-up`), so no TAP is ever created — the test
  asserts the runner has no `tap-up`/`tap-down` at all.
- keep the TAP backend for profiles that intentionally permit package or internet
  access: **done** — `package-access`/`internet` cannot select `vsock` at all,
  and `resolveTransport` returns `tap` for every profile but `proxy-only`.
- a protocol-level health check: **partially done.** The launcher's preflight
  probes the endpoint the GUEST path ends at (the loopback proxy under `vsock`),
  and `doctor` checks every per-VM forwarder socket. What is NOT done is an
  in-guest health probe of the AF_VSOCK hop itself before declaring readiness —
  that is the same "readiness as a positive protocol signal" item phase 7 defers.

### Security requirements — met

- **destination-fixed, not a CONNECT proxy**: the host forwarder's only argument
  is `127.0.0.1:<litellmPort>`; the guest supplies no address, port or CID.
- **one listener per slot** (the plan's alternative to CID validation): each VM
  has its own socket path inside its own state directory, so slot A cannot reach
  slot B's forwarder — there is no shared namespace and no CID to spoof.

### Acceptance criteria — met

- In proxy-only VSOCK mode, the guest has no network interface other than
  loopback — asserted from the evaluated guest config (`microvm.interfaces == [
  ]`, no networkd, no networks, no dhcpcd, no nameservers, no open TCP port) and
  from the BUILT runner (no `tap-up`/`tap-down`).
- There is no TAP device or per-slot bridge/firewall configuration — asserted on
  the host config (`networking.bridges == { }`, no bridge address, no
  `AGENT_MICROVM_*` in `extraCommands`/`extraStopCommands`, no
  `agent-microvm-attach-*` unit, no NetworkManager exclusions, no
  `agent-litellm-proxy` socket/service), each with a positive control proving the
  same assertion FAILS on the `tap` reference host.
- The agent can call the LiteLLM endpoint — the endpoint is byte-identical
  (`OPENAI_BASE_URL`/`ANTHROPIC_BASE_URL` equal the `tap` host's), the guest
  bridge is wired to CID 2 + the model port, and the per-VM host forwarder ends
  at the loopback proxy. RUNTIME proof needs KVM: `--section net` on a
  `vsock`-transport host is the recorded proof event.
- Attempts to access arbitrary host ports fail — structurally (the forwarder is
  destination-fixed and confined to `localhost`) and, at runtime, through the
  suite's loopback probes.
- Attempts to resolve DNS or reach LAN/internet addresses fail — there is no
  interface, no route and no resolver; the suite asserts each ABSENCE positively
  instead of reporting a denial that would be true either way.
- Multi-slot tests prove that one slot cannot use another slot's proxy path — at
  eval, a two-slot `vsock` host is asserted to declare one forwarder per slot on
  DISTINCT socket paths; the runtime multi-slot proof is part of the pending
  real-KVM run.

### Acceptance criteria of the CONTROL channel (unchanged from the first landing)

- A batch+vsock guest sets `microvm.vsock.cid = <slot.cid>`, enables
  `services.openssh`, declares the `sshd-vsock` socket, SUPPRESSES the TCP
  `sshd.service` AND closes its TAP firewall opening for 22, provisions the
  per-slot host key + the VSOCK `known_hosts` entry, and authorises the dedicated
  key. Since the literal transport landed, the SAME suppression applies to an
  INTERACTIVE host under the `vsock` transport: its TCP sshd could not be reached
  either (there is no interface), so it is masked whatever `enableSsh` says and
  `agent-microvm ssh` / `run --attach` go over VSOCK.
- `vsock` is rejected without an `sshPublicKeyFile`, with
  `networkProfile = "package-access"`/`"internet"`, without
  `programs.ssh.systemd-ssh-proxy.enable`, with a `stateRoot` that differs from
  `microvm.stateDir`, and (new) with `litellmPort = 8888`, the AF_VSOCK port
  microvm.nix reserves for the guest's systemd notify socket — all pinned to the
  specific assertion message by tests.
- A batch+vsock host runs ALL eight `runtime-validation.sh` sections over the
  VSOCK transport; a batch-only-without-vsock host still runs only `seed`.
- The module stays on ONE code path: `vsock` is a capability axis and the
  transport is a resolved table of flags; no consumer gained a second shape.

## Phase 7 — Optimize repository cloning and startup

### Repository clone — **DONE**

Implemented in `../launcher.nix` (`create_clone` / `verify_clone`). Deviation:
`--no-checkout` is *not* used — the guest agent needs a working tree, and the
clone is checked out exactly once either way. `--shared` / `--reference` are
additionally forbidden by a build-time check on the generated launcher.

Replace local cloning through normal Git transport with an independent optimized local clone where safe:

```bash
git clone \
  --local \
  --no-hardlinks \
  --no-checkout \
  -- "$source_repo" "$destination"
```

Requirements:

- never use `--shared`;
- verify there is no `.git/objects/info/alternates`;
- verify the Git common directory is inside the disposable clone;
- reject source repositories whose Git metadata resolves outside the expected root;
- document Git’s local-clone race behavior;
- optionally fall back to `--no-local` when the source repository is being mutated concurrently.

Measure this change independently.

### Readiness — **PARTIALLY DONE** (option 3, the interim step)

Replace fixed three-second SSH polling with one of:

1. VSOCK readiness notification;
2. a root-owned readiness file in the controller directory;
3. short exponential-backoff SSH polling as an interim step.

Do not consider the VM ready until:

- the workspace is mounted;
- configuration staging completed;
- required proxy forwarding is healthy;
- the selected agent executable exists;
- SSH is listening in interactive mode.

### Slot installation

Before restarting the slot install unit, compare the expected runner/store path with the currently installed symlink. Restart only when the generation changed.

### Acceptance criteria

- Clone remains independent from the source repository.
- Warm launch latency is lower than baseline.
- No fixed multi-second readiness delay remains.
- Repeated launches do not restart install units unnecessarily.

---

## Phase 8 — Minimize the guest package closure — **DONE** (unconditional since the `full` path was deleted)

Implemented in `../guest.nix` (`guestCommonPackages` with a per-package
rationale, `guestShell = pkgs.bashInteractive`,
`environment.defaultPackages = [ ]`), plus the registry's per-agent
`extraPackages` so an agent's own runtime dependencies are added only while that
agent is selected. It was gated behind the profile's `minimalGuestPackages` field
until the `full` path (the historical toolset plus the fish login shell) was
deleted.

### Tasks

Define a small common package set for the lite profile:

```nix
[
  bashInteractive
  coreutils
  findutils
  git
  gnugrep
  gnused
  gawk
  patch
  ripgrep
  jq
  openssh # interactive mode only
]
```

Add packages only when required by the selected agent or repository workflow.

Review and remove:

- unused shells;
- documentation;
- locale data beyond what is needed;
- compilers and language runtimes included only for disabled agents;
- duplicate CLI tools;
- package managers not required by the target workload;
- batch utilities from interactive images;
- SSH packages from batch images.

Do not remove basic diagnostic tools needed to understand failures. Prefer keeping a deliberately small troubleshooting set.

### Acceptance criteria

- Every package in the lite image has a documented consumer.
- Agent-specific runtimes are included only when that agent is enabled.
- Interactive and batch closures differ appropriately.
- Closure-size regression checks are added.

---

## Phase 9 — Testing

### Evaluation tests

Add Nix evaluation tests for:

- unknown agent names;
- incompatible capability/transport combinations (`capabilities = [ ]`, an
  unknown capability token, `enableSsh` without `interactive`, a `batch` host
  with no batch-capable agent — all DONE, see
  `checks.microvm-capabilities`);
- zero-slot configurations;
- duplicate VSOCK CIDs;
- duplicate resource identifiers;
- invalid config paths;
- attempts to stage paths outside the host home;
- persistence options used when persistence is disabled;
- VSOCK transport selected with internet access (DONE — `vsock` +
  `package-access`/`internet` are rejected, pinned to the message, and so are
  `vsock` without an `sshPublicKeyFile`, without
  `programs.ssh.systemd-ssh-proxy.enable`, with a divergent `stateRoot` and with
  `litellmPort = 8888`, the port microvm.nix reserves for the notify socket).

### VM integration tests

Test at least (SUPERSEDED in part: there is no `full`/`lite` axis any more, so
the matrix collapses to the ONE guest shape crossed with `capabilities` and
`networkProfile`):

1. `capabilities = [ "interactive" ]`, Codex only, VSOCK transport;
2. `capabilities = [ "batch" ]`, Codex only, VSOCK transport;
3. ~~full compatibility profile~~ — removed with the `full` path; its
   replacement is the evaluated-slice proof described in
   [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path);
4. TAP internet/package profile if supported;
5. two concurrent slots.

Verify:

- agent can modify `/workspace`;
- agent cannot modify source repository;
- agent cannot modify staged host configuration;
- home changes disappear after session destruction;
- disabled agent executables are absent;
- host sockets are absent;
- host home is absent;
- host store is absent in secure mode;
- host Nix daemon is absent;
- internet, LAN, VPN, DNS, and metadata access fail in proxy-only mode;
- LiteLLM access succeeds;
- arbitrary host-port access fails;
- agent cannot modify root-owned batch control files;
- slot cleanup removes mounts, processes, and temporary state.

### Adversarial filesystem tests

From inside the guest, attempt:

- symlink replacement in the workspace;
- hardlink attacks where supported;
- path traversal through staged config;
- modification of `controller/` and `input/`;
- remount attempts;
- access to other slots;
- access to SSH private host keys;
- creation of setuid files;
- persistence through `/nix/store`, root, home, logs, and machine-id.

### Resource tests

Verify host-side enforcement of:

- memory limit;
- CPU quota;
- process/task limit;
- disk usage;
- maximum session duration if configured.

### Acceptance criteria

- Tests run in CI or through a documented local command.
- Security failures are fatal.
- Benchmarks report improvements or regressions for each major phase.
- The removal of the `full` profile is proved by an evaluated-slice diff against
  the pre-collapse `lite` shape, not by keeping `full` tests alive.

---

## Phase 10 — Documentation and rollout

### Documentation

Document:

- the threat model;
- what the MicroVM protects;
- what it does not protect;
- config-staging rules;
- credential handling;
- ~~full versus lite profiles~~ — there is ONE guest shape; document THAT
  instead ([The guest shape](./agent-microvm.md#the-guest-shape));
- interactive versus batch modes;
- VSOCK versus TAP transport;
- persistence semantics;
- how to add a new agent safely;
- how to inspect staged configuration;
- how to benchmark and troubleshoot launches.

Explicitly state that any credential intentionally exposed to the guest may be exfiltrated or abused by the agent. Filesystem isolation cannot prevent misuse of credentials the process is authorized to read.

### Rollout sequence

1. Merge baseline measurement tooling.
2. ~~Merge the lite profile with existing implementation paths.~~ DONE, then
   collapsed — there is one path.
3. Merge selected-agent filtering. DONE.
4. ~~Merge runtime config staging behind an option.~~ DONE, then made
   unconditional.
5. ~~Enable config staging by default only in the lite profile.~~ SUPERSEDED:
   it is unconditional.
6. Consolidate shares. DONE (one writable + one read-only share).
7. ~~Split interactive and batch modes.~~ DONE (`capabilities`, default both).
8. Add VSOCK transport behind an option (phase 6).
9. Make VSOCK the proxy-only default after tests pass. NOT done, deliberately:
   the transport is opt-in per host (`capabilities = [ … "vsock" ]`). Flipping the
   module-wide default would silently remove every guest's network interface —
   including on hosts whose operators expect `agent-microvm ssh` over the TAP —
   and the AF_VSOCK model path has not had its real-KVM run yet. That run is the
   gate for this step.
10. Optimize cloning, readiness, and install-unit behavior.
11. ~~Review whether any lite changes should become full-profile defaults.~~
    SUPERSEDED: every lightweight change IS the default; the `full` path is
    gone.

Keep each phase independently reviewable and revertible.

---

## Definition of done

The work is complete when all of the following are true:

- A Codex-only interactive VM can be launched with one command.
- It uses one slot, two vCPUs, and approximately 4 GiB RAM by default.
- It uses a separate optimized guest store.
- It does not run guest Home Manager activation.
- It receives current, allowlisted host agent configuration through a disposable staged copy.
- It has one writable session virtiofs share and, when needed, one read-only SSH-key share.
- It has no ordinary network interface in proxy-only mode. **DONE** for a host
  that selects the `vsock` capability (phase 6's model transport): the guest then
  declares no `microvm.interfaces`, runs no networkd/dhcpcd and has no address or
  route. A host that does NOT select `vsock` still gets the TAP + the
  `AGENT_MICROVM_*` chains, which is the unchanged default — the switch is
  opt-in, per host, exactly like every other capability.
- It can access only the fixed host LiteLLM endpoint through VSOCK. **DONE** for
  a `vsock`-transport host: one destination-fixed forwarder per VM, confined to
  the host loopback. RUNTIME confirmation of the AF_VSOCK hop is part of the
  pending real-KVM run.
- It cannot access the host home, host store, host Nix daemon, control sockets, LAN, internet, VPN, DNS, or metadata services.
- Disabled agents and their runtimes are absent from the guest closure.
- Repository clones are independent and disposable.
- Interactive and batch modes do not include each other’s unnecessary services. DONE for hosts that narrow `capabilities`; the DEFAULT still selects both on purpose (so nothing a host relies on disappears silently).
- Launch latency, closure size, host process count, and virtiofsd count are all
  lower than the recorded baseline. **PARTIALLY DONE**, measured with
  `tests/measure-boot.sh`: guest closure −17.4 % and runner closure −13.3 % for
  the unchanged f13 shape (−71.4 % / −53.3 % for the Codex-only shape the plan
  aims at), virtiofsd 4 → 2 per slot, generated guest units 66 → 65 (→ 62 under
  the `vsock` transport), and the host's shared network units (bridge socket,
  bridge IPv6 oneshot, per-TAP attach) gone entirely on a `vsock`-transport host.
  LAUNCH LATENCY and idle RSS are NOT measured yet — they need `/dev/kvm` and are
  deliberately not estimated; see
  [runtime validation → Measurements](./agent-microvm-runtime-validation.md#measurements-plan-phase-0--the-phase-6-acceptance-numbers).
- ~~The full profile remains available and compatible.~~ SUPERSEDED: the `full`
  profile was deliberately removed once the lightweight path was reviewed; the
  compatibility requirement was replaced by the evaluated-slice proof described in
  [Collapsing the two profiles into one path](#collapsing-the-two-profiles-into-one-path).
- Security and integration tests pass.

---

## Coding-agent operating instructions

While implementing this plan:

- Work in small commits aligned with the phases above.
- Do not combine security-boundary changes with unrelated formatting or refactoring.
- Preserve existing behavior unless a phase explicitly changes it.
- Add tests before changing defaults.
- Report benchmark results in every performance-related commit.
- Stop and document the issue rather than weakening an invariant to make a test pass.
- Do not add broad host mounts, generic host-port forwarding, privileged guest services, or shared credentials as shortcuts.
- Prefer explicit Nix assertions over runtime failure for invalid configurations.
- Keep the secure guest shape understandable from its generated units and mount declarations; avoid hidden imperative state.
