// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The backend registry (bd myconfig-9wkp.2): one table describing
//! every backend the CLI accepts, instead of the ~6 hard-coded
//! name lists that lib.rs and doctor.rs each kept a private copy
//! of. Adding `microvm-ch` (the epic this refactor serves) is a
//! table row plus its builder, never a hunt for every
//! `matches!(backend, …)` chain.
//!
//! The registry owns NAMES and CAPABILITY PREDICATES, not behavior:
//! each call site that matched a name list before now asks a
//! predicate here, and the predicate is the same set of names the
//! old match arm listed — the golden tests must not see a diff.
//!
//! `REGISTRY` is the single source of truth. `NAMES` (the ordered
//! universe the doctor listing, the `--<backend>` shorthands and
//! the refusal messages iterate) is spelled per row for Rust's
//! const-eval limits — [`the_table_and_the_names_agree`] enforces
//! the two stay in lockstep, so adding a row without its name (or
//! vice versa) fails the suite, never a silent drift.

/// How a backend handles D21's allowlist keys
/// (`allow-domains`/`connect-ports`/`listen-ports`) — the
/// restructure of the old `network_allowlist` bool (bd
/// myconfig-ew9s, docs/design/egress-connect.md E12): the two
/// booleans could not express "enforceable only with a broker
/// pinned" (bubblewrap's row would need to be true with the pin and
/// false without it, while nono's is unconditionally true, and
/// `network_proxy_only` already says something else about both —
/// so the D20/D21 distinction would collapse). One enum instead:
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Allowlist {
    /// The backend cannot enforce the keys; the run is refused
    /// (config.md D21's table).
    Refused,
    /// nono enforces them itself: its per-domain hostname proxy,
    /// Landlock and seccomp port grants (bd myconfig-6di.2).
    NonosEgress,
    /// The bwrap-built tiers enforce them through the per-run
    /// egress broker, and only when the wrapper pinned one
    /// (`MYSBX_EGRESS_BROKER`); without the pin the run is refused,
    /// never advisory (docs/design/egress-connect.md E7/E10).
    Broker,
}

/// How a backend treats a config key that asks for a FEATURE it does
/// not have. The split is a boundary rule: a missing feature only
/// ever removes capability, so the run proceeds with a warning — the
/// same shape `store = "baked"`'s fallback and D23's unenforced
/// limits already use. A key whose whole content is a BOUNDARY (a
/// network narrowing, an allowlist, the merge's user-deny upgrade)
/// is never in this table: weakening or widening a boundary is
/// refused, not traded for a warning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingFeature {
    /// The backend has the feature; the key is honored.
    Honored,
    /// The backend lacks it; the key is dropped with a warning and
    /// the run continues (the `nested-virt` case, config.md D27).
    WarnAndDrop,
}

/// One backend's row (all fields are facts about the backend, never
/// run state).
pub struct Backend {
    /// The `backend = "…"` / `--backend …` spelling — a public,
    /// stable contract (config.md, cli.md D18).
    pub name: &'static str,
    /// Whether the backend is always available: `bubblewrap` and
    /// `nono` are the base tier (backends.md D10) and have no module
    /// opt-in, so they can never be refused as disabled.
    pub always_on: bool,
    /// The module option that enables the backend on a host (the
    /// disabled-refusal hint, backends.md D10). `None` exactly when
    /// `always_on` is true.
    pub enable_option: Option<&'static str>,
    /// Whether the backend runs a real VM with the shared krun
    /// contract (backends.md D3/D9): the staged-spec argv, the
    /// guest init, the manifest channel, virtio shares. This is
    /// the "direct VM" tier — podman-krun is a podman tier with a
    /// VM runtime inside, not a direct VM backend, and its argv
    /// stays the podman-gvisor shape.
    pub vm: bool,
    /// Whether the backend can attach the guest-own store disk
    /// (`store = "baked"`, docs/design/guest-store.md): only the
    /// direct VM backends can (a virtio-blk device on the shared
    /// guest contract).
    pub store_disk: bool,
    /// Whether `memory`/`cpus` size the sandbox itself (they size
    /// the VM) instead of being unenforceable limits: the direct VM
    /// backends plus the podman-krun VM runtime (the krun limits
    /// mapping); `pids-limit` is REFUSED by the direct builders
    /// either way and warned about on podman-krun.
    pub sizes_sandbox: bool,
    /// What the backend does with `nested-virt = true`
    /// (docs/design/config.md D27): `microvm-qemu` and `microvm-ch`
    /// honor it, every other backend WARNS and drops it — a run that
    /// starts without the feature is the point of the key, so a
    /// missing feature never blocks the backend (the run path
    /// pre-flights the host's `nested` parameter only for a backend
    /// that honors the key). `krun` is a VM but its guest kernel has
    /// no KVM, so it drops the key like the container tiers do.
    pub nested_virt: MissingFeature,
    /// Whether `--detach`/`--detachable` are refused outright
    /// (docs/design/detach.md B9): the podman backends carry a
    /// per-repo container name and `--replace`, so a detached run
    /// is not safe from the runs that follow it.
    pub detach_refused: bool,
    /// Whether `display = "waypipe"` is refused (backends.md D9's
    /// table): AF_UNIX socket objects do not cross virtiofs, so
    /// the VM backends with no guest channel for it refuse it.
    pub waypipe_refused: bool,
    /// Whether the backend enforces D21's allowlist keys
    /// (`allow-domains`/`connect-ports`/`listen-ports`) — the HOW
    /// is [`Allowlist`]: nono enforces them itself; the bwrap-built
    /// tiers enforce them through the per-run egress broker, and
    /// only when the wrapper pinned one; every other backend
    /// refuses the keys (config.md D21), never accepts-and-ignores
    /// them.
    pub allowlist: Allowlist,
    /// Whether the backend enforces the D20 proxy-only network
    /// profile (`network = "proxy-only"`): `true` for the bwrap-built
    /// tiers, whose unshared netns plus forwarder socket and
    /// in-sandbox relay is the shipped transport (bd myconfig-mo3.2.6,
    /// myconfig-vpi9.1). Every other backend refuses the profile.
    pub network_proxy_only: bool,
    /// Whether the backend enforces the fvi public profile
    /// (`network = "public"`, bd myconfig-fvi): the SAME empty-netns
    /// transport as `proxy-only`, with the broker's `--public`
    /// CONNECT lane (every target admitted, the hard-deny set the
    /// rule). `true` for the plain `bubblewrap` backend only: the
    /// layered nono tier maps its OWN grant model over the netns
    /// (its argv builder cannot express the deny set), and the VM and
    /// podman transports share or fully route. Every other backend
    /// refuses the value (D21: never accepted-and-ignored).
    pub network_public: bool,
}

impl Backend {
    /// Whether the backend belongs to the podman tier: the
    /// podman-gvisor argv shape, the image and its in-image pins.
    /// podman-krun shares the image and swaps the runtime.
    pub fn podman(&self) -> bool {
        // podman-gvisor and podman-krun: matched by name before;
        // there is no third podman backend.
        self.name == "podman-gvisor" || self.name == "podman-krun"
    }
}

/// The registry — one row per backend, in the doctor's listing
/// order. THE table: every name-matching call site funnels through
/// [`find`] and asks a predicate of the row it gets.
pub const REGISTRY: &[Backend] = &[
    Backend {
        name: "bubblewrap",
        always_on: true,
        enable_option: None,
        vm: false,
        store_disk: false,
        sizes_sandbox: false,
        nested_virt: MissingFeature::WarnAndDrop,
        detach_refused: false,
        waypipe_refused: false,
        allowlist: Allowlist::Broker,
        network_proxy_only: true,
        network_public: true,
    },
    Backend {
        name: "podman-gvisor",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.podman.enable"),
        vm: false,
        store_disk: false,
        sizes_sandbox: false,
        nested_virt: MissingFeature::WarnAndDrop,
        detach_refused: true,
        waypipe_refused: false,
        allowlist: Allowlist::Refused,
        network_proxy_only: false,
        network_public: false,
    },
    Backend {
        name: "nono",
        always_on: true,
        enable_option: None,
        vm: false,
        store_disk: false,
        sizes_sandbox: false,
        nested_virt: MissingFeature::WarnAndDrop,
        detach_refused: false,
        waypipe_refused: false,
        allowlist: Allowlist::NonosEgress,
        network_proxy_only: true,
        network_public: false,
    },
    Backend {
        name: "podman-krun",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.podman.enable"),
        vm: false,
        store_disk: false,
        sizes_sandbox: true,
        nested_virt: MissingFeature::WarnAndDrop,
        detach_refused: true,
        waypipe_refused: true,
        allowlist: Allowlist::Refused,
        network_proxy_only: false,
        network_public: false,
    },
    Backend {
        name: "krun",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.krun.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: MissingFeature::WarnAndDrop,
        detach_refused: false,
        waypipe_refused: true,
        allowlist: Allowlist::Refused,
        network_proxy_only: false,
        network_public: false,
    },
    Backend {
        name: "microvm-qemu",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.microvm-qemu.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: MissingFeature::Honored,
        detach_refused: false,
        waypipe_refused: true,
        allowlist: Allowlist::Refused,
        network_proxy_only: false,
        network_public: false,
    },
    Backend {
        name: "microvm-ch",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.microvm-ch.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: MissingFeature::Honored,
        detach_refused: false,
        waypipe_refused: true,
        allowlist: Allowlist::Refused,
        network_proxy_only: false,
        network_public: false,
    },
];

/// The ordered backend-name universe — the doctor's listing, the
/// `--<backend>` shorthands and the refusal messages' "available"
/// list iterate this. One name per [`REGISTRY`] row, same order;
/// [`the_table_and_the_names_agree`] pins the two together.
pub const NAMES: &[&str] = &[
    "bubblewrap",
    "podman-gvisor",
    "nono",
    "podman-krun",
    "krun",
    "microvm-qemu",
    "microvm-ch",
];

/// The row of a backend name, if the registry knows it — the ONE
/// lookup every name-matching call site funnels through.
pub fn find(name: &str) -> Option<&'static Backend> {
    REGISTRY.iter().find(|b| b.name == name)
}

/// The ordered backend-name universe as an owned vec — the
/// convenience spelling a few callers use.
pub fn names() -> Vec<&'static str> {
    NAMES.to_vec()
}

/// The "available" spelling of the universe for a refusal message:
/// `` `name1`, `name2` `` — the exact format the old hard-coded
/// messages printed, kept byte-stable for the tests that match on
/// them.
pub fn names_backticked() -> String {
    NAMES
        .iter()
        .map(|n| format!("`{n}`"))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The table's contract with the universe: one name per row,
    /// same order — adding a row without its name (or a name
    /// without a row) fails here, never drifts silently.
    #[test]
    fn the_table_and_the_names_agree() {
        assert_eq!(
            REGISTRY.iter().map(|b| b.name).collect::<Vec<_>>(),
            NAMES.to_vec(),
            "REGISTRY and NAMES must list the same backends in the same order"
        );
    }

    /// The structural invariants the module layer and the doctor
    /// rely on: exactly one row per name, the always-on pair has
    /// no enable option, every opt-in backend names one.
    #[test]
    fn the_registry_is_wellformed() {
        for (i, b) in REGISTRY.iter().enumerate() {
            assert!(!b.name.is_empty());
            // unique names
            assert!(
                !REGISTRY[i + 1..].iter().any(|o| o.name == b.name),
                "duplicate backend `{}`",
                b.name
            );
            assert_eq!(
                b.always_on,
                b.enable_option.is_none(),
                "`{}`: always-on and the enable option must agree",
                b.name
            );
            // The always-on base tier is exactly the documented pair.
            assert_eq!(
                b.always_on,
                b.name == "bubblewrap" || b.name == "nono",
                "`{}`: always-on is the base tier's pair",
                b.name
            );
            // A direct VM backend carries the shared krun contract:
            // the store disk belongs to the VM tier (backends.md D9).
            if b.vm {
                assert!(
                    b.store_disk,
                    "`{}`: a VM backend attaches the store disk",
                    b.name
                );
            }
            // D21's allowlist enforcement, per the E12 restructure
            // (bd myconfig-ew9s): nono enforces it itself; the
            // bwrap-built tiers through the per-run broker (and
            // only with one pinned — the gate asks the pin too);
            // every other backend refuses. The old `network_allowlist`
            // bool could not express the middle row (true with a pin,
            // false without one, while `network_proxy_only` already
            // says something else about the same backends), which is
            // why this is an enum. The D20/D21 distinction stays:
            // the proxy-only profile remains its own bool below.
            let expect = if b.name == "nono" {
                Allowlist::NonosEgress
            } else if b.name == "bubblewrap" {
                Allowlist::Broker
            } else {
                Allowlist::Refused
            };
            assert_eq!(
                b.allowlist, expect,
                "`{}`: allowlist must match config.md D21's table",
                b.name
            );
            // D20's proxy-only profile needs an EMPTY network
            // namespace to put one loopback port into, which only the
            // bwrap-built tiers have (bd myconfig-mo3.2.6 for
            // bubblewrap, bd myconfig-vpi9.1 for the layered nono
            // backend, which IS bubblewrap plus nono inside). That is
            // exactly the always-on base tier; every other backend
            // refuses the profile.
            assert_eq!(
                b.network_proxy_only, b.always_on,
                "`{}`: network_proxy_only must match config.md D20's table",
                b.name
            );
            // D27's nested-virt honoring set: a backend without a VM
            // at all can never nest, so `Honored` implies `vm`. The
            // converse does NOT hold — `krun` is a VM whose guest
            // kernel has no KVM, so it drops the key with a warning
            // (a missing feature, never a refusal).
            assert!(
                b.nested_virt != MissingFeature::Honored || b.vm,
                "`{}`: a backend without a VM cannot honor nested-virt",
                b.name
            );
            assert_eq!(
                b.nested_virt == MissingFeature::Honored,
                b.name == "microvm-qemu" || b.name == "microvm-ch",
                "`{}`: the two direct VM backends are the honoring set",
                b.name
            );
        }
    }

    #[test]
    fn find_answers_every_name_and_refuses_the_rest() {
        assert_eq!(find("bubblewrap").map(|b| b.name), Some("bubblewrap"));
        assert_eq!(find("microvm-qemu").map(|b| b.name), Some("microvm-qemu"));
        assert_eq!(find("microvm-ch").map(|b| b.name), Some("microvm-ch"));
        assert!(find("").is_none());
        // The ordered universe, in registry order.
        assert_eq!(
            NAMES.to_vec(),
            vec![
                "bubblewrap",
                "podman-gvisor",
                "nono",
                "podman-krun",
                "krun",
                "microvm-qemu",
                "microvm-ch",
            ]
        );
        // The refusal-message spelling is byte-stable.
        assert_eq!(
            names_backticked(),
            "`bubblewrap`, `podman-gvisor`, `nono`, `podman-krun`, `krun`, `microvm-qemu`, `microvm-ch`"
        );
    }
}
