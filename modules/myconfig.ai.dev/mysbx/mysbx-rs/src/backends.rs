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
    /// Whether the backend honors `nested-virt = true`
    /// (docs/design/config.md D27). Every other backend REFUSES the
    /// key, never silently drops it.
    pub nested_virt: bool,
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
    /// (`allow-domains`/`connect-ports`/`listen-ports`):
    /// `true` only for nono (nono's per-domain proxy and
    /// Landlock/seccomp port grants). Every other backend refuses
    /// an allowlist (config.md D21), never accepts-and-ignores it.
    pub network_allowlist: bool,
    /// Whether the backend enforces the D20 proxy-only network
    /// profile (`network = "proxy-only"`): `true` for the bwrap-built
    /// tiers, whose unshared netns plus forwarder socket and
    /// in-sandbox relay is the shipped transport (bd myconfig-mo3.2.6,
    /// myconfig-vpi9.1). Every other backend refuses the profile.
    pub network_proxy_only: bool,
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
        nested_virt: false,
        detach_refused: false,
        waypipe_refused: false,
        network_allowlist: false,
        network_proxy_only: true,
    },
    Backend {
        name: "podman-gvisor",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.podman.enable"),
        vm: false,
        store_disk: false,
        sizes_sandbox: false,
        nested_virt: false,
        detach_refused: true,
        waypipe_refused: false,
        network_allowlist: false,
        network_proxy_only: false,
    },
    Backend {
        name: "nono",
        always_on: true,
        enable_option: None,
        vm: false,
        store_disk: false,
        sizes_sandbox: false,
        nested_virt: false,
        detach_refused: false,
        waypipe_refused: false,
        network_allowlist: true,
        network_proxy_only: true,
    },
    Backend {
        name: "podman-krun",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.podman.enable"),
        vm: false,
        store_disk: false,
        sizes_sandbox: true,
        nested_virt: false,
        detach_refused: true,
        waypipe_refused: true,
        network_allowlist: false,
        network_proxy_only: false,
    },
    Backend {
        name: "krun",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.krun.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: false,
        detach_refused: false,
        waypipe_refused: true,
        network_allowlist: false,
        network_proxy_only: false,
    },
    Backend {
        name: "microvm-qemu",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.microvm-qemu.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: true,
        detach_refused: false,
        waypipe_refused: true,
        network_allowlist: false,
        network_proxy_only: false,
    },
    Backend {
        name: "microvm-ch",
        always_on: false,
        enable_option: Some("myconfig.ai.dev.mysbx.microvm-ch.enable"),
        vm: true,
        store_disk: true,
        sizes_sandbox: true,
        nested_virt: true,
        detach_refused: false,
        waypipe_refused: true,
        network_allowlist: false,
        network_proxy_only: false,
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
            // D21's allowlist is enforced by exactly one backend
            // (nono); D20's proxy-only profile by the bwrap-based
            // tiers. The registry predicates the egress work asks
            // stay derivable from the table (bd myconfig-mo3.2.1).
            assert_eq!(
                b.network_allowlist,
                b.name == "nono",
                "`{}`: network_allowlist must match config.md D21's table",
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
