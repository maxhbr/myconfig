// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The direct-libkrun backend spec builder (`backend = "krun"`,
//! docs/design/backends.md D3, bd myconfig-dak.3).
//!
//! Precedent: the spike of bd myconfig-dak.1 proved the chain live on
//! f13 — bwrap → mysbx-krun → VM, 282 ms warm boot, ro/rw virtiofs
//! enforcement, exit-code propagation — with no podman, no crun and
//! no OCI image anywhere. This builder turns the mysbx merged config
//! into the argv of THAT launcher binary (../krun-rs, packaged by
//! nix/krun-launcher.nix): lib.rs execs
//! `Command::new(bwrap).args(bwrap_argv_of_krun_argv)`, where the
//! inner argv is what this module produces — the same two-layer
//! convention as the nono backend (backends.md D1: bwrap carries the
//! host-side filesystem view, the confined child builds the VM).
//!
//! The argv is the STABLE TEXT FORM of the spec (the bead's words):
//! one flag per line under `--dry-run`, byte-comparable by the
//! golden tests, and — because the launcher's flags ARE the spec —
//! the dry run prints the very argv a real run execs. Sections, in
//! fixed order:
//!
//! 0. `--cpus N --ram MIB` — `krun_set_vm_config`. The defaults (2
//!    vCPU, 2048 MiB) match the spike's launcher; the config keys
//!    `cpus`/`memory` override them through the same podman-shaped
//!    value grammar `parse_krun_cpus`/`parse_krun_ram_mib` accept
//!    (the keys are backend-agnostic, config.md D23 — a value a VM
//!    cannot express is refused, never rounded, exactly as on
//!    podman-krun).
//! 1. `--rootfs DIR` — the Nix-built plain directory shared
//!    read-only over virtiofs as KRUN_FS_ROOT_TAG (D3: it bakes only
//!    the guest entry path — the static busybox, the guest init,
//!    shell symlinks and mountpoints; every toolchain resolves
//!    through the ro store share).
//! 2. `--init PATH` — the guest entry (bd myconfig-dak.5) baked into
//!    the rootfs; it receives the payload as its argv (`$1..`) and
//!    the shares' placement as `MYSBX_KRUN_SHARES` in the env.
//! 3. `--ro-share`/`--rw-share TAG@DEST=DIR` — one per virtiofs
//!    share. The ro store share (D3's default: the whole host
//!    /nix/store, the same visibility the bwrap tier grants) comes
//!    first, then the workspace, then every configured mount and
//!    state-dir backing store. The virtiofs SERVER enforces the ro
//!    flag; the guest kernel cannot remount it (spike probe 2).
//!    DEST is the GUEST path of the share mount — under
//!    `/tmp/mysbx-shares/` on the guest tmpfs, never below another
//!    virtiofs mount (spike finding 9: nesting returns EBUSY); the
//!    payload sees each share at its SANDBOX path through the rootfs
//!    symlink farm the guest init mounts (section 5 of the init).
//! 4. `--env K=V` — the payload environment, in the exact order
//!    bwrap.rs/podman_gvisor.rs emit it (config layers, then
//!    infrastructure, then the forwarded host variables). The
//!    launcher passes an ALWAYS-explicit envp: envp=NULL would
//!    inherit the whole (bwrap-cleared) host environment (spike
//!    finding 3).
//! 5. `--chdir DIR` — the payload's working directory
//!    (`krun_set_workdir`): the repo root, where every other
//!    backend's `--chdir`/`--workdir` starts the payload too.
//! 6. `-- CMD [ARGS...]` — the payload, verbatim. The launcher
//!    rides it on the kernel cmdline behind ` -- `; /init.krun
//!    overwrites argv[0] with KRUN_INIT, so the guest entry sees
//!    the payload at `$1..` and `exec "\"$@\"`s it (spike finding 11).
//!
//! Exit codes (cli.md D8): a plain run EXECs bwrap → launcher →
//! VM, so the payload's own code propagates unchanged through every
//! link (live-proven: the spike's exit-42 probe). The links' OWN
//! codes are disjoint from mysbx's `70` by construction: bwrap
//! exits 125 on setup failure, the launcher exits 2 on a usage
//! error and 125 on a libkrun setup failure, and libkrun's implicit
//! init maps its own setup failures to 125 and the payload's exec
//! failure to 126/127 — the same values a payload may exit itself,
//! which is exactly the bwrap backend's property and needs no
//! remapping: mysbx exited before them, its own `70` never rides
//! the chain.
//!
//! Refusals, shaped like the podman ones (`Error` below): a config a
//! VM cannot enforce is refused, never accepted-and-ignored — the
//! same rule as every backend. The builder is pure (no canonicalization,
//! no existence checks — the merge already did them, config.md D8).

use crate::bwrap::{Payload, Workspace};
use crate::config::Multiplexer;
use crate::merge::Merged;
use crate::podman_gvisor::{parse_krun_cpus, parse_krun_ram_mib};
use crate::repo::Repo;
use std::borrow::Cow;

pub type HostEnv = std::collections::BTreeMap<String, String>;

/// The guest-side root of every virtiofs share mount (backends.md
/// D3): the guest init mounts a tmpfs on /tmp (the ro root virtiofs
/// can take no mountpoints below it, and NESTING a second virtiofs
/// device under the root share returns EBUSY — spike finding 9),
/// then mounts every share's tag under this directory keyed by the
/// tag, so the sandbox paths the payload sees are symlinks the init
/// controls.
pub const GUEST_SHARE_ROOT: &str = "/tmp/mysbx-shares";

/// The guest-side link target of the ro host store share: the share
/// is the `store` SLOT of the staged ro device (the rootfs's
/// `/nix/store` is a symlink to this path, so store paths resolve
/// unchanged once the init mounted the device). The store needs no
/// device of its own — grouping it with the other ro shares keeps
/// the device count at two, and `/nix/store` is ro exactly like
/// every other ro share.
pub const GUEST_STORE: &str = "/tmp/mysbx-shares/stage-ro/store";

/// The device tags of the two STAGED share devices (live finding,
/// first wrapped run): every krun_add_virtiofs3 tag is a full
/// virtiofs device, and libkrun's MMIO budget is 11 slots
/// (arch IRQ_BASE=5..IRQ_MAX=15) minus balloon, rng, the implicit
/// console and the implicit vsock — about 6 fs slots total. One
/// share per device cannot scale; instead the caller stages every
/// share's host dir inside ONE tree per access mode and shares the
/// whole tree as ONE device. The device count is constant, the
/// share count is not.
pub const STAGE_RO_TAG: &str = "stage-ro";
pub const STAGE_RW_TAG: &str = "stage-rw";

/// The host-side root of the per-run staging trees (inside the
/// bwrap view, never shared into the guest as a sandbox path):
/// `<STAGE_ROOT>/<ro|rw>/<slot>` is the slot dir of one share, the
/// place bwrap binds the share's host dir at. The trees live in the
/// sidecar (`<sidecar>/krun-stage/<pid>/…`), the same per-run
/// lifecycle as the git trust files.
pub const STAGE_ROOT: &str = "/mysbx-krun-stage";

/// The manifest's slot inside the ro stage tree (krun-rootfs.nix's
/// guest init reads it at GUEST_STAGE_RO/manifest; the launcher
/// writes the records there — the cmdline-budget finding).
pub const MANIFEST_SLOT: &str = "manifest";

/// The guest-side mount of the ro staging device. All ro shares'
/// sandbox paths link at `<GUEST_STAGE_RO>/<slot>`.
pub const GUEST_STAGE_RO: &str = "/tmp/mysbx-shares/stage-ro";

/// The guest-side mount of the rw staging device.
pub const GUEST_STAGE_RW: &str = "/tmp/mysbx-shares/stage-rw";

/// The default vCPU count of a run (the spike's launcher default;
/// `cpus` in the config overrides it).
pub const DEFAULT_CPUS: u32 = 2;

/// The default RAM of a run, in MiB (the spike's launcher default;
/// `memory` in the config overrides it).
pub const DEFAULT_RAM_MIB: u32 = 2048;

/// Everything lib.rs hands the builder besides the merged config —
/// the pins and resolved layout a run carries (the same shape as
/// bwrap::Params/podman_gvisor::Params).
pub struct Params<'a> {
    /// The rootfs derivation's store path (nix/krun-spike-rootfs.nix's
    /// successor, the baked guest init of bd myconfig-dak.5 inside):
    /// a PLAIN DIRECTORY, shared read-only as KRUN_FS_ROOT_TAG.
    pub rootfs: &'a str,
    /// The payload shell — a store path, resolved through the ro
    /// store share once the guest init has mounted it (D3: nothing
    /// is baked for the payload).
    pub shell: &'a str,
    /// The dev-tool closure on PATH (the same MYSBX_TOOLS_PATH pin
    /// the bwrap backend carries): a host store path, visible
    /// inside the guest through the ro store share.
    pub tools_path: &'a str,
    /// The CA bundle pin (MYSBX_CA_BUNDLE): a host store path whose
    /// `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` entries
    /// are infrastructure — set after the config layers, so no
    /// layer can repoint the trust anchors. `None` for an unwrapped
    /// build sets none (the same contract as bwrap::Params).
    pub ca_bundle: Option<&'a str>,
    /// The multiplexer entry, a store path — `None` refuses a config
    /// that selects a session multiplexer, exactly as on the other
    /// backends (`Error::MultiplexerUnavailable`).
    pub mux_entry: Option<&'a str>,
    /// The live/clone workspace of the run (the repo itself, or the
    /// session clone bound at the repo's own path).
    pub workspace: Workspace<'a>,
    /// The `memory`/`cpus` limits, already env-pin-overridden by
    /// lib.rs (config.md D23): podman-shaped values, parsed with the
    /// SAME grammar the podman-krun annotations use — a value the VM
    /// cannot express is refused, never rounded.
    pub memory: Option<Cow<'a, str>>,
    pub cpus: Option<Cow<'a, str>>,
    /// The guest-root git trust (bd myconfig-zj2's krun twin, bd
    /// myconfig-dak.5): the payload runs as GUEST ROOT over
    /// virtiofs files that keep their host uid, so every ordinary
    /// git command dies with `dubious ownership` without it.
    /// `Some` adds two ro shares — the per-run global config at
    /// `/etc/mysbx/gitconfig` (`GIT_CONFIG_GLOBAL`) and the libgit2
    /// system config at `/etc/gitconfig` (bd myconfig-jn0) — and
    /// sets `GIT_CONFIG_GLOBAL` last, after every config `[env]`.
    /// `None` (a `--dry-run`, an unwrapped build) shares no trust.
    pub git_trust: Option<&'a GitTrust<'a>>,
}

/// The two host files of a run's git trust (the podman arm's
/// `gittrust/<pid>/` pair): the global config (`git_trust_text`'s
/// exact-path entries plus the in-sandbox user-config includes) and
/// the libgit2 system config (`libgit2_trust_text`'s exact entries
/// only). Bound read-only as virtiofs shares, never writable.
pub struct GitTrust<'a> {
    pub global_host: &'a str,
    pub system_host: &'a str,
}

/// What the direct-krun builder refuses — each a configuration a
/// microVM cannot enforce or express, never accepted-and-ignored.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// The config selects a session multiplexer but no entry is
    /// baked for the guest (the same refusal as bwrap/podman: absent
    /// infrastructure is a loud failure, never a plain-shell
    /// fallback).
    MultiplexerUnavailable { multiplexer: Multiplexer },
    /// A `pids-limit` a whole-VM run cannot express: the VM is the
    /// boundary, no pids controller is wired for it — the same
    /// refusal as the podman-krun variant (backends.md D2).
    PidsLimit { pids: u32 },
    /// A `cpus`/`memory` value the VM cannot express (fractional
    /// vCPUs, sub-128-MiB memory) — reused from the podman-krun
    /// parser, whose variants carry the raw value and the expected
    /// shape.
    KrunLimit {
        key: &'static str,
        raw: String,
        why: &'static str,
    },
    /// A configured mount's `dest` (or same-path source) the guest
    /// cannot place: a single-component absolute path (`/data`) —
    /// its link would have to sit on the READ-ONLY root virtiofs
    /// (no new entries, EROFS) and no second virtiofs device may
    /// nest below the root share (EBUSY, spike finding 9). Paths
    /// with a parent get a tmpfs at their first component; paths
    /// at the root itself are refused, never silently mis-placed.
    RootLevelDest { dest: String },
    /// A mount's `dest` collides with the fixed sandbox paths the
    /// rootfs bakes links for (config.md D14/D15): /nix/store and
    /// /mysbx-home are the store share's and the tmpfs home's
    /// contract paths — a share may live UNDER the home
    /// (`/mysbx-home/.cache`, the state-dirs model) but never
    /// replace one of the two roots.
    ProtectedDest { dest: String },
    /// A configured mount's dest sits below a first path component
    /// the rootfs bakes no mountpoint for: the init can only place a
    /// share by mounting a tmpfs OVER the first component (the ro
    /// root cannot grow the mountpoint at run time), so an unbaked
    /// root is a build-time refusal, never a run-time ENOENT.
    UnknownShareRoot { dest: String, root: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MultiplexerUnavailable { multiplexer } => write!(
                f,
                "the configuration selects multiplexer `{multiplexer}` but no entry is baked for the krun guest (MYSBX_KRUN_MUX_ENTRY_{multiplexer:?} unset)"
            ),
            Error::PidsLimit { pids } => write!(
                f,
                "pids-limit = {pids} cannot be enforced: a krun run is a whole microVM, no pids controller is wired for it"
            ),
            Error::KrunLimit { key, raw, why } => {
                write!(f, "{key} = `{raw}` is not {why}")
            }
            Error::RootLevelDest { dest } => write!(
                f,
                "the mount dest `{dest}` sits at the guest root, where the krun backend cannot place a share (the ro root virtiofs takes no new entries and no second virtiofs device may nest below it) — move it below a parent path"
            ),
            Error::ProtectedDest { dest } => write!(
                f,
                "the mount dest `{dest}` collides with a fixed sandbox path (/nix/store, /mysbx-home) — a share may live below the home, never replace one of its roots"
            ),
            Error::UnknownShareRoot { dest, root } => write!(
                f,
                "the mount dest `{dest}` sits below `{root}/`, a first path component the guest rootfs bakes no mountpoint for (the init mounts a tmpfs over it to place the share; the read-only root cannot grow one at run time) — move it below one of the baked roots: {}",
                BAKED_SHARE_ROOTS.join(", ")
            ),
        }
    }
}

/// One virtiofs share: a SLOT inside its access mode's staging
/// device and the SANDBOX path the payload must see it at. The
/// guest init (bd myconfig-dak.5) mounts each DEVICE once, under
/// [`GUEST_SHARE_ROOT`] keyed by the device tag (nesting a virtiofs
/// device under another virtiofs mount returns EBUSY, spike finding
/// 9, so nothing ever mounts at the sandbox path directly), and then
/// links each sandbox path at `<device-mount>/<slot>`, so the
/// payload's contract is the SANDBOX layout, the tmpfs placement is
/// the init's. The `MYSBX_KRUN_SHARES` encoding the launcher hands
/// the init is one `DEVICE SLOT SANDBOX_PATH ro|rw` entry per share.
struct Share {
    /// The share's slot inside its device's staging tree — distinct
    /// per share (a digest of the sandbox path, like the podman
    /// container names).
    slot: String,
    sandbox_path: String,
    read_only: bool,
}

/// The fixed sandbox paths the rootfs bakes links for — a share's
/// path may live BELOW the home but never replace one of the roots.
const BAKED_LINK_PATHS: [&str; 2] = ["/nix/store", "/mysbx-home"];

/// The first path components the rootfs bakes MOUNTPOINT dirs for
/// (nix/krun-rootfs.nix's baked list — the same names): the init
/// places a share below one of them by mounting a tmpfs over the
/// component, which needs the mountpoint to exist on the ro root.
/// A dest below anything else is refused (`UnknownShareRoot`):
/// never a run-time ENOENT the init cannot diagnose. `/etc` carries
/// the git trust; `/home` the live repo; the rest are the common
/// mount targets of the config layers.
const BAKED_SHARE_ROOTS: [&str; 7] = ["/etc", "/home", "/srv", "/mnt", "/media", "/opt", "/data"];

/// The sandbox path of the git trust's global config — the same
/// container path the podman variant binds (`GIT_CONFIG_GLOBAL`, bd
/// myconfig-zj2), reached here as an ro virtiofs share.
const GIT_TRUST_GLOBAL_DEST: &str = "/etc/mysbx/gitconfig";
/// The sandbox path of the libgit2 system config — git's own system
/// scope, which libgit2 reads INSTEAD of `GIT_CONFIG_GLOBAL` (bd
/// myconfig-jn0).
const GIT_TRUST_SYSTEM_DEST: &str = "/etc/gitconfig";

/// Whether the guest can place a share at `sandbox_path`: it must
/// have a parent (the link lives on the parent's surface — the
/// tmpfs the init mounts at the first component), it must not
/// collide with the baked links, and its FIRST component must be a
/// root the rootfs bakes a mountpoint for (the init's tmpfs-over-
/// first-component needs the mountpoint on the ro root). The
/// WORKSPACE share runs only the first-component half (the repo
/// path is config-independent: the merge refused a repo at a baked
/// path long ago, and a repo below /tmp would be refused at
/// discovery); the full check is for CONFIGURED mounts, whose dests
/// are free-form.
fn check_share_dest(sandbox_path: &str) -> Result<(), Error> {
    let path = std::path::Path::new(sandbox_path);
    let parent = path.parent().and_then(|p| p.to_str()).unwrap_or("");
    if parent.is_empty() || parent == "/" {
        return Err(Error::RootLevelDest {
            dest: sandbox_path.to_owned(),
        });
    }
    for baked in BAKED_LINK_PATHS {
        if sandbox_path == baked {
            return Err(Error::ProtectedDest {
                dest: sandbox_path.to_owned(),
            });
        }
    }
    check_share_root(sandbox_path)
}

/// The first-component half of [`check_share_dest`] — the one the
/// workspace share runs too: the init places EVERY non-baked share
/// by mounting a tmpfs over its first component, so a repo below an
/// unbaked root is as unplaceable as a configured mount dest.
fn check_share_root(sandbox_path: &str) -> Result<(), Error> {
    let path = std::path::Path::new(sandbox_path);
    // Paths below /tmp and the home need no first-component
    // mountpoint (their surfaces are already tmpfs).
    if let (Some(_), Some(first)) = (
        path.strip_prefix("/").ok(),
        path.strip_prefix("/")
            .ok()
            .and_then(|p| p.components().next())
            .and_then(|c| c.as_os_str().to_str())
            .map(|c| format!("/{c}")),
    ) {
        if first != "/tmp"
            && first != "/mysbx-home"
            && first != "/nix"
            && !BAKED_SHARE_ROOTS.contains(&first.as_str())
        {
            return Err(Error::UnknownShareRoot {
                dest: sandbox_path.to_owned(),
                root: first,
            });
        }
    }
    Ok(())
}

/// The built spec — sections in fixed order (the module docs), the
/// argv of the launcher binary plus the shares' guest placement
/// (which rides as `MYSBX_KRUN_SHARES` in the env, section 4).
struct Spec {
    cpus: u32,
    ram_mib: u32,
    rootfs: String,
    init: String,
    shares: Vec<Share>,
    env: Vec<(String, String)>,
    /// The payload's working directory (bwrap's `--chdir`
    /// equivalent): the workspace path the shares already placed —
    /// the repo root, where every other backend's payload starts.
    /// The launcher maps it onto `krun_set_workdir`.
    workdir: String,
    payload: Vec<String>,
    /// The merged config's `network` (bd myconfig-dak.6, backends.md
    /// D6): `true` keeps libkrun's implicit vsock (the TSI proxy
    /// dials from the launcher's netns), `false` disables it — no
    /// socket path to the host at all.
    network: bool,
}

/// The staging-tree BINDS of a run's shares — one `(host_dir,
/// mode, slot)` triple per share (backends.md D3's chain: the
/// caller creates `<stage>/<ro|rw>/<slot>` and bwrap binds the
/// host dir into it, the staging tree then being the ONE virtiofs
/// device per access mode; the slot budget — see STAGE_RO_TAG).
/// The store share rides in the ro tree like every other ro share:
/// the wrap's own --ro-bind /nix/store (the launcher's runtime)
/// already grants the visibility, this bind only adds the slot.
pub fn stage_binds(cfg: &Merged, repo: &Repo, params: &Params<'_>) -> Vec<(String, bool, String)> {
    let mut binds: Vec<(String, bool, String)> = Vec::new();
    // The store share's slot: a ro bind of /nix/store into the ro
    // tree — the same source the wrap's own --ro-bind /nix/store
    // serves, so this adds no new visibility, only the slot view.
    binds.push(("/nix/store".to_owned(), true, "store".to_owned()));
    let workspace_dir = match &params.workspace {
        Workspace::Live => repo.root.clone(),
        Workspace::Clone { clone } => clone.to_path_buf(),
    };
    binds.push((
        workspace_dir.to_string_lossy().into_owned(),
        false,
        "workspace".to_owned(),
    ));
    for mount in &cfg.mounts {
        let sandbox_path = mount.dest.clone().unwrap_or_else(|| mount.path.clone());
        let read_only = matches!(mount.mode, crate::config::Mode::Ro);
        binds.push((
            mount.path.clone(),
            read_only,
            format!("m-{}", fnv1a10(&sandbox_path)),
        ));
    }
    for entry in cfg.effective_state_dirs() {
        binds.push((
            repo.sidecar
                .join("state")
                .join(&entry)
                .to_string_lossy()
                .into_owned(),
            false,
            state_tag(&entry),
        ));
    }
    // The git trust files (ro slots like any other — the staging
    // device's OWN ro flag enforces the read-only side end to end).
    if let Some(trust) = params.git_trust {
        binds.push((
            trust.global_host.to_owned(),
            true,
            "gittrust-global".to_owned(),
        ));
        binds.push((
            trust.system_host.to_owned(),
            true,
            "gittrust-system".to_owned(),
        ));
    }
    binds
}

pub fn krun_argv(
    cfg: &Merged,
    repo: &Repo,
    payload: &Payload,
    host_env: &HostEnv,
    params: &Params<'_>,
) -> Result<Vec<String>, Error> {
    // The multiplexer applies to the INTERACTIVE payload only (cli.md
    // D11), like on every backend: a one-shot `run -- CMD` is never
    // wrapped. A session multiplexer without a pinned entry is
    // refused — never a plain-shell fallback.
    let mux = if matches!(payload, Payload::Shell) {
        cfg.multiplexer
    } else {
        Multiplexer::None
    };
    if mux.starts_a_session() && params.mux_entry.is_none() {
        return Err(Error::MultiplexerUnavailable { multiplexer: mux });
    }

    // 4. the payload environment (the forwarded host variables,
    // then the config layers, then the infrastructure block last —
    // see sandbox_env): the launcher's --env IS the sandbox
    // environment.
    let mut env = sandbox_env(cfg, host_env, params, mux);
    // The guest init's trace switch (bd myconfig-2n8's live
    // debugging): set on the HOST it rides along as a plain entry,
    // so a silent guest hang leaves console evidence of every init
    // step. A config `[env]` layer may override or drop it — it is
    // a debugging aid, not an infrastructure pin.
    if let Ok(trace) = std::env::var("MYSBX_KRUN_TRACE") {
        if !env.iter().any(|(k, _)| k == "MYSBX_KRUN_TRACE") {
            env.push(("MYSBX_KRUN_TRACE".to_owned(), trace));
        }
    }

    // 0. VM size: the config's resource limits (config.md D23)
    // override the defaults through the same grammar the
    // podman-krun annotations use. `pids-limit` has no VM
    // expression — refused, never dropped silently.
    if let Some(pids) = cfg.pids_limit {
        return Err(Error::PidsLimit { pids });
    }
    let cpus = match &params.cpus {
        Some(raw) => parse_krun_cpus(raw).map_err(krun_limit("cpus"))?,
        None => DEFAULT_CPUS,
    };
    let ram_mib = match &params.memory {
        Some(raw) => parse_krun_ram_mib(raw).map_err(krun_limit("memory"))? as u32,
        None => DEFAULT_RAM_MIB,
    };

    // 4. the environment (the forwarded host variables, then the
    // config layers, then the infrastructure block — computed above
    // with the mux decision; one mux feeds the TMUX_TMPDIR entry and
    // the payload swap alike).

    // 3. the virtiofs shares, in mount order (the repo first, like
    // the implicit binds of every backend):
    //
    // - the ro host store (D3's default: the whole /nix/store, the
    //   same visibility the bwrap tier grants with --ro-bind),
    // - the workspace (live: the repo root; clone: the session
    //   clone, seen at the repo's own path — the same remap the
    //   other backends bind),
    // - every configured mount, at its sandbox destination (ro/rw as
    //   declared — the virtiofs server enforces it),
    // - every state-dir backing store, rw, at its sandbox path
    //   (config.md D15; the implicit .ssh of the unconditional
    //   keypair included via effective_state_dirs).
    //
    // The SANDBOX path is what the payload contract promises; the
    // GUEST mount lives under GUEST_SHARE_ROOT (spike finding 9),
    // and the guest init (bd myconfig-dak.5) links one at the other.
    // The store share keeps its own DEVICE (the rootfs's baked
    // /nix/store link targets it); every other share is a SLOT in
    // one of the two staging devices (the slot budget — see
    // STAGE_RO_TAG), so the device count is a constant 3 regardless
    // of how many mounts a config carries.
    let mut shares: Vec<Share> = vec![Share {
        slot: "store".to_owned(),
        sandbox_path: "/nix/store".to_owned(),
        read_only: true,
    }];
    // The workspace share runs the first-component half of the dest
    // check too: the init places the SANDBOX path (the repo's own
    // path — the clone remap binds the clone there), so a repo
    // below an unbaked root is a refusal, never a run-time ENOENT.
    // The other halves (parent, baked links) are the merge's own
    // old guarantees.
    check_share_root(&repo.root.to_string_lossy())?;
    shares.push(Share {
        slot: "workspace".to_owned(),
        sandbox_path: repo.root.to_string_lossy().into_owned(),
        read_only: false,
    });
    for mount in &cfg.mounts {
        shares.push(share_of_mount(mount)?);
    }
    for entry in cfg.effective_state_dirs() {
        shares.push(Share {
            slot: state_tag(&entry),
            sandbox_path: format!("/mysbx-home/{entry}"),
            read_only: false,
        });
    }
    // The git trust (bd myconfig-zj2's krun twin): the SAME two
    // per-run files the podman arm binds — here as ro slots in the
    // staging device, at the same container paths, so a repo
    // checked out at a different path still trusts exactly what
    // THIS run shares.
    if params.git_trust.is_some() {
        shares.push(Share {
            slot: "gittrust-global".to_owned(),
            sandbox_path: GIT_TRUST_GLOBAL_DEST.to_owned(),
            read_only: true,
        });
        shares.push(Share {
            slot: "gittrust-system".to_owned(),
            sandbox_path: GIT_TRUST_SYSTEM_DEST.to_owned(),
            read_only: true,
        });
    }

    // 5. the payload: the mux entry of a session, else the shell of
    // an interactive run, else the command verbatim.
    let payload_argv: Vec<String> = match payload {
        Payload::Shell if mux.starts_a_session() => {
            vec![params.mux_entry.expect("checked above").to_owned()]
        }
        Payload::Shell => vec![params.shell.to_owned()],
        Payload::Command(args) => args.clone(),
    };

    let spec = Spec {
        cpus,
        ram_mib,
        rootfs: params.rootfs.to_owned(),
        // The guest entry of bd myconfig-dak.5, baked into the
        // rootfs (the spike's /bin/spike-init successor).
        init: "/bin/mysbx-init".to_owned(),
        shares,
        env,
        // The payload works in the repo — the same starting
        // directory every other backend's `--chdir`/`--workdir`
        // gives it. A clone run's shares already remap the repo
        // path to the clone, so the path is the repo root either
        // way.
        workdir: repo.root.to_string_lossy().into_owned(),
        payload: payload_argv,
        network: cfg.network,
    };
    Ok(render(&spec))
}

/// The sandbox environment of a run — the exact entries and order
/// bwrap.rs section 6 applies: the forwarded host variables first,
/// then the config `[env]` layers (which win by being set later),
/// then the infrastructure variables LAST (HOME, the XDG base
/// dirs, PATH, the CA bundle, TMUX_TMPDIR — a later assignment
/// wins, so no layer can repoint them).
fn sandbox_env(
    cfg: &Merged,
    host_env: &HostEnv,
    params: &Params<'_>,
    mux: Multiplexer,
) -> Vec<(String, String)> {
    let mut env: Vec<(String, String)> = Vec::new();
    for (k, v) in host_env {
        env.push((k.clone(), v.clone()));
    }
    for (k, v) in &cfg.env {
        env.push((k.clone(), v.clone()));
    }
    // The infrastructure block, set after every layer (config.md
    // D14): HOME names the guest tmpfs the init created, the XDG
    // base dirs derive from it, PATH names the tool closure — all
    // paths THIS backend's layout created or pinned, so a layer
    // that repointed them would break the sandbox, not configure
    // it. Values are the bwrap backend's own constants: the guest
    // init (bd myconfig-dak.5) recreates the same layout inside the
    // VM (tmpfs home, state shares linked under it).
    env.push(("HOME".to_owned(), crate::bwrap::SANDBOX_HOME.to_owned()));
    env.push((
        "XDG_CONFIG_HOME".to_owned(),
        format!("{}/.config", crate::bwrap::SANDBOX_HOME),
    ));
    env.push((
        "XDG_CACHE_HOME".to_owned(),
        format!("{}/.cache", crate::bwrap::SANDBOX_HOME),
    ));
    env.push((
        "XDG_STATE_HOME".to_owned(),
        format!("{}/.local/state", crate::bwrap::SANDBOX_HOME),
    ));
    env.push((
        "XDG_DATA_HOME".to_owned(),
        format!("{}/.local/share", crate::bwrap::SANDBOX_HOME),
    ));
    // The CA-bundle pins (bd myconfig-938): a host STORE path, so it
    // resolves inside the guest through the ro store share like
    // every other tool.
    if let Some(ca_bundle) = params.ca_bundle {
        for key in ["SSL_CERT_FILE", "GIT_SSL_CAINFO", "NIX_SSL_CERT_FILE"] {
            env.push((key.to_owned(), ca_bundle.to_owned()));
        }
    }
    // The mux socket dir (config.md D16/D17): a directory inside the
    // tmpfs home the init created — guest-internal, never on a
    // virtiofs share (a host socket cannot cross virtiofs, the
    // spike's finding), never on a host-shared location.
    if mux.starts_a_session() {
        env.push((
            "TMUX_TMPDIR".to_owned(),
            crate::bwrap::MUX_SOCKET_DIR.to_owned(),
        ));
    }
    // PATH LAST of the infrastructure block, like the bwrap
    // backend's own ordering: the tool closure is this wrapper's
    // pin, and no later entry may repoint it.
    env.push(("PATH".to_owned(), params.tools_path.to_owned()));
    // The git trust's global config (bd myconfig-zj2): set after
    // PATH, the LAST entry of the block — git reads
    // `safe.directory` only from the protected system+global scope,
    // and the config `[env]` layers must not be able to repoint the
    // trust anchor. The file is the `gittrust-global` share at
    // `/etc/mysbx/gitconfig`, written by this run for exactly the
    // paths it shares.
    // The guest resolver (bd myconfig-dak.6, backends.md D6): TSI
    // dials from the launcher's netns — the host's (!) — but nothing
    // writes /etc/resolv.conf inside the guest (no DHCP on a TSI
    // socket path, no net device at all). The init does, from this
    // entry: the HOST's resolver content, base64'd over the
    // manifest env (newlines must survive; the manifest's env lines
    // are tab-separated). Set ONLY when the network is shared —
    // network = false has no resolver to reach and the entry would
    // advertise one.
    if cfg.network {
        if let Ok(host_resolv) = std::fs::read_to_string("/etc/resolv.conf") {
            env.push(("MYSBX_KRUN_RESOLV".to_owned(), b64(&host_resolv)));
        }
    }
    // The git trust's global config stays the block's LAST entry
    // (its docs: no layer may repoint the anchor) — the resolv line
    // rides BEFORE it.
    if params.git_trust.is_some() {
        env.push((
            "GIT_CONFIG_GLOBAL".to_owned(),
            GIT_TRUST_GLOBAL_DEST.to_owned(),
        ));
    }
    env
}

/// base64 (standard, padded) of arbitrary bytes — the manifest's
/// env-value encoding, same alphabet the launcher's manifest uses.
/// The crate is dependency-free by design; the alphabet by hand.
pub(crate) fn b64(data: &str) -> String {
    const TBL: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let bytes = data.as_bytes();
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let b = [
            chunk[0],
            *chunk.get(1).unwrap_or(&0),
            *chunk.get(2).unwrap_or(&0),
        ];
        let n = (u32::from(b[0]) << 16) | (u32::from(b[1]) << 8) | u32::from(b[2]);
        out.push(TBL[(n >> 18 & 63) as usize] as char);
        out.push(TBL[(n >> 12 & 63) as usize] as char);
        out.push(if chunk.len() > 1 {
            TBL[(n >> 6 & 63) as usize] as char
        } else {
            '='
        });
        out.push(if chunk.len() > 2 {
            TBL[(n & 63) as usize] as char
        } else {
            '='
        });
    }
    out
}

/// The virtiofs tag of a state-dir backing share — deterministic per
/// entry, stable across runs (the guest init matches tags to
/// mountpoints by MYSBX_KRUN_SHARES, but a readable tag is part of
/// the dry-run audit).
fn state_tag(entry: &str) -> String {
    let mut tag = String::from("state-");
    for c in entry.chars() {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' => tag.push(c.to_ascii_lowercase()),
            _ => tag.push('-'),
        }
    }
    tag
}

fn share_of_mount(mount: &crate::config::Mount) -> Result<Share, Error> {
    let sandbox_path = mount.dest.clone().unwrap_or_else(|| mount.path.clone());
    check_share_dest(&sandbox_path)?;
    let read_only = matches!(mount.mode, crate::config::Mode::Ro);
    // The slot must not contain whitespace or `;` (the
    // MYSBX_KRUN_SHARES encoding is `;`-separated) and must be a
    // stable identity of the share across runs — a digest of the
    // sandbox path always satisfies both.
    let slot = format!("m-{}", fnv1a10(&sandbox_path));
    Ok(Share {
        slot,
        sandbox_path,
        read_only,
    })
}

/// First 10 hex chars of the FNV-1a 64 hash of `path` — the same
/// stable digest podman_gvisor.rs uses for container names: a
/// share's tag must not contain whitespace or `;` (the launcher's
/// MYSBX_KRUN_SHARES encoding is `;`-separated), and a digest always
/// satisfies that.
fn fnv1a10(path: &str) -> String {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in path.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")[..10].to_owned()
}

fn krun_limit(key: &'static str) -> impl Fn(crate::podman_gvisor::Error) -> Error {
    move |e| match e {
        crate::podman_gvisor::Error::KrunLimit { raw, why, .. } => {
            Error::KrunLimit { key, raw, why }
        }
        // parse_krun_cpus/parse_krun_ram_mib produce no other
        // variants; the unreachable arm keeps the match exhaustive.
        _ => Error::KrunLimit {
            key,
            raw: String::new(),
            why: "a value the VM can express",
        },
    }
}

/// The spec's stable text form: the launcher argv, sections in fixed
/// order (the module docs). `--dry-run` prints exactly this, one
/// argument per line, and the goldens compare bytes.
fn render(spec: &Spec) -> Vec<String> {
    let mut argv: Vec<String> = vec![
        "--cpus".to_owned(),
        spec.cpus.to_string(),
        "--ram".to_owned(),
        spec.ram_mib.to_string(),
    ];
    // 1. the rootfs
    argv.push("--rootfs".to_owned());
    argv.push(spec.rootfs.clone());
    // 2. the guest init
    argv.push("--init".to_owned());
    argv.push(spec.init.clone());
    // 3. the devices and their shares. The DEVICES are the staged
    // trees (one per access mode — the slot budget, STAGE_RO_TAG's
    // docs); the SHARES name a slot in their device and the sandbox
    // path — the payload's contract. The guest init mounts each
    // device under GUEST_SHARE_ROOT keyed by its tag, then links
    // each share's sandbox path at <device-mount>/<slot> (spike
    // finding 9: nothing may mount at the sandbox path directly, it
    // sits on the ro root virtiofs). The store share is a SLOT of
    // the ro device like every other ro share (the rootfs's baked
    // /nix/store link targets GUEST_STORE) — two devices total.
    // THE MANIFEST (the seventh live finding, the x86 2048-byte
    // COMMAND_LINE_SIZE): env, shares and chdir no longer ride the
    // kernel cmdline — the launcher assembles the records from
    // these same flags and writes the file into the ro stage tree
    // (bound rw at STAGE_ROOT for the write; the virtiofs device
    // serves it read-only to the guest), the cmdline carries only
    // the pointer, and the guest init reads it right after mounting
    // stage-ro. The --env/--ro-share/--rw-share flags are the
    // launcher's INPUT; nothing about the argv interface changes.
    let ro_slots = spec.shares.iter().filter(|s| s.read_only).count();
    let rw_slots = spec.shares.iter().filter(|s| !s.read_only).count();
    if ro_slots > 0 {
        argv.push("--ro-device".to_owned());
        argv.push(format!("{STAGE_RO_TAG}={}/ro", STAGE_ROOT));
    }
    if rw_slots > 0 {
        argv.push("--rw-device".to_owned());
        argv.push(format!("{STAGE_RW_TAG}={}/rw", STAGE_ROOT));
    }
    if ro_slots > 0 {
        argv.push("--manifest".to_owned());
        argv.push(format!("{STAGE_ROOT}/ro/{}", MANIFEST_SLOT));
    }
    for share in &spec.shares {
        let device = if share.read_only {
            STAGE_RO_TAG
        } else {
            STAGE_RW_TAG
        };
        argv.push(if share.read_only {
            "--ro-share".to_owned()
        } else {
            "--rw-share".to_owned()
        });
        argv.push(format!(
            "{device}:{}@{} {}",
            share.slot,
            share.sandbox_path,
            if share.read_only { "ro" } else { "rw" }
        ));
    }
    // 4. the environment
    for (k, v) in &spec.env {
        argv.push("--env".to_owned());
        argv.push(format!("{k}={v}"));
    }
    // 4b. the network (bd myconfig-dak.6): `none` is the only mode
    // the argv must NAME — shared is the launcher's default (the
    // implicit vsock's TSI). `none` disables the vsock in the
    // launcher AND unshares the net around the whole chain (lib.rs's
    // bwrap --unshare-net): neither the VMM nor the guest could
    // dial out.
    if !spec.network {
        argv.push("--network".to_owned());
        argv.push("none".to_owned());
    }
    // 5. the working directory (bwrap's --chdir equivalent:
    // `krun_set_workdir`)
    argv.push("--chdir".to_owned());
    argv.push(spec.workdir.clone());
    // 6. the payload
    argv.push("--".to_owned());
    argv.extend(spec.payload.iter().cloned());
    argv
}
