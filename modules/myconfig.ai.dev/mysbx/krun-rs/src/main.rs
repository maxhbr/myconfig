// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//
// mysbx-krun — the libkrun launcher of the future `backend = "krun"`
// (bd myconfig-dak, spike bd myconfig-dak.1).
//
// A deliberately small, zero-dependency binary that drives libkrun
// DIRECTLY (no podman, no crun, no OCI image): it creates one krun
// context, wires the microVM to the host and krun_start_enter()s into
// it. The intended chain is `bwrap -> mysbx-krun -> VM` (the epic's
// design): the bwrap argv builder already confines the host side, so
// the virtiofs server inside this process can only ever open paths
// bwrap left visible.
//
// libkrun 1.19 facts this launcher is built on (all verified against
// include/libkrun.h and src/libkrun/src/lib.rs of the pinned tag):
//
// - krun_create_ctx()/krun_set_vm_config()/krun_add_virtiofs3()/
//   krun_set_exec()/krun_start_enter() are the whole API needed for a
//   plain run. `krun_add_virtiofs3` with the tag "/dev/root"
//   (KRUN_FS_ROOT_TAG) is the documented root configuration path and
//   the ONLY one with a read-only flag.
// - The DEFAULT nixpkgs libkrun is built WITHOUT `withNet`/`withBlk`:
//   krun_set_passt_fd and krun_add_disk2 do not exist in it. The
//   launcher therefore resolves EVERY symbol through dlopen/dlsym at
//   startup and refuses with the symbol name when one is missing —
//   the failure mode is "this lib is missing feature X", never a
//   link error per host.
// - krun_set_exec with envp=NULL inherits the WHOLE host environment
//   (src/libkrun krun_set_exec reads env::vars()). The launcher
//   ALWAYS passes an explicit envp — the caller's --env entries plus
//   the shares' guest placement. That is the spike's "explicit envp"
//   requirement.
// - krun_set_exec's argv rides the kernel cmdline behind " -- " and
//   arrives as the guest INIT's argv; /init.krun then overwrites
//   argv[0] with KRUN_INIT (the exec_path) and execvp()s the vector.
//   argv[0] is therefore a placeholder the init consumes: with
//   --init the entry script sees the payload as $1.. and execs it;
//   without it argv[0] doubles as the payload path.
// - THE CMDLINE BUDGET (the seventh live finding, the root cause of
//   every wrapped hang since grouping): the x86 guest kernel copies
//   exactly COMMAND_LINE_SIZE = 2048 bytes of the cmdline libkrun
//   builds (head64.c copy_bootdata; libkrun's own CMDLINE_MAX_SIZE
//   of 64 KiB never reaches the kernel), and a real config's env +
//   shares block measured 3075 bytes — the tail, the `--` payload
//   argv included, silently never booted. The spike's ~600-byte
//   cmdline is why the spike worked. The channel therefore carries
//   ONLY what is structurally tiny: KRUN_INIT, KRUN_WORKDIR, the
//   -- payload argv, and the manifest pointer. Shares and env live
//   in a MANIFEST FILE the launcher writes into the ro stage device
//   (a path the init mounts before reading); its lines are
//   tab-separated records, values base64 (arbitrary bytes survive):
//     env<TAB>KEY<TAB>base64(value)
//     share<TAB>DEVICE<TAB>SLOT<TAB>DEST<TAB>ro|rw
//     chdir<TAB>base64(dir)
// - The implicit init (/init.krun) mounts devtmpfs/proc/sysfs/cgroup2
//   but mounts NO extra virtiofs tags; the guest payload is expected
//   to do that (or the rootfs carries a custom init — bd
//   myconfig-dak.5). For the spike the payload itself mounts the
//   tags, which the kernel makes available as virtiofs devices.
// - Exit codes: the implicit init maps workload exit to the
//   KRUN_EXIT_CODE_IOCTL on the root virtiofs; krun_start_enter
//   returns it to the caller. 125/126/127 are init-level errors.
// - The DEVICE-SLOT BUDGET (live finding, first wrapped run):
//   every krun_add_virtiofs3 tag is a full virtiofs device, and
//   libkrun's MMIO budget is 11 slots (arch IRQ_BASE=5..
//   IRQ_MAX=15) minus balloon, rng, the implicit console and the
//   implicit vsock — about 6 fs slots TOTAL. One share per device
//   therefore cannot scale beyond a handful of mounts. The
//   interface is GROUPED instead: --ro-device/--rw-device declare
//   one virtiofs DEVICE per access mode (the caller stages every
//   share's host dir inside it, one slot dir per share), and
//   --ro-share/--rw-share declare each share's SLOT and sandbox
//   dest inside its device. The device count is constant, the
//   share count is not.
//
// Usage (grouped interface — argv and env, no TOML):
//
//   mysbx-krun [--cpus N] [--ram MIB] --rootfs DIR [--init PATH]
//              [--ro-device TAG=DIR] [--rw-device TAG=DIR]
//              [--ro-share TAG:SLOT@DEST] [--rw-share TAG:SLOT@DEST]
//              [--env K=V]... [--chdir DIR] -- CMD [ARGS...]
//
// The rootfs is shared READ-ONLY (the root of a sandbox is never
// writable from inside). The devices carry their own ro/rw flag —
// enforced by the virtiofs server itself, which is the point of the
// direct backend (backends.md D2's per-share enforcement): a share
// in the ro device is read-only end to end, a share in the rw
// device writable end to end.
//
// No passt for the spike's first step: the launcher never calls a
// net API and, when the lib carries the implicit-vsock symbol (a
// net-enabled build adds TSI by default), it DISABLES the implicit
// vsock so a run with no net device is honest about having no
// network. Both nixpkgs libkrun builds the wrapper pins (the stock
// one and the withBlk one) carry the symbol — verified with nm -D
// (bd myconfig-au2); only `--network none` CONSUMES it, a shared run
// boots without touching the symbol at all.

use std::ffi::{c_char, c_int, c_uchar, c_uint, CString};
use std::path::PathBuf;

/// The virtiofs tag libkrun reserves for the root filesystem
/// (KRUN_FS_ROOT_TAG, include/libkrun.h).
const FS_ROOT_TAG: &str = "/dev/root";

/// libkrun's init-level exit codes (krun_start_enter doc comment):
/// 125 setup failure, 126/127 exec failure of the payload. Reported
/// verbatim — mysbx's own error mapping is the backend's business.
const EXIT_SETUP: i32 = 125;

/// The network/symbol decision at VM setup (bd myconfig-dak.6, the
///odule doc's net section): `--network none` must disable the
/// implicit TSI vsock — possible only when the lib carries
/// [`crate::KrunApi::krun_disable_implicit_vsock`]. A shared run
/// NEVER consumes the symbol: the vsock is the implicit device
/// (attached unless disabled), so a symbol-less lib keeps TSI on
/// and the shared semantics hold unchanged.
///
/// Extracted pure so the four combinations are unit-pinned: the
/// `--network none` WITH-symbol call, the `--network none`
/// symbol-less REFUSAL (exit 125, the bd myconfig-rrg regression
/// guard), and the two shared modes that proceed without touching
/// the symbol.
fn vsock_decision(network_shared: bool, symbol_present: bool) -> VsockDecision {
    if network_shared {
        VsockDecision::KeepTsi
    } else if symbol_present {
        VsockDecision::DisableTsi
    } else {
        VsockDecision::RefusedNoSymbol
    }
}

enum VsockDecision {
    /// Shared network: the implicit vsock stays attached (TSI on).
    KeepTsi,
    /// `--network none` with the symbol: the run disables the vsock.
    DisableTsi,
    /// `--network none` without the symbol: the run is refused.
    RefusedNoSymbol,
}

// The fd ceiling of the LAUNCHER itself (bd myconfig-mnr, the host
// half): libkrun runs its virtiofs server in THIS process, and every
// file the guest opens is a host fd here — the guest kernel's page
// cache holds fids far longer than any single process would, so a
// big-repo walk inside the VM exhausts the HOST's soft nofile even
// though the guest-side raise succeeded. The launcher usually runs
// at the invoking session's limit (systemd default: 1024). Zero
// dependencies by design, so the raw setrlimit(2) surface is
// declared here like the libkrun symbols: rlimit is { cur, max },
// both u64.
const RLIMIT_NOFILE: c_int = 7;
const RLIM_INFINITY: u64 = u64::MAX;

#[repr(C)]
struct Rlimit {
    cur: u64,
    max: u64,
}

unsafe extern "C" {
    fn getrlimit(resource: c_int, rlim: *mut Rlimit) -> c_int;
    fn setrlimit(resource: c_int, rlim: *const Rlimit) -> c_int;
}

/// The host-side fd ceiling raise (bd myconfig-mnr): hard first (the
/// soft cannot exceed it), then the soft, best-effort — a constrained
/// session keeps its own ceiling and the soft then goes to THAT
/// (still a raise over 1024), every refusal is a WARNING, the run
/// never dies over limits. Returns the (soft, hard) pair that stands
/// after the attempt.
fn raise_nofile() -> (u64, u64) {
    unsafe {
        let mut rlim = Rlimit { cur: 0, max: 0 };
        if getrlimit(RLIMIT_NOFILE, &mut rlim) != 0 {
            return (0, 0);
        }
        let hard_before = rlim.max;
        if rlim.max < RLIM_INFINITY {
            rlim.max = RLIM_INFINITY;
            if setrlimit(RLIMIT_NOFILE, &rlim) != 0 {
                rlim.max = hard_before; // the raise failed; keep what stands
                eprintln!(
                    "mysbx-krun: WARNING: cannot raise the hard fd limit to infinity (keeping {hard_before})"
                );
            }
        }
        // re-read after the hard attempt so the soft raise uses
        // whatever ceiling actually stands
        if getrlimit(RLIMIT_NOFILE, &mut rlim) != 0 {
            return (0, 0);
        }
        if rlim.cur < rlim.max {
            let soft_before = rlim.cur;
            rlim.cur = rlim.max;
            if setrlimit(RLIMIT_NOFILE, &rlim) != 0 {
                rlim.cur = soft_before;
                eprintln!(
                    "mysbx-krun: WARNING: cannot raise the soft fd limit (keeping {soft_before})"
                );
            }
        }
        (rlim.cur, rlim.max)
    }
}

// ---------------------------------------------------------------------------
// dlopen'd libkrun. Resolving the symbols at runtime (not link time)
// keeps the binary portable across libkrun FEATURE SETS: the default
// nixpkgs libkrun has no net/blk symbols, a `withNet`/`withBlk`
// override has them, and both must run the same binary.
// ---------------------------------------------------------------------------

#[repr(C)]
struct KrunApi {
    krun_create_ctx: unsafe extern "C" fn() -> c_int,
    krun_set_vm_config:
        unsafe extern "C" fn(ctx_id: c_uint, num_vcpus: c_uchar, ram_mib: c_uint) -> c_int,
    krun_add_virtiofs3: unsafe extern "C" fn(
        ctx_id: c_uint,
        c_tag: *const c_char,
        c_path: *const c_char,
        shm_size: u64,
        read_only: bool,
    ) -> c_int,
    krun_set_workdir: unsafe extern "C" fn(ctx_id: c_uint, c_workdir_path: *const c_char) -> c_int,
    krun_set_exec: unsafe extern "C" fn(
        ctx_id: c_uint,
        c_exec_path: *const c_char,
        c_argv: *const *const c_char,
        c_envp: *const *const c_char,
    ) -> c_int,
    krun_start_enter: unsafe extern "C" fn(ctx_id: c_uint) -> c_int,
    // Optional (feature-gated in libkrun): a net-enabled build adds
    // TSI by default when no net device is configured; disabling the
    // implicit vsock is the only honest `network = false` there.
    krun_disable_implicit_vsock: Option<unsafe extern "C" fn(ctx_id: c_uint) -> c_int>,
    // Optional (blk-feature-gated): the scratch disk (bd
    // myconfig-dak.7). A lib WITHOUT the symbol refuses a --scratch
    // run — never a silent run without the disk.
    krun_add_disk2: Option<
        unsafe extern "C" fn(
            ctx_id: c_uint,
            c_block_id: *const c_char,
            c_disk_path: *const c_char,
            disk_format: u32,
            read_only: bool,
        ) -> c_int,
    >,
    // Optional (only in builds with the logger wired): directs
    // libkrun's own log (the VMM's error! calls — otherwise INVISIBLE,
    // the live findings' diagnosis gap) to a raw fd. Level 4 = debug
    // when the trace is on, 1 = error otherwise; the style and
    // options constants are krun_init_log's (auto, honor RUST_LOG).
    krun_init_log: Option<
        unsafe extern "C" fn(target_fd: c_int, level: u32, style: u32, options: u32) -> c_int,
    >,
    // Optional (bd myconfig-dak.8, the git trust's overlay files):
    // a file backed by HOST MEMORY, registered on a virtiofs device
    // — no host file, no share slot, no init placement. The data
    // pointer is NOT copied by the lib: the launcher keeps the
    // decoded bytes in its own Vec (cfg.overlays) across
    // krun_start_enter, which spans the VM's whole lifetime.
    krun_fs_add_overlay_file: Option<
        unsafe extern "C" fn(
            ctx_id: c_uint,
            c_fs_tag: *const c_char,
            c_path: *const c_char,
            data: *const u8,
            data_len: usize,
            mode: u32,
            one_shot: bool,
        ) -> c_int,
    >,
}

unsafe fn load_api() -> Result<KrunApi, String> {
    // MYSBX_KRUN_LIB (the wrapper pin, nix/krun-launcher.nix) names
    // the exact .so; the bare name is the unwrapped-build fallback.
    let lib = match std::env::var("MYSBX_KRUN_LIB") {
        Ok(path) => std::ffi::CString::new(path).unwrap(),
        Err(_) => CString::new("libkrun.so").unwrap(),
    };
    let handle = dlopen(lib.as_ptr(), 0x102);
    if handle.is_null() {
        return Err(format!(
            "cannot dlopen {} (is libkrun on the sandbox view?)",
            lib.to_string_lossy()
        ));
    }
    let sym = |name: &[u8]| -> Result<*mut (), String> {
        let mut c = name.to_vec();
        c.push(0);
        let p = dlsym(handle, c.as_ptr().cast());
        if p.is_null() {
            Err(format!(
                "libkrun.so is missing the symbol {}",
                String::from_utf8_lossy(name)
            ))
        } else {
            Ok(p)
        }
    };
    Ok(KrunApi {
        // SAFETY: each transmute reinterprets the raw dlsym(3) void
        // pointer as the extern "C" function type declared in the
        // struct — the signature is pinned by include/libkrun.h of the
        // libkrun this launcher targets (1.19), and dlsym cannot
        // verify it. The same pattern libkrun's own test suite uses.
        krun_create_ctx: unsafe {
            std::mem::transmute::<*mut (), unsafe extern "C" fn() -> c_int>(sym(
                b"krun_create_ctx",
            )?)
        },
        krun_set_vm_config: unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(ctx_id: c_uint, num_vcpus: c_uchar, ram_mib: c_uint) -> c_int,
            >(sym(b"krun_set_vm_config")?)
        },
        krun_add_virtiofs3: unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(
                    ctx_id: c_uint,
                    c_tag: *const c_char,
                    c_path: *const c_char,
                    shm_size: u64,
                    read_only: bool,
                ) -> c_int,
            >(sym(b"krun_add_virtiofs3")?)
        },
        krun_set_workdir: unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(ctx_id: c_uint, c_workdir_path: *const c_char) -> c_int,
            >(sym(b"krun_set_workdir")?)
        },
        krun_set_exec: unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(
                    ctx_id: c_uint,
                    c_exec_path: *const c_char,
                    c_argv: *const *const c_char,
                    c_envp: *const *const c_char,
                ) -> c_int,
            >(sym(b"krun_set_exec")?)
        },
        krun_start_enter: unsafe {
            std::mem::transmute::<*mut (), unsafe extern "C" fn(ctx_id: c_uint) -> c_int>(sym(
                b"krun_start_enter",
            )?)
        },
        krun_disable_implicit_vsock: sym(b"krun_disable_implicit_vsock").ok().map(|p| unsafe {
            std::mem::transmute::<*mut (), unsafe extern "C" fn(ctx_id: c_uint) -> c_int>(p)
        }),
        krun_add_disk2: sym(b"krun_add_disk2").ok().map(|p| unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(
                    ctx_id: c_uint,
                    c_block_id: *const c_char,
                    c_disk_path: *const c_char,
                    disk_format: u32,
                    read_only: bool,
                ) -> c_int,
            >(p)
        }),
        krun_fs_add_overlay_file: sym(b"krun_fs_add_overlay_file").ok().map(|p| unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(
                    ctx_id: c_uint,
                    c_fs_tag: *const c_char,
                    c_path: *const c_char,
                    data: *const u8,
                    data_len: usize,
                    mode: u32,
                    one_shot: bool,
                ) -> c_int,
            >(p)
        }),
        krun_init_log: sym(b"krun_init_log").ok().map(|p| unsafe {
            std::mem::transmute::<
                *mut (),
                unsafe extern "C" fn(
                    target_fd: c_int,
                    level: u32,
                    style: u32,
                    options: u32,
                ) -> c_int,
            >(p)
        }),
    })
}

extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut ();
    fn dlsym(handle: *mut (), symbol: *const c_char) -> *mut ();
}

// ---------------------------------------------------------------------------
// argv/env plumbing
// ---------------------------------------------------------------------------

struct Device {
    tag: String,
    host_dir: PathBuf,
    read_only: bool,
}

struct Share {
    /// The DEVICE tag the share lives in — one virtiofs device per
    /// access mode (see the module docs' slot budget), its host dir
    /// a staging tree the caller built.
    tag: String,
    /// The share's SLOT inside its device's staging tree — the
    /// guest entry script links the dest at
    /// <device-mount>/<slot>, so two shares of one device need
    /// distinct slots. Carried through MYSBX_KRUN_SHARES.
    slot: String,
    /// The SANDBOX path the payload must see the share at — carried
    /// through to the guest via MYSBX_KRUN_SHARES, so the guest
    /// entry script knows where each share belongs. The GUEST mount
    /// itself lives under /tmp/mysbx-shares keyed by the device tag
    /// (a virtiofs device cannot nest below the ro root share —
    /// EBUSY, the spike's finding 9); the entry script mounts the
    /// device there and links this path at the mount's slot.
    dest: String,
}

struct OverlayFile {
    /// The virtiofs tag the file registers on — the root device for
    /// the guest-nix story; extending this to the stage devices is a
    /// follow-up when a use asks for it.
    fs_tag: &'static str,
    /// The guest path INSIDE the device's tree — "etc/gitconfig" is
    /// /etc/gitconfig on the rootfs device (no leading slash).
    path: String,
    /// File mode bits (0100644 = a regular file).
    mode: u32,
    /// Content the launcher owns; registered by pointer.
    data: Vec<u8>,
}

struct Config {
    cpus: u8,
    ram_mib: u32,
    rootfs: PathBuf,
    /// A guest entry script used as krun_set_exec's exec_path. It
    /// receives the payload as its argv ($1.. — the cmdline "--"
    /// vector with argv[0] replaced by this script) and the shares'
    /// guest placement as MYSBX_KRUN_SHARES in the envp. None execs
    /// the payload directly.
    init: Option<String>,
    devices: Vec<Device>,
    shares: Vec<Share>,
    env: Vec<(String, String)>,
    /// The overlay files (bd myconfig-dak.8): (fs_tag, path, mode,
    /// decoded bytes). The BYTES must outlive krun_start_enter — cfg
    /// lives in main's frame, the registered pointers point into
    /// these Vecs, the lib does not copy.
    overlays: Vec<OverlayFile>,
    /// The MANIFEST carrying the shares (and the env when the
    /// cmdline would overflow — see the module docs' budget
    /// finding). A HOST-side file the launcher writes before
    /// krun_start_enter; the guest init reads it from the ro stage
    /// device. When set, --ro-share/--rw-share must be absent.
    manifest: Option<PathBuf>,
    /// The network mode (bd myconfig-dak.6, backends.md D6):
    /// `true` keeps libkrun's implicit vsock (TSI dials from this
    /// netns), `false` disables the vsock — no socket path to the
    /// host at all. Defaults to shared.
    network: bool,
    /// The payload's working directory, handed to krun_set_workdir
    /// (bwrap's --chdir equivalent). Defaults to `/` — the spec
    /// always carries the workspace path.
    workdir: PathBuf,
    /// The per-run nix scratch disk (bd myconfig-dak.7, backends.md
    /// D7): a HOST-side sparse raw file attached as a real
    /// virtio-blk device (`krun_add_disk2`, RAW — the guest init
    /// mkfs.ext4s and mounts it; the libkrun security note forbids
    /// re-probing an image a guest could write). The block id is
    /// `scratch`, stable for the init to find. None attaches no
    /// disk — a run without the scratch keeps the plain ro store
    /// share.
    scratch: Option<PathBuf>,
    payload: Vec<String>,
}

fn usage() -> ! {
    eprintln!(
        "usage: mysbx-krun [--cpus N] [--ram MIB] --rootfs DIR [--init PATH] \
         [--ro-device TAG=DIR] [--rw-device TAG=DIR] \
         [--manifest FILE] [--network shared|none] \
         [--scratch FILE] [--env K=V]... \
         [--krun-overlay TAG@PATH:MODE:B64]... [--chdir DIR] -- CMD [ARGS...] \
  (the manifest FILE carries the shares; --ro-share/--rw-share remain \
   accepted for cmdline-sized debug runs)"
    );
    std::process::exit(2);
}

fn parse_args(args: impl Iterator<Item = String>) -> Config {
    parse_args_result(args).unwrap_or_else(|e| {
        eprintln!("mysbx-krun: {e}");
        usage()
    })
}

fn parse_args_result(mut args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut cfg = Config {
        cpus: 2,
        ram_mib: 2048,
        rootfs: PathBuf::new(),
        init: None,
        devices: Vec::new(),
        shares: Vec::new(),
        env: Vec::new(),
        overlays: Vec::new(),
        manifest: None,
        network: true,
        workdir: PathBuf::from("/"),
        scratch: None,
        payload: Vec::new(),
    };
    let mut it = args.by_ref().peekable();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--cpus" => {
                cfg.cpus = it
                    .next()
                    .ok_or_else(|| "missing value for --cpus".to_owned())?
                    .parse()
                    .map_err(|_| "invalid value for --cpus".to_owned())?
            }
            "--ram" => {
                cfg.ram_mib = it
                    .next()
                    .ok_or_else(|| "missing value for --ram".to_owned())?
                    .parse()
                    .map_err(|_| "invalid value for --ram".to_owned())?
            }
            "--rootfs" => {
                cfg.rootfs = PathBuf::from(
                    it.next()
                        .ok_or_else(|| "missing value for --rootfs".to_owned())?,
                )
            }
            "--init" => {
                cfg.init = Some(
                    it.next()
                        .ok_or_else(|| "missing value for --init".to_owned())?,
                )
            }
            "--ro-device" => add_device(&mut cfg, true, &mut it)?,
            "--rw-device" => add_device(&mut cfg, false, &mut it)?,
            "--ro-share" => add_share(&mut cfg, true, &mut it)?,
            "--rw-share" => add_share(&mut cfg, false, &mut it)?,
            // The manifest PATH is where the launcher WRITES the
            // assembled records (inside the ro stage device's dir,
            // which the caller staged and bwrap bound); the shares
            // and env flags are the INPUT — exactly the argv of the
            // pre-manifest interface, so callers change nothing.
            "--manifest" => {
                let path = it
                    .next()
                    .ok_or_else(|| "missing value for --manifest".to_owned())?;
                cfg.manifest = Some(PathBuf::from(path));
            }
            "--network" => {
                let value = it
                    .next()
                    .ok_or_else(|| "missing value for --network".to_owned())?;
                cfg.network = match value.as_str() {
                    "shared" => true,
                    "none" => false,
                    other => {
                        return Err(format!("network expects `shared` or `none`, got `{other}`"))
                    }
                };
            }
            "--scratch" => {
                cfg.scratch = Some(PathBuf::from(
                    it.next()
                        .ok_or_else(|| "missing value for --scratch".to_owned())?,
                ))
            }
            "--env" => {
                let value = it
                    .next()
                    .ok_or_else(|| "missing value for --env".to_owned())?;
                let (k, v) = value
                    .split_once('=')
                    .ok_or_else(|| format!("--env expects K=V, got `{value}`"))?;
                cfg.env.push((k.to_owned(), v.to_owned()));
            }
            // An overlay file (bd myconfig-dak.8): value is
            // FS_TAG@PATH:MODE:B64 — the launcher registers the
            // DECODED bytes on the device, so a per-run file needs
            // NO host file, no stage slot, no init placement. The
            // only fs_tag today is the root device ("/dev/root").
            "--krun-overlay" => {
                let value = it
                    .next()
                    .ok_or_else(|| "missing value for --krun-overlay".to_owned())?;
                add_overlay(&mut cfg, &value)?;
            }
            "--chdir" => {
                cfg.workdir = PathBuf::from(
                    it.next()
                        .ok_or_else(|| "missing value for --chdir".to_owned())?,
                )
            }
            "--" => {
                cfg.payload.extend(it.by_ref());
                break;
            }
            _ => return Err(format!("unknown argument `{arg}`")),
        }
    }
    if cfg.rootfs.as_os_str().is_empty() || cfg.payload.is_empty() {
        return Err("--rootfs and a payload are required".to_owned());
    }
    Ok(cfg)
}

fn add_device(
    cfg: &mut Config,
    read_only: bool,
    it: &mut std::iter::Peekable<impl Iterator<Item = String>>,
) -> Result<(), String> {
    let value = it
        .next()
        .ok_or_else(|| "missing value for device".to_owned())?;
    let (tag, dir) = value
        .split_once('=')
        .ok_or_else(|| format!("device expects TAG=DIR, got `{value}`"))?;
    if tag.is_empty() || dir.is_empty() {
        return Err(format!("device expects TAG=DIR, got `{value}`"));
    }
    if tag.chars().any(|c| c.is_whitespace() || c == ';') {
        return Err("device tags cannot contain whitespace or `;`".to_owned());
    }
    cfg.devices.push(Device {
        tag: tag.to_owned(),
        host_dir: PathBuf::from(dir),
        read_only,
    });
    Ok(())
}

/// The DEDICATED virtual device overlay files register on (bd
/// myconfig-dak.8): a NULL-fs device — `krun_add_virtiofs3` with a
/// NULL host path maps to NullFs (libkrun lib.rs: "NULL path means
/// NullFs"; worker.rs: NullFs + AugmentFs) — whose tree is VIRTUAL
/// ONLY. `krun_fs_add_overlay_file`'s contract ("all intermediate
/// dirs must already exist", resolved against the device's OWN
/// virtual entries, never the rootfs's real dirs) is met by
/// ROOT-LEVEL file names with no intermediate dirs at all. A
/// dedicated device also avoids the virtual-/etc trap: a virtual
/// dir named `etc` on the root device would intercept EVERY /etc
/// lookup (AugmentFs::lookup delegates a miss inside a virtual
/// tree with the VIRTUAL parent ino, which the inner fs never saw
/// — EBADF), hiding ca-bundle/passwd. The share records name the
/// SAME tag, so the init mounts + links exactly like any share.
const OVERLAY_FS_TAG: &str = "trustfs";

/// `--krun-overlay FS_TAG@PATH:OCTAL_MODE:B64` — decode + validate
/// (the guest path must be ONE device-root name of the trustfs
/// virtual device, the base64 must decode). The one_shot flag is
/// always false: a config file the payload's git reads REPEATEDLY
/// must survive more than one lookup.
fn add_overlay(cfg: &mut Config, value: &str) -> Result<(), String> {
    const FILE_MODE: u32 = 0o100644;
    let (tags, rest) = value
        .split_once('@')
        .ok_or_else(|| format!("--krun-overlay expects FS@PATH:MODE:B64, got `{value}`"))?;
    if tags != OVERLAY_FS_TAG {
        return Err(format!(
            "--krun-overlay: the fs tag must be `{OVERLAY_FS_TAG}`, got `{tags}`"
        ));
    }
    let (path, meta) = rest
        .split_once(':')
        .ok_or_else(|| format!("--krun-overlay expects {tags}PATH:MODE:B64, got `{value}`"))?;
    let (mode, data) = meta
        .split_once(':')
        .ok_or_else(|| format!("--krun-overlay expects {tags}PATH:MODE:B64, got `{value}`"))?;
    let mode: u32 = u32::from_str_radix(mode, 8)
        .map_err(|_| format!("--krun-overlay: the mode must be octal, got `{mode}`"))?;
    if mode != FILE_MODE {
        return Err(format!(
            "--krun-overlay: the mode must be {FILE_MODE:o} (a regular 0644 file), got {mode:o}"
        ));
    }
    if path.starts_with('/') {
        return Err(format!(
            "--krun-overlay: the path is device-relative ({OVERLAY_FS_TAG}'s tree), got `{path}` — drop the leading slash"
        ));
    }
    if path.is_empty() || path.contains("..") || path.contains('/') {
        return Err(format!(
            "--krun-overlay: the path must be ONE device-root name ({OVERLAY_FS_TAG}'s \
             virtual tree has no directories — the intermediate-dir contract needs \
             every parent to be a prior overlay dir), got `{path}`"
        ));
    }
    let decoded = unb64(data)?;
    if decoded.is_empty() {
        return Err("--krun-overlay: the data must not be empty".to_owned());
    }
    cfg.overlays.push(OverlayFile {
        fs_tag: OVERLAY_FS_TAG,
        path: path.to_owned(),
        mode,
        data: decoded,
    });
    Ok(())
}

fn add_share(
    cfg: &mut Config,
    read_only: bool,
    it: &mut std::iter::Peekable<impl Iterator<Item = String>>,
) -> Result<(), String> {
    let value = it
        .next()
        .ok_or_else(|| "missing value for share".to_owned())?;
    let (spec, mode) = value
        .split_once(' ')
        .ok_or_else(|| format!("share expects TAG:SLOT@DEST ro|rw, got `{value}`"))?;
    if mode != "ro" && mode != "rw" {
        return Err(format!("share mode must be `ro` or `rw`, got `{mode}`"));
    }
    let (tags, dest) = spec
        .split_once('@')
        .ok_or_else(|| format!("share expects TAG:SLOT@DEST ro|rw, got `{value}`"))?;
    let (tag, slot) = tags
        .split_once(':')
        .ok_or_else(|| format!("share expects TAG:SLOT@DEST ro|rw, got `{value}`"))?;
    if tag.is_empty() || slot.is_empty() || dest.is_empty() {
        return Err(format!("share expects TAG:SLOT@DEST ro|rw, got `{value}`"));
    }
    if tag.chars().any(|c| c.is_whitespace() || c == ';')
        || slot.chars().any(|c| c.is_whitespace() || c == ';')
        || dest.chars().any(|c| c.is_whitespace() || c == ';')
    {
        return Err("share tag, slot and destination cannot contain whitespace or `;`".to_owned());
    }
    let want_ro = mode == "ro";
    if want_ro != read_only {
        return Err(format!(
            "share `{value}` declares mode {mode} under the {} flag",
            if read_only {
                "--ro-share"
            } else {
                "--rw-share"
            }
        ));
    }
    // The share must name a device the caller explicitly declared --
    // EXCEPT the launcher's own virtual overlay device (bd
    // myconfig-dak.8): `trustfs` carries no `--ro-device` — the
    // launcher adds it itself (NULL host path → NullFs) from the
    // --krun-overlay flags, its read-only flag is hardcoded at that
    // addition, and its ro content lives in host memory, so there is
    // nothing a host dir could declare. The ro check still applies:
    // a share naming trustfs in a RW position would brief the guest
    // init to mount+link rw what the device serves read-only — the
    // init then writes nowhere anyway, but the lie is refused.
    if tag != OVERLAY_FS_TAG
        && !cfg
            .devices
            .iter()
            .any(|d| d.tag == tag && d.read_only == want_ro)
    {
        return Err(format!(
            "share `{value}` names device `{tag}` ({mode}), which no --{}-device declared",
            if read_only { "ro" } else { "rw" }
        ));
    }
    if tag == OVERLAY_FS_TAG && !want_ro {
        return Err(format!(
            "share `{value}` declares the overlay device `{OVERLAY_FS_TAG}` rw — its content is launcher-owned memory served read-only"
        ));
    }
    cfg.shares.push(Share {
        tag: tag.to_owned(),
        slot: slot.to_owned(),
        dest: dest.to_owned(),
    });
    Ok(())
}

/// NUL-terminated C strings for krun_set_exec's argv/envp arrays —
/// the CALLER appends the NULL sentinel in its pointer vector.
fn c_strings(items: &[String]) -> Vec<CString> {
    items
        .iter()
        .map(|s| CString::new(s.as_str()).unwrap())
        .collect()
}

/// The inverse of [`b64`]: decoded BINARY (overlay
/// file content rides argv b64 — the trust text has newlines argv
/// can carry but keeping it b64 keeps the flag grammar byte-blind).
/// Non-base64 input (length % 4 != 0 or an off-alphabet char) is an
/// error — malformed argv, exit 2.
fn unb64(text: &str) -> Result<Vec<u8>, String> {
    const REV: fn(u8) -> Option<u8> = |c: u8| match c {
        b'A'..=b'Z' => Some(c - b'A'),
        b'a'..=b'z' => Some(c - b'a' + 26),
        b'0'..=b'9' => Some(c - b'0' + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        b'=' => None,
        _ => None,
    };
    let trimmed: Vec<u8> = text
        .as_bytes()
        .iter()
        .copied()
        .filter(|c| *c != b'\n' && *c != b'\r' && *c != b' ')
        .collect();
    if !trimmed.len().is_multiple_of(4) {
        return Err(format!(
            "krun-overlay: the data is not valid base64 ({} bytes, not a multiple of 4)",
            trimmed.len()
        ));
    }
    let mut out = Vec::with_capacity(trimmed.len() / 4 * 3);
    for chunk in trimmed.chunks(4) {
        let mut vals = [0u8; 4];
        let mut pad = 0usize;
        for (slot, c) in chunk.iter().enumerate() {
            match REV(*c) {
                Some(v) => vals[slot] = v,
                None if *c == b'=' && slot >= 2 => {
                    pad += 1;
                    vals[slot] = 0;
                }
                None => {
                    return Err(format!(
                        "krun-overlay: invalid base64 character {}:{}",
                        *c, slot
                    ))
                }
            }
        }
        let n = (u32::from(vals[0]) << 18)
            | (u32::from(vals[1]) << 12)
            | (u32::from(vals[2]) << 6)
            | u32::from(vals[3]);
        out.push((n >> 16) as u8);
        if pad < 2 {
            out.push((n >> 8) as u8);
        }
        if pad < 1 {
            out.push(n as u8);
        }
    }
    Ok(out)
}

/// base64 (standard, padded) of arbitrary bytes — the manifest's
/// value encoding: env values and the chdir path may contain any
/// byte; the guest init decodes with busybox `base64 -d`.
fn b64(data: &str) -> String {
    // No external crate (the launcher is dependency-free by design):
    // the standard alphabet, by hand. Only needed for the manifest.
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

/// The manifest text (module docs): tab-separated records, values
/// base64. One `env` line per env entry, one `share` line per
/// share, one optional `chdir` line.
fn manifest_text(cfg: &Config) -> String {
    let mut lines = String::new();
    for (k, v) in &cfg.env {
        lines.push_str(&format!("env\t{k}\t{}\n", b64(v)));
    }
    for s in &cfg.shares {
        let mode = cfg
            .devices
            .iter()
            .find(|d| d.tag == s.tag)
            .map(|d| if d.read_only { "ro" } else { "rw" })
            // The trustfs virtual device is not in devices (the
            // launcher adds it itself, NULL-fs, read-only — bd
            // myconfig-dak.8).
            .unwrap_or(if s.tag == OVERLAY_FS_TAG { "ro" } else { "rw" });
        lines.push_str(&format!(
            "share\t{}\t{}\t{}\t{}\n",
            s.tag, s.slot, s.dest, mode
        ));
    }
    if cfg.workdir.as_os_str() != "/" {
        lines.push_str(&format!("chdir\t{}\n", b64(&cfg.workdir.to_string_lossy())));
    }
    lines
}

/// The manifest's SLOT inside the ro stage device (a fixed name —
/// the init reads it at a known path once stage-ro is mounted).
const MANIFEST_SLOT: &str = "manifest";

/// The scratch disk's BLOCK ID (a fixed name — the guest init finds
/// the device by it, bd myconfig-dak.7).
const SCRATCH_BLOCK_ID: &str = "scratch";

fn exec_spec(cfg: &Config) -> (String, Vec<String>, Vec<String>) {
    let exec_path = cfg.init.clone().unwrap_or_else(|| cfg.payload[0].clone());
    // krun_set_exec's argv vector rides the cmdline behind " -- "
    // and becomes the guest init's argv: /init.krun OVERWRITES
    // argv[0] with KRUN_INIT itself (init.c exec_argv[0] =
    // clone_str(krun_init)) and forwards the REST. The vector
    // therefore carries the payload ONLY — a leading placeholder
    // (the tenth live finding) would arrive as the init's $1 and
    // make it exec ITSELF.
    let argv = cfg.payload.clone();
    // With a manifest, only the manifest pointer rides the cmdline
    // (plus the trace flag — the init's EARLY steps and the
    // launcher's own log level need it before the file is read):
    // env and shares are read from the file (the budget finding).
    // Without a manifest, the legacy envp route remains for
    // cmdline-sized debug runs.
    let env: Vec<String> = match &cfg.manifest {
        Some(_) => {
            let mut env = vec![format!("MYSBX_KRUN_MANIFEST=stage-ro:{MANIFEST_SLOT}")];
            if let Some((_, v)) = cfg.env.iter().find(|(k, _)| k == "MYSBX_KRUN_TRACE") {
                env.push(format!("MYSBX_KRUN_TRACE={v}"));
            }
            env
        }
        None => {
            let mut env: Vec<String> = cfg.env.iter().map(|(k, v)| format!("{k}={v}")).collect();
            if !cfg.shares.is_empty() {
                // One "DEVICE SLOT DEST ro|rw" entry per share: the
                // guest entry script mounts each DEVICE once (under
                // /tmp/mysbx-shares keyed by the device tag) and
                // links each DEST at <device-mount>/<slot>. The mode
                // is the DEVICE's — a share is exactly as writable
                // as its device.
                let shares = cfg
                    .shares
                    .iter()
                    .map(|s| {
                        format!(
                            "{} {} {} {}",
                            s.tag,
                            s.slot,
                            s.dest,
                            cfg.devices
                                .iter()
                                .find(|d| d.tag == s.tag)
                                .map(|d| if d.read_only { "ro" } else { "rw" })
                                // The trustfs virtual device is not in
                                // devices (the launcher adds it itself,
                                // NULL-fs, read-only — bd myconfig-dak.8).
                                .unwrap_or(if s.tag == OVERLAY_FS_TAG { "ro" } else { "rw" })
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(";");
                env.push(format!("MYSBX_KRUN_SHARES={shares}"));
            }
            env
        }
    };
    (exec_path, argv, env)
}

fn main() {
    let cfg = parse_args(std::env::args().skip(1));

    raise_nofile();

    // The /dev/kvm pre-flight (the spike's finding 7): libkrun's
    // KvmContext::new() PANICS — abort, no return code to map —
    // when it cannot open /dev/kvm, and a panic in krun_start_enter
    // crosses a `extern "C"` boundary into `panic cannot unwind`.
    // A real O_RDWR open here turns that abort into a diagnosable
    // 125 with a message naming the cause — critical under bwrap,
    // where the pre-flight of the mysbx wrapper runs on the HOST
    // and a shadowed bind would otherwise abort here with nothing
    // but a Rust backtrace.
    if let Err(e) = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/kvm")
    {
        eprintln!("mysbx-krun: cannot open /dev/kvm read-write: {e}");
        eprintln!(
            "  the direct libkrun backend starts every run as a KVM microVM; \
             inside this sandbox /dev/kvm is not visible or not writable"
        );
        std::process::exit(EXIT_SETUP);
    }

    // dlopen with RTLD_NOW|RTLD_GLOBAL: libkrun dlopen()s its own
    // libkrunfw (the guest-kernel blob) lazily, and the global
    // namespace lets that resolve against the same handle.
    let api = unsafe {
        match load_api() {
            Ok(api) => api,
            Err(e) => {
                eprintln!("mysbx-krun: {e}");
                std::process::exit(EXIT_SETUP);
            }
        }
    };

    let check = |rc: c_int, what: &str| -> c_int {
        if rc < 0 {
            eprintln!("mysbx-krun: libkrun {what} failed: {rc}");
            std::process::exit(EXIT_SETUP);
        }
        rc
    };

    // The trace flag: the bwrap chain hands the launcher a --clearenv
    // environment, so the HOST's MYSBX_KRUN_TRACE never reaches
    // this process — only the config-side --env entry does (the
    // sixth live finding: the flag sat in config.toml, the launcher
    // never saw it). Both routes checked.
    let trace = std::env::var("MYSBX_KRUN_TRACE").is_ok_and(|v| !v.is_empty())
        || cfg
            .env
            .iter()
            .any(|(k, v)| k == "MYSBX_KRUN_TRACE" && !v.is_empty());

    // libkrun's own log, redirected to stderr BEFORE anything else:
    // every error! of the VMM (device attach failures, build_microvm
    // diagnoses — the -22 live finding was a silent IrqsExhausted)
    // is otherwise invisible, and the trace half doubles its level
    // to debug so a hang shows the boot's own story.
    if let Some(init_log) = api.krun_init_log {
        // Level: 1 = error, 4 = debug. Style 0 = auto, options 0 =
        // honor RUST_LOG when the host sets it (the trace's own
        // escalation path).
        let _ = unsafe { (init_log)(2, if trace { 4 } else { 1 }, 0, 0) };
    }

    // The banner (the sixth live finding: a hung run printed NOTHING,
    // leaving no way to tell a dead launcher from a dead console):
    // stderr, unconditional — one line per boot, trace adds the
    // device and share list.
    eprintln!(
        "mysbx-krun: booting {} cpus {} MiB rootfs={} init={}",
        cfg.cpus,
        cfg.ram_mib,
        cfg.rootfs.display(),
        cfg.init.clone().unwrap_or_default(),
    );
    if trace {
        for d in &cfg.devices {
            eprintln!(
                "mysbx-krun: device {} = {} ({})",
                d.tag,
                d.host_dir.display(),
                if d.read_only { "ro" } else { "rw" }
            );
        }
        for s in &cfg.shares {
            eprintln!("mysbx-krun: share {}:{}@{}", s.tag, s.slot, s.dest);
        }
    }
    eprintln!(
        "mysbx-krun: stdio is {}",
        if std::io::IsTerminal::is_terminal(&std::io::stdin())
            && std::io::IsTerminal::is_terminal(&std::io::stdout())
        {
            "a terminal (console wiring: hvc0 <-> fds)"
        } else {
            "NOT a terminal (console wiring: pipes, the krun-stdin/-stdout virtio ports)"
        }
    );

    unsafe {
        let ctx = check((api.krun_create_ctx)(), "krun_create_ctx") as c_uint;
        check(
            (api.krun_set_vm_config)(ctx, cfg.cpus, cfg.ram_mib),
            "krun_set_vm_config",
        );
        // The overlays' VIRTUAL DEVICE first (bd myconfig-dak.8, the
        // review's findings 1+2): krun_add_virtiofs3 with a NULL
        // host path maps to NullFs — and fs_add_overlay_entry
        // matches the device by fs_id, so the registration must come
        // AFTER the device exists. A dedicated virtual tree
        // sidesteps the rootfs-bake trap (real dirs are invisible to
        // the overlay resolver) and the virtual-/etc shadowing trap.
        // The files' pointers alias cfg.overlays' Vecs, which live
        // in main's frame past krun_start_enter (the VM's whole
        // lifetime). one_shot=false: the payload's git reads its
        // config file REPEATEDLY. A symbol-less lib gets the
        // krun_add_disk2 treatment: REFUSED (exit 125), never a
        // silent run — the trust files would exist nowhere.
        if !cfg.overlays.is_empty() {
            check(
                (api.krun_add_virtiofs3)(
                    ctx,
                    cstr(OVERLAY_FS_TAG).as_ptr(),
                    std::ptr::null(),
                    0,
                    true,
                ),
                "krun_add_virtiofs3(trustfs)",
            );
            let add_file = match api.krun_fs_add_overlay_file {
                Some(sym) => sym,
                None => {
                    eprintln!(
                        "mysbx-krun: the run carries --krun-overlay files but the loaded libkrun \
                         has no krun_fs_add_overlay_file — the lib build is too old for the feature"
                    );
                    std::process::exit(EXIT_SETUP);
                }
            };
            for o in &cfg.overlays {
                check(
                    (add_file)(
                        ctx,
                        cstr(o.fs_tag).as_ptr(),
                        cstr(&o.path).as_ptr(),
                        o.data.as_ptr(),
                        o.data.len(),
                        o.mode,
                        false,
                    ),
                    "krun_fs_add_overlay_file",
                );
            }
        }
        // The root: READ-ONLY by design — a sandbox root the payload
        // could write is no sandbox root. krun_add_virtiofs3's flag
        // makes the VIRTIOFS SERVER enforce it, not the guest kernel.
        check(
            (api.krun_add_virtiofs3)(
                ctx,
                cstr(FS_ROOT_TAG).as_ptr(),
                cstr(&cfg.rootfs).as_ptr(),
                0,
                true,
            ),
            "krun_add_virtiofs3(/dev/root)",
        );
        for device in &cfg.devices {
            check(
                (api.krun_add_virtiofs3)(
                    ctx,
                    cstr(&device.tag).as_ptr(),
                    cstr(&device.host_dir).as_ptr(),
                    0,
                    device.read_only,
                ),
                "krun_add_virtiofs3",
            );
        }
        // The scratch disk (bd myconfig-dak.7, backends.md D7):
        // the per-run sparse raw file attaches as a REAL virtio-blk
        // device. RAW always — the mkfs is the guest init's own (the
        // file is per-run, nothing to probe), and libkrun's security
        // note forbids re-probing an image a guest could have
        // written (a guest with full write access could recast a raw
        // file as qcow2 and reference host files). The symbol is
        // blk-feature-gated; its absence is REFUSED, never a silent
        // run without the disk.
        if let Some(scratch) = &cfg.scratch {
            match api.krun_add_disk2 {
                Some(add_disk2) => {
                    const KRUN_DISK_FORMAT_RAW: u32 = 0;
                    check(
                        (add_disk2)(
                            ctx,
                            cstr(SCRATCH_BLOCK_ID).as_ptr(),
                            cstr(scratch).as_ptr(),
                            KRUN_DISK_FORMAT_RAW,
                            false,
                        ),
                        "krun_add_disk2",
                    );
                }
                None => {
                    eprintln!(
                        "mysbx-krun: --scratch but libkrun has no \
                         krun_add_disk2 (blk feature) — the scratch disk cannot attach"
                    );
                    std::process::exit(EXIT_SETUP);
                }
            }
        }
        // The network mode (bd myconfig-dak.6): `none` disables the
        // implicit vsock — no vsock device, no tsi_hijack on the
        // guest cmdline, NO socket path to the host (the stock
        // nixpkgs libkrun enables TSI even without its net feature,
        // so shared is the default the caller need not name). The
        // symbol is present in the stock lib AND the withBlk pin (the
        // vsock is not net-gated — nm -D verified, bd myconfig-au2);
        // its absence in some other build cannot honor `none` and is
        // REFUSED — an unfilterable TSI proxy is exactly what
        // `network = false` forbids. A SHARED run never consumes the
        // symbol: the vsock is the IMPLICIT device — attached unless
        // disabled — so a symbol-less lib simply keeps TSI on, and the
        // shared semantics hold unchanged (bd myconfig-au2: no
        // warning here, silent is CORRECT, not fail-closed-by-accident).
        match vsock_decision(cfg.network, api.krun_disable_implicit_vsock.is_some()) {
            VsockDecision::KeepTsi => {}
            VsockDecision::DisableTsi => {
                let disable = api
                    .krun_disable_implicit_vsock
                    .expect("vsock_decision returned DisableTsi with the symbol set");
                check((disable)(ctx), "krun_disable_implicit_vsock");
            }
            VsockDecision::RefusedNoSymbol => {
                eprintln!(
                    "mysbx-krun: --network none but libkrun has no krun_disable_implicit_vsock — TSI cannot be disabled"
                );
                std::process::exit(EXIT_SETUP);
            }
        }
        // The payload's working directory (krun_set_workdir, the
        // spec's --chdir): the workspace path, where every other
        // backend's payload starts too. In manifest mode the
        // workdir rides as a `chdir` record the init applies AFTER
        // the shares exist (the cmdline route had /init.krun chdir
        // BEFORE the workspace share was mounted — silently landing
        // at / — plus it spent cmdline bytes, the budget finding).
        if cfg.manifest.is_none() {
            check(
                (api.krun_set_workdir)(ctx, cstr(&cfg.workdir).as_ptr()),
                "krun_set_workdir",
            );
        }
        // ALWAYS an explicit envp: envp=NULL makes libkrun inherit
        // the launcher's whole environment (env::vars() of
        // krun_set_exec), which under bwrap is exactly what the
        // caller did not ask for. The shares' guest placement rides
        // along as MYSBX_KRUN_SHARES (one "TAG DEST ro|rw" entry per
        // --ro-share/--rw-share, ';'-separated).
        //
        // The argv contract of krun_set_exec: the vector lands on the
        // kernel cmdline behind " -- " and becomes the guest INIT's
        // argv, whose argv[0] /init.krun overwrites with KRUN_INIT
        // (the exec_path). So argv[0] is a placeholder the init
        // consumes: with --init the vector is [init, payload...]
        // (init.c exec_argv[0] = KRUN_INIT, argv[1..] forwarded — the
        // entry script sees payload at $1..), without it
        // [payload0, payload0, args...] so the overwrite keeps the
        // payload path at argv[0].
        // The manifest (the budget finding): its records live in a
        // HOST file inside the ro stage device's directory, which
        // the caller staged — the launcher only writes the records.
        if let Some(manifest) = &cfg.manifest {
            if !cfg
                .devices
                .iter()
                .any(|d| d.read_only && d.tag == "stage-ro")
            {
                eprintln!("mysbx-krun: --manifest needs a stage-ro --ro-device to reach the guest");
                std::process::exit(EXIT_SETUP);
            }
            let text = manifest_text(&cfg);
            if let Err(e) = std::fs::write(manifest, &text) {
                eprintln!(
                    "mysbx-krun: cannot write the manifest {}: {e}",
                    manifest.display()
                );
                std::process::exit(EXIT_SETUP);
            }
        }

        let (exec_path, argv_vec, env) = exec_spec(&cfg);
        let argv = c_strings(&argv_vec);
        let envp = c_strings(&env);
        let mut argv_ptrs: Vec<*const c_char> = argv.iter().map(|s| s.as_ptr()).collect();
        argv_ptrs.push(std::ptr::null());
        let mut envp_ptrs: Vec<*const c_char> = envp.iter().map(|s| s.as_ptr()).collect();
        envp_ptrs.push(std::ptr::null());
        check(
            (api.krun_set_exec)(
                ctx,
                cstr(&exec_path).as_ptr(),
                argv_ptrs.as_ptr(),
                envp_ptrs.as_ptr(),
            ),
            "krun_set_exec",
        );
        // krun_start_enter runs the VM and returns the payload's exit
        // code (via the init's KRUN_EXIT_CODE_IOCTL) — propagation is
        // the whole exit-code story of the spike.
        std::process::exit(check((api.krun_start_enter)(ctx), "krun_start_enter"));
    }
}

fn cstr(s: impl AsRef<std::path::Path>) -> CString {
    CString::new(s.as_ref().to_string_lossy().as_bytes()).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overlay_flag_round_trips_and_refuses_the_bad_spellings() {
        // bd myconfig-dak.8: the trust text rides argv b64 — the
        // grammar must be byte-blind and refuse malformed input at
        // the parse edge (exit 2, never a half-registered VM).
        let payload = "safe.directory = /tmp/x\n[include]\n\tpath = y\n";
        let value = format!("trustfs@gitconfig:0100644:{}", b64(payload));
        let mut cfg = test_cfg();
        add_overlay(&mut cfg, &value).expect("the flag parses");
        assert_eq!(cfg.overlays.len(), 1);
        let o = &cfg.overlays[0];
        assert_eq!(o.fs_tag, OVERLAY_FS_TAG);
        assert_eq!(o.path, "gitconfig");
        assert_eq!(o.mode, 0o100644);
        assert_eq!(o.data, payload.as_bytes());
        assert!(!o.data.is_empty());

        // the refusals: wrong tag, leading slash, bad mode, bad b64,
        // empty content
        let good = |spec: &str, data: &str| format!("dev/root@{spec}:{data}");
        let mut cfg = test_cfg();
        assert!(add_overlay(&mut cfg, "etc/root@p:0100644:AAAA").is_err());
        let mut cfg = test_cfg();
        assert!(add_overlay(&mut cfg, &good("/abs", "0100644:AAAA")).is_err());
        let mut cfg = test_cfg();
        assert!(
            add_overlay(&mut cfg, &good("p", "040755:AAAA")).is_err(),
            "the dir mode is not a file mode"
        );
        let mut cfg = test_cfg();
        assert!(
            add_overlay(&mut cfg, &good("p", "0100644:AB*D")).is_err(),
            "the b64 is malformed"
        );
        let mut cfg = test_cfg();
        assert!(
            add_overlay(&mut cfg, &good("p", "0100644:")).is_err(),
            "empty content"
        );
        let mut cfg = test_cfg();
        assert!(add_overlay(&mut cfg, "dev/root@../escape:0100644:AAAA").is_err());
    }

    fn test_cfg() -> Config {
        Config {
            cpus: 2,
            ram_mib: 2048,
            rootfs: PathBuf::new(),
            init: None,
            devices: Vec::new(),
            shares: Vec::new(),
            env: Vec::new(),
            overlays: Vec::new(),
            manifest: None,
            network: true,
            workdir: PathBuf::from("/"),
            scratch: None,
            payload: Vec::new(),
        }
    }

    #[test]
    fn vsock_decision_pins_all_four_combinations() {
        // bd myconfig-ot1, the rrg regression guard: reintroducing
        // the refusal that also swallowed SHARED runs (bd
        // myconfig-rrg) would flip a shared+no-symbol case to
        // RefusedNoSymbol — this pin catches it.
        use VsockDecision::*;
        assert!(matches!(vsock_decision(true, true), KeepTsi));
        assert!(
            matches!(vsock_decision(true, false), KeepTsi),
            "a shared run proceeds on a symbol-less lib (the rrg fix)"
        );
        assert!(matches!(vsock_decision(false, true), DisableTsi));
        assert!(
            matches!(vsock_decision(false, false), RefusedNoSymbol),
            "none on a symbol-less lib stays refused"
        );
    }

    #[test]
    fn raising_nofile_lifts_the_soft_limit_to_the_hard_one() {
        // bd myconfig-mnr: the launcher hosts libkrun's virtiofs
        // server — every guest file handle is a HOST fd here — so the
        // soft limit must not stay at a session's 1024. The raise is
        // best-effort: this test only pins that the soft limit ends
        // at (or above) what it was, and at the hard ceiling when the
        // kernel allows the raises (the CI/sandbox case is the
        // capped-hard one, where soft == hard afterwards).
        let before = unsafe {
            let mut rlim = Rlimit { cur: 0, max: 0 };
            assert_eq!(getrlimit(RLIMIT_NOFILE, &mut rlim), 0);
            (rlim.cur, rlim.max)
        };
        let (soft, hard) = raise_nofile();
        assert!(
            soft >= before.0,
            "the soft limit must not drop: {before:?} -> ({soft}, {hard})"
        );
        assert!(
            hard >= before.1,
            "the hard limit must not drop: {before:?} -> ({soft}, {hard})"
        );
        assert!(
            soft == hard,
            "best effort: soft should reach the hard ceiling ({soft} vs {hard})"
        );
    }

    use super::*;

    fn parse(args: &[&str]) -> Result<Config, String> {
        parse_args_result(args.iter().map(|arg| (*arg).to_owned()))
    }

    #[test]
    fn requires_rootfs_and_payload() {
        assert!(parse(&["--"]).is_err());
        assert!(parse(&["--rootfs", "/root", "--"]).is_err());
        assert!(parse(&["--rootfs", "/root", "--", "/bin/true"]).is_ok());
    }

    #[test]
    fn scratch_parses_a_path() {
        // --scratch names the HOST-side sparse raw file (bd
        // myconfig-dak.7); without it no disk attaches.
        let with = parse(&[
            "--rootfs",
            "/root",
            "--scratch",
            "/scratch/s.img",
            "--",
            "/bin/true",
        ])
        .unwrap();
        assert_eq!(with.scratch, Some(PathBuf::from("/scratch/s.img")));
        let without = parse(&["--rootfs", "/root", "--", "/bin/true"]).unwrap();
        assert_eq!(without.scratch, None);
        assert!(parse(&["--rootfs", "/root", "--scratch", "--", "/bin/true"]).is_err());
    }

    #[test]
    fn builds_init_and_direct_argv_layouts() {
        let direct = parse(&["--rootfs", "/root", "--", "/bin/sh", "-c", "true"]).unwrap();
        assert_eq!(exec_spec(&direct).1, ["/bin/sh", "-c", "true"]);

        let with_init = parse(&[
            "--rootfs", "/root", "--init", "/init", "--", "/bin/sh", "-c", "true",
        ])
        .unwrap();
        assert_eq!(exec_spec(&with_init).0, "/init");
        // NO placeholder: /init.krun replaces argv[0] with KRUN_INIT
        // itself; a leading placeholder would arrive as the init's
        // $1 (the tenth live finding — the init exec'd ITSELF).
        assert_eq!(exec_spec(&with_init).1, ["/bin/sh", "-c", "true"]);
    }

    #[test]
    fn chdir_defaults_to_root_and_parses() {
        let cfg = parse(&["--rootfs", "/root", "--", "/bin/true"]).unwrap();
        assert_eq!(cfg.workdir, std::path::Path::new("/"));
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--chdir",
            "/workspace",
            "--",
            "/bin/true",
        ])
        .unwrap();
        assert_eq!(cfg.workdir, std::path::Path::new("/workspace"));
        assert!(parse(&["--rootfs", "/root", "--chdir", "--"]).is_err());
    }

    #[test]
    fn encodes_devices_slots_and_environment() {
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--ro-device",
            "stage-ro=/stage/ro",
            "--rw-device",
            "stage-rw=/stage/rw",
            "--ro-share",
            "stage-ro:store@/nix/store ro",
            "--rw-share",
            "stage-rw:repo@/home/synth/repo rw",
            "--env",
            "KEY=value",
            "--",
            "/bin/true",
        ])
        .unwrap();
        let env = exec_spec(&cfg).2;
        assert!(env.contains(&"KEY=value".to_owned()));
        assert!(env.contains(
            &"MYSBX_KRUN_SHARES=stage-ro store /nix/store ro;stage-rw repo /home/synth/repo rw"
                .to_owned()
        ));
    }

    #[test]
    fn the_manifest_replaces_the_cmdline_env_and_shares() {
        // The cmdline-budget regression guard (seventh live
        // finding): with --manifest the envp carries ONLY the
        // pointer, and the manifest text carries everything else —
        // the cmdline stays under COMMAND_LINE_SIZE regardless of
        // the share and env count.
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--ro-device",
            "stage-ro=/stage/ro",
            "--rw-device",
            "stage-rw=/stage/rw",
            "--manifest",
            "/stage/ro/manifest",
            "--env",
            "KEY=a value with spaces and 'quotes'",
            "--chdir",
            "/home/synth/repo",
            "--",
            "/bin/true",
        ])
        .unwrap();
        let env = exec_spec(&cfg).2;
        assert_eq!(env.len(), 1);
        assert_eq!(env[0], "MYSBX_KRUN_MANIFEST=stage-ro:manifest");
        let text = manifest_text(&cfg);
        assert!(text.contains(&format!(
            "env\tKEY\t{}\n",
            b64("a value with spaces and 'quotes'")
        )));
        assert!(text.starts_with("env\t"));
        assert!(text.contains("chdir\t"));
    }

    #[test]
    fn network_defaults_to_shared_and_parses_both_modes() {
        // The default is SHARED (bd myconfig-dak.6): the implicit
        // vsock's TSI is the guest's egress; the caller names
        // `none` only to disable it.
        assert!(
            parse(&["--rootfs", "/root", "--", "/bin/true"])
                .unwrap()
                .network
        );
        assert!(
            !parse(&["--rootfs", "/root", "--network", "none", "--", "/bin/true"])
                .unwrap()
                .network
        );
        assert!(
            parse(&[
                "--rootfs",
                "/root",
                "--network",
                "shared",
                "--",
                "/bin/true"
            ])
            .unwrap()
            .network
        );
        assert!(parse(&["--rootfs", "/root", "--network", "off", "--", "/bin/true"]).is_err());
    }

    #[test]
    fn the_manifest_combines_with_the_share_and_env_flags() {
        // The manifest flag names the OUTPUT file; the share/env
        // flags are the INPUT it absorbs — the argv interface is
        // exactly the pre-manifest one.
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--ro-device",
            "stage-ro=/stage/ro",
            "--manifest",
            "/stage/ro/manifest",
            "--ro-share",
            "stage-ro:x@/x ro",
            "--env",
            "A=b",
            "--",
            "/bin/true",
        ])
        .unwrap();
        let text = manifest_text(&cfg);
        assert!(text.contains("share\tstage-ro\tx\t/x\tro\n"));
        assert!(text.contains("env\tA\t"));
    }

    #[test]
    fn the_device_count_is_the_virtiofs_budget_not_the_shares() {
        // The slot-budget regression guard (live finding): the
        // launcher registers ONE krun_add_virtiofs3 per device, so a
        // config with many shares still builds few devices.
        let mut args: Vec<String> = vec![
            "--rootfs".into(),
            "/root".into(),
            "--ro-device".into(),
            "stage-ro=/stage/ro".into(),
            "--rw-device".into(),
            "stage-rw=/stage/rw".into(),
        ];
        for i in 0..10 {
            if i % 2 == 0 {
                args.push("--ro-share".into());
                args.push(format!("stage-ro:s{i}@/srv/m{i} ro"));
            } else {
                args.push("--rw-share".into());
                args.push(format!("stage-rw:s{i}@/srv/m{i} rw"));
            }
        }
        args.push("--".into());
        args.push("/bin/true".into());
        let cfg = parse(&args.iter().map(|s| s.as_str()).collect::<Vec<_>>()).unwrap();
        assert_eq!(cfg.devices.len(), 2);
        assert_eq!(cfg.shares.len(), 10);
        // The guest encoding: one entry per share, the device count
        // nowhere in it — mounting is per device, linking per slot.
        let env = exec_spec(&cfg).2;
        let shares = env.last().unwrap();
        assert_eq!(shares.matches(';').count(), 9);
    }

    #[test]
    #[test]
    fn the_trustfs_overlay_device_needs_no_ro_device_declaration() {
        // bd myconfig-dak.8's live finding: the share validation
        // demanded a --ro-device for the launcher-internal virtual
        // device — every real run refused before the VM started. The
        // device is launcher-owned (added from the --krun-overlay
        // flags, NULL host path → NullFs), so only the RW lie is
        // still refused, and undeclared EXTERNAL devices keep the
        // strict check.
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--krun-overlay",
            "trustfs@gitconfig:0100644:QUJD",
            "--ro-share",
            "trustfs:gitconfig@/etc/mysbx/gitconfig ro",
            "--",
            "/bin/true",
        ])
        .expect("the trustfs share with no --ro-device passes");
        assert!(cfg.shares.iter().any(|s| s.tag == "trustfs"));
        assert!(cfg.overlays.iter().any(|o| o.path == "gitconfig"));
        // the rw lie:
        assert!(parse(&[
            "--rootfs",
            "/root",
            "--krun-overlay",
            "trustfs@gitconfig:0100644:QUJD",
            "--rw-share",
            "trustfs:gitconfig@/etc/mysbx/gitconfig rw",
            "--",
            "/bin/true"
        ])
        .is_err());
        // the undeclared EXTERNAL device stays refused (the original
        // guard's contract, case-verified via rejects_ambiguous_
        // share_delimiters' nodev entry).
    }

    fn rejects_ambiguous_share_delimiters() {
        for value in [
            "bad tag@/dest ro",
            "tag@/bad dest ro",
            "tag:/slot@/dest;bad ro",
            "stage-ro:/slot@/dest rw", // mode mismatch with --ro-share
            "nodev:/slot@/dest ro",    // undeclared device
        ] {
            assert!(parse(&[
                "--rootfs",
                "/root",
                "--ro-device",
                "stage-ro=/stage/ro",
                "--ro-share",
                value,
                "--",
                "/bin/true"
            ])
            .is_err());
        }
    }
}
