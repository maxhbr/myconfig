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
// - The implicit init (/init.krun) mounts devtmpfs/proc/sysfs/cgroup2
//   but mounts NO extra virtiofs tags; the guest payload is expected
//   to do that (or the rootfs carries a custom init — bd
//   myconfig-dak.5). For the spike the payload itself mounts the
//   tags, which the kernel makes available as virtiofs devices.
// - Exit codes: the implicit init maps workload exit to the
//   KRUN_EXIT_CODE_IOCTL on the root virtiofs; krun_start_enter
//   returns it to the caller. 125/126/127 are init-level errors.
//
// Usage (spike interface — argv and env, no TOML):
//
//   mysbx-krun [--cpus N] [--ram MIB] --rootfs DIR [--ro-share TAG=DIR]
//              [--rw-share TAG=DIR] [--env K=V]... -- CMD [ARGS...]
//
// The rootfs is shared READ-ONLY (the root of a sandbox is never
// writable from inside). Extra virtiofs tags carry their own ro/rw
// flag — enforced by the virtiofs server itself, which is the point
// of the direct backend (backends.md D2's per-share enforcement).
//
// No passt for the spike's first step: the launcher never calls a
// net API and, when the lib carries the implicit-vsock symbol (a
// net-enabled build adds TSI by default), it DISABLES the implicit
// vsock so a run with no net device is honest about having no
// network. The default nixpkgs build has neither the symbol nor TSI,
// so nothing is needed there.

use std::ffi::{c_char, c_int, c_uchar, c_uint, CString};
use std::path::PathBuf;

/// The virtiofs tag libkrun reserves for the root filesystem
/// (KRUN_FS_ROOT_TAG, include/libkrun.h).
const FS_ROOT_TAG: &str = "/dev/root";

/// libkrun's init-level exit codes (krun_start_enter doc comment):
/// 125 setup failure, 126/127 exec failure of the payload. Reported
/// verbatim — mysbx's own error mapping is the backend's business.
const EXIT_SETUP: i32 = 125;

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
    })
}

extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut ();
    fn dlsym(handle: *mut (), symbol: *const c_char) -> *mut ();
}

// ---------------------------------------------------------------------------
// argv/env plumbing
// ---------------------------------------------------------------------------

struct Share {
    tag: String,
    /// The SANDBOX path the payload must see the share at — carried
    /// through to the guest via MYSBX_KRUN_SHARES, so the guest
    /// entry script knows where each tag belongs. The GUEST mount
    /// itself lives under /tmp/mysbx-shares keyed by the tag (a
    /// virtiofs device cannot nest below the ro root share — EBUSY,
    /// the spike's finding 9); the entry script mounts the tag
    /// there and links this path at the mount.
    dest: String,
    host_dir: PathBuf,
    read_only: bool,
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
    shares: Vec<Share>,
    env: Vec<(String, String)>,
    /// The payload's working directory, handed to krun_set_workdir
    /// (bwrap's --chdir equivalent). Defaults to `/` — the spec
    /// always carries the workspace path.
    workdir: PathBuf,
    payload: Vec<String>,
}

fn usage() -> ! {
    eprintln!(
        "usage: mysbx-krun [--cpus N] [--ram MIB] --rootfs DIR [--init PATH] \
         [--ro-share TAG@DEST=DIR] [--rw-share TAG@DEST=DIR] [--env K=V]... \
         [--chdir DIR] -- CMD [ARGS...]"
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
        shares: Vec::new(),
        env: Vec::new(),
        workdir: PathBuf::from("/"),
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
            "--ro-share" => add_share(&mut cfg, true, &mut it)?,
            "--rw-share" => add_share(&mut cfg, false, &mut it)?,
            "--env" => {
                let value = it
                    .next()
                    .ok_or_else(|| "missing value for --env".to_owned())?;
                let (k, v) = value
                    .split_once('=')
                    .ok_or_else(|| format!("--env expects K=V, got `{value}`"))?;
                cfg.env.push((k.to_owned(), v.to_owned()));
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

fn add_share(
    cfg: &mut Config,
    read_only: bool,
    it: &mut std::iter::Peekable<impl Iterator<Item = String>>,
) -> Result<(), String> {
    let value = it
        .next()
        .ok_or_else(|| "missing value for share".to_owned())?;
    let (spec, dir) = value
        .split_once('=')
        .ok_or_else(|| format!("share expects TAG@DEST=DIR, got `{value}`"))?;
    let (tag, dest) = spec
        .split_once('@')
        .ok_or_else(|| format!("share expects TAG@DEST=DIR, got `{value}`"))?;
    if tag.is_empty() || dest.is_empty() || dir.is_empty() {
        return Err(format!("share expects TAG@DEST=DIR, got `{value}`"));
    }
    if tag.chars().any(|c| c.is_whitespace() || c == ';')
        || dest.chars().any(|c| c.is_whitespace() || c == ';')
    {
        return Err("share tag and destination cannot contain whitespace or `;`".to_owned());
    }
    cfg.shares.push(Share {
        tag: tag.to_owned(),
        dest: dest.to_owned(),
        host_dir: PathBuf::from(dir),
        read_only,
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

fn exec_spec(cfg: &Config) -> (String, Vec<String>, Vec<String>) {
    let exec_path = cfg.init.clone().unwrap_or_else(|| cfg.payload[0].clone());
    let argv = match &cfg.init {
        Some(_) => {
            let mut argv = vec![exec_path.clone()];
            argv.extend(cfg.payload.iter().cloned());
            argv
        }
        None => cfg.payload.clone(),
    };
    let mut env: Vec<String> = cfg.env.iter().map(|(k, v)| format!("{k}={v}")).collect();
    if !cfg.shares.is_empty() {
        let shares = cfg
            .shares
            .iter()
            .map(|s| {
                format!(
                    "{} {} {}",
                    s.tag,
                    s.dest,
                    if s.read_only { "ro" } else { "rw" }
                )
            })
            .collect::<Vec<_>>()
            .join(";");
        env.push(format!("MYSBX_KRUN_SHARES={shares}"));
    }
    (exec_path, argv, env)
}

fn main() {
    let cfg = parse_args(std::env::args().skip(1));

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

    unsafe {
        let ctx = check((api.krun_create_ctx)(), "krun_create_ctx") as c_uint;
        check(
            (api.krun_set_vm_config)(ctx, cfg.cpus, cfg.ram_mib),
            "krun_set_vm_config",
        );
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
        for share in &cfg.shares {
            check(
                (api.krun_add_virtiofs3)(
                    ctx,
                    cstr(&share.tag).as_ptr(),
                    cstr(&share.host_dir).as_ptr(),
                    0,
                    share.read_only,
                ),
                "krun_add_virtiofs3",
            );
        }
        // No network: when the lib knows the implicit vsock (a
        // net-enabled build), kill it — that is the only way a run
        // with no net device is honest about having no network. A
        // default nixpkgs build has the symbol absent and no TSI
        // either (verified: no krun_add_net_*/krun_set_passt_fd
        // symbols in the plain lib), so nothing is needed there.
        if let Some(disable) = api.krun_disable_implicit_vsock {
            check((disable)(ctx), "krun_disable_implicit_vsock");
        }
        // The payload's working directory (krun_set_workdir, the
        // spec's --chdir): the workspace path, where every other
        // backend's payload starts too.
        check(
            (api.krun_set_workdir)(ctx, cstr(&cfg.workdir).as_ptr()),
            "krun_set_workdir",
        );
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
    fn builds_init_and_direct_argv_layouts() {
        let direct = parse(&["--rootfs", "/root", "--", "/bin/sh", "-c", "true"]).unwrap();
        assert_eq!(exec_spec(&direct).1, ["/bin/sh", "-c", "true"]);

        let with_init = parse(&[
            "--rootfs", "/root", "--init", "/init", "--", "/bin/sh", "-c", "true",
        ])
        .unwrap();
        assert_eq!(exec_spec(&with_init).0, "/init");
        assert_eq!(exec_spec(&with_init).1, ["/init", "/bin/sh", "-c", "true"]);
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
    fn encodes_share_modes_and_environment() {
        let cfg = parse(&[
            "--rootfs",
            "/root",
            "--ro-share",
            "ro@/ro=/host-ro",
            "--rw-share",
            "rw@/rw=/host-rw",
            "--env",
            "KEY=value",
            "--",
            "/bin/true",
        ])
        .unwrap();
        let env = exec_spec(&cfg).2;
        assert!(env.contains(&"KEY=value".to_owned()));
        assert!(env.contains(&"MYSBX_KRUN_SHARES=ro /ro ro;rw /rw rw".to_owned()));
    }

    #[test]
    fn rejects_ambiguous_share_delimiters() {
        for value in [
            "bad tag@/dest=/host",
            "tag@/bad dest=/host",
            "tag@/dest;bad=/host",
        ] {
            assert!(parse(&["--rootfs", "/root", "--ro-share", value, "--", "/bin/true"]).is_err());
        }
    }
}
