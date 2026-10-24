// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! mysbx-microvm-qemu — the qemu twin of the mysbx-krun launcher
//! (docs/design/backends.md D9, bd myconfig-kvq8).
//!
//! The argv contract is the krun launcher's verbatim (mysbx-rs
//! renders the SAME staged spec — flags, shares, manifest pointer,
//! `--` payload) plus the two qemu-only pieces:
//!
//! - `--kernel PATH` — the pinned bzImage libkrun would have
//!   bundled (MYSBX_MICROVM_QEMU_KERNEL of the wrapper).
//! - `--exit-channel` — arm the virtio-serial port the guest init
//!   reports the payload's exit code over (libkrun gets the code
//!   from krun_start_enter; a stock qemu guest PANICS on PID-1
//!   exit, so the code rides out of band — the init writes it to
//!   /dev/virtio-ports/mysbx-exit before powering off).
//!
//! What the launcher DOES (the builders.nix chain, rootless):
//!
//! 1. one virtiofsd per device (the root INCLUDED — qemu has no
//!    in-process virtiofs server): `--sandbox none --cache=auto
//!    [--readonly]`, socket in a per-run temp dir
//! 2. wait for every socket (qemu exits if a chardev socket is
//!    dead)
//! 3. write the manifest records into the ro stage dir's slot
//! 4. exec qemu-system-x86_64: -M microvm,accel=kvm:tcg, the
//!    kernel + `-append` cmdline (kernel params, the manifest
//!    pointer; the payload argv rides the MANIFEST's argv records
//!    — the stock kernel's `--` cmdline split is lossy, so a
//!    manifest-bearing run keeps the cmdline payload-free, bd
//!    myconfig-hq7k), one
//!    `-chardev socket` + `-device vhost-user-fs-pci` per device,
//!    the SLiRP `-netdev user` when `--network shared` (the qemu
//!    tier's model), the exit-channel virtserialport
//! 5. wait for qemu, read the exit code off the channel, kill the
//!    daemons, propagate the code
//!
//! The binaries are PINNED (no PATH fallback — the run path's own
//! rule): MYSBX_MICROVM_QEMU_VIRTIOFSD and MYSBX_MICROVM_QEMU_QEMU
//! name absolute paths, set by the nix wrapper
//! (nix/microvm-qemu-launcher.nix). A missing pin is a 125 with a
//! diagnosis, never a silent PATH guess.
//!
//! Zero dependencies by design: qemu and virtiofsd are separate
//! PROCESSES invoked by absolute paths — no FFI, no dlopen, no
//! unsafe code at all.

use std::io::BufRead;
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

const EXIT_SETUP: i32 = 125;

/// The device tag of the read-only ROOT (the init mounts it as the
/// real root before anything else) — the krun launcher's constant,
/// shared verbatim.
const FS_ROOT_TAG: &str = "/dev/root";

/// The manifest's SLOT inside the ro stage device (a fixed name —
/// the init reads it at a known path once stage-ro is mounted).
const MANIFEST_SLOT: &str = "manifest";

/// The exit-channel chardev/port ids (a stable guest contract —
/// the init opens /dev/virtio-ports/mysbx-exit).
const EXIT_CHARDEV: &str = "exitchan";
const EXIT_PORT: &str = "mysbx-exit";

/// The scratch disk's BLOCK id (the guest init finds the device by
/// it, bd myconfig-dak.7 — the same fixed name the krun launcher
/// attaches with krun_add_disk2).
const SCRATCH_DRIVE_ID: &str = "scratch";

/// The per-run dir the virtiofsd sockets and the exit-channel
/// socket live in (the builders.nix chain runs from ONE cwd — the
/// same isolation here: one dir per launcher pid, swept at exit).
fn runtime_dir() -> PathBuf {
    std::env::temp_dir().join(format!("mysbx-microvm-qemu-{}", std::process::id()))
}

// ---------------------------------------------------------------------------
// The flag grammar: krun-rs's verbatim, plus --kernel and
// --exit-channel. The parse is a PURE function over an iterator of
// Strings — unit-tested below without touching the environment.
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
struct Device {
    tag: String,
    host_dir: PathBuf,
    read_only: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct Share {
    tag: String,
    slot: String,
    dest: String,
}

#[derive(Debug, Clone, PartialEq)]
struct OverlayFile {
    fs_tag: String,
    path: String,
    mode: u32,
    data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq)]
struct Config {
    cpus: u8,
    ram_mib: u32,
    rootfs: PathBuf,
    kernel: PathBuf,
    exit_channel: bool,
    verbose: bool,
    init: Option<String>,
    devices: Vec<Device>,
    shares: Vec<Share>,
    env: Vec<(String, String)>,
    overlays: Vec<OverlayFile>,
    manifest: Option<PathBuf>,
    network: bool,
    workdir: PathBuf,
    scratch: Option<PathBuf>,
    payload: Vec<String>,
}

fn usage() -> ! {
    eprintln!(
        "usage: mysbx-microvm-qemu [--cpus N] [--ram MIB] [--verbose] --rootfs DIR \
         --kernel BZIMAGE [--exit-channel] [--init PATH] \
         [--ro-device TAG=DIR] [--rw-device TAG=DIR] \
         [--ro-share TAG:SLOT@DEST] [--rw-share TAG:SLOT@DEST] \
         [--manifest FILE] [--network shared|none] \
         [--scratch FILE] [--env K=V]... \
         [--krun-overlay TAG@PATH:MODE:B64]... [--chdir DIR] -- CMD [ARGS...]"
    );
    std::process::exit(2);
}

/// Parse `TAG=DIR` (a device flag's value).
fn add_device(cfg: &mut Config, value: &str, read_only: bool) -> Result<(), String> {
    let Some((tag, dir)) = value.split_once('=') else {
        return Err(format!("device `{value}` is not TAG=DIR"));
    };
    if tag.is_empty() || dir.is_empty() {
        return Err(format!("device `{value}` has an empty tag or dir"));
    }
    cfg.devices.push(Device {
        tag: tag.to_owned(),
        host_dir: PathBuf::from(dir),
        read_only,
    });
    Ok(())
}

/// Parse `TAG:SLOT@DEST` (a share flag's value).
fn add_share(cfg: &mut Config, value: &str, read_only: bool) -> Result<(), String> {
    let Some((tag, rest)) = value.split_once(':') else {
        return Err(format!("share `{value}` is not TAG:SLOT@DEST"));
    };
    let Some((slot, dest)) = rest.split_once('@') else {
        return Err(format!("share `{value}` is not TAG:SLOT@DEST"));
    };
    if tag.is_empty() || slot.is_empty() || dest.is_empty() {
        return Err(format!("share `{value}` has an empty tag, slot or dest"));
    }
    cfg.shares.push(Share {
        tag: tag.to_owned(),
        slot: slot.to_owned(),
        dest: dest.to_owned(),
    });
    // The DEVICE carries the mode (a share is exactly as writable
    // as its device — the krun-rs rule); the flag spelling only
    // selects the parser arm.
    let _ = read_only;
    Ok(())
}

fn add_overlay(cfg: &mut Config, value: &str) -> Result<(), String> {
    // TAG@PATH:MODE:B64 — the krun launcher's encoding verbatim.
    let Some((fs_tag, rest)) = value.split_once('@') else {
        return Err(format!("overlay `{value}` is not TAG@PATH:MODE:B64"));
    };
    let Some((path, rest)) = rest.split_once(':') else {
        return Err(format!("overlay `{value}` is not TAG@PATH:MODE:B64"));
    };
    let Some((mode, b64)) = rest.split_once(':') else {
        return Err(format!("overlay `{value}` is not TAG@PATH:MODE:B64"));
    };
    let Ok(mode) = u32::from_str_radix(mode, 8) else {
        return Err(format!("overlay `{value}`: mode `{mode}` is not octal"));
    };
    let data = unb64(b64)?;
    cfg.overlays.push(OverlayFile {
        fs_tag: fs_tag.to_owned(),
        path: path.to_owned(),
        mode,
        data,
    });
    Ok(())
}

fn parse_args_result(mut args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut cfg = Config {
        cpus: 2,
        ram_mib: 2048,
        rootfs: PathBuf::new(),
        kernel: PathBuf::new(),
        exit_channel: false,
        verbose: false,
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
    // The `--` split first: everything after it is the payload, never
    // a flag — the same rule krun-rs applies.
    let payload: Vec<String>;
    loop {
        match args.next() {
            None => return Err("no `--` and no payload".to_owned()),
            Some(a) if a == "--" => {
                payload = args.collect();
                break;
            }
            Some(a) => {
                let mut value_of = |flag: &str| -> Result<String, String> {
                    if a != flag {
                        return Err(format!("internal: expected {flag}, got {a}"));
                    }
                    args.next().ok_or_else(|| format!("{flag} needs a value"))
                };
                let _ = &mut value_of;
                match a.as_str() {
                    "--cpus" => {
                        let v = value_of("--cpus")?;
                        cfg.cpus = v
                            .parse()
                            .map_err(|_| format!("--cpus `{v}` is not a number"))?;
                    }
                    "--ram" => {
                        let v = value_of("--ram")?;
                        cfg.ram_mib = v
                            .parse()
                            .map_err(|_| format!("--ram `{v}` is not a number"))?;
                    }
                    "--verbose" => cfg.verbose = true,
                    "--rootfs" => cfg.rootfs = PathBuf::from(value_of("--rootfs")?),
                    "--kernel" => cfg.kernel = PathBuf::from(value_of("--kernel")?),
                    "--exit-channel" => cfg.exit_channel = true,
                    "--init" => cfg.init = Some(value_of("--init")?),
                    "--ro-device" => add_device(&mut cfg, &value_of("--ro-device")?, true)?,
                    "--rw-device" => add_device(&mut cfg, &value_of("--rw-device")?, false)?,
                    "--ro-share" => add_share(&mut cfg, &value_of("--ro-share")?, true)?,
                    "--rw-share" => add_share(&mut cfg, &value_of("--rw-share")?, false)?,
                    "--env" => {
                        let v = value_of("--env")?;
                        let Some((k, val)) = v.split_once('=') else {
                            return Err(format!("--env `{v}` is not K=V"));
                        };
                        cfg.env.push((k.to_owned(), val.to_owned()));
                    }
                    "--krun-overlay" => add_overlay(&mut cfg, &value_of("--krun-overlay")?)?,
                    "--manifest" => cfg.manifest = Some(PathBuf::from(value_of("--manifest")?)),
                    "--network" => match value_of("--network")?.as_str() {
                        "shared" => cfg.network = true,
                        "none" => cfg.network = false,
                        other => return Err(format!("--network `{other}` is not shared|none")),
                    },
                    "--scratch" => cfg.scratch = Some(PathBuf::from(value_of("--scratch")?)),
                    "--chdir" => cfg.workdir = PathBuf::from(value_of("--chdir")?),
                    other => return Err(format!("unknown argument `{other}`")),
                }
            }
        }
    }
    if cfg.rootfs.as_os_str().is_empty() {
        return Err("no --rootfs".to_owned());
    }
    if cfg.kernel.as_os_str().is_empty() {
        return Err("no --kernel".to_owned());
    }
    if payload.is_empty() {
        return Err("no payload after `--`".to_owned());
    }
    cfg.payload = payload;
    Ok(cfg)
}

// The base64 of the overlay values (std RFC 4648, with padding —
// the krun-rs encoding verbatim, so the spec builder's records
// decode identically).
const B64: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn b64(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    for chunk in data.chunks(3) {
        let b = [
            chunk[0],
            chunk.get(1).copied().unwrap_or(0),
            chunk.get(2).copied().unwrap_or(0),
        ];
        out.push(B64[(b[0] >> 2) as usize] as char);
        out.push(B64[(((b[0] & 0x03) << 4) | (b[1] >> 4)) as usize] as char);
        out.push(if chunk.len() > 1 {
            B64[(((b[1] & 0x0f) << 2) | (b[2] >> 6)) as usize] as char
        } else {
            '='
        });
        out.push(if chunk.len() > 2 {
            B64[(b[2] & 0x3f) as usize] as char
        } else {
            '='
        });
    }
    out
}

fn unb64(text: &str) -> Result<Vec<u8>, String> {
    let mut vals = Vec::with_capacity(text.len());
    for (i, c) in text.chars().enumerate() {
        if c == '=' {
            break;
        }
        let v = B64
            .iter()
            .position(|&b| b as char == c)
            .ok_or_else(|| format!("base64 byte {i} (`{c}`) is not valid"))?;
        vals.push(v as u8);
    }
    let mut out = Vec::with_capacity(vals.len() * 3 / 4 + 3);
    for chunk in vals.chunks(4) {
        match chunk.len() {
            4 => {
                out.push((chunk[0] << 2) | (chunk[1] >> 4));
                out.push((chunk[1] << 4) | (chunk[2] >> 2));
                out.push((chunk[2] << 6) | chunk[3]);
            }
            3 => {
                out.push((chunk[0] << 2) | (chunk[1] >> 4));
                out.push((chunk[1] << 4) | (chunk[2] >> 2));
            }
            2 => {
                out.push((chunk[0] << 2) | (chunk[1] >> 4));
            }
            _ => return Err("base64 tail is too short".to_owned()),
        }
    }
    Ok(out)
}

/// The manifest records (env/argv/share/chdir, tab-separated,
/// base64 values) — the krun launcher's `manifest_text` plus the
/// one record the qemu bridge adds: the payload argv. The stock
/// kernel's `--` split is LOSSY (init/main.c next_arg
/// whitespace-splits, an `=`-token lands in INIT'S ENVP via
/// repair_env_string, MAX_INIT_ARGS caps the vector at 31), so
/// the cmdline carries NO payload whenever a manifest exists —
/// the argv rides the manifest losslessly instead (bd
/// myconfig-hq7k). One `argv <index> <base64>` record per token,
/// indices 0.. ascending; the init validates the running count
/// against each index, so a truncated or reordered manifest is a
/// diagnosis, never a mangled payload.
fn manifest_text(cfg: &Config) -> String {
    let mut lines = String::new();
    for (k, v) in &cfg.env {
        lines.push_str(&format!("env\t{k}\t{}\n", b64(v.as_bytes())));
    }
    for (i, arg) in cfg.payload.iter().enumerate() {
        lines.push_str(&format!("argv\t{i}\t{}\n", b64(arg.as_bytes())));
    }
    for s in &cfg.shares {
        let mode = cfg
            .devices
            .iter()
            .find(|d| d.tag == s.tag)
            .map(|d| if d.read_only { "ro" } else { "rw" })
            .unwrap_or("rw");
        lines.push_str(&format!(
            "share\t{}\t{}\t{}\t{}\n",
            s.tag, s.slot, s.dest, mode
        ));
    }
    if cfg.workdir.as_os_str() != "/" {
        lines.push_str(&format!(
            "chdir\t{}\n",
            b64(&cfg.workdir.to_string_lossy().into_owned().into_bytes())
        ));
    }
    lines
}

// ---------------------------------------------------------------------------
// The argv synthesis — PURE functions over the config, unit-tested
// below. No environment reads, no fs touches.
// ---------------------------------------------------------------------------

/// One virtiofsd per device, root included: `--sandbox none` (the
/// builders.nix chain — rootless, the daemon re-exports only what
/// the invoking user can already reach), `--cache=auto`, and
/// `--readonly` for ro devices (the SERVER enforces the mode, not
/// the guest kernel — the krun root flag's own semantics).
fn virtiofsd_argv(bin: &Path, socket: &Path, device: &Device) -> Vec<String> {
    let mut argv = vec![
        bin.to_string_lossy().into_owned(),
        format!("--socket-path={}", socket.display()),
        format!("--shared-dir={}", device.host_dir.display()),
        "--sandbox=none".to_owned(),
        "--cache=auto".to_owned(),
    ];
    if device.read_only {
        argv.push("--readonly".to_owned());
    }
    argv
}

/// The guest kernel cmdline: `reboot=t panic=-1` (PID-1 exit
/// panics; -1 + `-no-reboot` makes qemu EXIT on it — the teardown
/// path), the manifest pointer (`mysbx.manifest=TAG:SLOT` — the
/// init's cmdline fallback reads it when its environment carries
/// no pointer, the backend-agnostic bridge), and — ONLY on a
/// manifest-less debug run — the payload argv behind ` -- `.
/// With a manifest the cmdline carries NO payload at all: the
/// kernel's `--` split is lossy (whitespace-split, `=`-tokens to
/// envp, MAX_INIT_ARGS=31), so the argv rides the manifest's
/// records losslessly instead (bd myconfig-hq7k) and the kernel
/// execs the init with no args beyond init= itself.
fn kernel_cmdline(cfg: &Config) -> String {
    // root=/dev/root: the kernel mounts the virtiofs tag FS_ROOT_TAG
    // ("/dev/root") as the root fs itself — libkrun's own contract,
    // which the SHARED init relies on (it never mounts the root).
    // rootwait: the virtiofs device appears with the vhost-user
    // daemon handshake, which can land after the kernel's device
    // probe — never fail the boot over a race the wait solves.
    let mut params = String::from("root=/dev/root rootfstype=virtiofs rootwait reboot=t panic=-1");
    if let Some(manifest) = &cfg.manifest {
        // The pointer's TAG:SLOT spelling matches
        // MYSBX_KRUN_MANIFEST's.
        let tag = "stage-ro";
        let _ = manifest; // the file is host-side; the pointer is fixed
        params.push_str(&format!(" mysbx.manifest={tag}:{MANIFEST_SLOT}"));
    }
    if cfg.exit_channel {
        // The init arms its side of the virtio-serial port by this
        // param alone (the env route is libkrun's; the cmdline is
        // qemu's).
        params.push_str(&format!(" mysbx.exit-channel={EXIT_PORT}"));
    }
    if cfg.verbose {
        params.push_str(" mysbx.trace=1");
    }
    // init= names the guest entry (the baked /bin/mysbx-init — the
    // SAME init the krun rootfs serves both backends); without a
    // flag the kernel's default init applies (a debug run against a
    // plain rootfs).
    if let Some(init) = &cfg.init {
        params.push_str(&format!(" init={init}"));
    }
    if cfg.manifest.is_some() {
        // The payload rides the manifest's argv records (bd
        // myconfig-hq7k) — the cmdline stays payload-free, byte-safe
        // against any argument spelling.
        return params;
    }
    // The manifest-less debug run keeps the kernel's own argv
    // route: the `--` split hands the rest to init as $1.. — the
    // lossy limits (MAX_INIT_ARGS, the `=` envp theft) are
    // documented and accepted for a debug path, never the
    // builder's route.
    format!("{params} -- {}", cfg.payload.join(" "))
}

/// The qemu argv (headless microVM, the microvm.nix proven shape):
/// `-M microvm,accel=kvm:tcg` (KVM first, TCG the fallback —
/// D9: boots without /dev/kvm, a diagnosis not a supported mode),
/// one `-chardev socket` + `-device vhost-user-fs-pci` per device,
/// the memfd-shared memory backend vhost-user-fs needs, SLiRP or
/// no netdev, the exit-channel virtserialport, the scratch
/// virtio-blk drive, `-no-reboot` so a panicking guest EXITS
/// instead of hanging.
fn qemu_argv(
    bin: &Path,
    cfg: &Config,
    socket_of: &dyn Fn(&str) -> PathBuf,
    exit_socket: Option<&Path>,
) -> Vec<String> {
    let mut argv = vec![
        bin.to_string_lossy().into_owned(),
        "-name".to_owned(),
        "mysbx".to_owned(),
        "-M".to_owned(),
        "microvm,accel=kvm:tcg,pit=off,pic=off,pcie=on,acpi=on,rtc=on".to_owned(),
        "-m".to_owned(),
        format!("{}M", cfg.ram_mib),
        "-smp".to_owned(),
        cfg.cpus.to_string(),
        "-nodefaults".to_owned(),
        "-no-user-config".to_owned(),
        "-no-reboot".to_owned(),
        "-kernel".to_owned(),
        cfg.kernel.to_string_lossy().into_owned(),
        "-append".to_owned(),
        kernel_cmdline(cfg),
    ];
    // The root fs device (the same chardev/device pair shape as
    // every share — qemu has no in-process virtiofs).
    let root_socket = socket_of(FS_ROOT_TAG);
    argv.push("-chardev".to_owned());
    argv.push(format!("socket,id=fsroot,path={}", root_socket.display()));
    argv.push("-device".to_owned());
    argv.push("vhost-user-fs-pci,chardev=fsroot,tag=/dev/root".to_owned());
    // Every staged device.
    for (i, device) in cfg.devices.iter().enumerate() {
        let socket = socket_of(&device.tag);
        argv.push("-chardev".to_owned());
        argv.push(format!("socket,id=fs{i},path={}", socket.display()));
        argv.push("-device".to_owned());
        argv.push(format!(
            "vhost-user-fs-pci,chardev=fs{i},tag={}",
            device.tag
        ));
    }
    // The shared-memory backend vhost-user-fs requires (the
    // microvm.nix pair: -object memory-backend + -numa).
    argv.push("-object".to_owned());
    argv.push(format!(
        "memory-backend-memfd,id=mem,size={}M,share=on",
        cfg.ram_mib
    ));
    argv.push("-numa".to_owned());
    argv.push("node,memdev=mem".to_owned());
    // The network (bd myconfig-dak.6's spelling, the qemu tier's
    // model): SLiRP user mode when shared, NO netdev at all when
    // `none` (bwrap additionally unshare-nets the chain — the
    // shared arm of the run pipeline).
    if cfg.network {
        argv.push("-netdev".to_owned());
        argv.push("user,id=net0".to_owned());
        argv.push("-device".to_owned());
        argv.push("virtio-net-pci,netdev=net0".to_owned());
    }
    // The scratch disk (bd myconfig-dak.7): the per-run sparse raw
    // file as a real virtio-blk drive, the same block id the init
    // finds it by.
    if let Some(scratch) = &cfg.scratch {
        argv.push("-drive".to_owned());
        argv.push(format!(
            "id={SCRATCH_DRIVE_ID},format=raw,file={},if=none",
            scratch.display()
        ));
        argv.push("-device".to_owned());
        argv.push(format!("virtio-blk-pci,drive={SCRATCH_DRIVE_ID}"));
    }
    // The exit channel (D9): the virtio-serial port the guest init
    // writes the payload's exit code to. The launcher connects to
    // the socket AFTER qemu creates it (server=on,wait=off) and
    // reads until EOF.
    if let Some(sock) = exit_socket {
        argv.push("-device".to_owned());
        argv.push("virtio-serial-pci".to_owned());
        argv.push("-chardev".to_owned());
        argv.push(format!(
            "socket,id={EXIT_CHARDEV},path={},server=on,wait=off",
            sock.display()
        ));
        argv.push("-device".to_owned());
        argv.push(format!(
            "virtserialport,chardev={EXIT_CHARDEV},name={EXIT_PORT}"
        ));
    }
    argv.push("-nographic".to_owned());
    argv
}

// ---------------------------------------------------------------------------
// The run path: pins, daemons, qemu, teardown.
// ---------------------------------------------------------------------------

/// A PINNED binary path (no PATH fallback — the run pipeline's own
/// rule): the wrapper sets it; a missing pin is a 125, never a
/// silent guess off the PATH.
fn pinned_bin(var: &str, what: &str) -> PathBuf {
    match std::env::var_os(var) {
        Some(path) if !path.is_empty() => PathBuf::from(path),
        _ => {
            eprintln!("mysbx-microvm-qemu: no {what} pinned ({var})");
            eprintln!(
                "  the Nix wrapper pins {var} when the host builds the \
                 microvm-qemu backend; an unwrapped build sets none"
            );
            std::process::exit(EXIT_SETUP);
        }
    }
}

fn wait_for_socket(path: &Path, what: &str) {
    // The builders.nix wait loop: qemu exits immediately on a dead
    // chardev socket, so every daemon must be serving first. 100 ×
    // 10 ms, the proven budget.
    for _ in 0..100 {
        if path.exists() {
            return;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    eprintln!(
        "mysbx-microvm-qemu: timed out waiting for the {what} socket {}",
        path.display()
    );
    std::process::exit(EXIT_SETUP);
}

struct Daemons {
    children: Vec<Child>,
    _dir: PathBuf,
}

impl Drop for Daemons {
    // The trap semantics of the builders.nix launcher: the daemons
    // die with the run, whichever way it ends.
    fn drop(&mut self) {
        for child in &mut self.children {
            let _ = child.kill();
            let _ = child.wait();
        }
        let _ = std::fs::remove_dir_all(&self._dir);
    }
}

fn main() {
    let boot_started = Instant::now();
    let cfg = parse_args_result(std::env::args().skip(1)).unwrap_or_else(|e| {
        eprintln!("mysbx-microvm-qemu: {e}");
        usage();
    });

    // The banner (the krun launcher's rule: a hung run must not be
    // a silent one).
    eprintln!(
        "mysbx-microvm-qemu: booting {} cpus {} MiB kernel={} rootfs={}",
        cfg.cpus,
        cfg.ram_mib,
        cfg.kernel.display(),
        cfg.rootfs.display(),
    );
    if cfg.verbose {
        eprintln!("mysbx-microvm-qemu: payload: {:?}", cfg.payload);
        eprintln!("mysbx-microvm-qemu: manifest: {:?}", cfg.manifest);
        eprintln!(
            "mysbx-microvm-qemu: network: {}",
            if cfg.network { "shared" } else { "none" }
        );
        eprintln!("mysbx-microvm-qemu: scratch: {:?}", cfg.scratch);
        eprintln!(
            "mysbx-microvm-qemu: {} env entries, {} overlay files",
            cfg.env.len(),
            cfg.overlays.len()
        );
        for d in &cfg.devices {
            eprintln!(
                "mysbx-microvm-qemu: device {} = {} ({})",
                d.tag,
                d.host_dir.display(),
                if d.read_only { "ro" } else { "rw" }
            );
        }
        for s in &cfg.shares {
            eprintln!("mysbx-microvm-qemu: share {}:{}@{}", s.tag, s.slot, s.dest);
        }
    }

    // The overlay files (bd myconfig-dak.8) have no qemu twin yet —
    // the virtiofs NullFs trick is libkrun's own. The git trust on
    // this backend rides the STAGE device (mysbx-rs writes the
    // trust files into the staged tree before the run): REFUSED
    // here, never a silent run without the trust (the payload's
    // git would hit dubious ownership over virtiofs and refuse
    // every command).
    if !cfg.overlays.is_empty() {
        eprintln!(
            "mysbx-microvm-qemu: --krun-overlay files are not supported on this \
             backend (the in-memory trustfs device is libkrun's own); the trust \
             must ride the staged tree instead"
        );
        std::process::exit(EXIT_SETUP);
    }

    let virtiofsd = pinned_bin("MYSBX_MICROVM_QEMU_VIRTIOFSD", "virtiofsd");
    let qemu = pinned_bin("MYSBX_MICROVM_QEMU_QEMU", "qemu");

    // The per-run dir: every socket lives in it, the sweep is the
    // Daemons drop.
    let dir = runtime_dir();
    if let Err(e) = std::fs::create_dir_all(&dir) {
        eprintln!(
            "mysbx-microvm-qemu: cannot create the runtime dir {}: {e}",
            dir.display()
        );
        std::process::exit(EXIT_SETUP);
    }
    let socket_of = |tag: &str| -> PathBuf {
        // A stable, daemon-safe spelling of the tag (the root's
        // "/dev/root" carries a slash).
        let safe = tag.replace('/', "-");
        dir.join(format!("{safe}.sock"))
    };
    let exit_socket = cfg
        .exit_channel
        .then(|| dir.join(format!("{EXIT_PORT}.sock")));

    // One virtiofsd per device, the root INCLUDED (qemu has no
    // in-process server). The root is read-only by design — a
    // sandbox root the payload could write is no sandbox root.
    let mut daemons = Daemons {
        children: Vec::new(),
        _dir: dir.clone(),
    };
    let mut all_devices = vec![Device {
        tag: FS_ROOT_TAG.to_owned(),
        host_dir: cfg.rootfs.clone(),
        read_only: true,
    }];
    all_devices.extend(cfg.devices.iter().cloned());
    for device in &all_devices {
        let socket = socket_of(&device.tag);
        let argv = virtiofsd_argv(&virtiofsd, &socket, device);
        if cfg.verbose {
            eprintln!("mysbx-microvm-qemu: spawning {:?}", argv);
        }
        match Command::new(&virtiofsd)
            .args(&argv[1..])
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(child) => daemons.children.push(child),
            Err(e) => {
                eprintln!(
                    "mysbx-microvm-qemu: cannot spawn virtiofsd for {} ({}): {e}",
                    device.tag,
                    virtiofsd.display()
                );
                std::process::exit(EXIT_SETUP);
            }
        }
    }
    for device in &all_devices {
        wait_for_socket(
            &socket_of(&device.tag),
            &format!("virtiofsd {}", device.tag),
        );
    }
    eprintln!(
        "mysbx-microvm-qemu: {} virtiofsd daemons ready ({:?} total)",
        all_devices.len(),
        boot_started.elapsed()
    );

    // The manifest: the SAME records, written into the ro stage
    // device's dir at its slot — the guest init reads it from the
    // mounted device. A --manifest without a stage-ro device is a
    // 125 (the init would find no file — the krun launcher's own
    // check).
    if let Some(manifest) = &cfg.manifest {
        if !cfg
            .devices
            .iter()
            .any(|d| d.read_only && d.tag == "stage-ro")
        {
            eprintln!(
                "mysbx-microvm-qemu: --manifest needs a stage-ro --ro-device to reach the guest"
            );
            std::process::exit(EXIT_SETUP);
        }
        if let Err(e) = std::fs::write(manifest, manifest_text(&cfg)) {
            eprintln!(
                "mysbx-microvm-qemu: cannot write the manifest {}: {e}",
                manifest.display()
            );
            std::process::exit(EXIT_SETUP);
        }
    }

    // qemu: stdio wired through (the payload's console is the
    // caller's), the daemons die with this process (the drop runs
    // on every exit path below through the Daemons guard).
    let argv = qemu_argv(&qemu, &cfg, &socket_of, exit_socket.as_deref());
    if cfg.verbose {
        eprintln!("mysbx-microvm-qemu: qemu: {:?}", argv);
    }
    // The exit code (D9): with the channel armed, the reader thread
    // connects to the chardev socket WHILE QEMU LIVES (qemu unlinks
    // the socket on exit — a read after vm-wait would race the
    // unlink and lose the code), buffers the one line the guest
    // init writes, and hands it over once qemu has exited. The
    // connect loop's whole budget runs during the boot, so a slow
    // guest still connects.
    let exit_reader = exit_socket
        .clone()
        .map(|sock| std::thread::spawn(move || read_exit_code(&sock)));

    let vm = match Command::new(&qemu).args(&argv[1..]).status() {
        Ok(status) => status,
        Err(e) => {
            eprintln!(
                "mysbx-microvm-qemu: cannot spawn qemu ({}): {e}",
                qemu.display()
            );
            std::process::exit(EXIT_SETUP);
        }
    };
    eprintln!(
        "mysbx-microvm-qemu: qemu exited with {} after {:?} total",
        vm.code().unwrap_or(-1),
        boot_started.elapsed()
    );

    if let Some(reader) = exit_reader {
        match reader.join().ok().flatten() {
            Some(code) => {
                eprintln!("mysbx-microvm-qemu: payload exited with {code}");
                std::process::exit(code);
            }
            None => {
                eprintln!(
                    "mysbx-microvm-qemu: no exit code on the {EXIT_PORT} channel — \
                     the guest init did not report one (an init without the \
                     channel support, bd myconfig-2l9b, or a boot that died \
                     before the payload ran)"
                );
                std::process::exit(EXIT_SETUP);
            }
        }
    }
    // Without the channel: qemu's own status is the run's (a
    // panicking guest reboots into -no-reboot's exit; the code the
    // payload exited with is lost — the reason the channel is the
    // builder's DEFAULT).
    std::process::exit(vm.code().unwrap_or(EXIT_SETUP));
}

/// Read the exit code the guest init wrote to the channel: one
/// `N\n` line. The connect loop runs while qemu starts up (the
/// caller spawns this BEFORE waiting on qemu); the read then
/// blocks until the guest writes or qemu's exit closes the
/// socket — `None` is a diagnosis, never a guess.
fn read_exit_code(sock: &Path) -> Option<i32> {
    for _ in 0..100 {
        if let Ok(mut stream) = UnixStream::connect(sock) {
            let mut reader = std::io::BufReader::new(&mut stream);
            let mut line = String::new();
            // The read ends at EOF: qemu closes the chardev when it
            // exits, so a code sent before the shutdown arrives as
            // one line, then EOF.
            if reader.read_line(&mut line).is_err() {
                return None;
            }
            let code = line.trim().parse::<i32>().ok()?;
            return Some(code);
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    None
}

// ---------------------------------------------------------------------------
// Unit tests — the pure functions only (the run path needs a real
// qemu/virtiofsd pair and lives in the nix checks, bd
// myconfig-6x9c).
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn test_cfg() -> Config {
        Config {
            cpus: 2,
            ram_mib: 2048,
            rootfs: PathBuf::from("/synth/rootfs"),
            kernel: PathBuf::from("/synth/bzImage"),
            exit_channel: true,
            verbose: false,
            init: None,
            devices: vec![Device {
                tag: "stage-ro".to_owned(),
                host_dir: PathBuf::from("/stage/ro"),
                read_only: true,
            }],
            shares: Vec::new(),
            env: Vec::new(),
            overlays: Vec::new(),
            manifest: Some(PathBuf::from("/stage/ro/manifest")),
            network: true,
            workdir: PathBuf::from("/home/synth/repo"),
            scratch: None,
            payload: vec!["/synth/shell".to_owned()],
        }
    }

    #[test]
    fn the_flags_parse_and_default() {
        let cfg = parse_args_result(
            "--cpus 4 --ram 8192 --rootfs /r --kernel /k --exit-channel \
             --ro-device stage-ro=/s/ro --manifest /s/ro/manifest \
             --env A=b --chdir /w -- /bin/sh -c true"
                .split_whitespace()
                .map(str::to_owned),
        )
        .unwrap();
        assert_eq!(cfg.cpus, 4);
        assert_eq!(cfg.ram_mib, 8192);
        assert_eq!(cfg.rootfs, PathBuf::from("/r"));
        assert_eq!(cfg.kernel, PathBuf::from("/k"));
        assert!(cfg.exit_channel);
        assert_eq!(cfg.devices.len(), 1);
        assert!(cfg.devices[0].read_only);
        assert_eq!(cfg.env, vec![("A".to_owned(), "b".to_owned())]);
        assert_eq!(cfg.workdir, PathBuf::from("/w"));
        assert_eq!(
            cfg.payload,
            vec!["/bin/sh".to_owned(), "-c".to_owned(), "true".to_owned()]
        );
    }

    #[test]
    fn a_missing_kernel_refuses() {
        // The one qemu-specific parse refusal: no --kernel, no run.
        let err = parse_args_result(
            "--rootfs /r -- /bin/true"
                .split_whitespace()
                .map(str::to_owned),
        )
        .unwrap_err();
        assert_eq!(err, "no --kernel");
    }

    #[test]
    fn an_unknown_flag_is_the_usage_wording() {
        let err = parse_args_result(
            "--doctor-probe-invalid"
                .split_whitespace()
                .map(str::to_owned),
        )
        .unwrap_err();
        assert_eq!(err, "unknown argument `--doctor-probe-invalid`");
    }

    #[test]
    fn the_overlay_encoding_roundtrips_with_krun_rss_grammar() {
        let cfg = parse_args_result(
            "--rootfs /r --kernel /k \
             --krun-overlay trustfs@/etc/gitconfig:0644:aGVsbG8= \
             -- /bin/true"
                .split_whitespace()
                .map(str::to_owned),
        )
        .unwrap();
        assert_eq!(cfg.overlays.len(), 1);
        assert_eq!(cfg.overlays[0].fs_tag, "trustfs");
        assert_eq!(cfg.overlays[0].path, "/etc/gitconfig");
        assert_eq!(cfg.overlays[0].mode, 0o644);
        assert_eq!(cfg.overlays[0].data, b"hello");
    }

    #[test]
    fn base64_roundtrips() {
        for data in [&b"hello"[..], b"a", b"ab", b"abc", b"\x00\xff\x10"] {
            assert_eq!(unb64(&b64(data)).unwrap(), data);
        }
    }

    #[test]
    fn the_manifest_records_match_the_krun_format() {
        // env/argv/share/chdir, tab-separated, base64 values — the shared
        // guest init parses ONE format for both backends.
        let mut cfg = test_cfg();
        cfg.env.push(("HOME".to_owned(), "/mysbx-home".to_owned()));
        cfg.payload = vec![
            "/bin/sh".to_owned(),
            "-c".to_owned(),
            "exit 7".to_owned(),
        ];
        cfg.shares.push(Share {
            tag: "stage-ro".to_owned(),
            slot: "store".to_owned(),
            dest: "/nix/store".to_owned(),
        });
        let text = manifest_text(&cfg);
        let lines: Vec<&str> = text.lines().collect();
        assert!(lines[0].starts_with("env\tHOME\t"));
        // The argv records (bd myconfig-hq7k): indexed, base64, in
        // argv order — lossless for spaces, quotes, `=` and empty
        // tokens alike (the lines below assert the spelling of
        // each record, plus one decoded roundtrip for the value
        // the live run must reproduce: `sh -c "exit 7"` → 7).
        assert_eq!(
            lines[1],
            format!("argv\t0\t{}", b64("/bin/sh".as_bytes()))
        );
        assert_eq!(lines[2], format!("argv\t1\t{}", b64("-c".as_bytes())));
        assert_eq!(
            lines[3],
            format!("argv\t2\t{}", b64("exit 7".as_bytes()))
        );
        // An EMPTY argv token survives the roundtrip — the kernel
        // cmdline's `--` split would drop it entirely.
        cfg.payload.push(String::new());
        let text = manifest_text(&cfg);
        assert!(
            text.lines()
                .any(|l| l == format!("argv\t3\t{}", b64("".as_bytes())))
        );
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[5], "share\tstage-ro\tstore\t/nix/store\tro");
        assert!(lines[6].starts_with("chdir\t"));
        // base64 of /home/synth/repo
        assert!(lines[6].contains("L2hvbWUvc3ludGgvcmVwbw=="));
    }

    #[test]
    fn the_kernel_cmdline_carries_the_pointer_not_the_payload() {
        let mut cfg = test_cfg();
        cfg.init = Some("/bin/mysbx-init".to_owned());
        cfg.payload = vec![
            "/bin/sh".to_owned(),
            "-c".to_owned(),
            "exit 7".to_owned(),
        ];
        let cmdline = kernel_cmdline(&cfg);
        assert!(
            cmdline.starts_with("root=/dev/root rootfstype=virtiofs rootwait reboot=t panic=-1")
        );
        assert!(cmdline.contains("init=/bin/mysbx-init"));
        assert!(cmdline.contains("mysbx.manifest=stage-ro:manifest"));
        assert!(cmdline.contains("mysbx.exit-channel=mysbx-exit"));
        // The payload NEVER rides the cmdline of a manifest-bearing
        // run (bd myconfig-hq7k): no `--` split, no argv text — the
        // manifest's records carry it losslessly.
        assert!(!cmdline.contains(" -- "));
        assert!(!cmdline.contains("exit 7"));
        // Without an init flag the kernel's default applies — the
        // debug-run path.
        cfg.init = None;
        assert!(!kernel_cmdline(&cfg).contains("init="));
        // And without a MANIFEST (the manifest-less debug run) the
        // kernel's own argv route remains: the payload behind `-- `
        // — documented as lossy, never the builder's route.
        cfg.manifest = None;
        let cmdline = kernel_cmdline(&cfg);
        assert!(cmdline.ends_with(" -- /bin/sh -c exit 7"));
    }

    #[test]
    fn a_verbose_launch_traces_and_a_plain_one_does_not() {
        let mut cfg = test_cfg();
        cfg.verbose = true;
        assert!(kernel_cmdline(&cfg).contains("mysbx.trace=1"));
        cfg.verbose = false;
        assert!(!kernel_cmdline(&cfg).contains("mysbx.trace=1"));
    }

    #[test]
    fn the_qemu_argv_has_the_proven_shape() {
        let cfg = test_cfg();
        // The caller's socket mapping (the run path maps "/dev/root"
        // to a daemon-safe file name — mirrored here).
        let sockets =
            |tag: &str| PathBuf::from(format!("/run/sock/{}.sock", tag.replace('/', "-")));
        let argv = qemu_argv(
            Path::new("/synth/qemu"),
            &cfg,
            &sockets,
            Some(Path::new("/run/sock/exit.sock")),
        );
        let joined = argv.join("\n");
        // The machine: microvm, KVM first, TCG fallback.
        assert!(joined.contains("-M\nmicrovm,accel=kvm:tcg"));
        // The kernel pair.
        assert!(joined.contains("-kernel\n/synth/bzImage"));
        // The root chardev + device pair.
        assert!(joined.contains("socket,id=fsroot,path=/run/sock/-dev-root.sock"));
        assert!(joined.contains("vhost-user-fs-pci,chardev=fsroot,tag=/dev/root"));
        // The stage device pair.
        assert!(joined.contains("socket,id=fs0,path=/run/sock/stage-ro.sock"));
        assert!(joined.contains("vhost-user-fs-pci,chardev=fs0,tag=stage-ro"));
        // The shared-memory backend pair.
        assert!(joined.contains("memory-backend-memfd,id=mem,size=2048M,share=on"));
        assert!(joined.contains("-numa\nnode,memdev=mem"));
        // SLiRP when shared.
        assert!(joined.contains("user,id=net0"));
        // The exit channel.
        assert!(joined.contains("virtio-serial-pci"));
        assert!(joined.contains("socket,id=exitchan,path=/run/sock/exit.sock,server=on,wait=off"));
        assert!(joined.contains("virtserialport,chardev=exitchan,name=mysbx-exit"));
        // The teardown pair.
        assert!(joined.contains("-no-reboot"));
        // Headless.
        assert!(joined.contains("-nographic"));
    }

    #[test]
    fn network_none_means_no_netdev_at_all() {
        let mut cfg = test_cfg();
        cfg.network = false;
        let sockets =
            |tag: &str| PathBuf::from(format!("/run/sock/{}.sock", tag.replace('/', "-")));
        let argv = qemu_argv(
            Path::new("/synth/qemu"),
            &cfg,
            &sockets,
            Some(Path::new("/run/sock/exit.sock")),
        );
        assert!(!argv.iter().any(|a| a.starts_with("user,id=")));
        assert!(!argv.iter().any(|a| a.contains("virtio-net")));
    }

    #[test]
    fn the_scratch_attaches_as_a_virtio_blk_drive() {
        let mut cfg = test_cfg();
        cfg.scratch = Some(PathBuf::from("/scratch.img"));
        let sockets =
            |tag: &str| PathBuf::from(format!("/run/sock/{}.sock", tag.replace('/', "-")));
        let argv = qemu_argv(
            Path::new("/synth/qemu"),
            &cfg,
            &sockets,
            Some(Path::new("/run/sock/exit.sock")),
        );
        assert!(argv
            .iter()
            .any(|a| a == "id=scratch,format=raw,file=/scratch.img,if=none"));
        assert!(argv.iter().any(|a| a == "virtio-blk-pci,drive=scratch"));
    }

    #[test]
    fn the_virtiofsd_argv_is_the_rootless_proven_line() {
        let argv = virtiofsd_argv(
            Path::new("/synth/virtiofsd"),
            Path::new("/run/sock/ro.sock"),
            &Device {
                tag: "stage-ro".to_owned(),
                host_dir: PathBuf::from("/stage/ro"),
                read_only: true,
            },
        );
        assert_eq!(
            argv,
            vec![
                "/synth/virtiofsd",
                "--socket-path=/run/sock/ro.sock",
                "--shared-dir=/stage/ro",
                "--sandbox=none",
                "--cache=auto",
                "--readonly",
            ]
        );
        // An rw device drops --readonly (the SERVER enforces the
        // mode).
        let argv = virtiofsd_argv(
            Path::new("/synth/virtiofsd"),
            Path::new("/run/sock/rw.sock"),
            &Device {
                tag: "stage-rw".to_owned(),
                host_dir: PathBuf::from("/stage/rw"),
                read_only: false,
            },
        );
        assert!(!argv.iter().any(|a| a == "--readonly"));
    }

    #[test]
    fn a_missing_payload_or_rootfs_refuses() {
        assert!(parse_args_result(
            "--rootfs /r --kernel /k"
                .split_whitespace()
                .map(str::to_owned),
        )
        .is_err());
        assert!(parse_args_result("-- /bin/true".split_whitespace().map(str::to_owned)).is_err());
    }
}
