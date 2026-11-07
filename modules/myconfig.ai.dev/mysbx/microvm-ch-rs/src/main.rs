// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! mysbx-microvm-ch — the cloud-hypervisor twin of the
//! mysbx-microvm-qemu launcher (docs/design/backends.md D-entry, bd
//! myconfig-9wkp.6).
//!
//! The argv contract is the qemu twin's verbatim (mysbx-rs renders
//! the SAME staged spec — flags, shares, manifest pointer, `--`
//! payload) with the CH-only differences the spike (bd
//! myconfig-9wkp.1) proved:
//!
//! - `--exit-channel` arms the 16550 exit channel: the guest init
//!   writes `mysbx-exit:N` to /dev/ttyS0 (CH's `--serial file=`
//!   wires it to a file the launcher reads) and tears down with
//!   `poweroff -f` (CH has NO -no-reboot: a PID-1 exit's kernel
//!   panic would HANG the VM, so the qemu twin's panic=-1 route is
//!   REFUSED here — the init's serial16550 branch, bd
//!   myconfig-9wkp.4).
//! - `--network shared` is one passt `--vhost-user` child mimicking
//!   SLiRP's address plan (10.0.2.15/24, gateway 10.0.2.2, DNS
//!   10.0.2.3) so the shared guest init's static config and the
//!   10.0.2.2 model-endpoint pins carry over unchanged (bd
//!   myconfig-9wkp.5). `--network none` attaches NO net device at
//!   all.
//! - `nested=off` is FORCED on the CPU line unless `--nested-virt`
//!   — CH defaults nested ON (the opposite of qemu), and a plain
//!   run must not silently expose VMX/SVM (D27's contract).
//! - `--memory size=NM,shared=on` — vhost-user-fs needs shared
//!   memory, exactly like the qemu twin's memory-backend-memfd.
//! - `--pvpanic` + `--event-monitor` arm the crash signal; the
//!   launcher ALSO owns a teardown timeout (CH never exits on a
//!   guest panic — it hangs, the spike's Q5), so a hung VM is
//!   killed and reported, never a wedged session.
//!
//! What the launcher DOES (the builders.nix chain, rootless):
//!
//! 1. one virtiofsd per device (the root INCLUDED — CH has no
//!    in-process virtiofs server), sockets in a per-run temp dir
//! 2. one passt child per run (shared networking)
//! 3. wait for every socket
//! 4. write the manifest records into the ro stage dir's slot
//! 5. exec cloud-hypervisor: the kernel + cmdline (manifest
//!    pointer, `mysbx.exit-channel=serial16550`; the payload argv
//!    rides the manifest), one `--fs tag=,socket=` per device, the
//!    scratch and store disks via `--disk`, `--net
//!    vhost_user=on,...,vhost_mode=client,mac=...` when shared,
//!    `--console tty` (the virtio console hvc0), `--serial file=`
//!    (the exit channel), `--pvpanic` + `--event-monitor`
//! 6. wait for CH (the crash watch kills a panicked VM, the
//!    teardown timeout any other hang), read the exit code off the
//!    serial file, kill the daemons, propagate
//!
//! The binaries are PINNED (no PATH fallback — the run path's own
//! rule): MYSBX_MICROVM_CH_VIRTIOFSD, MYSBX_MICROVM_CH_PASST and
//! MYSBX_MICROVM_CH_CH name absolute paths, set by the nix wrapper
//! (nix/microvm-ch-launcher.nix). A missing pin is a 125 with a
//! diagnosis, never a silent PATH guess.
//!
//! Zero dependencies by design: CH, virtiofsd and passt are separate
//! PROCESSES invoked by absolute paths — no FFI, no dlopen. The one
//! unsafe block is the raw setrlimit(2) fd-ceiling raise (the qemu
//! twin's own).

use std::collections::VecDeque;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

// The fd ceiling of the virtiofsd children (the qemu twin's raise,
// bd myconfig-qq3i): virtiofsd keeps one fd per guest-visited
// inode, so a closure-heavy build inside the guest exhausts the
// INHERITED soft nofile long before the guest kernel's own limits.
// The children run under the launcher's limits, so the raise here
// covers them. Zero dependencies by design, so the raw setrlimit(2)
// surface is declared like the krun launcher's: rlimit is
// { cur, max }, both u64.
const RLIMIT_NOFILE: std::ffi::c_int = 7;
const RLIM_INFINITY: u64 = u64::MAX;

#[repr(C)]
struct Rlimit {
    cur: u64,
    max: u64,
}

unsafe extern "C" {
    fn getrlimit(resource: std::ffi::c_int, rlim: *mut Rlimit) -> std::ffi::c_int;
    fn setrlimit(resource: std::ffi::c_int, rlim: *const Rlimit) -> std::ffi::c_int;
}

/// The fd-ceiling raise (the qemu twin's raise_nofile verbatim):
/// hard first (the soft cannot exceed it), then the soft,
/// best-effort — a constrained session keeps its own ceiling and
/// the soft then goes to THAT, every refusal is a WARNING, the run
/// never dies over limits. Returns the (soft, hard) pair that
/// stands after the attempt.
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
                    "mysbx-microvm-ch: WARNING: cannot raise the hard fd limit to infinity (keeping {hard_before})"
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
                    "mysbx-microvm-ch: WARNING: cannot raise the soft fd limit (keeping {soft_before})"
                );
            }
        }
        (rlim.cur, rlim.max)
    }
}

const NOFILE_FLOOR: u64 = 65536;

/// The raise's gate (the qemu twin's ensure_nofile verbatim):
/// refuses below the floor — a low ceiling guarantees the wedge
/// (virtiofsd pins one fd per guest-visited inode, bd
/// myconfig-qq3i), refusing it makes the cause diagnosable at
/// launch instead of mid-session.
fn ensure_nofile(soft: u64, hard: u64) -> bool {
    eprintln!("mysbx-microvm-ch: fd ceiling soft={soft} hard={hard}");
    if soft < NOFILE_FLOOR || hard < NOFILE_FLOOR {
        eprintln!(
            "mysbx-microvm-ch: the fd ceiling after the raise is below {NOFILE_FLOOR} — virtiofsd \
             keeps one fd per guest-visited inode, so a closure-heavy build WILL wedge the \
             guest store on it. Check the session's RLIMIT_NOFILE"
        );
        return false;
    }
    true
}

const EXIT_SETUP: i32 = 125;

/// The virtiofs tag of the root device (the qemu twin's contract —
/// the kernel mounts the tag /dev/root as the root fs itself, which
/// the SHARED init relies on: it never mounts the root).
const FS_ROOT_TAG: &str = "/dev/root";

/// The manifest slot inside the stage-ro device (the qemu twin's
/// fixed pointer).
const MANIFEST_SLOT: &str = "manifest";

/// The exit-channel value the guest init understands (bd
/// myconfig-9wkp.4): the 16550 the launcher reads back through
/// `--serial file=`.
const EXIT_CHANNEL_SERIAL: &str = "serial16550";

/// The marker the guest init writes to the 16550 (one
/// `mysbx-exit:N` line — the qemu twin reads a bare `N`, but the
/// 16550 file can carry anything a guest binary writes to
/// /dev/ttyS0, so the marker makes the line unambiguous).
const EXIT_MARKER: &str = "mysbx-exit:";

/// The CH disk ids of the scratch and store drives (the qemu twin's
/// block ids — the shared init finds them by these names in
/// /sys/block).
const SCRATCH_DRIVE_ID: &str = "scratch";
const STORE_DRIVE_ID: &str = "nixstore";

/// The SLiRP-compatible address plan (bd myconfig-9wkp.5): the
/// exact addresses the qemu tier's -netdev user hands out, so the
/// guest's static NIC config and the 10.0.2.2 model-endpoint pins
/// carry over unchanged.
const NET_ADDR: &str = "10.0.2.15";
const NET_MASK: &str = "24";
const NET_GW: &str = "10.0.2.2";
const NET_DNS: &str = "10.0.2.3";
/// The MAC CH must present on the net device: passt in vhost-user
/// mode frames packets for the MAC its own banner names — the
/// guest's static config never announces one, so the spike proved
/// the pin is load-bearing (frames for any other MAC are dropped).
const NET_MAC: &str = "9a:55:9a:55:9a:55";

/// The teardown timeout of a CH run (the spike's Q5: CH NEVER exits
/// on a guest kernel panic — it hangs until killed, and there is no
/// -no-reboot to turn a panic into an exit). The launcher owns the
/// bound: a VM that neither powers off nor panics within it is
/// killed and reported, never a wedged session. Generous over the
/// longest payload: the timeout is the BACKSTOP, not the norm.
const TEARDOWN_TIMEOUT: Duration = Duration::from_secs(60 * 60 * 24);

/// How often the crash watch re-reads the event monitor for the
/// pvpanic event: the delay between a guest panic and the kill.
const EVENT_POLL: Duration = Duration::from_millis(250);

fn runtime_dir() -> PathBuf {
    std::env::temp_dir().join(format!("mysbx-microvm-ch-{}", std::process::id()))
}

// ---------------------------------------------------------------------------
// The flag grammar: the qemu twin's verbatim (the spec builder, bd
// myconfig-9wkp.7, renders the SAME staged spec for both VM
// backends). The parse is a PURE function over an iterator of
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
    /// The nested-virtualization opt-in (docs/design/config.md D27):
    /// `nested=on` ONLY with the flag — CH defaults it ON, the exact
    /// opposite of qemu, so the launcher must force `nested=off` on
    /// every plain run.
    nested_virt: bool,
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
    store_disk: Option<PathBuf>,
    payload: Vec<String>,
}

fn usage() -> ! {
    eprintln!(
        "usage: mysbx-microvm-ch [--cpus N] [--ram MIB] [--verbose] --rootfs DIR \
         --kernel BZIMAGE [--exit-channel] [--nested-virt] [--init PATH] \
         [--ro-device TAG=DIR] [--rw-device TAG=DIR] \
         [--ro-share TAG:SLOT@DEST] [--rw-share TAG:SLOT@DEST] \
         [--manifest FILE] [--network shared|none] \
         [--scratch FILE] [--store-disk FILE] [--env K=V]... \
         [--krun-overlay TAG@PATH:MODE:B64]... [--chdir DIR] -- CMD [ARGS...]"
    );
    std::process::exit(2);
}

/// Parse `TAG=DIR` (a device flag's value) — the qemu twin's parser
/// verbatim.
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

/// Parse `TAG:SLOT@DEST` (a share flag's value) — the qemu twin's
/// parser verbatim.
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

/// Parse `TAG@PATH:MODE:B64` (an overlay flag's value) — the qemu
/// twin's parser verbatim (refused at run time on this backend).
fn add_overlay(cfg: &mut Config, value: &str) -> Result<(), String> {
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
        nested_virt: false,
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
        store_disk: None,
        payload: Vec::new(),
    };
    // The `--` split first: everything after it is the payload, never
    // a flag — the same rule the qemu twin applies.
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
                    "--nested-virt" => cfg.nested_virt = true,
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
                    "--store-disk" => {
                        cfg.store_disk = Some(PathBuf::from(value_of("--store-disk")?))
                    }
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

// The base64 of the overlay and manifest values (std RFC 4648, with
// padding — the krun-rs encoding verbatim, so the spec builder's
// records decode identically).
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
/// base64 values) — the qemu twin's `manifest_text` VERBATIM: the
/// shared guest init parses ONE format for both VM backends.
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

/// One virtiofsd per device, root included — the qemu twin's argv
/// VERBATIM: `--sandbox none` (the builders.nix chain — rootless,
/// the daemon re-exports only what the invoking user can already
/// reach), `--cache=auto`, and `--readonly` for ro devices (the
/// SERVER enforces the mode, not the guest kernel).
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

/// The passt child of `--network shared` (bd myconfig-9wkp.5): one
/// `--vhost-user` passt per run, mimicking SLiRP's address plan so
/// the guest's static NIC config and the 10.0.2.2 model-endpoint
/// pins carry over unchanged. `--foreground` keeps it a child of
/// THIS process (the Daemons guard kills it with the run);
/// `--ipv4-only` matches the guest's static config (the plan has no
/// v6 addresses). The pinned MAC matches what CH presents on the
/// net device (NET_MAC) — passt frames for it, the spike proved the
/// pin load-bearing.
fn passt_argv(bin: &Path, socket: &Path) -> Vec<String> {
    vec![
        bin.to_string_lossy().into_owned(),
        "--vhost-user".to_owned(),
        format!("--socket={}", socket.display()),
        "--foreground".to_owned(),
        "--ipv4-only".to_owned(),
        format!("--address={NET_ADDR}"),
        format!("--netmask={NET_MASK}"),
        format!("--gateway={NET_GW}"),
        format!("--dns-forward={NET_DNS}"),
        // The MAC the net device presents (passt's own banner
        // value) — see NET_MAC.
        format!("--mac-addr={NET_MAC}"),
    ]
}

/// The guest kernel cmdline: `root=/dev/root rootfstype=virtiofs rw`
/// (the virtiofs root tag the SHARED init relies on; NO rootwait —
/// a virtiofs tag never becomes a block device, the qemu twin's
/// own rule; `rw` because the init must write /etc/resolv.conf into
/// the root's tmpfs-mounted parents — CH's spike used it), the
/// manifest pointer, the serial16550 exit channel, and — ONLY on a
/// manifest-less debug run — the payload argv behind ` -- `.
///
/// NO `reboot=t panic=-1` here: the qemu twin's panic=-1 +
/// -no-reboot teardown is qemu's OWN; CH has no -no-reboot, so
/// panic=-1 would REBOOT and re-run the payload. The CH teardown is
/// the init's `poweroff -f` (the serial16550 branch, bd
/// myconfig-9wkp.4).
///
/// `console=hvc0` routes the kernel console (and the init's step
/// trace, the payload's stdio) to the VIRTIO console — CH's
/// `--console tty` wires it to the caller's stdio. The 16550 stays
/// free for the exit channel alone.
fn kernel_cmdline(cfg: &Config) -> String {
    let mut params = String::from("root=/dev/root rootfstype=virtiofs rw console=hvc0");
    if let Some(manifest) = &cfg.manifest {
        // The pointer's TAG:SLOT spelling matches the qemu twin's
        // fixed pointer.
        let tag = "stage-ro";
        let _ = manifest; // the file is host-side; the pointer is fixed
        params.push_str(&format!(" mysbx.manifest={tag}:{MANIFEST_SLOT}"));
    }
    if cfg.exit_channel {
        // The CH bridge (bd myconfig-9wkp.4): the 16550 exit
        // channel — the init writes mysbx-exit:N to /dev/ttyS0 and
        // powers off.
        params.push_str(&format!(" mysbx.exit-channel={EXIT_CHANNEL_SERIAL}"));
    }
    if cfg.verbose {
        params.push_str(" mysbx.trace=1");
    }
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
    // route — the same documented lossy limits as the qemu twin.
    format!("{params} -- {}", cfg.payload.join(" "))
}

/// The CH argv (the spike's proven shape, bd myconfig-9wkp.1):
/// `nested=off` on every plain run (CH defaults it ON — the exact
/// opposite of qemu, D27's contract), `shared=on` memory
/// (vhost-user-fs needs it, like the qemu twin's memfd backend),
/// one `--fs tag=,socket=` per device, the scratch and store disks
/// via `--disk` (`readonly=on` for the store — the write protection
/// rides the virtio config into the guest kernel, which marks the
/// device ro in /sys/block), `--net
/// vhost_user=on,socket=...,vhost_mode=client,mac=...` when shared
/// (passt is the SERVER of the socket, CH the client — the
/// spike's Q1), NO net flag at all when `none`, `--console tty`
/// (hvc0, the caller's stdio), `--serial file=` (the 16550 exit
/// channel), `--pvpanic` + `--event-monitor` (the crash signal).
fn ch_argv(
    bin: &Path,
    cfg: &Config,
    socket_of: &dyn Fn(&str) -> PathBuf,
    exit_serial: Option<&Path>,
    events_path: Option<&Path>,
) -> Vec<String> {
    let mut argv = vec![
        bin.to_string_lossy().into_owned(),
        // D27 on CH: nested is ON by default — the flag is forced
        // OFF unless the opt-in, the exact inverse of the qemu
        // twin's `-cpu max,vmx=off,svm=off`.
        format!(
            "--cpus=boot={},nested={}",
            cfg.cpus,
            if cfg.nested_virt { "on" } else { "off" }
        ),
        format!("--memory=size={}M,shared=on", cfg.ram_mib),
        format!("--kernel={}", cfg.kernel.display()),
        format!("--cmdline={}", kernel_cmdline(cfg)),
    ];
    // The root fs device plus every staged device, one vhost-user-fs
    // backend each.
    let mut all_devices = vec![Device {
        tag: FS_ROOT_TAG.to_owned(),
        host_dir: cfg.rootfs.clone(),
        read_only: true,
    }];
    all_devices.extend(cfg.devices.iter().cloned());
    for device in &all_devices {
        let socket = socket_of(&device.tag);
        argv.push(format!(
            "--fs=tag={},socket={}",
            device.tag,
            socket.display()
        ));
    }
    // The network (bd myconfig-9wkp.5): passt serves the
    // vhost-user socket, CH connects as the client. No flag at all
    // when `none` — CH attaches no net device.
    if cfg.network {
        let socket = socket_of("passt");
        argv.push(format!(
            "--net=vhost_user=on,socket={},vhost_mode=client,mac={NET_MAC}",
            socket.display()
        ));
    }
    // The scratch disk (bd myconfig-dak.7): the per-run sparse raw
    // file as a real virtio-blk drive, the same id the init finds it
    // by in /sys/block. `image_type=raw` (CH 54's spelling — the
    // qemu twin's `format=raw` is not a CH option).
    if let Some(scratch) = &cfg.scratch {
        argv.push(format!(
            "--disk=id={},path={},image_type=raw",
            SCRATCH_DRIVE_ID,
            scratch.display()
        ));
    }
    // The guest-own nix store disk (docs/design/guest-store.md):
    // the erofs image as a second virtio-blk drive, read-only —
    // `image_type=raw` applies to the FILE (the erofs payload is
    // raw bytes; the guest mounts it as erofs).
    if let Some(disk) = &cfg.store_disk {
        argv.push(format!(
            "--disk=id={},path={},image_type=raw,readonly=on",
            STORE_DRIVE_ID,
            disk.display()
        ));
    }
    // The 16550 exit channel (bd myconfig-9wkp.4): the file the
    // guest init writes mysbx-exit:N to and the launcher reads
    // after the VM powered off.
    if let Some(serial) = exit_serial {
        argv.push(format!("--serial=file={}", serial.display()));
    }
    // The crash signal (the spike's Q5): pvpanic turns a guest
    // panic into an event-monitor line; CH itself NEVER exits on
    // one, so the launcher's teardown timeout is the backstop that
    // owns the kill.
    argv.push("--pvpanic".to_owned());
    if let Some(events) = events_path {
        argv.push(format!("--event-monitor=path={}", events.display()));
    }
    // The virtio console (hvc0): CH wires it to its own stdio —
    // the caller's, with the tty size carried (the spike's Q6).
    argv.push("--console=tty".to_owned());
    argv
}

// ---------------------------------------------------------------------------
// The run path: pins, daemons, CH, teardown.
// ---------------------------------------------------------------------------

/// The nested-virt host probe (the qemu twin's verbatim,
/// docs/design/config.md D27): a `--nested-virt` launch needs an
/// openable /dev/kvm — refuse here, where the cause belongs,
/// instead of dying inside CH with its raw error.
fn nested_virt_probe(enabled: bool) -> Result<(), String> {
    if !enabled {
        return Ok(());
    }
    match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/kvm")
    {
        Ok(_) => Ok(()),
        Err(e) => Err(format!(
            "--nested-virt needs an openable /dev/kvm on the host (docs/design/config.md \
             D27); the launch has no emulation fallback, so the run is refused here: {e}"
        )),
    }
}

/// A PINNED binary path (no PATH fallback — the qemu twin's rule):
/// the wrapper sets it; a missing pin is a 125, never a silent
/// guess off the PATH.
fn pinned_bin(var: &str, what: &str) -> Option<PathBuf> {
    match std::env::var_os(var) {
        Some(path) if !path.is_empty() => Some(PathBuf::from(path)),
        _ => {
            eprintln!("mysbx-microvm-ch: no {what} pinned ({var})");
            eprintln!(
                "  the Nix wrapper pins {var} when the host builds the \
                 microvm-ch backend; an unwrapped build sets none"
            );
            None
        }
    }
}

/// Ok(()) when the socket came up in the 100 × 10 ms budget, Err(())
/// with the diagnosis printed — the qemu twin's budget (CH, like
/// qemu, exits immediately on a dead vhost-user socket).
fn wait_for_socket(path: &Path, what: &str, log: &DaemonLog) -> Result<(), ()> {
    for _ in 0..100 {
        if path.exists() {
            return Ok(());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    eprintln!(
        "mysbx-microvm-ch: timed out waiting for the {what} socket {}",
        path.display()
    );
    for line in log.lock().unwrap_or_else(|e| e.into_inner()).iter() {
        eprintln!("mysbx-microvm-ch: {what}: {line}");
    }
    Err(())
}

/// How many of a daemon's last stderr lines a non-verbose run keeps
/// for the readiness-failure diagnosis.
const DAEMON_LOG_TAIL: usize = 20;

/// The kept tail of one daemon's stderr (see `drain_stderr`).
type DaemonLog = Arc<Mutex<VecDeque<String>>>;

/// Reads a daemon's piped stderr on a thread for the daemon's whole
/// life: an unread pipe fills (64 KiB) and blocks the daemon on its
/// next log write. `--verbose` forwards every line to the launcher's
/// stderr; otherwise the last DAEMON_LOG_TAIL lines are kept for
/// `wait_for_socket` to print when the daemon never serves.
fn drain_stderr(child: &mut Child, what: String, verbose: bool) -> DaemonLog {
    let log = DaemonLog::default();
    let Some(stderr) = child.stderr.take() else {
        return log;
    };
    let kept = Arc::clone(&log);
    std::thread::spawn(move || {
        let mut reader = BufReader::new(stderr);
        let mut buf = Vec::new();
        loop {
            buf.clear();
            match reader.read_until(b'\n', &mut buf) {
                Ok(0) | Err(_) => break,
                Ok(_) => {}
            }
            let line = String::from_utf8_lossy(&buf).trim_end().to_owned();
            if verbose {
                eprintln!("mysbx-microvm-ch: {what}: {line}");
                continue;
            }
            let mut kept = kept.lock().unwrap_or_else(|e| e.into_inner());
            if kept.len() == DAEMON_LOG_TAIL {
                kept.pop_front();
            }
            kept.push_back(line);
        }
    });
    log
}

struct Daemons {
    children: Vec<Child>,
    _dir: PathBuf,
}

impl Drop for Daemons {
    // The qemu twin's trap semantics: the daemons die with the run,
    // whichever way it ends — and passt, virtiofsd and CH itself
    // are ALL in the list (the spike proved virtiofsd re-parents
    // out of a plain SIGTERM'd bwrap, so the guard kills each
    // child directly).
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
        eprintln!("mysbx-microvm-ch: {e}");
        usage();
    });
    // The ONLY process::exit of the run path (bd myconfig-1ct7): it
    // fires AFTER run() returned — after the Daemons guard DROPPED,
    // so the daemon children die and the runtime dir is swept on
    // every path, success and failure alike. Every exit inside
    // run() is a plain `return`, which unwinds through the guard.
    std::process::exit(run(cfg, boot_started));
}

/// The run path (the qemu twin's shape): owns the Daemons guard for
/// its whole scope and RETURNS the exit code instead of exiting.
fn run(cfg: Config, boot_started: Instant) -> i32 {
    eprintln!(
        "mysbx-microvm-ch: booting {} cpus {} MiB kernel={} rootfs={}{}",
        cfg.cpus,
        cfg.ram_mib,
        cfg.kernel.display(),
        cfg.rootfs.display(),
        if cfg.nested_virt {
            " (nested-virt)"
        } else {
            ""
        },
    );
    if cfg.verbose {
        eprintln!("mysbx-microvm-ch: payload: {:?}", cfg.payload);
        eprintln!("mysbx-microvm-ch: manifest: {:?}", cfg.manifest);
        eprintln!(
            "mysbx-microvm-ch: network: {}",
            if cfg.network { "shared" } else { "none" }
        );
        eprintln!("mysbx-microvm-ch: scratch: {:?}", cfg.scratch);
        eprintln!("mysbx-microvm-ch: store-disk: {:?}", cfg.store_disk);
        eprintln!(
            "mysbx-microvm-ch: nested-virt: {}",
            if cfg.nested_virt { "on" } else { "off" }
        );
        eprintln!(
            "mysbx-microvm-ch: {} env entries, {} overlay files",
            cfg.env.len(),
            cfg.overlays.len()
        );
        for d in &cfg.devices {
            eprintln!(
                "mysbx-microvm-ch: device {} = {} ({})",
                d.tag,
                d.host_dir.display(),
                if d.read_only { "ro" } else { "rw" }
            );
        }
        for s in &cfg.shares {
            eprintln!("mysbx-microvm-ch: share {}:{}@{}", s.tag, s.slot, s.dest);
        }
    }

    // Nested virt (docs/design/config.md D27): refuse where the
    // cause belongs.
    if let Err(e) = nested_virt_probe(cfg.nested_virt) {
        eprintln!("mysbx-microvm-ch: {e}");
        return EXIT_SETUP;
    }

    // The overlay files (bd myconfig-dak.8) have no CH twin yet —
    // the virtiofs NullFs trick is libkrun's own. The git trust on
    // this backend rides the STAGE device, exactly like the qemu
    // twin: REFUSED here, never a silent run without the trust.
    if !cfg.overlays.is_empty() {
        eprintln!(
            "mysbx-microvm-ch: --krun-overlay files are not supported on this \
             backend (the in-memory trustfs device is libkrun's own); the trust \
             must ride the staged tree instead"
        );
        return EXIT_SETUP;
    }

    let Some(virtiofsd) = pinned_bin("MYSBX_MICROVM_CH_VIRTIOFSD", "virtiofsd") else {
        return EXIT_SETUP;
    };
    let Some(passt) = pinned_bin("MYSBX_MICROVM_CH_PASST", "passt") else {
        return EXIT_SETUP;
    };
    let Some(ch) = pinned_bin("MYSBX_MICROVM_CH_CH", "cloud-hypervisor") else {
        return EXIT_SETUP;
    };

    // The fd ceiling raise (the qemu twin's twin, bd
    // myconfig-qq3i): the virtiofsd children inherit THIS
    // process's limits, so the raise must stand before the first
    // spawn.
    let (soft, hard) = raise_nofile();
    if !ensure_nofile(soft, hard) {
        return EXIT_SETUP;
    }

    // The per-run dir: every socket and the serial file live in it,
    // the sweep is the Daemons drop.
    let dir = runtime_dir();
    if let Err(e) = std::fs::create_dir_all(&dir) {
        eprintln!(
            "mysbx-microvm-ch: cannot create the runtime dir {}: {e}",
            dir.display()
        );
        return EXIT_SETUP;
    }
    let socket_of = |tag: &str| -> PathBuf {
        // A stable, daemon-safe spelling of the tag (the root's
        // "/dev/root" carries a slash).
        let safe = tag.replace('/', "-");
        dir.join(format!("{safe}.sock"))
    };
    let exit_serial = cfg.exit_channel.then(|| dir.join("exit.serial"));
    let events_path = dir.join("events.log");

    // One virtiofsd per device, the root INCLUDED (CH has no
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
    let mut device_logs = Vec::with_capacity(all_devices.len());
    for device in &all_devices {
        let socket = socket_of(&device.tag);
        let argv = virtiofsd_argv(&virtiofsd, &socket, device);
        if cfg.verbose {
            eprintln!("mysbx-microvm-ch: spawning {:?}", argv);
        }
        match Command::new(&virtiofsd)
            .args(&argv[1..])
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(mut child) => {
                device_logs.push(drain_stderr(
                    &mut child,
                    format!("virtiofsd {}", device.tag),
                    cfg.verbose,
                ));
                daemons.children.push(child);
            }
            Err(e) => {
                eprintln!(
                    "mysbx-microvm-ch: cannot spawn virtiofsd for {} ({}): {e}",
                    device.tag,
                    virtiofsd.display()
                );
                return EXIT_SETUP;
            }
        }
    }
    // The passt child of shared networking (bd myconfig-9wkp.5):
    // spawned BEFORE CH (CH connects to the socket as the client)
    // and killed with the run (the Daemons guard).
    let mut passt_log = DaemonLog::default();
    if cfg.network {
        let socket = socket_of("passt");
        let argv = passt_argv(&passt, &socket);
        if cfg.verbose {
            eprintln!("mysbx-microvm-ch: spawning {:?}", argv);
        }
        match Command::new(&passt)
            .args(&argv[1..])
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(mut child) => {
                passt_log = drain_stderr(&mut child, "passt".to_owned(), cfg.verbose);
                daemons.children.push(child);
            }
            Err(e) => {
                eprintln!(
                    "mysbx-microvm-ch: cannot spawn passt ({}): {e}",
                    passt.display()
                );
                return EXIT_SETUP;
            }
        }
    }
    for (device, log) in all_devices.iter().zip(&device_logs) {
        if wait_for_socket(
            &socket_of(&device.tag),
            &format!("virtiofsd {}", device.tag),
            log,
        )
        .is_err()
        {
            return EXIT_SETUP;
        }
    }
    if cfg.network && wait_for_socket(&socket_of("passt"), "passt", &passt_log).is_err() {
        return EXIT_SETUP;
    }
    eprintln!(
        "mysbx-microvm-ch: daemons ready ({:?} total)",
        boot_started.elapsed()
    );

    // The manifest: the SAME records, written into the ro stage
    // device's dir at its slot — the qemu twin's check (the init
    // would find no file).
    if let Some(manifest) = &cfg.manifest {
        if !cfg
            .devices
            .iter()
            .any(|d| d.read_only && d.tag == "stage-ro")
        {
            eprintln!(
                "mysbx-microvm-ch: --manifest needs a stage-ro --ro-device to reach the guest"
            );
            return EXIT_SETUP;
        }
        if let Err(e) = std::fs::write(manifest, manifest_text(&cfg)) {
            eprintln!(
                "mysbx-microvm-ch: cannot write the manifest {}: {e}",
                manifest.display()
            );
            return EXIT_SETUP;
        }
    }

    // CH: stdio wired through (the virtio console is the caller's),
    // the daemons die with this process (the drop runs on every
    // exit path below through the Daemons guard).
    let argv = ch_argv(
        &ch,
        &cfg,
        &socket_of,
        exit_serial.as_deref(),
        Some(&events_path),
    );
    if cfg.verbose {
        eprintln!("mysbx-microvm-ch: ch: {:?}", argv);
    }
    let mut ch_child = match Command::new(&ch).args(&argv[1..]).spawn() {
        Ok(child) => child,
        Err(e) => {
            eprintln!(
                "mysbx-microvm-ch: cannot spawn cloud-hypervisor ({}): {e}",
                ch.display()
            );
            return EXIT_SETUP;
        }
    };

    // The crash watch (the spike's Q5): CH NEVER exits on a guest
    // panic — it hangs until killed. A watcher thread polls the
    // event monitor for the pvpanic event and kills the hung VM the
    // moment it appears; the teardown timeout is the backstop for a
    // VM that neither powers off nor panics. The channel keeps the
    // join cheap: every wait is bounded by recv_timeout and ends the
    // moment the VM exits. The qemu twin needs no such watch: its
    // -no-reboot turns every panic into an exit.
    let watch_pid = ch_child.id();
    let watch_events = events_path.clone();
    let (done_tx, done_rx) = std::sync::mpsc::channel::<()>();
    let timeout_handle = std::thread::spawn(move || {
        let deadline = Instant::now() + TEARDOWN_TIMEOUT;
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            if left.is_zero() {
                break;
            }
            match done_rx.recv_timeout(EVENT_POLL.min(left)) {
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
                _ => return false, // the VM exited first
            }
            if guest_panicked(&watch_events) {
                eprintln!(
                    "mysbx-microvm-ch: the guest PANICKED (pvpanic on the event monitor) — \
                     killing the hung VM"
                );
                // kill(pid) only fails when the pid is gone — the
                // race window is harmless (the waiter reaped it).
                unsafe {
                    kill(watch_pid, 15);
                }
                return false;
            }
        }
        unsafe {
            kill(watch_pid, 15);
        }
        true
    });

    let vm = match ch_child.wait() {
        Ok(status) => status,
        Err(e) => {
            eprintln!("mysbx-microvm-ch: waiting on cloud-hypervisor failed: {e}");
            let _ = done_tx.send(());
            let _ = timeout_handle.join();
            return EXIT_SETUP;
        }
    };
    let _ = done_tx.send(());
    eprintln!(
        "mysbx-microvm-ch: cloud-hypervisor exited with {} after {:?} total",
        vm.code().unwrap_or(-1),
        boot_started.elapsed()
    );
    let timed_out = timeout_handle.join().unwrap_or(false);

    if timed_out {
        eprintln!(
            "mysbx-microvm-ch: the VM hit the teardown timeout and was killed — \
             a guest that neither powered off nor panicked in time"
        );
        return EXIT_SETUP;
    }

    // The exit code (bd myconfig-9wkp.4): the init wrote
    // `mysbx-exit:N` to the 16550; `--serial file=` captured it.
    // Read AFTER the VM exited (the file is complete then), any
    // where in its lines — the serial carries ONLY what the guest
    // wrote to /dev/ttyS0 (the kernel console is on hvc0).
    if let Some(serial) = &exit_serial {
        match read_exit_code(serial) {
            Some(code) => {
                eprintln!("mysbx-microvm-ch: payload exited with {code}");
                code
            }
            None => {
                // The event monitor says whether the guest
                // PANICKED (pvpanic): a panic is a crash report,
                // never a silent zero.
                if guest_panicked(&events_path) {
                    eprintln!(
                        "mysbx-microvm-ch: the guest PANICKED (pvpanic on the event \
                         monitor) — no exit code; the payload crashed the VM"
                    );
                } else {
                    eprintln!(
                        "mysbx-microvm-ch: no {EXIT_MARKER} line on the 16550 — the \
                         VM exited without the guest init reporting one (an init \
                         without the serial16550 support, or a boot that died before \
                         the payload ran)"
                    );
                }
                EXIT_SETUP
            }
        }
    } else {
        // Without the channel: CH's own status is the run's. A
        // guest poweroff exits CH 0; a killed/crashed VM maps to
        // its signal/exit status. The code the payload exited with
        // is lost — the reason the channel is the builder's DEFAULT.
        vm.code().unwrap_or(EXIT_SETUP)
    }
}

/// Read the exit code the guest init wrote to the 16550: one
/// `mysbx-exit:N` line. The serial FILE is read after the VM exited
/// (no connect race — unlike the qemu twin's chardev socket, a file
/// needs no live reader and cannot be unlinked mid-read). The
/// marker makes the line unambiguous against anything else a guest
/// binary might write to /dev/ttyS0.
fn read_exit_code(serial: &Path) -> Option<i32> {
    let text = std::fs::read_to_string(serial).ok()?;
    for line in text.lines() {
        if let Some(rest) = line.trim().strip_prefix(EXIT_MARKER) {
            if let Ok(code) = rest.trim().parse::<i32>() {
                return Some(code);
            }
        }
    }
    None
}

/// Whether the event monitor (a plain file CH appends to — the
/// spike's finding) recorded a guest panic. CH 54's pvpanic device
/// emits `event!("guest", "panic", "event", "panic")`, serialized
/// with serde_json's `to_string_pretty` (one event spans several
/// lines), so the match runs over the whitespace-free text.
fn guest_panicked(path: &Path) -> bool {
    let text = std::fs::read_to_string(path).unwrap_or_default();
    let compact: String = text.chars().filter(|c| !c.is_whitespace()).collect();
    compact.contains("\"event\":\"panic\"")
}

unsafe extern "C" {
    fn kill(pid: u32, sig: i32) -> std::ffi::c_int;
}

// ---------------------------------------------------------------------------
// Unit tests — the pure functions only (the run path needs a real
// CH/virtiofsd/passt trio and lives in the nix checks, the qemu
// twin's own split).
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
            nested_virt: false,
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
            store_disk: None,
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
        // env/argv/share/chdir, tab-separated, base64 values — the
        // shared guest init parses ONE format for both VM backends;
        // byte-identical to the qemu twin's output.
        let mut cfg = test_cfg();
        cfg.env.push(("HOME".to_owned(), "/mysbx-home".to_owned()));
        cfg.payload = vec!["/bin/sh".to_owned(), "-c".to_owned(), "exit 7".to_owned()];
        cfg.shares.push(Share {
            tag: "stage-ro".to_owned(),
            slot: "store".to_owned(),
            dest: "/nix/store".to_owned(),
        });
        let text = manifest_text(&cfg);

        assert!(text.contains("env\tHOME\tL215c2J4LWhvbWU="));
        // argv records, indices ascending
        assert!(text.contains("argv\t0\tL2Jpbi9zaA=="));
        assert!(text.contains("argv\t1\tLWM="));
        assert!(text.contains("argv\t2\tZXhpdCA3"));
        // the share record with its device-derived mode
        assert!(text.contains("share\tstage-ro\tstore\t/nix/store\tro"));
        // the chdir record
        assert!(text.contains(&format!("chdir\t{}", b64(b"/home/synth/repo"))));
        // order: env before argv before share before chdir
        let env_at = text.find("env\t").unwrap();
        let argv_at = text.find("argv\t").unwrap();
        let share_at = text.find("share\t").unwrap();
        let chdir_at = text.find("chdir\t").unwrap();
        assert!(env_at < argv_at);
        assert!(argv_at < share_at);
        assert!(share_at < chdir_at);
    }

    #[test]
    fn the_kernel_cmdline_carries_the_pointer_not_the_payload() {
        // The manifest pointer and the serial16550 exit channel ride
        // the cmdline; the payload NEVER does (it rides the
        // manifest's argv records, bd myconfig-hq7k). NO panic=-1:
        // that is the qemu twin's -no-reboot teardown; on CH it
        // would reboot and re-run the payload (bd myconfig-9wkp.4).
        let cfg = test_cfg();
        let cmdline = kernel_cmdline(&cfg);
        assert!(cmdline.contains("root=/dev/root rootfstype=virtiofs"));
        assert!(cmdline.contains("console=hvc0"));
        assert!(cmdline.contains("mysbx.manifest=stage-ro:manifest"));
        assert!(cmdline.contains("mysbx.exit-channel=serial16550"));
        assert!(!cmdline.contains("panic="));
        assert!(!cmdline.contains("-- /synth/shell"));

        // The manifest-less debug run keeps the lossy `--` argv
        // route, like the qemu twin.
        let mut debug = test_cfg();
        debug.manifest = None;
        debug.exit_channel = false;
        let cmdline = kernel_cmdline(&debug);
        assert!(cmdline.contains("-- /synth/shell"));
        assert!(!cmdline.contains("mysbx.manifest="));
        assert!(!cmdline.contains("mysbx.exit-channel="));
    }

    #[test]
    fn a_verbose_launch_traces_and_a_plain_one_does_not() {
        let mut cfg = test_cfg();
        cfg.verbose = false;
        assert!(!kernel_cmdline(&cfg).contains("mysbx.trace=1"));
        cfg.verbose = true;
        assert!(kernel_cmdline(&cfg).contains("mysbx.trace=1"));
    }

    #[test]
    fn nested_virt_is_off_unless_the_flag() {
        // D27 on CH: nested is ON by default — the argv must force
        // it OFF on every plain run (the exact inverse of the qemu
        // twin, whose -cpu max,vmx=off,svm=off does the masking).
        let cfg = test_cfg();
        let argv = ch_argv(
            Path::new("/ch"),
            &cfg,
            &|t: &str| PathBuf::from(format!("/run/{t}.sock")),
            None,
            None,
        );
        assert!(argv.iter().any(|a| a.contains("nested=off")));
        assert!(!argv.iter().any(|a| a.contains("nested=on")));

        let mut nested = test_cfg();
        nested.nested_virt = true;
        let argv = ch_argv(
            Path::new("/ch"),
            &nested,
            &|t: &str| PathBuf::from(format!("/run/{t}.sock")),
            None,
            None,
        );
        assert!(argv.iter().any(|a| a.contains("nested=on")));
    }

    #[test]
    fn the_ch_argv_has_the_proven_shape() {
        // The spike's shape (bd myconfig-9wkp.1): shared=on memory,
        // one --fs per device (root INCLUDED, tag=/dev/root), the
        // vhost-user net with the pinned MAC and vhost_mode=client,
        // --console tty, --pvpanic, the serial file, the event
        // monitor.
        let cfg = test_cfg();
        let argv = ch_argv(
            Path::new("/bin/ch"),
            &cfg,
            &|t: &str| PathBuf::from(format!("/run/{}.sock", t.replace('/', "-"))),
            Some(Path::new("/run/exit.serial")),
            Some(Path::new("/run/events.log")),
        );
        assert_eq!(argv[0], "/bin/ch");
        assert!(argv.contains(&"--memory=size=2048M,shared=on".to_owned()));
        assert!(argv.contains(&"--console=tty".to_owned()));
        assert!(argv.contains(&"--pvpanic".to_owned()));
        assert!(argv.contains(&"--event-monitor=path=/run/events.log".to_owned()));
        assert!(argv.contains(&"--serial=file=/run/exit.serial".to_owned()));
        // the root device
        assert!(argv
            .iter()
            .any(|a| a == "--fs=tag=/dev/root,socket=/run/-dev-root.sock"));
        // the staged device
        assert!(argv
            .iter()
            .any(|a| a == "--fs=tag=stage-ro,socket=/run/stage-ro.sock"));
        // the net device, client mode, the pinned MAC
        assert!(argv.iter().any(
            |a| a == "--net=vhost_user=on,socket=/run/passt.sock,vhost_mode=client,mac=9a:55:9a:55:9a:55"
        ));
        // the kernel and its cmdline
        assert!(argv.contains(&"--kernel=/synth/bzImage".to_owned()));
        assert!(argv
            .iter()
            .any(|a| a.starts_with("--cmdline=root=/dev/root")));
    }

    #[test]
    fn network_none_means_no_net_flag_at_all() {
        // `--network none`: no --net in the argv — CH attaches no
        // device; the bwrap wrap of the run pipeline unshare-nets
        // the chain, exactly like the qemu twin's arm.
        let mut cfg = test_cfg();
        cfg.network = false;
        let argv = ch_argv(
            Path::new("/ch"),
            &cfg,
            &|t: &str| PathBuf::from(format!("/run/{t}.sock")),
            None,
            None,
        );
        assert!(!argv.iter().any(|a| a.starts_with("--net=")));
    }

    #[test]
    fn the_store_disk_attaches_as_a_read_only_drive() {
        // The guest-own nix store disk (docs/design/guest-store.md):
        // readonly=on rides the virtio config into the guest kernel
        // — the init tells it from the WRITABLE scratch by exactly
        // that flag.
        let mut cfg = test_cfg();
        cfg.store_disk = Some(PathBuf::from("/store/disk.img"));
        let argv = ch_argv(
            Path::new("/ch"),
            &cfg,
            &|t: &str| PathBuf::from(format!("/run/{t}.sock")),
            None,
            None,
        );
        assert!(argv
            .iter()
            .any(|a| a == "--disk=id=nixstore,path=/store/disk.img,image_type=raw,readonly=on"));
    }

    #[test]
    fn the_scratch_attaches_as_a_drive() {
        let mut cfg = test_cfg();
        cfg.scratch = Some(PathBuf::from("/run/scratch.img"));
        let argv = ch_argv(
            Path::new("/ch"),
            &cfg,
            &|t: &str| PathBuf::from(format!("/run/{t}.sock")),
            None,
            None,
        );
        assert!(argv
            .iter()
            .any(|a| a == "--disk=id=scratch,path=/run/scratch.img,image_type=raw"));
    }

    #[test]
    fn the_virtiofsd_argv_is_the_rootless_proven_line() {
        // The qemu twin's virtiofsd line VERBATIM: --sandbox none,
        // --cache=auto, --readonly for ro devices (the SERVER
        // enforces the mode).
        let argv = virtiofsd_argv(
            Path::new("/bin/virtiofsd"),
            Path::new("/run/fs.sock"),
            &Device {
                tag: "stage-ro".to_owned(),
                host_dir: PathBuf::from("/stage/ro"),
                read_only: true,
            },
        );
        assert_eq!(argv[0], "/bin/virtiofsd");
        assert!(argv.contains(&"--sandbox=none".to_owned()));
        assert!(argv.contains(&"--cache=auto".to_owned()));
        assert!(argv.contains(&"--readonly".to_owned()));
        assert!(argv.iter().any(|a| a == "--shared-dir=/stage/ro"));

        let argv = virtiofsd_argv(
            Path::new("/bin/virtiofsd"),
            Path::new("/run/fs.sock"),
            &Device {
                tag: "stage-rw".to_owned(),
                host_dir: PathBuf::from("/stage/rw"),
                read_only: false,
            },
        );
        assert!(!argv.contains(&"--readonly".to_owned()));
    }

    #[test]
    fn the_passt_argv_mimics_the_slirp_plan() {
        // bd myconfig-9wkp.5: the exact addresses of the qemu
        // tier's -netdev user, so the guest's static config and the
        // 10.0.2.2 model-endpoint pins carry over unchanged; the
        // pinned MAC matches the CH net device (the spike's Q1:
        // passt frames for its own banner MAC, any other is
        // dropped).
        let argv = passt_argv(Path::new("/bin/passt"), Path::new("/run/passt.sock"));
        assert_eq!(argv[0], "/bin/passt");
        assert!(argv.contains(&"--vhost-user".to_owned()));
        assert!(argv.contains(&"--foreground".to_owned()));
        assert!(argv.contains(&"--address=10.0.2.15".to_owned()));
        assert!(argv.contains(&"--netmask=24".to_owned()));
        assert!(argv.contains(&"--gateway=10.0.2.2".to_owned()));
        assert!(argv.contains(&"--dns-forward=10.0.2.3".to_owned()));
        assert!(argv.contains(&"--mac-addr=9a:55:9a:55:9a:55".to_owned()));
        assert!(argv.iter().any(|a| a == "--socket=/run/passt.sock"));
    }

    #[test]
    fn a_missing_payload_or_rootfs_refuses() {
        assert!(parse_args_result(
            "--kernel /k -- /bin/true"
                .split_whitespace()
                .map(str::to_owned),
        )
        .is_err());
        assert!(parse_args_result(
            "--rootfs /r --kernel /k --"
                .split_whitespace()
                .map(str::to_owned),
        )
        .is_err());
    }

    #[test]
    fn the_exit_reader_finds_the_marker_line() {
        // The 16550 file carries whatever a guest binary writes to
        // /dev/ttyS0; only the marker line is the code. Read AFTER
        // the VM exits — a file, not the qemu twin's chardev socket,
        // so there is no connect race to own.
        let dir = std::env::temp_dir().join(format!("mysbx-ch-test-exit-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let serial = dir.join("exit.serial");
        std::fs::write(
            &serial,
            "noise from a stray guest write\nmysbx-exit:7\nmore noise\n",
        )
        .unwrap();
        assert_eq!(read_exit_code(&serial), Some(7));
        // the marker must be at a line start, whole
        std::fs::write(&serial, "xmysbx-exit:9\n").unwrap();
        assert_eq!(read_exit_code(&serial), None);
        // whitespace tolerated
        std::fs::write(&serial, "  mysbx-exit: 12 \n").unwrap();
        assert_eq!(read_exit_code(&serial), Some(12));
        // a missing file is no code, never a panic
        assert_eq!(read_exit_code(&dir.join("nope.serial")), None);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn the_event_reader_reports_panics() {
        // CH 54's pvpanic event, as its event monitor writes it
        // (serde_json's pretty form — one event spans lines).
        let dir = std::env::temp_dir().join(format!("mysbx-ch-test-events-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let events = dir.join("events.log");
        std::fs::write(
            &events,
            "{\n  \"timestamp\": {\n    \"secs\": 0,\n    \"nanos\": 1\n  },\n  \"source\": \"vm\",\n  \"event\": \"booted\",\n  \"properties\": null\n}\n\
             {\n  \"timestamp\": {\n    \"secs\": 3,\n    \"nanos\": 2\n  },\n  \"source\": \"guest\",\n  \"event\": \"panic\",\n  \"properties\": {\n    \"event\": \"panic\"\n  }\n}\n",
        )
        .unwrap();
        assert!(guest_panicked(&events));
        std::fs::write(
            &events,
            "{\n  \"source\": \"vm\",\n  \"event\": \"shutdown\",\n  \"properties\": null\n}\n",
        )
        .unwrap();
        assert!(!guest_panicked(&events));
        // a missing file is no panic, never an error
        assert!(!guest_panicked(&dir.join("nope.log")));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn raising_nofile_lifts_the_soft_limit_to_the_hard_one() {
        // The raise itself is live-probed here (the qemu twin's
        // test): the soft limit must land at the hard one.
        let (soft, hard) = raise_nofile();
        assert!(soft <= hard);
        if hard >= NOFILE_FLOOR {
            assert_eq!(
                soft, hard,
                "the soft limit should be raised to the hard one"
            );
        }
    }

    #[test]
    fn the_nofile_gate_refuses_a_low_ceiling() {
        assert!(!ensure_nofile(1024, 2048));
        assert!(ensure_nofile(NOFILE_FLOOR, NOFILE_FLOOR));
    }
}
