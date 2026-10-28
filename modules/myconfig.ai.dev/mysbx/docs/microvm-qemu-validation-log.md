# mysbx microvm-qemu live-validation log

> **Scope / current status:** The eleven probes and manual interactive
> observations below are historical KVM evidence for the configuration
> tested, not baked-store, scratch-stress, nested-Podman or detach
> acceptance. Complete baked-store live acceptance remains unrecorded.
> Console-size coverage is the initial snapshot only; mid-session resize
> does not propagate. Normal mysbx QEMU runs require KVM, with no supported
> TCG fallback. See the canonical current six-backend
> [`backend-comparison.md`](./backend-comparison.md); the dated entry and
> its timings are preserved, not a current cross-backend benchmark.

The runbook (../nix/microvm-qemu-live-validation.sh, the krun twin's
scripted half of bd myconfig-26z4) appends one block per host run:
host, date, kernel, PASS/FAIL per probe. A FAIL invalidates the
decision row — a bead goes against the decision document, not the
runbook. backends.md D9 is the decision record.

## 2026-10-25, host 'thing' (x86_64, KVM) — the scripted acceptance (bd myconfig-26z4)

`./nix/microvm-qemu-live-validation.sh ~/myconfig/myconfig` from the
`microvm-qemu` branch checkout at the rebuilt switch: all 11 probes
PASS (first fully green run; three earlier findings below).

- PASS 1 boot — one-shot `true` boots the pinned kernel through the
  bwrap → launcher → rootless virtiofsd → qemu chain; the PID-1
  init runs the payload and reports 0 over the exit channel
- PASS 2 guest kernel — `uname -r` inside the VM is 6.12.114, the
  PINNED kernel (`-linux-6.12.114/bzImage`, wrapper pin), not the
  host's
- PASS 3 exit code — `sh -c "exit 7"` surfaces as 7 (manifest argv
  records + virtio-serial channel; bd myconfig-hq7k/dnmk)
- PASS 4 console — the verbose run's output carries the kernel log
  (`-serial mon:stdio` + `console=ttyS0`, bd myconfig-mbib)
- PASS 5 live-repo edit — the workspace bind reaches the host tree
- PASS 6 ro rootfs — a write to `/` is refused (virtiofsd
  `--readonly`, server-side)
- PASS 7 network control — cache.nixos.org reachable through the
  SLiRP NIC (static 10.0.2.15/24, gw 10.0.2.2, DNS 10.0.2.3)
- PASS 8 network=false — both URLs denied (https curl exit 6, http
  1.1.1.1 curl exit 7 — no netdev at all + bwrap `--unshare-net`)
- PASS 9 dry-run surface — `--kernel`/`--exit-channel`/`--rootfs`
  named, no `--krun-overlay` (bd myconfig-govd)
- PASS 10 guest bootstrap — devpts+ptmx, tmpfs /dev/shm, /dev/fd,
  lo administratively UP (flags-word grep, bd myconfig-xxmy);
  probed through `/bin/busybox` — the payload PATH carries no
  iproute2 (the first run's false-fail, bd myconfig-udj3)
- PASS 11 console size — the payload under a pinned 35x226 pty
  reports `WINSIZE-35 226`: the caller's terminal size rode the
  manifest env (`MYSBX_MICROVM_CONSOLE_SIZE`, bd myconfig-a2uq)
  and the init stty'd the console before the payload ran

Interactive (same host, same switch, outside the runbook): fish
renders correctly, herdr attaches and runs (both `--none` one-shot
sessions and `--herdr`) — the two live findings' fixes proven.

### Findings fixed during this validation (each live-found on thing)

1. bd myconfig-dx95 — `mysbx-microvm-qemu-wrapper-test` had never
   been executed; its first build (thing's remote builder) refused
   the dry run: the check spelled it as the BARE form. Fixed
   (`mysbx run --dry-run -- CMD`), now builds green in the sandbox
   AND on thing's remote builder.
2. bd myconfig-gyrq — the init's PID-1 arm panicked at boot: the
   pinned kernel automounts devtmpfs (`CONFIG_DEVTMPFS_MOUNT=y`),
   the init's own mount hit EBUSY and failed hard. Fixed (probe
   `/proc/mounts` first; the krun arm is untouched).
3. bd myconfig-a2uq — interactive payloads saw a 0x0 console (the
   16550 serial port carries no winsize): herdr refused to attach,
   fish rendered blind. Fixed (caller's winsize rides the manifest
   env, the init stty's the console; a mid-session resize still
   cannot propagate — inherent to the device, recorded in D9).
4. bd myconfig-udj3 — probe 10's lo check assumed `ip` on the
   payload PATH; thing's tool set has no iproute2. The probes run
   `/bin/busybox` by absolute path now.

Timings (the launcher's stderr phase lines, first boot): virtiofsd
daemons ready in ~10 ms (3 daemons, socket-per-device), a full
one-shot boot-to-payload ~3 s on KVM (kernel log `[2.3]` root
mounted → `[2.4]` init runs; qemu exit ~3.1 s total). The deferred
fallback (D9) is validated as a working backend on a real KVM host.
