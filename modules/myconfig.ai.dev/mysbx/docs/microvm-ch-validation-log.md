# mysbx microvm-ch live-validation log

> **Scope / current status:** The probes below are one live run's
> evidence for the configuration tested. Eg­RESS validation is
> SKIPPED on the recorded host (it is itself a SLiRP guest whose
> interface sits inside 10.0.2.0/24 — the spike's documented SNAT
> collision), so egress through passt is pending a clean-host run.
> Baked-store, scratch-stress, detach and nested-virt=on live runs
> are unrecorded. See the canonical current
> [`backend-comparison.md`](./backend-comparison.md);
> backends.md D11 is the decision record.

The runbook (`../nix/microvm-ch-live-validation.sh`, the
microvm-qemu twin's scripted half of bd myconfig-9wkp.9) prints one
PASS/FAIL per probe. A FAIL invalidates the decision row.
backends.md D11 is the decision record.

## 2026-11-07, host: a microvm-qemu sandbox with nested-virt (x86_64, KVM accels=host), bd myconfig-9wkp.9

`MYSBX=<wrapped> ./nix/microvm-ch-live-validation.sh /tmp/ch-val-repo`
from the `mysbx/cloud-hyporvisor` branch checkout, wrapped mysbx
built from the tree (microvm-ch enabled, the SHARED rootfs and the
PVPANIC_PCI kernel):

- PASS 1 boot — one-shot `true` boots the pinned kernel through the
  bwrap → mysbx-microvm-ch → rootless virtiofsd ×N + passt +
  cloud-hypervisor chain; the shared PID-1 init runs the payload
  from the manifest's argv records; CH exits 0 after ~2.5 s
- PASS 2 guest kernel — `uname -r` inside the VM is 6.12.115, the
  PINNED kernel (`-linux-6.12.115/bzImage`), not the host's
- PASS 3 exit code — payload `sh -c "exit 7"` surfaces as 7
  (manifest argv + the 16550 serial channel, bd myconfig-9wkp.4)
- PASS 3b signal death — a SIGKILL'd payload surfaces as 137
- PASS 4 console — the verbose run carries the kernel log (the
  virtio console hvc0 rides `--console tty`; the log starts at the
  driver's bring-up, so the probe greps the timestamp shape)
- PASS 5 live-repo edit — a `touch` through the workspace bind
  reached the host repo tree
- PASS 6 ro rootfs — a write to / refused (virtiofsd `--readonly`)
- PASS 7 network shared — the passt NIC is up with the PINNED MAC
  (9a:55:9a:55:9a:55), the static SLiRP plan applied (10.0.2.15/24,
  gw 10.0.2.2)
- SKIP 7b egress — this host interface is 10.0.2.15/24 itself (the
  spike's nested-sandbox SNAT collision): passt's SNAT return
  traffic is delivered host-locally. A clean host runs the probe.
- PASS 9 dry-run surface — `--kernel`/`--exit-channel`/`--init`
  (`/bin/mysbx-init`)/`--manifest` present, NO `--krun-overlay`
- PASS 10 guest bootstrap — devpts, /dev/shm, /dev/fd, lo UP (the
  shared PID-1 bootstrap, backend-agnostic)
- PASS 11 nested=off — the default CPU carries NO vmx/svm (D27's
  inversion: CH defaults nested ON, the launcher forces OFF; the
  nested-virt=on half is the qemu runbook's live-accepted contract)

Live findings fixed during the run (each a commit on this branch):

1. the VM arms' bwrap view lacked `/etc/passwd`+`/etc/group` —
   passt drops to nobody at startup and died on the unnamed user db
   (the spike's own finding, now also a bind of the run pipeline)
2. cloud-hypervisor 54's `--disk` spelling is `image_type=`, not
   `format=` (the qemu twin's word) — both disk argv and goldens
   moved
3. the CH trust write into the staged `gittrust-*` slots was
   qemu-only — the govd rule now covers both VM backends
4. the teardown-timeout thread held the launcher's exit for the
   full timeout — a clean 2 s run never returned; the waiter now
   ends the moment the VM exits (a channel's recv_timeout)
