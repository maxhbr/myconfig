# Suspend debugging on f13 (Framework 13 AMD AI 300 series)

## Background

This machine only supports s2idle (S0ix / modern standby) -- there is no
traditional S3 deep sleep. S0ix requires cooperation between CPU, GPU,
firmware, and all peripherals. When something prevents proper S0ix entry
the platform can lose power integrity and cold-reboot instead of resuming.

## What was changed

1. **Diagnostics** (`suspend-debug.nix`) -- `suspend-debug` script and a
   systemd service that logs S0ix residency stats around each suspend cycle.
2. **`amd_pmc.enable_stb=1`** -- enables the AMD PMC Smart Trace Buffer so
   the PMC driver initializes properly and provides trace data.
3. **XHCI wakeup disabled** (`hardware.framework.do-not-wake-on-input.nix`)
   -- all four USB host controllers (XHC0/1/3/4) are toggled off as ACPI
   wake sources at boot. They were enabled and could cause spurious wakes
   or prevent S0ix entry.
4. **Deduplicated kernel params** -- `amdgpu.dcdebugmask=0x10`,
   `amd_pstate=active`, and `power-profiles-daemon` were set both inline
   and via the nixos-hardware `amd.nix` import, appearing twice on the
   command line. The inline duplicates are removed.
5. **`no-psr-workaround` specialisation** (`specialisation.nix`) -- a boot
   entry that overrides `amdgpu.dcdebugmask` to `0x0`, disabling the PSR
   workaround that may interfere with s2idle.

## Testing plan

After rebuilding (`nixos-rebuild switch`):

### Step 1 -- Baseline diagnostics

```
sudo suspend-debug
```

Check the output, in particular the **AMD PMC S0ix stats** section. Note the
current entry count and residency values.

### Step 2 -- Test suspend/resume

Suspend the machine (lid close, power button, or `systemctl suspend`) and
wait a few seconds, then wake it.

- If it **resumes normally**: the XHCI wakeup fix (change 3) likely resolved
  the issue. Proceed to step 3 to verify.
- If it **reboots again**: boot the `no-psr-workaround` specialisation from
  the systemd-boot menu and repeat (step 4).

### Step 3 -- Verify S0ix entry after resume

```
sudo suspend-debug
```

Compare the S0ix entry count and residency with the baseline from step 1.
If the entry count increased and residency is non-zero, the platform is
entering S0ix properly.

Also check the pre/post logs:

```
journalctl -u suspend-debug-log -b
```

### Step 4 -- Test without PSR workaround (if still failing)

Reboot and select the **`no-psr-workaround`** entry from the systemd-boot
menu. This disables `amdgpu.dcdebugmask=0x10` (panel self-refresh
workaround). Then repeat the suspend test.

- If suspend works in this specialisation but not the default: the PSR
  workaround interferes with s2idle. Consider removing it upstream or
  filing a bug against amdgpu.
- If suspend still fails: the issue is elsewhere (firmware, other wake
  source, kernel bug). Check `fwupdmgr get-updates` for BIOS updates.

### Step 5 -- Long-term monitoring

The `suspend-debug-log` systemd service automatically logs S0ix stats
before every suspend and after every resume. Review with:

```
journalctl -u suspend-debug-log --no-pager
```

Look for entries where the S0ix count does **not** increase across a suspend
cycle -- those indicate failed S0ix entry.
