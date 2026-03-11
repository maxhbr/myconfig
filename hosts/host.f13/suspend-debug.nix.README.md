# Suspend debugging on f13 (Framework 13 AMD AI 300 series)

## Background

This machine only supports s2idle (S0ix / modern standby) -- there is no
traditional S3 deep sleep. S0ix requires cooperation between CPU, GPU,
firmware, and all peripherals. When something prevents proper S0ix entry
the platform can lose power integrity and cold-reboot instead of resuming.

## Root cause: kernel regression in 6.19.3

Journal analysis across 26 boots revealed the actual cause:

| Kernel | Suspend entries | Successful resumes | Status      |
|--------|----------------:|-------------------:|-------------|
| 6.18.9 |             many |               many | **Working** |
| 6.19.2 |             16+ |               15+  | **Working** |
| 6.19.3 |              8+ |                 0  | **Broken**  |
| 6.19.6 |              2+ |                 0  | **Broken**  |

s2idle suspend/resume worked reliably on 6.18.x and 6.19.2 but broke
100% starting with 6.19.3 -- every suspend entry results in a cold
reboot, with zero successful resumes across 8+ boots.

**Current fix**: kernel pinned to `linuxPackages_6_18` (6.18.16).

## Additional finding: amd_pmc.enable_stb=1 is incompatible

The `amd_pmc.enable_stb=1` kernel parameter (sometimes recommended for
older AMD platforms) causes `SMU cmd failed. err: 0xff` during
`amd_stb_s2d_init` on Strix Point, making the entire `amd_pmc` driver
fail to probe (error -12). Without `amd_pmc`, S0ix is completely
unmanaged. **Do not enable this on Strix Point.**

## What was changed

1. **Diagnostics** (`suspend-debug.nix`) -- `suspend-debug` script and a
   systemd service that logs S0ix residency stats around each suspend cycle.
2. **XHCI wakeup disabled** (`hardware.framework.do-not-wake-on-input.nix`)
   -- all four USB host controllers (XHC0/1/3/4) are toggled off as ACPI
   wake sources at boot to prevent spurious wakes.
3. **Deduplicated kernel params** -- `amdgpu.dcdebugmask=0x10`,
   `amd_pstate=active`, and `power-profiles-daemon` were set both inline
   and via the nixos-hardware `amd.nix` import. Duplicates removed.
4. **Kernel pinned to 6.18** (`default.nix`) -- last known working series.
5. **Boot specialisations** (`specialisation.nix`):
   - `no-psr-workaround` -- disables `amdgpu.dcdebugmask=0x10` (PSR fix)
   - `kernel-6_19-latest` -- uses `linuxPackages_latest` to test new releases

## Testing plan

After rebuilding and rebooting:

### Step 1 -- Verify suspend works on 6.18

```
sudo suspend-debug          # baseline
systemctl suspend            # suspend
# wake the machine
sudo suspend-debug          # compare S0ix stats
journalctl -u suspend-debug-log -b   # check pre/post logs
```

Suspend should work. If not, the issue is not purely the kernel regression.

### Step 2 -- Periodically test 6.19.x

When nixpkgs updates `linuxPackages_latest` to a new 6.19.x point release
(or 6.20+), reboot and select the **`kernel-6_19-latest`** entry from the
systemd-boot menu. Test suspend:

- If it **works**: the upstream regression is fixed. Update `default.nix`
  to switch back to `linuxPackages_latest`.
- If it **still reboots**: keep the 6.18 pin and check again on the next
  point release.

### Step 3 -- PSR workaround testing (optional)

If display hangs or glitches appear on 6.18, test the
**`no-psr-workaround`** entry. If the display is stable without it, the
PSR workaround may no longer be needed on this kernel.

### Long-term monitoring

The `suspend-debug-log` service automatically logs S0ix stats around each
suspend cycle:

```
journalctl -u suspend-debug-log --no-pager
```

Look for entries where the S0ix count does **not** increase across a
suspend cycle -- those indicate failed S0ix entry.

## TODO

- [ ] File upstream kernel bug report for the 6.19.3 s2idle regression
      on AMD Strix Point (Framework 13 AMD AI 300 series)
- [ ] Switch back to `linuxPackages_latest` once the regression is fixed
- [ ] Remove `no-psr-workaround` specialisation if no longer needed
