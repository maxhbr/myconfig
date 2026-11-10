---
name: kanshi-add-layout
description: Capture a host's current display setup and persist it as a kanshi profile. Use when the user wants to add or update a display layout ("add a kanshi layout", "save monitor setup", "dock profile") for one of the NixOS hosts.
---

# Add a kanshi display layout

Capture the current monitor configuration of one host and save it as a
kanshi profile, so kanshi switches back to it automatically whenever those
outputs are connected.

## What to gather

Ask the user for three things. The CLI output must come from **the target
host itself**, inside the Wayland session — run it there (in a terminal or
over ssh) and paste the result:

1. **Host** — one of the NixOS hosts in this repo (`hosts/host.<name>/`).
   Must have `myconfig.desktop.wayland.enable = true`.
2. **Layout name** — short, kebab-case, describes the physical setup
   (`docked`, `undocked`, `desk-home`, `beamer`). It becomes the file name
   and the kanshi profile name.
3. **Output description** — the output of exactly one of, in this order of
   preference:
   - `niri msg --json outputs` (session is niri — the default on these
     hosts)
   - `wlr-randr` (session is labwc, river, or another wlroots compositor)
   - `swaymsg -t get_outputs` (session is sway)

   Accept the pasted text verbatim; do not invent outputs. If the paste is
   missing mode, position, scale or transform for an output you judge
   relevant, ask before guessing.

## Translate to kanshi

Parse the pasted output into one `profile` for `services.kanshi.settings`.
Rules:

- **Criteria**: prefer the manufacturer/model/serial description over the
  connector name. Connector names (`DP-5`, `eDP-1`) are unstable across
  reboots and docking-order changes; descriptions are not. Build it as
  `"<make> <model> <serial>"`, substituting `Unknown` for any missing
  field (e.g. `"Foocorp ASDF 1234"`, `"Foocorp ASDF Unknown"`).
  - From `niri msg --json outputs`: per output, `make`, `model`, `serial`
    (may be null → `Unknown`), current mode via `current_mode` index into
    `modes` (`width`, `height`, `refresh_rate` in millihertz → Hz),
    `logical.x`/`logical.y` → `position`, `logical.scale` → `scale`,
    `logical.transform` (`"normal"` → omit).
  - From `wlr-randr`/`swaymsg`: description, current mode, position and
    scale appear directly in the text.
- Only list outputs that should be part of this layout's identity. Every
  output listed in the profile must be **connected** for the profile to
  match; the panel of a docked laptop can be omitted (stays on) or given
  `enable`/`disable` explicitly. Include `position` for every output —
  relative arrangement is the point of the profile.
- Modes: use the *current* mode from the paste, as
  `<width>x<height>@<rate>Hz`. Round millihertz to the nearest Hz (e.g.
  59997 → 60). Omit `@rate` only if the paste has no rate.

## Write the layout file

Write `hosts/host.<host>/kanshi/<layout>.nix` as a home-manager module that
appends exactly one profile:

```nix
# <layout> layout on <host>, captured with `<cli command>`
{ ... }:
{
  services.kanshi.settings = [
    {
      profile.name = "<layout>";
      profile.outputs = [
        {
          criteria = "<make> <model> <serial>";
          mode = "<WxH>@<rate>Hz";
          position = "X,Y";
          scale = 1.0;
        }
        # one entry per output
      ];
    }
  ];
}
```

The directory is auto-imported (see
`modules/myconfig.desktop.wayland/home-manager.kanshi.nix`); no import list
to update. Every `.nix` file in it is a profile — keep exactly one per
file, named after the layout. If a file for the layout already exists,
overwrite it in place (that is how a layout gets updated).

If the directory `hosts/host.<host>/kanshi/` does not exist yet, create it.
The first layout on a host also needs kanshi enabled — add to the host's
`default.nix`:

```nix
home-manager.sharedModules = [
  { services.kanshi.enable = true; }
];
```

(merge into an existing `home-manager.sharedModules` list if the host has
one; the wayland module only sets a `mkDefault false`).

## Finish

1. `git add` the new/changed files — Nix evaluates the git tree; untracked
   files are invisible.
2. Format: `nix fmt` (the new nix files).
3. Validate the host still evaluates:

   ```bash
   nix eval --raw .#nixosConfigurations.test-<host>.config.system.build.toplevel.drvPath
   ```

4. Report to the user which file was written and that the layout becomes
   active after the next `switch.sh <host>` (or suggest running it).
