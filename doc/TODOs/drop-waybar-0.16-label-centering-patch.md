# Drop the waybar 0.16 label-centering patch

The overlay in `flake.pkgs_overrides.nix` (attribute `waybar`) appends
`modules/myconfig.desktop.wayland/programs.waybar/patches/fix-0.16-label-centering.patch`
to the nixpkgs waybar derivation.

## Why the patch exists

Waybar 0.16.0 (nixpkgs bump that landed with the 2026-11-12 system switch,
commit `7c2496a40f` "flake.lock: Update") changed the widget tree of two
module families:

- `modules/custom.cpp`: `Custom` was re-based from `ALabel` to `AIconLabel`
  (upstream PRs around `0.15.0...0.16.0`, `src/AIconLabel.cpp`). The
  previously-bare `Gtk::Label` is now packed inside a `Gtk::Box` via
  `box_.add(label_)` — i.e. without expand/fill — and the
  `#custom-<name>` widget name/classes moved onto the (natural-width)
  label. Result: the blinking error `✗` of `custom/mbsync-get-status`
  (email pull failure) is left-aligned inside the module and the blink
  background covers only the glyph, not the module width.
- `src/modules/niri/workspace.cpp`: the new per-workspace `Workspace`
  class packs `label_` with `pack_start(label_, false, false, 0)` inside
  a horizontal box inside the button, so the workspace digit hugs the
  start edge of the (rotated) button instead of being centered, as the
  0.15.0 plain-`GtkButton` label was.

The patch changes both packings to expand/fill (and sets
`label_.set_halign(Gtk::ALIGN_CENTER)` for niri workspaces), restoring the
0.15.0 layout. GTK3 CSS cannot express this (no percentage `min-width`,
button child allocation is not CSS-addressable).

## What to do

1. Watch upstream for a fix or report one:
   <https://github.com/Alexays/Waybar/issues> (as of 2026-11-12 no issue
   covers these packing regressions; #5460 covers a related hover-state
   regression of 0.16.0 custom classes).
2. Once a waybar release includes the fix and the pinned nixos-unstable
   input ships it, remove:
   - the `waybar` override block in `flake.pkgs_overrides.nix` (and its
     table entry in the header comment),
   - `modules/myconfig.desktop.wayland/programs.waybar/patches/fix-0.16-label-centering.patch`.
3. Verify on a waybar host (f13, niri session): workspace digits centered
   in the vertical bar; the mbsync `✗` blinking with a full-width
   background when mail sync fails.

## Verification of the regression (for reference)

- system profile ≤851 (2026-11-11) → waybar 0.15.0, styling fine;
  profile 852/853 (2026-11-12) → waybar 0.16.0, styling broken.
- The repo's own waybar CSS/config last changed long before
  (commit `0e9388e434`, unrelated to styling).
