{ combinators, ... }:
let
  inherit (combinators)
    add-runtime
    compose
    fwd-env
    noescape
    pipewire
    pulse
    readonly
    readonly-paths-from-var
    runtime-deep-ro-bind
    try-fwd-env
    wayland
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes everything required to get graphical applications to work.

    This composes [pulse](#pulse), [pipewire](#pipewire),
    [wayland](#wayland), and forwards/binds a few other paths to get fonts
    and cursor to render correctly.
  '';
  impl = compose [
    (add-runtime "mkdir -p ~/.config/dconf")
    pulse
    pipewire
    wayland
    (runtime-deep-ro-bind (noescape "/etc/fonts"))
    (readonly (noescape "~/.config/dconf"))
    (fwd-env "XDG_RUNTIME_DIR")
    (fwd-env "XDG_DATA_DIRS")
    (readonly-paths-from-var "XDG_DATA_DIRS" ":")

    # Cursor
    (try-fwd-env "XCURSOR_THEME")
    (try-fwd-env "XCURSOR_PATH")
    (try-fwd-env "XCURSOR_SIZE")
    (readonly-paths-from-var "XCURSOR_PATH" " ")
  ];
}
