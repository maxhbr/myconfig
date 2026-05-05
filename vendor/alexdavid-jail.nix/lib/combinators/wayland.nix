{ combinators, ... }:
let
  inherit (combinators)
    compose
    fwd-env
    noescape
    readonly
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes your wayland compositor to the jail.
  '';
  impl = compose [
    (fwd-env "WAYLAND_DISPLAY")
    (fwd-env "XDG_RUNTIME_DIR")
    (fwd-env "XDG_SESSION_TYPE")
    (readonly (noescape "\"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\""))
  ];
}
