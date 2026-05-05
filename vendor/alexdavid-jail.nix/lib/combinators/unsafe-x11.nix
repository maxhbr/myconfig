{ combinators, ... }:
let
  inherit (combinators)
    compose
    fwd-env
    readwrite
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes X11 to the jailed application.

    Note that applications may be able to break out of the jail because X11
    is not designed to be a security boundary.

    For a safer alternative, consider using the [xwayland](#xwayland)
    combinator inside of a wayland compositor.
  '';
  impl = compose [
    (fwd-env "DISPLAY")
    (readwrite "/tmp/.X11-unix")
  ];
}
