{
  combinators,
  lib,
  pkgs,
  ...
}:
let
  inherit (combinators)
    compose
    include-once
    set-env
    wayland
    wrap-entry
    ;
in
{
  sig = "Permission";
  doc = ''
    Safely allow X11 apps to render to a wayland compositor.

    This combinator runs
    [xwayland-satellite](https://github.com/Supreeeme/xwayland-satellite)
    *inside the jail* and only exposes [wayland combinator](#wayland).

    This has the advantage of not allowing multiple jailed X11 applicaitons
    to see each other since each jailed applicaiton gets its own
    xwayland-satelite server.

    However, doing it this way does mean that every jailed applicaiton you
    run with this combinator will spin up its own personal xwayland-satelite
    server, which will consume more resources than having a global one.
  '';
  impl = include-once "xwayland" (compose [
    wayland
    (set-env "DISPLAY" ":42")
    (wrap-entry (entry: ''
      ${lib.getExe pkgs.xwayland-satellite} :42 &
      ${entry}
    ''))
  ]);
}
