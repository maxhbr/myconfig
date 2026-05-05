{ combinators, ... }:
let
  inherit (combinators)
    compose
    noescape
    readonly
    set-env
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes D-Bus to the jailed program.

    This does no message filtering so it is marked as unsafe. If you want
    more control over the messages that can be sent/received, consider using
    the [dbus](#dbus) combinator instead.
  '';
  impl = compose [
    (readonly (noescape "\"$XDG_RUNTIME_DIR/bus\""))
    (set-env "DBUS_SESSION_BUS_ADDRESS" (noescape "\"$DBUS_SESSION_BUS_ADDRESS\""))
  ];
}
