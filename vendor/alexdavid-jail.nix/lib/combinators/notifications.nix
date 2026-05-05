{ combinators, ... }:
let
  inherit (combinators) dbus;
in
{
  sig = "Permission";
  doc = ''
    This adds the [dbus combinator](#dbus) with talk permission to
    `org.freedesktop.Notifications` which allows the jailed software to send
    [desktop
    notifications](https://specifications.freedesktop.org/notification-spec/1.3/).
  '';
  impl = dbus { talk = [ "org.freedesktop.Notifications" ]; };
}
