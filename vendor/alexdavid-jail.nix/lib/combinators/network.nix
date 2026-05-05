{ combinators, ... }:
let
  inherit (combinators)
    compose
    escape
    include-once
    runtime-deep-ro-bind
    share-ns
    time-zone
    try-readonly
    unsafe-add-raw-args
    write-text
    ;
in
{
  sig = "Permission";
  doc = ''
    Grants network access to the jail.

    This also exposes everything required to allow TLS connections.

    You can set your desired hostname with [set-hostname](#set-hostname). The
    default is `jail`.
  '';
  impl = include-once "network" (
    state:
    compose [
      time-zone
      (share-ns "net")
      (runtime-deep-ro-bind "/etc/hosts")
      (runtime-deep-ro-bind "/etc/nsswitch.conf")
      (runtime-deep-ro-bind "/etc/resolv.conf")
      (runtime-deep-ro-bind "/etc/ssl")
      (try-readonly "/run/systemd/resolve")
      (write-text "/etc/hostname" "${state.hostname}\n")
      (unsafe-add-raw-args "--hostname ${escape state.hostname}")
    ] state
  );
}
