{
  combinators,
  helpers,
  jail,
  lib,
  pkgs,
  ...
}:
let
  inherit (combinators)
    add-cleanup
    add-runtime
    compose
    defer
    include-once
    noescape
    readwrite
    set-env
    unsafe-dbus
    ;
in
{
  sig = "{ own? :: [String], talk? :: [String], see? :: [String], call? :: [String], broadcast? :: [String] } -> Permission";
  doc = ''
    Grants access to dbus, using
    [`xdg-dbus-proxy`](https://github.com/flatpak/xdg-dbus-proxy) (inside a
    jail itself) to filter messages that can be sent/received.

    All of the args in the passed attrset turn into arguments for
    `xdg-dbus-proxy`. They are all optional.

    Example:
    ```
    dbus {
      talk = [
        "ca.desrt.dconf"
        "org.a11y.Bus"
        "org.freedesktop.DBus"
        "org.freedesktop.portal.*"
        "org.gtk.vfs"
        "org.gtk.vfs.*"
      ];
    }
    ```

    Multiple calls to `dbus` will only run a single xdg-dbus-proxy with a
    union of all passed own/talk/see/call/broadcast permissions. This makes
    it possible to write wrapper combinators for specific dbus permissions
    and use them in conjunction with each other.
  '';
  impl =
    {
      own ? [ ],
      talk ? [ ],
      see ? [ ],
      call ? [ ],
      broadcast ? [ ],
    }:
    compose [
      # Just set the options on state so dbus can be called multiple times
      (helpers.pushState "dbusPermissions" {
        inherit
          own
          talk
          see
          call
          broadcast
          ;
      })
      # Then add deferred runtime logic to spin up xdg-dbus-proxy:
      (include-once "dbus" (
        defer (
          state:
          let
            proxy = jail "xdg-dbus-proxy" pkgs.xdg-dbus-proxy [
              unsafe-dbus
              (readwrite (noescape "\"$PROXIED_DBUS_SOCKET_DIR\""))
            ];

            getFlags =
              type:
              lib.pipe state.dbusPermissions [
                (map (p: p.${type}))
                lib.flatten
                lib.unique
                (map (id: "--${type}=${lib.escapeShellArg id}"))
              ];

            args = [
              "--filter"
            ]
            ++ getFlags "own"
            ++ getFlags "talk"
            ++ getFlags "see"
            ++ getFlags "call"
            ++ getFlags "broadcast";
          in
          compose [
            (add-runtime ''
              PROXIED_DBUS_SOCKET_DIR=$(mktemp -d)
              export PROXIED_DBUS_SOCKET_DIR
              PROXIED_DBUS_SOCKET="$PROXIED_DBUS_SOCKET_DIR/socket"
              mkfifo "$PROXIED_DBUS_SOCKET_DIR/ready"
              exec {XDG_DBUS_PROXY_READY_FD}<>"$PROXIED_DBUS_SOCKET_DIR/ready"
              ${lib.getExe proxy} \
                "$DBUS_SESSION_BUS_ADDRESS" \
                "$PROXIED_DBUS_SOCKET" \
                --fd="$XDG_DBUS_PROXY_READY_FD" \
                ${lib.concatStringsSep " " args} \
                &
              PROXY_PID=$!
              IFS= read -rn1 -u "$XDG_DBUS_PROXY_READY_FD"
            '')
            (add-cleanup ''
              kill "$PROXY_PID"
              if [ -e "''${PROXIED_DBUS_SOCKET_DIR-}" ]; then
                rm -rf "$PROXIED_DBUS_SOCKET_DIR"
              fi
            '')
            (readwrite (noescape "\"$PROXIED_DBUS_SOCKET_DIR\""))
            (set-env "DBUS_SESSION_BUS_ADDRESS" (noescape "\"unix:path=$PROXIED_DBUS_SOCKET\""))
          ] state
        )
      ))
    ];
}
