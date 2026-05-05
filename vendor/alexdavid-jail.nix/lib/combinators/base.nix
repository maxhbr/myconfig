{
  combinators,
  pkgs,
  helpers,
  ...
}:
let
  inherit (combinators)
    add-path
    add-pkg-deps
    ro-bind
    compose
    fwd-env
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  includedInBasePermissions = true;
  doc = ''
    This sets up a basic minimal set of permissions that a lot of software will
    need.

    This combinator:

      * Sets up a fake proc at `/proc` (Using `--proc /proc` bwrap args)
      * Sets up a fake dev at `/dev` (Using `--dev /dev` bwrap args)
      * Sets up a tmpfs at `/tmp` (Using `--tmpfs /tmp` bwrap args)
      * Sets up a tmpfs at your home (Using `--tmpfs ~` bwrap args)
      * Adds coreutils to the package deps (`add-pkg-deps [ pkgs.coreutils ]`)
      * Binds `/bin/sh` from `pkgs.bash` and adds `/bin` to `$PATH`
        (`ro-bind "''${pkgs.bash}/bin/sh" "/bin/sh"`)
      * Clears the environment except `LANG`, `HOME` and `TERM`

    This is included in the base permissions by default so you shouldn't need
    to include it unless you override base permissions. It is exposed as a
    combinator (like the other default included combinators) so that you can
    use it in a custom
    [`basePermissions`](advanced-configuration.md#basepermissions).
  '';
  impl = compose [
    (unsafe-add-raw-args "--proc /proc")
    (unsafe-add-raw-args "--dev /dev")
    (unsafe-add-raw-args "--tmpfs /tmp")
    (unsafe-add-raw-args "--tmpfs ~")
    (ro-bind "${pkgs.bash}/bin/sh" "/bin/sh")
    (add-path "/bin")
    (helpers.pushState "additionalRuntimeClosures" pkgs.bash)
    (add-pkg-deps [ pkgs.coreutils ])
    (unsafe-add-raw-args "--clearenv")
    (fwd-env "LANG")
    (fwd-env "HOME")
    (fwd-env "TERM")
  ];
}
