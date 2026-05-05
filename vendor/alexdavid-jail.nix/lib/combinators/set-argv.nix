{ helpers, ... }:
{
  sig = "[String] -> Permission";
  doc = ''
    Overrides the current argv that is passed to the jailed executable.

    By default argv is set to `noescape "$@"` which will forward whatever
    arguments are provided to the wrapper script at runtime. Calling this
    will override the current value.
  '';
  impl =
    argv: state: state // { argv = builtins.concatStringsSep " " (builtins.map helpers.escape argv); };
}
