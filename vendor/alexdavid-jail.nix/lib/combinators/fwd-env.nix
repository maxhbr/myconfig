{ combinators, helpers, ... }:
let
  inherit (combinators) set-env;
in
{
  sig = "String -> Permission";
  doc = ''
    Forwards the specified environment variable to the underlying process.

    If the env var is not set when the jailed application is run, it will
    exit non-zero.

    If you want to be tolerant of the environment being unset, use
    [try-fwd-env](#try-fwd-env) instead.
  '';
  impl = name: set-env name (helpers.noescape "\"\$${name}\"");
}
