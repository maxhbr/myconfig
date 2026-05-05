{ combinators, helpers, ... }:
let
  inherit (combinators) unsafe-add-raw-args;
in
{
  sig = "String -> Permission";
  doc = ''
    Mounts a new tmpfs at the specified location.
  '';
  impl = path: unsafe-add-raw-args "--tmpfs ${helpers.escape path}";
}
