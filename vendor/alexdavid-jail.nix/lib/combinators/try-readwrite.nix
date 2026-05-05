{ combinators, ... }:
let
  inherit (combinators) try-rw-bind;
in
{
  sig = "String -> Permission";
  doc = ''
    Binds the specified path in the jail as read-write if it exists.
  '';
  impl = path: try-rw-bind path path;
}
