{ combinators, ... }:
let
  inherit (combinators) try-ro-bind;
in
{
  sig = "String -> Permission";
  doc = ''
    Binds the specified path in the jail as read-only if it exists.
  '';
  impl = path: try-ro-bind path path;
}
