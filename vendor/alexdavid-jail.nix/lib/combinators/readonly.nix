{ combinators, ... }:
let
  inherit (combinators) ro-bind;
in
{
  sig = "String -> Permission";
  doc = ''
    Binds the specified path in the jail as read-only.

    This will error if the file does not exist. If you want to be tolerant of
    missing files, see [try-readonly](#try-readonly).
  '';
  impl = path: ro-bind path path;
}
