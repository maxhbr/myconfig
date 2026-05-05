{ combinators, ... }:
let
  inherit (combinators) rw-bind;
in
{
  sig = "String -> Permission";
  doc = ''
    Binds the specified path in the jail as read-write.

    This will error if the file does not exist. If you want to be tolerant of
    missing files, see [try-readwrite](#try-readwrite).
  '';
  impl = path: rw-bind path path;
}
