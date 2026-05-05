{ combinators, ... }:
let
  inherit (combinators) escape unsafe-add-raw-args;
in
{
  sig = "String -> String -> Permission";
  doc = ''
    Binds the specified path on the host to a path in the jail as read-only.

    This will error if the host file does not exist. If you want to be
    tolerant of missing files, see [try-ro-bind](#try-ro-bind).

    Example:
    ```nix
    # Binds /foo on the host to /bar in the jail
    ro-bind "/foo" "/bar"
    ```
  '';
  impl = from: to: unsafe-add-raw-args "--ro-bind ${escape from} ${escape to}";
}
