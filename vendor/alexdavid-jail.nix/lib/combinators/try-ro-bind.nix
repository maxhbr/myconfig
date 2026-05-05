{ combinators, ... }:
let
  inherit (combinators) escape unsafe-add-raw-args;
in
{
  sig = "String -> String -> Permission";
  doc = ''
    Binds the specified path on the host to a path in the jail as read-only
    if it exists.

    Example:
    ```nix
    # Binds /foo on the host to /bar in the jail if /foo exists
    try-ro-bind "/foo" "/bar"
    ```
  '';
  impl = from: to: unsafe-add-raw-args "--ro-bind-try ${escape from} ${escape to}";
}
