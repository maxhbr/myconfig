{ combinators, helpers, ... }:
let
  inherit (combinators) compose ro-bind;
in
{
  sig = "String -> Package -> Permission";
  doc = ''
    Bind mounts the passed derivation at a specified location.

    Example:
    ```nix
    bind-pkg "/foo" (pkgs.writeText "foo" "bar")
    ```
  '';
  impl =
    path: pkg:
    compose [
      (ro-bind (toString pkg) path)
      (helpers.pushState "additionalRuntimeClosures" pkg)
    ];
}
