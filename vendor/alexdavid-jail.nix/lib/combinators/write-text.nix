{
  combinators,
  lib,
  pkgs,
  ...
}:
let
  inherit (combinators) bind-pkg escape;
in
{
  sig = "String -> String -> Permission";
  doc = ''
    Bind mounts a read-only text file at a path.

    Example:
    ```nix
    # This will create a text file in the jail at `/hello.txt`
    write-text "/hello.txt" "Hello, world!"
    ```
  '';
  impl =
    path: contents:
    bind-pkg path (
      pkgs.writeText "jail-write-text-${lib.strings.sanitizeDerivationName (escape path)}" contents
    );
}
