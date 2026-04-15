{ combinators, ... }:
let
  inherit (combinators) escape unsafe-add-raw-args;
in
{
  experimental = true;
  sig = "[String] -> String -> Permission";
  doc = ''
    Creates a tempfs overlay mount at a destination.

    The first argument is a list of sources to be composed, the second is the
    destination in the jail where to mount the overlayfs.

    If multiple sources contain the same files, sources specified later in the
    sources list will shadow earlier ones.

    At least one source must be specified.

    Example:
    ```nix
    overlay-tmp ["/source/foo" "/source/bar"] "/dest"
    ```
  '';
  impl =
    sources: dest:
    assert builtins.length sources > 0;
    unsafe-add-raw-args (
      builtins.concatStringsSep " " (
        map (source: "--overlay-src ${escape source}") sources ++ [ "--tmp-overlay ${escape dest}" ]
      )
    );
}
