{ combinators, ... }:
let
  inherit (combinators) escape unsafe-add-raw-args;
in
{
  experimental = true;
  sig = "[String] -> String -> String -> String -> Permission";
  doc = ''
    Creates an overlay fs mount at a destination.

    The first argument is a list of sources to be composed.

    The second argument is the rw source, a directory to persist writes to.

    The third argument is the workdir, a scratch space the kernel uses for the
    overlayFS (it must be on the same filesystem as the rw source).

    The final argument is the destination in the jail where to mount the overlayfs.

    If multiple sources contain the same files, sources specified later in the
    sources list will shadow earlier ones.

    At least one source must be specified.

    Example:
    ```nix
    overlay-rw ["/source/foo" "/source/bar"] "/tmp/rw" "/tmp/workdir" "/dest"
    ```
  '';
  impl =
    sources: rwSource: workdir: dest:
    assert builtins.length sources > 0;
    unsafe-add-raw-args (
      builtins.concatStringsSep " " (
        map (source: "--overlay-src ${escape source}") sources
        ++ [ "--overlay ${escape rwSource} ${escape workdir} ${escape dest}" ]
      )
    );
}
