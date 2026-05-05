{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Adds arbitrary logic to run at runtime, before the jail starts.

    You can push additional bubblewrap arguments by appending the bash
    array `$RUNTIME_ARGS`. This allows you to modify the bubblewrap flags
    to be dependent on runtime conditions.

    Note that anything added here is *not* run inside the jail. To run
    arbitrary things at runtime inside the jail see
    [wrap-entry](#wrap-entry).

    Example:
    ```nix
    add-runtime ${"''"}
      # binds /foo only if /bar exists on the host
      if [ -e /bar ]; then
        RUNTIME_ARGS+=(--bind /foo /foo)
      fi
    ${"''"}
    ```

    If you create any resources in add-runtime that you want to automatically
    clean up when the jail exits use [add-cleanup](#add-cleanup).
  '';
  impl = runtime: state: state // { runtime = "${state.runtime}\n${runtime}\n"; };
}
