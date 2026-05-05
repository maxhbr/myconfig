{ helpers, ... }:
{
  sig = "String -> Permission";
  doc = ''
    Adds arbitrary logic to run when the jail exits.

    This is designed to be an easy way to register cleanup actions for things
    created in [add-runtime](#add-runtime). These scripts run in the same
    scope as `add-runtime` so any shell variables defined there will be in
    scope.

    The cleanup actions may run even if the runtime doesn't— for example if a
    previous runtime exits non-zero the jail will exit prematurely, but the
    cleanup actions will still run.

    Example:
    ```nix
    compose [
      (add-runtime ${"''"}
        TMP_FILE=$(mktemp)
        do-something "$TMP_FILE"
      ${"''"})
      (add-cleanup ${"''"}
        if [ -e "${"''"}''${TMP_FILE-}" ]; then
          rm "$TMP_FILE"
        fi
      ${"''"})
    ]
    ```
  '';
  impl = helpers.pushState "cleanup";
}
