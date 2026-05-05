{ combinators, lib, ... }:
let
  inherit (combinators)
    add-runtime
    include-once
    ;
in
{
  sig = "String -> String -> Permission";
  doc = ''
    This binds multiple paths as read-only specified by a single runtime
    environment variable.

    The first argument to this combinator is the runtime environment variable
    that contains a list of paths to be bound. The second argument is a
    deliminator to split the paths (Typically either `" "` or `":"`).

    This is useful for variables like `XDG_DATA_DIRS`, `GTK_PATH`,
    `XCURSOR_PATH`, etc.

    Example:
    ```nix
    compose [
      (readonly-paths-from-var "XDG_DATA_DIRS" ":")
      (readonly-paths-from-var "XCURSOR_PATH" " ")
    ]
    ```
  '';
  impl =
    var: separator:
    assert lib.isValidPosixName var;
    include-once "readonly-paths-from-var-${var}" (add-runtime ''
      IFS=${lib.escapeShellArg separator} read -ra DIRS <<< "''${${var}-}"
      for DIR in "''${DIRS[@]}"; do
        if [ -e "$DIR" ]; then
          P="$(realpath "$DIR")"
          RUNTIME_ARGS+=(--ro-bind "$P" "$P")
        fi
      done
    '');
}
