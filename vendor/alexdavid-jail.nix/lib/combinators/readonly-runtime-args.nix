{ combinators, ... }:
let
  inherit (combinators)
    add-runtime
    include-once
    ;
in
{
  sig = "Permission";
  doc = ''
    Binds any valid paths passed in as arguments to the jailed program at
    runtime as read-only.
  '';
  impl = include-once "readonly-runtime-args" (add-runtime ''
    for MAYBE_PATH in "$@"; do
      if [ -e "$MAYBE_PATH" ]; then
        P="$(realpath "$MAYBE_PATH")"
        RUNTIME_ARGS+=(--ro-bind "$P" "$P")
      fi
    done
  '');
}
