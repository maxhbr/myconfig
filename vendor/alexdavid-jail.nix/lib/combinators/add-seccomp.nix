{
  combinators,
  helpers,
  ...
}:
let
  inherit (combinators)
    add-runtime
    compose
    defer
    escape
    include-once
    ;
in
{
  sig = "String -> Permission";
  doc = ''
    Adds the cBPF program as a bubblewrap argument.

    Accepts a path or a derivation that resolves to either a compiled cBPF program
    or an executable that writes the program to stdout. In the latter case,
    the executable will be run at each invocation of the wrapped binary at runtime.

    This combinator can be composed according to the same rules as
    bubblewrap's `--add-seccomp-fd` option.

    See BWRAP(1) for more information.
  '';
  impl =
    path:
    compose [
      (helpers.pushState "seccompPermissions" (escape path))
      (include-once "seccomp" (
        defer (
          state:
          add-runtime ''
            declare -a SECCOMP_FILTERS=(${toString (state.seccompPermissions)})
            for FILTER in "''${SECCOMP_FILTERS[@]}"; do
              if [[ -x "$FILTER" ]]; then
                exec {FILTER_FD}< <("$FILTER")
              else
                exec {FILTER_FD}<"$FILTER"
              fi
              RUNTIME_ARGS+=(--add-seccomp-fd "$FILTER_FD")
            done
          '' state
        )
      ))
    ];
}
