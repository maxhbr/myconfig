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
    Exposes your timezone.
  '';
  impl = include-once "time-zone" (add-runtime ''
    if [ -L /etc/localtime ]; then
      RUNTIME_ARGS+=(
        --ro-bind "$(realpath /etc/localtime)" "$(readlink /etc/localtime)"
        --symlink "$(readlink /etc/localtime)" /etc/localtime
      )
    fi
  '');
}
