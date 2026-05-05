{ combinators, ... }:
let
  inherit (combinators) add-runtime include-once;
in
{
  sig = "Permission";
  doc = ''
    Allows access to webcams and other V4L2 video devices at `/dev/video*`.
  '';
  impl = include-once "camera" (add-runtime ''
    for v in /dev/video*; do
      [ -e "$v" ] || continue
      RUNTIME_ARGS+=(--dev-bind "$v" "$v")
    done
  '');
}
