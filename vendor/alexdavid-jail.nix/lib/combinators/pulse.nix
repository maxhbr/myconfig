{ combinators, ... }:
let
  inherit (combinators)
    compose
    fwd-env
    include-once
    try-fwd-env
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes pulseaudio to the jailed application.
  '';
  impl = include-once "pulse" (compose [
    (fwd-env "XDG_RUNTIME_DIR")
    (try-fwd-env "PULSE_SERVER")
    (unsafe-add-raw-args "--bind-try /run/pulse /run/pulse")
    (unsafe-add-raw-args "--bind-try \"$XDG_RUNTIME_DIR/pulse\" \"$XDG_RUNTIME_DIR/pulse\"")
  ]);
}
