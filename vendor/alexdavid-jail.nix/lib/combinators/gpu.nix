{ combinators, ... }:
let
  inherit (combinators)
    add-runtime
    compose
    include-once
    noescape
    readonly
    runtime-deep-ro-bind
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes GPUs to jailed application.

    This currently only binds PCI GPUs. If you need to expose GPUs over other
    interfaces, or a GPU in PCI domain other than `0000`, please [open an
    issue](https://todo.sr.ht/~alexdavid/jail.nix).
  '';
  impl = include-once "gpu" (compose [
    (add-runtime ''
      # jail.nix currently only supports binding GPUs in PCI domain 0000, if
      # you need other domains or interfaces, please open an issue!
      DOMAIN=0000
      function bind-gpus {
        for DEV in "$1"/"$DOMAIN":*; do
          if [ -d "$DEV" ]; then
            if [ -e "$DEV/drm" ]; then
              RUNTIME_ARGS+=(--ro-bind "$DEV" "$DEV")
            else
              # This may be a bridge, look for GPUs within:
              bind-gpus "$DEV"
            fi
          fi
        done
      }
      bind-gpus /sys/devices/pci"$DOMAIN":00
    '')
    (readonly "/sys/dev/char")
    (runtime-deep-ro-bind (noescape "/run/opengl-driver"))
    (runtime-deep-ro-bind (noescape "/run/opengl-driver-32"))
    (unsafe-add-raw-args "--dev-bind /dev/dri /dev/dri")
  ]);
}
