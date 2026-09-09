{
  combinators,
  pkgs,
  ...
}:
let
  inherit (combinators)
    add-runtime
    compose
    ;
in
{
  sig = "Permission";
  doc = ''
    Allows access to a FIDO token like a yubikey inside the sandbox.
    Beware this requires the token to be plugged in on startup.

    An alternative is using the [USB desktop portal](https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.Usb.html) if your program supports it.
    Only tested with a yubikey series 5 if you have another token and experience issues, please [open an issue](https://todo.sr.ht/~alexdavid/jail.nix).
  '';
  impl = compose [
    (add-runtime ''
      HIDRAW_PATH=/sys/class/hidraw
      while read -r P; do
        if [ "$P" = "" ] ; then
          continue
        fi
        HID_DEVICE_NAME=$(echo "$P" | cut - -d '/' -f 5)

        USAGE_PAGE=$(${pkgs.util-linux}/bin/hexdump -C "$P"/device/report_descriptor | head  --lines=1 | cut - -d ' ' -f 3,4,5)
        [ "$USAGE_PAGE" == '06 d0 f1' ] && {
          RUNTIME_ARGS+=(--bind "$P" "$P")
          RUNTIME_ARGS+=(--dev-bind /dev/"$HID_DEVICE_NAME" /dev/"$HID_DEVICE_NAME")
        }
      done <<< "$(${pkgs.findutils}/bin/find "$HIDRAW_PATH" -regex '.*[0-9]+')"
    '')
  ];
}
