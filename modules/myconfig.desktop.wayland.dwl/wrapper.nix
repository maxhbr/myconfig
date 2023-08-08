{ lib, dwl-unwrapped, makeWrapper, symlinkJoin, writeShellScriptBin, conf ? null
, withBaseWrapper ? true, extraSessionCommands ? "", dbus
, withGtkWrapper ? false, wrapGAppsHook, gdk-pixbuf, glib, gtk3
, extraPaths ? [ ], extraOptions ? [ ] # E.g.: [ "--verbose" ]
, enable-xwayland ? true, dbusSupport ? true }:

assert extraSessionCommands != "" -> withBaseWrapper;

with lib;

let
  dwl = dwl-unwrapped.override { inherit conf enable-xwayland; };
  baseWrapper = writeShellScriptBin "dwl" ''
    set -o errexit
    if [ ! "$_DWL_WRAPPER_ALREADY_EXECUTED" ]; then
      export XDG_CURRENT_DESKTOP=dwl
      ${extraSessionCommands}
      export _DWL_WRAPPER_ALREADY_EXECUTED=1
    fi
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${dwl}/bin/dwl "$@"
    else
      exec ${
        if !dbusSupport then "" else "${dbus}/bin/dbus-run-session"
      } ${dwl}/bin/dwl "$@"
    fi
  '';
in symlinkJoin {
  name = "dwl-${dwl.version}";

  paths = (optional withBaseWrapper baseWrapper) ++ [ dwl ] ++ extraPaths;

  strictDeps = false;
  nativeBuildInputs = [ makeWrapper ]
    ++ (optional withGtkWrapper wrapGAppsHook);

  buildInputs = optionals withGtkWrapper [ gdk-pixbuf glib gtk3 ];

  # We want to run wrapProgram manually
  dontWrapGApps = true;

  postBuild = ''
    ${optionalString withGtkWrapper "gappsWrapperArgsHook"}

    mkdir -p "$out/share/wayland-sessions"
    cat <<EOF >"$out/share/wayland-sessions/dwl.desktop"
    [Desktop Entry]
    Name=dwl
    Comment=dwl
    Exec=$out/bin/dwl
    Type=Application
    EOF

    wrapProgram $out/bin/dwl \
      ${optionalString withGtkWrapper ''"''${gappsWrapperArgs[@]}"''} \
      ${
        optionalString (extraOptions != [ ])
        "${concatMapStrings (x: " --add-flags " + x) extraOptions}"
      }
  '';

  passthru = {
    inherit (dwl.passthru) tests;
    providedSessions = [ "dwl" ];
  };

  inherit (dwl) meta man;
}
