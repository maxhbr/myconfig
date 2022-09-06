{ lib
, qtile
, makeWrapper, symlinkJoin, writeShellScriptBin
, withBaseWrapper ? true, extraSessionCommands ? "", dbus
, withGtkWrapper ? false, wrapGAppsHook, gdk-pixbuf, glib, gtk3
, extraPaths ? []
, extraOptions ? [] # E.g.: [ "--verbose" ]
, xwaylandSupport ? true
, dbusSupport ? true
}:

assert extraSessionCommands != "" -> withBaseWrapper;

with lib;

let
  baseWrapper = writeShellScriptBin "qtile" ''
     set -o errexit
     if [ ! "$_qtile_WRAPPER_ALREADY_EXECUTED" ]; then
       export XDG_CURRENT_DESKTOP=qtile
       ${extraSessionCommands}
       export _qtile_WRAPPER_ALREADY_EXECUTED=1
     fi
     if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
       export DBUS_SESSION_BUS_ADDRESS
       exec ${qtile}/bin/qtile "$@"
     else
       exec ${if !dbusSupport then "" else "${dbus}/bin/dbus-run-session"} ${qtile}/bin/qtile "$@"
     fi
   '';
in symlinkJoin {
  name = "qtile-${qtile.version}";

  paths = (optional withBaseWrapper baseWrapper)
    ++ extraPaths;

  strictDeps = false;
  nativeBuildInputs = [ makeWrapper ]
    ++ (optional withGtkWrapper wrapGAppsHook);

  buildInputs = optionals withGtkWrapper [ gdk-pixbuf glib gtk3 ];

  # We want to run wrapProgram manually
  dontWrapGApps = true;

  postBuild = ''
    ${optionalString withGtkWrapper "gappsWrapperArgsHook"}

    ls -alF "$out"
    ls -alF "$out/share"
    mkdir -p "$out/share/wayland-sessions"
    cat <<EOF >"$out/share/wayland-sessions/qtile.desktop"
    [Desktop Entry]
    Name=qtile
    Comment=qtile
    Exec=qtile start -b wayland
    Type=Application
    EOF

    wrapProgram $out/bin/qtile \
      ${optionalString withGtkWrapper ''"''${gappsWrapperArgs[@]}"''} \
      ${optionalString (extraOptions != []) "${concatMapStrings (x: " --add-flags " + x) extraOptions}"}
  '';

  passthru = {
    inherit (qtile.passthru) tests;
    providedSessions = [ "qtile" ];
  };

  inherit (qtile) meta;
}
