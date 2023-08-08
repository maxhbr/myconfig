{ lib, wayfire, makeWrapper, symlinkJoin, writeShellScriptBin
, withBaseWrapper ? true, extraSessionCommands ? "", dbus
, withGtkWrapper ? false, wrapGAppsHook, gdk-pixbuf, glib, gtk3
, extraPaths ? [ ], extraOptions ? [ ] # E.g.: [ "--verbose" ]
, xwaylandSupport ? true, dbusSupport ? true }:

assert extraSessionCommands != "" -> withBaseWrapper;

with lib;

let
  baseWrapper = writeShellScriptBin "wayfire" ''
    set -o errexit
    if [ ! "$_wayfire_WRAPPER_ALREADY_EXECUTED" ]; then
      export XDG_CURRENT_DESKTOP=wayfire
      ${extraSessionCommands}
      export _wayfire_WRAPPER_ALREADY_EXECUTED=1
    fi
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${wayfire}/bin/wayfire "$@"
    else
      exec ${
        if !dbusSupport then "" else "${dbus}/bin/dbus-run-session"
      } ${wayfire}/bin/wayfire "$@"
    fi
  '';
in symlinkJoin {
  name = "wayfire-${wayfire.version}";

  paths = (optional withBaseWrapper baseWrapper) ++ extraPaths;

  strictDeps = false;
  nativeBuildInputs = [ makeWrapper ]
    ++ (optional withGtkWrapper wrapGAppsHook);

  buildInputs = optionals withGtkWrapper [ gdk-pixbuf glib gtk3 ];

  # We want to run wrapProgram manually
  dontWrapGApps = true;

  postBuild = ''
    ${optionalString withGtkWrapper "gappsWrapperArgsHook"}

    wrapProgram $out/bin/wayfire \
      ${optionalString withGtkWrapper ''"''${gappsWrapperArgs[@]}"''} \
      ${
        optionalString (extraOptions != [ ])
        "${concatMapStrings (x: " --add-flags " + x) extraOptions}"
      }
  '';

  passthru = {
    inherit (wayfire.passthru) tests;
    providedSessions = [ "wayfire" ];
  };

  inherit (wayfire) meta man;
}
