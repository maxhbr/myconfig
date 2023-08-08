{ lib, river-unwrapped, makeWrapper, symlinkJoin, writeShellScriptBin
, withBaseWrapper ? true, extraSessionCommands ? "", dbus
, withGtkWrapper ? false, wrapGAppsHook, gdk-pixbuf, glib, gtk3
, extraPaths ? [ ], extraOptions ? [ ] # E.g.: [ "--verbose" ]
, xwaylandSupport ? true, dbusSupport ? true }:

assert extraSessionCommands != "" -> withBaseWrapper;

with lib;

let
  river = river-unwrapped.override { inherit xwaylandSupport; };
  baseWrapper = writeShellScriptBin "river" ''
    set -o errexit
    if [ ! "$_RIVER_WRAPPER_ALREADY_EXECUTED" ]; then
      export XDG_CURRENT_DESKTOP=river
      ${extraSessionCommands}
      export _RIVER_WRAPPER_ALREADY_EXECUTED=1
    fi
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${river}/bin/river "$@"
    else
      exec ${
        if !dbusSupport then "" else "${dbus}/bin/dbus-run-session"
      } ${river}/bin/river "$@"
    fi
  '';
in symlinkJoin {
  name = "my-river-${river.version}";

  paths = (optional withBaseWrapper baseWrapper) ++ [ river ] ++ extraPaths;

  strictDeps = false;
  nativeBuildInputs = [ makeWrapper ]
    ++ (optional withGtkWrapper wrapGAppsHook);

  buildInputs = optionals withGtkWrapper [ gdk-pixbuf glib gtk3 ];

  # We want to run wrapProgram manually
  dontWrapGApps = true;

  postBuild = ''
    ${optionalString withGtkWrapper "gappsWrapperArgsHook"}

    wrapProgram $out/bin/river \
      ${optionalString withGtkWrapper ''"''${gappsWrapperArgs[@]}"''} \
      ${
        optionalString (extraOptions != [ ])
        "${concatMapStrings (x: " --add-flags " + x) extraOptions}"
      }
  '';

  passthru = {
    inherit (river.passthru) tests;
    providedSessions = [ "river" ];
  };

  inherit (river) meta man;
}
