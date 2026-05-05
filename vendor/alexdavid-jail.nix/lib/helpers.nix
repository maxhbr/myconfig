pkgs: rec {
  dataDir = "~/.local/share/jail.nix";

  dataDirSubPath = subPath: "${dataDir}/${escape subPath}";

  noescape = value: { _noescape = value; };

  escape =
    rawOrStr:
    if builtins.typeOf rawOrStr == "set" && rawOrStr ? _noescape then
      rawOrStr._noescape
    else
      pkgs.lib.strings.escapeShellArg rawOrStr;

  pushState =
    key: toPush: state:
    state // { ${key} = state.${key} ++ [ toPush ]; };
}
