path:
if builtins.pathExists path then
  let content = builtins.readDir path;
  in map (n: import (path + ("/" + n))) (builtins.filter (n:
    builtins.match ".*\\.nix" n != null
    || builtins.pathExists (path + ("/" + n + "/default.nix")))
    (builtins.attrNames content))
else
  [ ]
