{ self, ... }@inputs:
let inherit (inputs.nixpkgs) lib;
in {
  importall = path:
    if builtins.pathExists path then
      let content = builtins.readDir path;
      in map (n: import (path + ("/" + n))) (builtins.filter (n:
        builtins.match ".*\\.nix" n != null
        || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content))
    else
      [ ];

  mkConfiguration = import ./mkConfiguration.nix inputs;
  evalConfiguration = system: hostName: args:
    (let cfg = self.lib.mkConfiguration system hostName args;
    in lib.nixosSystem (lib.recursiveUpdate cfg {
      modules = cfg.modules ++ [ (../hosts/host + ".${hostName}") ]
        ++ [ (../secrets + "/${hostName}") ]
        ++ (self.lib.importall (../secrets + "/${hostName}/imports"))
        ++ inputs.private.lib.getNixosModulesFor hostName;
    }));
}
