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

  mkNixpkgsModule = pkgs:
    ({ config, ... }: {
      config.nixpkgs = {
        inherit pkgs;
        inherit (pkgs) config system;
        overlays = [
          (self: super: {
            unstable = super.unstable or { }
              // import inputs.master { inherit (pkgs) config system; };
            nixos-unstable = super.nixos-unstable or { }
              // import inputs.large { inherit (pkgs) config system; };
            nixos-unstable-small = super.nixos-unstable-small or { }
              // import inputs.small { inherit (pkgs) config system; };
            nixos-2003-small = super.unstable or { }
              // import inputs.rel2003 { inherit (pkgs) config system; };
            nixos-2009-small = super.unstable or { }
              // import inputs.rel2009 { inherit (pkgs) config system; };
          })
        ];
      };
    });

  mkConfiguration = import ./mkConfiguration.nix inputs;
  evalConfiguration = system: hostName: args:
    (let cfg = self.lib.mkConfiguration system hostName args;
    in lib.nixosSystem (cfg // {
      modules = cfg.modules ++ [ (../hosts/host + ".${hostName}") ]
        ++ [ (../secrets + "/${hostName}") ]
        ++ (self.lib.importall (../secrets + "/${hostName}/imports"))
        ++ inputs.private.lib.getNixosModulesFor hostName;
    }));
}
