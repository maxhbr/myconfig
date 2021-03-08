{self, ...}@inputs:
{
  imports = [
      # home manager:
      inputs.home.nixosModules.home-manager
      ({ config, ... }: {
        options.home-manager.users = lib.mkOption {
          type = with lib.types;
            attrsOf (submoduleWith {
              specialArgs = specialArgs // { super = config; };
              modules = hmModules;
            });
        };
        config = {
          home-manager = {
            useUserPackages = true;
            useGlobalPkgs = true;
          };
        };
      })
  ];
}
