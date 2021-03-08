{self, ...}@inputs:
{
  imports = [
      # home manager:
      inputs.home.nixosModules.home-manager
      ({ config, lib, ... }: {
        config = {
          home-manager = {
            useUserPackages = true;
            useGlobalPkgs = true;
          };
        };
      })
  ];
}
