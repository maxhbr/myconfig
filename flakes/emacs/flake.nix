{
  inputs = {
    home-manager.url = "github:rycee/home-manager";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-doom-emacs,
    ...
  }: {
    nixosConfigurations.exampleHost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        {
          home-manager.users.exampleUser = { pkgs, ... }: {
            imports = [ nix-doom-emacs.hmModule ];
            programs.doom-emacs = {
              enable = true;
              doomPrivateDir = ./doom.d;
            };
          };
        }
      ];
    };
  };
}
