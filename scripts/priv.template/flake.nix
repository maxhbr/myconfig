{
  description = "custom part of myconfig";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    myconfig.url = "path:/home/mhuber/myconfig/myconfig";
    myconfig.inputs.nixpkgs.follows = "nixpkgs";
    myconfig.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      myconfig,
      ...
    }@inputs_:
    let
      inherit (inputs_.nixpkgs) lib;
      inputs = inputs_.myconfig.aggregatedInputs // inputs_;
      metadataOverrides = builtins.fromJSON (builtins.readFile (./hosts + "/metadata.json"));

      mkHostFromMyconfig =
        hostName: moreModules:
        (myconfig.nixosConfigurationsGen."host-${hostName}" (
          [ (./hosts + "/host.${hostName}") ] ++ moreModules
        ) metadataOverrides);

    in
    {
      inherit (myconfig) devShell;
    }
    // (
      let
        inherit (inputs.nixpkgs) lib;
      in
      lib.recursiveUpdate
        {
          nixosConfigurations = {
          };
        }
        (
          let
            eachDefaultSystem = inputs.flake-utils.lib.eachSystem [
              "x86_64-linux"
              "aarch64-linux"
            ];
            nixpkgsConfig = {
              allowUnfree = true;
              allowBroken = true;
            };

          in
          eachDefaultSystem (system: {
            formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;
            packages = {

            };
          })
        )
    );
}
