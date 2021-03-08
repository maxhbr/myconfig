{
  description = "my base configuration";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    staged.url = "github:nixos/nixpkgs/staging";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    large.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    # nix.url = "github:nixos/nix/flakes";
    # nix.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    license-compliance-toolbox.url = "path:flakes/license-compliance-toolbox/";
    license-compliance-toolbox.inputs.nixpkgs.follows = "nixpkgs";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = { self, ... }@inputs: {
    aggregatedInputs = inputs;
    lib.importall = path:
      if builtins.pathExists path then
        let content = builtins.readDir path;
        in map (n: import (path + ("/" + n))) (builtins.filter (n:
          builtins.match ".*\\.nix" n != null
          || builtins.pathExists (path + ("/" + n + "/default.nix")))
          (builtins.attrNames content))
      else
        [ ];
  };
}
