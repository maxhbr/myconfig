{
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

  hardware.url = "github:nixos/nixos-hardware";
}
