self: super: {
  nur = super.callPackage (import (super.builtins.fetchGit {
    url = "https://github.com/nix-community/NUR";
  }));
}
