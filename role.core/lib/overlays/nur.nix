self: super: {
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
    pkgs = super;
  };

  # nur = super.callPackage (import (super.builtins.fetchGit {
  #   url = "https://github.com/nix-community/NUR";
  # }));
}
