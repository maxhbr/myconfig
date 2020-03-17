import ./entrypoint.nix {
  # echo -n "HOSTNAME" | sudo tee ../hostname
  hostName = if builtins.pathExists ../hostname
             then builtins.readFile ../hostname
             else "minimal";
}
