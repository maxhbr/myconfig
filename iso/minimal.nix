import ./mkiso.nix {
  customModule = { ... }: { services.getty.autologinUser = "mhuber"; };
}
