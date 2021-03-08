import ./mkiso.nix {
  customModule = { config, lib, ... }:
    let user = config.myconfig.user;
    in { services.getty.autologinUser = lib.mkForce user; };
}
