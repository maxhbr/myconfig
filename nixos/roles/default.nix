{ config, lib, pkgs, ... }:

# stolen from:
# https://github.com/flyingcircusio/nixpkgs/blob/fc-15.09-dev/nixos/modules/flyingcircus/roles/default.nix

let

  cfg = config.myconfig;

  # mkrole = (name: path: {
  #   options = {
  #     myconfig.roles.${name}.value = {
  #       enable = lib.mkEnableOption "${name} role";
  #     };
  #     # myconfig.roles = listToAttrs [{
  #     #   name = name;
  #     #   value = {
  #     #     enable = lib.mkEnableOption "${name} role";
  #     #   };
  #     # }];
  #   };

  #   config = lib.mkIf config.myconfig.roles.${name}.enable {
  #     import path {inherit config lib pkgs;};
  #   };
  # });

in

{

  imports = [
    ./clamav.nix
    ./desktop
    ./dev.nix
    ./emacs.nix
    ./games.nix
    ./imagework.nix
    ./irc.nix
    ./mail.nix
    ./openssh.nix
    ./sundtek.nix
    ./taskserver.nix
    ./terminal.nix
    ./tex.nix
    ./virtualization.nix
    ./vsftp.nix
    ./wine.nix
    ./work.nix
    ];

  options = {

    myconfig.active-roles = lib.mkOption {
      default = [];
      type = lib.types.listOf lib.types.str;

      description = ''
        Which roles to activate. E.g:
          myconfig.active-roles = [ "work" "imagework" ];
      '';
    };

  };

  config =
    # Map list of roles to a list of attribute sets enabling each role.
    let
      # Turn the list of role names (["a", "b"]) into an attribute set
      # ala { <role> = { enable = true;}; }
      role_set = lib.listToAttrs (
        map (role: { name = role; value = { enable = true; }; })
          cfg.active-roles);
    in
      {
        myconfig.roles = role_set;
      };

}
