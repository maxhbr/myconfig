{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        let
          inherit (lib)
            mkOption
            types
            ;
          myconfigEmailAccountsModule =
            { name, config, ... }:
            {
              options = {
                userName = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  description = ''
                    The server username of this account. This will be used as
                    the SMTP, IMAP, and JMAP user name.
                  '';
                };

                passwordCommand = mkOption {
                  type = types.nullOr (types.either types.str (types.listOf types.str));
                  default = null;
                  apply = p: if lib.isString p then lib.splitString " " p else p;
                  example = "secret-tool lookup email me@example.org";
                  description = ''
                    A command, which when run writes the account password on
                    standard output.
                  '';
                };
                signature = mkOption {
                  type = types.str;
                };
                flavor = mkOption {
                  type = types.enum [
                    "plain"
                    "gmail.com"
                  ];
                  default = "plain";
                };

                homeManagerEmailAccountOverwrites = mkOption {
                  type = types.attrs;
                  default = { };
                };
              };
            };
          myconfigAccountsModule =
            { name, config, ... }:
            {
              options = {
                primary = mkOption {
                  type = types.bool;
                  default = false;
                  description = ''
                    Whether this is the primary account. Only one account may be
                    set as primary.
                  '';
                };

                address = mkOption {
                  type = types.strMatching ".*@.*";
                  example = "jane.doe@example.org";
                  description = "The email address of this account.";
                };

                aliases = mkOption {
                  type = types.listOf types.str; # TODO: this is actually more complex
                  default = [ ];
                };

                realName = mkOption {
                  type = types.str;
                  example = "Jane Doe";
                  description = "Name displayed when sending mails.";
                };

                pgp-key-id = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                };

                email = mkOption {
                  type = types.nullOr (types.submodule myconfigEmailAccountsModule);
                  default = null;
                };
              };
            };
        in
        {
          options = {
            myconfig = {
              accounts = lib.mkOption {
                type = types.attrsOf (types.submodule myconfigAccountsModule);
                default = { };
                description = "List of accounts.";
              };
            };
          };
        }
      )
    ];
  };
}
