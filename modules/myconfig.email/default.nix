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
  options.myconfig = with lib; {
    email = {
      enable = mkEnableOption "myconfig.email";
      indexer = mkOption {
        type = types.enum [
          "notmuch"
          "mu"
        ];
        default = "notmuch";
      };
      syncer = mkOption {
        type = types.enum [
          "mbsync"
        ];
        default = "mbsync";
      };
      clients = mkOption {
        type = types.listOf (
          types.enum [
            "neomutt"
            "aerc"
            "alot"
            "astroid"
            "evolution"
            "himalaya"
            "meli"
            "thunderbird"
          ]
        );
        description = lib.mdDoc ''
          List of email clients to enable.
        '';
        default = [
          "neomutt"
          "aerc"
        ];
      };
    };
  };
  imports = [
    ./aerc
    ./alot.nix
    ./astroid.nix
    ./evolution.nix
    ./himalaya.nix
    ./mbsync.nix
    ./meli.nix
    ./mu.nix
    ./neomutt
    ./notmuch.nix
    ./thunderbird.nix
  ];
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        let
          inherit (lib)
            mkOption
            types
            ;
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
                pgp-key-id = mkOption {
                  type = types.nullOr types.str;
                  default = null;
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

          myconfigAccountToHomeManagerEmailAccount =
            name: account:
            (lib.mkMerge (
              config.myconfig.homeManagerEmailConfig
              ++ [
                {
                  inherit (account)
                    realName
                    flavor
                    primary
                    address
                    aliases
                    userName
                    passwordCommand
                    ;
                  gpg = lib.mkIf (account.pgp-key-id != null) {
                    key = account.pgp-key-id;
                    signByDefault = true;
                    encryptByDefault = false;
                  };
                  signature = {
                    text = account.signature;
                    showSignature = "append";
                  };

                  maildir = {
                    path = name;
                  };
                  folders = lib.mkIf (account.flavor != "gmail.com") {
                    inbox = "Inbox";
                    drafts = "Drafts";
                    sent = "Sent";
                    trash = "Trash";
                  };
                  neomutt.mailboxName = name;
                  msmtp.enable = config.programs.msmtp.enable;
                  aerc.extraAccounts = {
                    inherit (account) pgp-key-id;
                    signature-file =
                      let
                        out = pkgs.writeText "signature" account.signature;
                      in
                      "${out}/signature";
                    pgp-auto-sign = true;
                    pgp-opportunistic-encrypt = true;
                  };
                  himalaya.settings = {
                    signature = account.signature;
                  };
                }
                (lib.mkIf (config.programs.lieer.enable && account.flavor == "gmail.com") {
                  lieer = {
                    enable = config.programs.lieer.enable;
                    sync.enable = config.programs.lieer.enable;
                    sync.frequency = "*:0/3";
                    settings.account = account.address;
                  };

                })
                account.homeManagerEmailAccountOverwrites
              ]
            ));

          mailAccountsToPersist =
            guard:
            lib.map (a: a.maildir.absPath) (
              lib.filter (a: guard a) (lib.attrValues config.accounts.email.accounts)
            );
        in
        {
          options = {
            myconfig = {
              homeManagerEmailConfig = lib.mkOption {
                type = types.listOf types.attrs;
                default = [ ];
                description = "Home Manager email config pieces applied to all accounts.";
              };
              accounts = lib.mkOption {
                type = types.attrsOf (types.submodule myconfigAccountsModule);
                default = { };
                description = "List of accounts.";
              };
            };
          };
          config = {
            home.packages = with pkgs; [
              abook
              aba
              dante # Socksify for rendering HTML
              extract_url
              urlscan
            ];

            accounts.email.accounts = lib.mapAttrs myconfigAccountToHomeManagerEmailAccount config.myconfig.accounts;
            programs.msmtp.enable = true;
            programs.lieer.enable = true; # lib.any (a: a.flavor == "gmail.com") (lib.attrValues config.accounts.email.accounts);
            services.lieer.enable = true; # config.programs.lieer.enable;
            myconfig.persistence.directories = mailAccountsToPersist (a: a.name != "tng");
            myconfig.persistence.work-directories = mailAccountsToPersist (a: a.name == "tng");
          };
        }
      )
    ];
  };
}
