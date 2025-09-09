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
        type = types.listOf types.str;
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
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        let
          inherit (lib)
            mkDefault
            mkIf
            mkOption
            types
            ;
          myconfigAccountsModule = { name, config, ... }: {
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
            name:
            account:
            (lib.mkMerge [
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
              }
              {
                mbsync = {
                  enable = config.programs.mbsync.enable;
                  create = "both";
                  expunge = "both";
                  extraConfig = {
                    channel = {
                      Sync = "All";
                    };
                    # account = {
                    #   Timeout = 120;
                    #   PipelineDepth = 1;
                    # };
                  };
                };
              }
              {
                msmtp.enable = config.programs.msmtp.enable;
              }
              {
                notmuch = {
                  enable = config.programs.notmuch.enable;
                  neomutt.enable = config.programs.neomutt.enable;
                };
              }
              {
                mu.enable = config.programs.mu.enable;
              }
              {
                aerc = {
                  enable = config.programs.aerc.enable;
                  extraAccounts = {
                    inherit (account) pgp-key-id;
                    signature-file =
                      let
                        out = pkgs.writeText "signature" account.signature;
                      in
                      "${out}/signature";
                    pgp-auto-sign = true;
                    pgp-opportunistic-encrypt = true;
                  };
                };
              }
              {
                thunderbird = {
                  enable = config.programs.thunderbird.enable;
                };
              }
              {
                himalaya = {
                  enable = config.programs.himalaya.enable;
                  settings = {
                    signature = account.signature;
                    pgp-type = "gpg";
                  };
                };
              }
              {
                meli = {
                  enable = config.programs.meli.enable;
                };
              }
              {
                neomutt = {
                  enable = config.programs.neomutt.enable;
                  mailboxName = name;
                };
              }
              {
                alot = {
                  enable = config.programs.alot.enable;
                  contactCompletion = {
                    type = "shellcommand";
                    command = "${pkgs.notmuch-addrlookup}/bin/notmuch-addrlookup ";
                    regexp = "(?P<name>.*).*<(?P<email>.+)>";
                    ignorecase = "True";
                  };
                };
              }
              {
                astroid = {
                  enable = config.programs.astroid.enable;
                }
                // (lib.mkIf config.programs.msmtp.enable {
                  sendMailCommand = "${pkgs.msmtp}/bin/msmtp -a ${name}";
                });
              }
              (lib.mkIf (config.programs.lieer.enable && account.flavor == "gmail.com") {
                lieer =
                  {
                      enable = config.programs.lieer.enable;
                      sync.enable = config.programs.lieer.enable;
                      sync.frequency = "*:0/3";
                      settings.account = account.address;
                    };

              })
              account.homeManagerEmailAccountOverwrites
            ]
          );

          mailAccountsToPersist =
            guard:
            lib.map (a: a.maildir.absPath) (
              lib.filter (a: guard a) (lib.attrValues config.accounts.email.accounts)
            );
        in
        {
          options = {
            myconfig.accounts = lib.mkOption {
              type = types.attrsOf (types.submodule myconfigAccountsModule);
              default = { };
              description = "List of accounts.";
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
            programs.lieer.enable = true;
            services.lieer.enable = true;
            programs.msmtp.enable = true;
            programs.afew.enable = false;
            accounts.email.accounts = lib.mapAttrs myconfigAccountToHomeManagerEmailAccount config.myconfig.accounts;
            myconfig.persistence.directories = mailAccountsToPersist (a: a.name != "tng");
            myconfig.persistence.work-directories = mailAccountsToPersist (a: a.name == "tng");
          };
        }
      )
    ];
  };
}
