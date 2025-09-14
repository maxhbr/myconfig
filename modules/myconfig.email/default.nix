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
            "alot" # untested
            "astroid"
            "evolution"
            "himalaya" # untested
            "meli" # untested
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
    ./aerc.nix
    ./alot.nix
    ./astroid.nix
    ./evolution.nix
    ./himalaya.nix
    ./mbsync.nix
    ./meli.nix
    ./mu.nix
    ./neomutt.nix
    ./notmuch.nix
    ./thunderbird.nix
  ];
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        let
          myconfigAccountToHomeManagerEmailAccount =
            name: account:
            (lib.mkMerge (
              config.myconfig.homeManagerEmailConfig
              ++ [
                {
                  inherit (account)
                    realName
                    primary
                    address
                    aliases
                    ;
                  inherit (account.email)
                    flavor
                    userName
                    passwordCommand
                    ;
                  signature = {
                    text = account.email.signature;
                    showSignature = "append";
                  };
                  maildir = {
                    path = name;
                  };
                  neomutt.mailboxName = name;
                  msmtp.enable = config.programs.msmtp.enable;
                  aerc.extraAccounts = {
                    signature-file = "${pkgs.writeText "signature" account.email.signature}";
                  };
                  himalaya.settings = {
                    signature = account.email.signature;
                  };
                }
                (lib.mkIf (account.pgp-key-id != null) {
                  gpg = {
                    key = account.pgp-key-id;
                    signByDefault = true;
                    encryptByDefault = false;
                  };
                  aerc.extraAccounts = {
                    inherit (account) pgp-key-id;
                    pgp-auto-sign = true;
                    pgp-opportunistic-encrypt = true;
                  };
                })
                (lib.mkIf (account.email.flavor != "gmail.com") {
                  folders = {
                    inbox = "Inbox";
                    drafts = "Drafts";
                    sent = "Sent";
                    trash = "Trash";
                  };
                })
                (lib.mkIf (config.programs.lieer.enable && account.email.flavor == "gmail.com") {
                  lieer = {
                    enable = config.programs.lieer.enable;
                    sync.enable = config.programs.lieer.enable;
                    sync.frequency = "*:0/3";
                    settings.account = account.address;
                  };
                  aerc.extraAccounts = {
                    folder-map = "${pkgs.writeText "folder-map" ''
                      * = [Gmail]/*
                    ''}";
                  };
                })
                account.email.homeManagerEmailAccountOverwrites
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
              homeManagerEmailConfig =
                with lib;
                mkOption {
                  type = types.listOf types.attrs;
                  default = [ ];
                  description = "Home Manager email config pieces applied to all accounts.";
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

            accounts.email.accounts = lib.mapAttrs myconfigAccountToHomeManagerEmailAccount (lib.filterAttrs (n: a: a.email != null) config.myconfig.accounts);
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
