{
  config,
  lib,
  pkgs,
  ...
}:

let
  mailcap_file = pkgs.writeText "mailcap" (
    let
      htmlViaLynx = ''
        text/html; ${pkgs.lynx}/bin/lynx %s
        text/html; ${pkgs.lynx}/bin/lynx -dump %s; copiousoutput
      '';
      htmlViaW3m = ''
        text/html; ${pkgs.w3m}/bin/w3m -dump -ppc 8 -graph -s -I %{charset} -T text/html; copiousoutput
      '';
      htmlViaElinks = ''
        text/html; ${pkgs.elinks}/bin/elinks %s; nametemplate=%s.html
        text/html; ${pkgs.elinks}/bin/elinks -dump %s; nametemplate=%s.html; copiousoutput
      '';
    in
    ''
      ${htmlViaElinks}
      application/pdf; ${pkgs.zathura}/bin/zathura %s
      image/*; ${pkgs.sxiv}/bin/sxiv %s
      application/x-tar; ${pkgs.xarchiver}/bin/xarchiver %s &;
    ''
  );
in
{
  config = lib.mkIf (cfg.email.enable && (builtins.elem "neomutt" cfg.email.clients)) {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        {
          config = {
            programs.neomutt = {
              sidebar = {
                enable = true;

              };
              sort = "threads";
              vimKeys = false;
              checkStatsInterval = 60;
              settings = {
                sort_browser = "reverse-date";
                sort_aux = "last-date-received";
                mailcap_path = "${mailcap_file}";
                envelope_from = "yes";
                edit_headers = "yes";
                query_command = ''"${pkgs.notmuch-addrlookup}/bin/notmuch-addrlookup --format=mutt '%s'"'';
              };

              binds = [
                {
                  key = "g";
                  action = "noop";
                }
                {
                  key = "gg";
                  action = "first-entry";
                }
                {
                  key = "G";
                  action = "last-entry";
                }
                {
                  map = [
                    "index"
                    "pager"
                  ];
                  key = "R";
                  action = "group-reply";
                }
                # # bind compose p pgp-menu
                # {
                #   key = "p";
                #   action = "pgp-menu";
                # }
              ];
              macros = [
                {
                  # "file as Spam"
                  map = [ "index" ];
                  key = "S";
                  action = "<tag-prefix><enter-command>unset resolve<enter><tag-prefix><clear-flag>N<tag-prefix><enter-command>set resolve<enter><tag-prefix><save-message>=Spam.Verified<enter>";
                }
                {
                  # "file as Spam"
                  map = [ "pager" ];
                  key = "S";
                  action = "<save-message>=Spam.Verified<enter>";
                }
                {
                  # macro index,pager M "<pipe-message>abook --add-email-quiet<return>" "add sender to abook"
                  map = [
                    "index"
                    "pager"
                  ];
                  key = "M";
                  action = "<pipe-message>abook --add-email-quiet<return>";
                }
                {
                  # macro index t "c=<tab><tab><tab>" #drücke t, um in den Ordnern des Postfaches zu navigieren
                  map = [ "index" ];
                  key = "t";
                  action = "c=<tab><tab><tab>";
                }
                {
                  # macro compose Y pfy "Send without GPG"
                  map = [ "compose" ];
                  key = "Y";
                  action = "pfy";
                }
                {
                  map = [ "index" ];
                  key = "<f5>";
                  action = "<sync-mailbox><refresh><enter-command>source ~/.config/neomutt/neomuttrc<enter><change-folder>!<enter>";
                }
                {
                  map = [
                    "index"
                    "pager"
                  ];
                  key = "\\cb";
                  action = "<pipe-message> ${pkgs.urlscan}/bin/urlscan<Enter>";
                }
                {
                  map = [
                    "attach"
                    "compose"
                  ];
                  key = "\\cb";
                  action = "<pipe-entry> ${pkgs.urlscan}/bin/urlscan<Enter>";
                }
              ];

              extraConfig = ''
                source ${config.programs.neomutt.package}/share/doc/neomutt/colorschemes/neonwolf-256.neomuttrc
                source `FILE=$HOME/.gnupg/gpg_groups.mutt; if [[ ! -s "$FILE" ]]; then FILE=/dev/null;fi;echo "$FILE"`

                ##############################################################################
                # http://www.gnuterrypratchett.com/
                my_hdr X-Clacks-Overhead: GNU Terry Pratchett
                unignore X-Clacks-Overhead
                ##############################################################################
              '';
            };
            home.packages = with pkgs; [
              (writeScriptBin "gnupg-to-mutt.pl" (builtins.readFile ./gnupg-to-mutt.pl))
              (writeScriptBin "tmux-neomutt.sh" (builtins.readFile ./tmux-neomutt.sh))
              (writeShellScriptBin "foot-neomutt" ''
                exec ${foot}/bin/foot \
                  -T foot-neomutt \
                  -a foot-neomutt \
                  tmux-neomutt.sh
              '')
            ];
            myconfig.persistence.cache-directories = [ ".cache/neomutt" ];
          };
        }
      )
    ];
  };
}
