{ config, lib, pkgs, ... }:

# see:
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt.nix
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt-accounts.nix

let
  mailcap_file = pkgs.writeText "mailcap" (let
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
  in ''
    ${htmlViaElinks}
    application/pdf; ${pkgs.zathura}/bin/zathura %s
    image/*; ${pkgs.sxiv}/bin/sxiv %s
    application/x-tar; ${pkgs.xarchiver}/bin/xarchiver %s &;
  '');
in {
  config = lib.mkIf config.programs.neomutt.enable {
    programs.neomutt = {
      sidebar = { enable = true; };
      sort = "threads";
      vimKeys = false;
      settings = {
        sort_browser = "reverse-date";
        sort_aux = "last-date-received";
        mailcap_path = "${mailcap_file}";
        envelope_from = "yes";
        edit_headers = "yes";
      };

      binds = [
        # bind index g noop
        # bind index gg first-entry
        # bind index G last-entry
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
          map = ["index" "pager"];
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
        # macro index S "<tag-prefix><enter-command>unset resolve<enter><tag-prefix><clear-flag>N<tag-prefix><enter-command>set resolve<enter><tag-prefix><save-message>=Spam.Verified<enter>" "file as Spam"
        # macro pager S "<save-message>=Spam.Verified<enter>" "file as Spam"
        {
          # "file as Spam"
          map = [ "index" ];
          key = "S";
          action =
            "<tag-prefix><enter-command>unset resolve<enter><tag-prefix><clear-flag>N<tag-prefix><enter-command>set resolve<enter><tag-prefix><save-message>=Spam.Verified<enter>";
        }
        {
          # "file as Spam"
          map = [ "pager" ];
          key = "S";
          action = "<save-message>=Spam.Verified<enter>";
        }
        # macro index,pager M "<pipe-message>abook --add-email-quiet<return>" "add sender to abook"
        {
          map = [ "index" "pager" ];
          key = "M";
          action = "<pipe-message>abook --add-email-quiet<return>";
        }
        # macro index t "c=<tab><tab><tab>" #drücke t, um in den Ordnern des Postfaches zu navigieren
        {
          map = [ "index" ];
          key = "t";
          action = "c=<tab><tab><tab>";
        }
        # macro compose Y pfy "Send without GPG"
        {
          map = [ "compose" ];
          key = "Y";
          action = "pfy";
        }
      ];

      # editor = "emacs";
      extraConfig = ''
        source ${config.programs.neomutt.package}/share/doc/neomutt/colorschemes/solarized-dark-256.neomuttrc
        source `FILE=$HOME/.gnupg/gpg_groups.mutt; if [ ! -s "$FILE" ]; then FILE=/dev/null;fi;echo "$FILE"`

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
  };
}
