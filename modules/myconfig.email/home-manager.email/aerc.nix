{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.programs.aerc.enable {
    home.packages = with pkgs; [ w3m ];
    programs.aerc = {
      # see for example https://man.archlinux.org/man/aerc-config.5.en
      extraConfig =
        let
          pager = "${pkgs.bat}/bin/bat";
        in
        {
          general = {
            unsafe-accounts-conf = true;
          };
          ui = {
            mouse-enabled = true;
          };
          viewer = {
            pager = "${pkgs.less}/bin/less -R";
            # show-headers = true;
            always-show-mime = true;
          };
          filters = {
            "text/plain" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
            "text/calendar" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/libexec/aerc/filters/calendar";
            "text/html" =
              "${pkgs.aerc}/libexec/aerc/filters/html | ${pkgs.aerc}/libexec/aerc/filters/colorize"; # Requires w3m, dante
            # "text/*" =
            #   ''${pkgs.bat}/bin/bat -fP --file-name="$AERC_FILENAME "'';
            "message/delivery-status" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
            "message/rfc822" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
            "application/x-sh" = "${pkgs.bat}/bin/bat -fP -l sh";
            "application/pdf" = "${pkgs.zathura}/bin/zathura -";
            "audio/*" = "${pkgs.mpv}/bin/mpv -";
            "image/*" = "${pkgs.feh}/bin/feh -";
          };
          compose = {
            # editor = config.home.sessionVariables.EDITOR;
            # address-book-cmd = "aba ls \"%s\"";
            empty-subject-warning = true;
          };
        };
    };
  };
}
