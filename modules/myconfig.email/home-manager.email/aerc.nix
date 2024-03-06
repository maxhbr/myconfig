{ config, lib, pkgs, ... }:

{
  programs.aerc = lib.mkIf config.programs.aerc.enable {
    # see for example https://man.archlinux.org/man/aerc-config.5.en
    extraConfig = let pager = "${pkgs.bat}/bin/bat";
    in {
      general = { unsafe-accounts-conf = true; };
      ui = { mouse-enabled = true; };
      compose = {
        # editor = "${pkgs.vim}/bin/vim";
        empty-subject-warning = true;
      };
      viewer = {
        inherit pager;
        # show-headers = true;
        always-show-mime = true;
      };
      filters = {
        "text/plain" = pager;
        "text/*" =
          ''${pager} -fP --file-name="$AERC_FILENAME" --style=plain'';
        "image/*" =
          "${pkgs.catimg}/bin/catimg -w$(${pkgs.ncurses}/bin/tput cols) -";
      };
    };
  };
}

