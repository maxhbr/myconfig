{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [{
      config = {
        programs.zathura = {
          enable = lib.mkDefault true;
          options = { };
          extraConfig = ''
            set recolor false # invert colors
            set recolor-keephue false

            set scroll-step 100

            set first-page-column 2 # book-like feeling

            map [normal] i scroll half-down
            map [fullscreen] i scroll half-down
            map [normal] u scroll half-up
            map [fullscreen] u scroll half-up

            map [normal] x set recolor true
            map [fullscreen] x set recolor true
            map [normal] v set recolor false
            map [fullscreen] v set recolor false
          '';
        };
        xdg.mimeApps = {
          defaultApplications."application/pdf" =
            [ "zathura.desktop" "mupdf.desktop" ];
        };
      };
    }];
  };
}
