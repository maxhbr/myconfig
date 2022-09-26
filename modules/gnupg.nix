{ config, pkgs, ... }: {
  config = {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = !config.programs.ssh.startAgent;
      pinentryFlavor =
        if config.myconfig.desktop.enable
        then (if config.myconfig.desktop.wayland.enable
              then "qt"
              else "gtk2")
        else "curses";
    };
    environment = { systemPackages = with pkgs; [ gnupg ] ; };
  };
}
