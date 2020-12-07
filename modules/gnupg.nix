{ config, pkgs, ... }: {
  config = {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = !config.programs.ssh.startAgent;
      pinentryFlavor =
        if config.services.xserver.enable then "gtk2" else "curses";
    };
    environment = { systemPackages = with pkgs; [ gnupg ]; };
  };
}
