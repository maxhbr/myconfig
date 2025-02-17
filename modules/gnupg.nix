{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.programs.gnupg.agent.enable {
    programs.gnupg.agent = {
      enableSSHSupport = !config.programs.ssh.startAgent;
      pinentryPackage = if config.myconfig.desktop.enable then
        (if config.myconfig.desktop.wayland.enable then
          pkgs.pinentry-qt
        else
          pkgs.pinentry-qt)
      else
        pkgs.pinentry-curses;
    };
    environment = { systemPackages = with pkgs; [ gnupg ]; };
  };
}
