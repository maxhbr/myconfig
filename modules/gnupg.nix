{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = lib.mkIf config.programs.gnupg.agent.enable {
    programs.gnupg.agent = {
      enableSSHSupport = !config.programs.ssh.startAgent;
      pinentryPackage = pkgs.pinentry-all;
      # pinentryPackage = if config.myconfig.desktop.enable then
      #   pkgs.pinentry-qt
      # else
      #   pkgs.pinentry-curses;
    };
    environment = {
      systemPackages = with pkgs; [ gnupg ];
    };
  };
}
