{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.irc = {
      enable = lib.mkEnableOption "Irc role";
    };
  };

  config = lib.mkIf config.myconfig.roles.irc.enable {
    environment.systemPackages = with pkgs; [
      weechat
    ];

    # # stolen from: https://github.com/garbas/dotfiles/blob/master/nixos/floki.nix
    # systemd.services."weechat" = with pkgs; {
    #   enable = true;
    #   description = "Weechat IRC Client (in tmux)";
    #   environment = {
    #     # LANG = "en_US.utf8";
    #     # LC_ALL = "en_US.utf8";
    #     TERM = "${rxvt_unicode.terminfo}";
    #   };
    #   path = [ tmux weechat termite.terminfo which binutils ];
    #   wantedBy = [ "multi-user.target" ];
    #   serviceConfig = {
    #     WorkingDirectory = "/var/weechat";
    #     Type = "oneshot";
    #     RemainAfterExit = "yes";
    #     ExecStart = "${tmux}/bin/tmux -v -S /run/tmux-weechat new-session -d -s weechat -n 'weechat' '${weechat}/bin/weechat-curses -d /var/weechat'";
    #     ExecStop = "${tmux}/bin/tmux -S /run/tmux-weechat kill-session -t weechat";
    #     KillMode = "none";
    #   };
    # };
  };
}
