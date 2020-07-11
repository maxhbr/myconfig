# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:

{
  config =  {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      forwardX11 = config.services.xserver.enable;
    };
    systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];

#     environment.interactiveShellInit = ''
# ###############################################################################
# # Start tmux on ssh
# if [ "$PS1" != "" -a "$${SSH_TTY:-x}" != x ]; then
#   if test -z $TMUX && [[ $TERM != "screen" ]]; then
#     ( (${pkgs.tmux}/bin/tmux has-session -t remote && ${pkgs.tmux}/bin/tmux attach-session -t remote) \
#       || (${pkgs.tmux}/bin/tmux -2 new-session -s remote) ) && exit 0
#     echo "tmux failed to start"
#   else
#     export DISPLAY=:0
#   fi
# fi
#     '';
  };
}
