# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{
  imports = [
    ./git
    ./vim
    ./tmux
    ./zsh
    ./dnsmasq.nix
    ./extrahosts
  ];
}
