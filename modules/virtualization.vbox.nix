# Copyright 2019-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  user = config.myconfig.user;
in {
  config = (lib.mkIf config.virtualisation.virtualbox.host.enable {
    # virtualisation.virtualbox.host.enableExtensionPack = true;
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ vagrant ];
      home.file = {
        ".vagrant.d/Vagrantfile".text = ''
Vagrant.configure("2") do |config|
  # might need: vagrant plugin install vagrant-vbguest
  # config.vbguest.auto_update = false
end
'';
      };
    };
    environment = {
      shellAliases = {
        vup = "vagrant up";
        vstart = "vagrant up";
        vreload = "vagrant reload";
        vhalt = "vagrant halt";
        vstop = "vagrant halt";
        vdown = "vagrant halt";
        vssh = "vagrant ssh";
        vdestroy = "vagrant destroy";
      };
    };
  });
}
