# Copyright 2019-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [
    # {
    #   home-manager.sharedModules = [{
    #     home.packages = with pkgs; [ vagrant ];
    #     home.file = {
    #       ".vagrant.d/Vagrantfile".text = ''
    #         Vagrant.configure("2") do |config|
    #           # might need: vagrant plugin install vagrant-vbguest
    #           # config.vbguest.auto_update = false
    #         end
    #       '';
    #     };
    #   }];
    # }
  ];
  config = (lib.mkIf config.virtualisation.virtualbox.host.enable {
    # virtualisation.virtualbox.host.enableExtensionPack = true;
    services.nfs.server.enable = true;
    networking.firewall.extraCommands = ''
      ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
    '';
    virtualisation.virtualbox.host.enableExtensionPack = true;
  });
}
