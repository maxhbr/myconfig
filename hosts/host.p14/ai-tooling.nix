{ config, lib, pkgs, ... }: {
  home-manager.sharedModules = [{
    home.packages = with pkgs; [ 
      aider-chat
      code-cursor
      zed
    ];
  }];
}



