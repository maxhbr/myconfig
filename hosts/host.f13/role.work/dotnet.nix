{ config, pkgs, lib, ... }: {
  home-manager.sharedModules = [{
    home.packages = with pkgs;
      [
        dotnet-sdk
        # dotnet-sdk_9
        # dotnet-sdk_10
      ];
  }];
}
