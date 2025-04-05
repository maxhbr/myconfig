{ config, pkgs, lib, myconfig, inputs, ... }: {
  config = lib.mkIf config.services.tabby.enable {
    services.tabby = {
      usageCollection = false;
    };
  };
}
