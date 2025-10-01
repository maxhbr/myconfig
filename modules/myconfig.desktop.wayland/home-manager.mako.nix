{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.services.mako.enable {
    services.mako = {
      settings = {
        layer = "overlay";
        background-color = "#282828BB";
        border-color = "#ee9a00";
        margin = 8;
        border-size = 4;
        border-radius = 8;
        default-timeout = 20000;
        icons = true;
      };
    };
  };
}
