{
  config,
  lib,
  ...
}:

{
  config = lib.mkIf config.services.wayvnc.enable {
    services.wayvnc = {
      autoStart = lib.mkDefault true;
      settings = {
        address = lib.mkDefault "localhost";
      };
    };
  };
}
