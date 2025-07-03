{
  config,
  lib,
  pkgs,
  ...
}:

{
  config.services.home-assistant.enable = true;
  imports = [
    (lib.mkIf config.services.home-assistant.enable {
      services.home-assistant = {
        openFirewall = true;
        configDir = "/var/lib/hass";
        port = 8123;
        config = {
          homeassistant = {
            name = "Home";
            latitude = config.location.latitude;
            longitude = config.location.longitude;
            elevation = "494";
            unit_system = "metric";
            time_zone = "UTC";
          };
          config = { };
          lovelace = {
            mode = "yaml";
          };
          logger = {
            default = "debug";
          };
          http = { };
          frontend = {
            themes = "!include_dir_merge_named themes";
          };
          # feedreader.urls = [ "https://nixos.org/blogs.xml" ];
        };
        configWritable = true;
        lovelaceConfig = {
          title = "My Awesome Home";
          views = [
            {
              title = "Example";
              cards = [
                {
                  type = "markdown";
                  title = "Lovelace";
                  content = "Welcome to your **Lovelace UI**.";
                }
              ];
            }
          ];
        };
        lovelaceConfigWritable = true;
      };
    })
  ];
}
