{ pkgs, config, ... }: {
  config = {
    networking.firewall.allowedTCPPorts = [ 8123 ];
    networking.firewall.allowedUDPPorts = [ 8123 ];

    users.users.hass = {
      home = "/home/hass";
      createHome = true;
      group = "hass";
      extraGroups = [ "dialout" ];
      uid = config.ids.uids.hass;
    };

    docker-containers = {
      homeassistant = {
        image = "homeassistant/home-assistant:stable";
        # args = ;
        # entrypoint = ;
        volumes = [
          "/home/hass:/config"
          "/etc/localtime:/etc/localtime:ro"
          # "/etc/timezone:/etc/timezone:ro"
        ];
        extraDockerOptions = [
          "--net=host"
          "-e"
          "USER_ID=${toString config.ids.uids.hass}"
          "-e"
          "GROUP_ID=${toString config.ids.uids.hass}"
        ];
      };
    };
  };
}
