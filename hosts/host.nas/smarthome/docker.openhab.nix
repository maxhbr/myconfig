{ pkgs, ... }:
{
  config = {
    users = {
      extraUsers."openhab" = {
        isNormalUser = false;
        group = "openhab";
        uid = 1200;
        home = "/opt/openhab";
        createHome = true;
        shell = pkgs.shadow;
      };
      extraGroups."openhab".gid = 1200;
    };

    networking.firewall.allowedTCPPorts = [
      8080
      8443
    ];
    networking.firewall.allowedUDPPorts = [
      8080
      8443
    ];

    docker-containers = {
      openhab = {
        image = "openhab/openhab:latest";
        # args = ;
        # entrypoint = ;
        volumes = [
          "/opt/openhab/conf:/openhab/conf"
          "/opt/openhab/userdata:/openhab/userdata"
          "/opt/openhab/addons:/openhab/addons"
          "/etc/localtime:/etc/localtime:ro"
          # "/etc/timezone:/etc/timezone:ro"
        ];
        extraDockerOptions = [
          "--net=host"
          "-e"
          "USER_ID=1200"
          "-e"
          "GROUP_ID=1200"
          "-e"
          "OPENHAB_HTTP_PORT=8080"
          "-e"
          "OPENHAB_HTTPS_PORT=8443"
          "--memory=2g"
        ];
      };
    };
  };
}
