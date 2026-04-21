{
  pkgs,
  lib,
  config,
  ...
}:
let
  crawl4ai = {
    image = "unclecode/crawl4ai:latest";

    environment = rec {
      "TZ" = "Europe/Amsterdam";
    };

    ports = [ "127.0.0.1:11235:11235" ];

    extraOptions = [
      "--pull=always"
      "--name=crawl4ai"
      "--hostname=crawl4ai"
      "--shm-size=1g"
    ];
  };
in
{
  options.myconfig = with lib; {
    ai.container.crawl4ai = {
      enable = mkEnableOption "myconfig.ai.container.crawl4ai";
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.crawl4ai.enable) {
    virtualisation.oci-containers.containers = { inherit crawl4ai; };
    system.activationScripts = {
      script.text = ''
        :
      '';
    };
  };
}
