{pkgs, lib, config, ...}: 
let
  open-webui = {
    image = "ghcr.io/open-webui/open-webui:main";

    environment = {
      "TZ" = "Europe/Amsterdam";
      "OLLAMA_API_BASE_URL" = "http://127.0.0.1:11434/api";
      "OLLAMA_BASE_URL" = "http://127.0.0.1:11434";
    };

    volumes = [
      "/home/open-webui/data:/app/backend/data"
    ];

    ports = [
      "127.0.0.1:3000:8080" # Ensures we listen only on localhost
    ];

    extraOptions = [
      "--pull=always" # Pull if the image on the registry is always
      "--name=open-webui"
      "--hostname=open-webui"
      "--network=host"
      "--add-host=host.containers.internal:host-gateway"
    ];
  };
in {
      config = lib.mkIf config.services.ollama.enable {
        home-manager.sharedModules = [{
          home.packages = with pkgs; [
            nvtopPackages.full
            # oterm
          ];
        }];
        # services.ollama = {
        #   # environmentVariables = {
        #   #   OLLAMA_LLM_LIBRARY = "cpu";
        #   #   HIP_VISIBLE_DEVICES = "0,1";
        #   # };
        # };

        system.activationScripts = {
          script.text = ''
            install -d -m 755 /home/open-webui/data -o root -g root
          '';
        };

        virtualisation.oci-containers.containers = {
          inherit open-webui;
        };
      };
    }
