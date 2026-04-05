{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  hostConfig = config;
  stateDir = "/home/mhuber/hermes-agent";

  cfg = config.myconfig.ai.hermes;
  thingIp = myconfig.metadatalib.getIp "thing";

  apiServerHost = if cfg.container.enable then config.containers.hermes.localAddress else "localhost";
  hermesServiceCfg = {
    enable = true;
    user = "mhuber";
    group = "mhuber";
    createUser = false;
    stateDir = stateDir;
    settings = {
      model = {
        default = "hermes";
        provider = "custom";
        base_url = "http://${thingIp}:33656/v1";
        api_key = "local-key";
      };
      fallback_model = {
        default = "hermes-fallback";
        provider = "custom";
        base_url = "http://${thingIp}:33656/v1";
        api_key = "local-key";
      };
      compression = {
        summary_provider = hermesServiceCfg.settings.model.provider;
        summary_model = hermesServiceCfg.settings.model.default;
      };
      custom_providers =
        let
          custom_local_providers = lib.concatMap (
            provider:
            let
              hostPort = "${provider.host}:${toString provider.port}";
              providerName = if provider.name != null then provider.name else hostPort;
              modelNames = if provider.models != [ ] then provider.models else [ providerName ];
            in
            lib.map (modelName: {
              name = "${providerName} / ${modelName}";
              base_url = "http://${hostPort}/v1";
              model = modelName;
              api_key = "local-key";
            }) modelNames
          ) config.myconfig.ai.localModels;
        in
        custom_local_providers;
      terminal.backend = "local";
      compression = {
        enabled = true;
        threshold = 0.85;
      };
      toolsets = [ "all" ];
      telegram = {
        require_mention = true;
      };
    };
    extraPackages = with pkgs; [ openhue-cli ];
    environmentFiles =
      let
        hermes-api-env = (
          pkgs.writeText "hermes-api-env" ''
            OPENAI_API_KEY=local-key
            API_SERVER_ENABLED=true
            API_SERVER_PORT=8642
            API_SERVER_HOST=${apiServerHost}
            HASS_URL=http://localhost:8123
          ''
        );
      in
      [
        "/home/mhuber/.hermes-secrets/env"
        "${hermes-api-env}"
      ];
    addToSystemPackages = true;
  };
in
{
  options = {
    myconfig.ai.hermes = {
      enable = lib.mkEnableOption "Hermes agent configuration";
      container = {
        enable = lib.mkEnableOption "Hermes gateway container";
        autostart = lib.mkEnableOption "Autostart Hermes gateway container";
      };
    };
  };

  imports = [
    inputs.hermes-agent.nixosModules.default
  ];

  config = lib.mkIf cfg.enable {
    myconfig.persistence.directories = [ "hermes-agent" ];
    environment.sessionVariables = {
      HERMES_HOME = "${stateDir}/.hermes";
    };
    home-manager.users.mhuber =
      { pkgs, ... }:
      {
        home.packages = [
          inputs.hermes-agent.packages.${pkgs.system}.default
        ];
      };
    services.hermes-agent = lib.mkIf (!cfg.container.enable) hermesServiceCfg;
    containers.hermes = lib.mkIf cfg.container.enable {
      autoStart = cfg.container.autostart;
      privateNetwork = true;
      hostAddress = "192.168.111.10";
      localAddress = "192.168.111.11";
      hostAddress6 = "fc00::1";
      localAddress6 = "fc00::2";
      bindMounts = {
        "${stateDir}" = {
          hostPath = stateDir;
          mountPoint = stateDir;
          isReadOnly = false;
        };
        "/home/mhuber/.hermes-secrets" = {
          hostPath = "/home/mhuber/.hermes-secrets";
          mountPoint = "/home/mhuber/.hermes-secrets";
          isReadOnly = false;
        };
      };

      config =
        {
          config,
          pkgs,
          lib,
          ...
        }:
        let
          containerConfig = config;
        in
        {
          imports = [
            inputs.hermes-agent.nixosModules.default
            inputs.home.nixosModules.home-manager
          ];

          services.hermes-agent = hermesServiceCfg;

          users.users.mhuber = lib.mkForce {
            isNormalUser = true;
            home = "/home/mhuber";
            createHome = true;
            uid = hostConfig.users.users.mhuber.uid or 1000;
            extraGroups = [ "mhuber" ];
          };
          users.groups.mhuber = { };

          home-manager.users.mhuber =
            { pkgs, ... }:
            {
              imports = [
              ];

              home.stateVersion = containerConfig.system.stateVersion;
            };

          system.stateVersion = "25.11";

          networking = {
            firewall = {
              enable = true;
              # allowedTCPPorts = [ 80 ];
            };
            # Use systemd-resolved inside the container
            # Workaround for bug https://github.com/NixOS/nixpkgs/issues/162686
            useHostResolvConf = lib.mkForce false;
          };

          services.resolved.enable = true;

        };
    };
    networking.firewall.interfaces."ve-hermes@if2".allowedTCPPorts = [ 33656 ];
    networking.firewall.interfaces."ve-hermes@if2".allowedUDPPorts = [ 33656 ];
  };
}
