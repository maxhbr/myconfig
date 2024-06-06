{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
  options.myconfig = with lib; {
    desktop.wayland = {
      sessions = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = lib.mdDoc ''
          List of greet desktop environments, fist will be default
        '';
      };
      directLoginFirstSession = mkOption {
        type = types.bool;
        default = false;
      };
      greetdSettings = let settingsFormat = pkgs.formats.toml { };
      in mkOption {
        type = settingsFormat.type;
        example = literalExpression ''
          {
            sway = {
              command = "''${pkgs.greetd.greetd}/bin/agreety --cmd sway";
            };
          }
        '';
        description = lib.mdDoc ''
          greetd configuration ([documentation](https://man.sr.ht/~kennylevinsen/greetd/))
          as a Nix attribute set.
        '';
      };
    };
  };
  imports = [
    # (lib.mkIf config.services.greetd.enable {
    #   services.greetd = {
    #     settings = let
    #       chosen_session =
    #         cfg.desktop.wayland.greetdSettings."${cfg.desktop.wayland.desktop}_session";
    #     in cfg.desktop.wayland.greetdSettings // {
    #       enable =  true;
    #       default_session = {
    #         command = "${
    #             lib.makeBinPath [ pkgs.greetd.tuigreet ]
    #           }/tuigreet --width 120 --time --cmd '${chosen_session.command}'";
    #         user = "greeter";
    #       };
    #       # initial_session = chosen_session;
    #     };
    #   };
    # })
  ];
  config = let
    sessions = cfg.desktop.wayland.sessions;
    cmd_for_session = session:
      cfg.desktop.wayland.greetdSettings."${session}_session".command;
    cmd0 = cmd_for_session (lib.elemAt sessions 0);
    cageSettings = {
      default_session.command = "${pkgs.cage}/bin/cage -s  -- ${pkgs.greetd.gtkgreet}/bin/gtkgreet -l";
      initial_session = {
        command = cmd0;
        user = "greeter";
      };
    };
    directLaunchSettings = rec {
      initial_session = {
        command = cmd0;
        user = user;
      };
      default_session = initial_session;
    };
  in (lib.mkIf (cfg.desktop.wayland.enable && sessions != [ ]) {
    services.greetd = {
      enable = true;
      settings = if cfg.desktop.wayland.directLoginFirstSession
                  then directLaunchSettings
                  else cageSettings;
    };

    environment.etc."greetd/environments".text =
      lib.foldr (session: str: (cmd_for_session session) + "\n" + str) "fish"
      sessions;

    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          (writeShellScriptBin "regreet"
            "sudo systemctl restart greetd.service")
        ];
    }];
  });
}

