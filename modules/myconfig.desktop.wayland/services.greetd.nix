{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
in {
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
    selectedSessions = cfg.desktop.wayland.selectedSessions;
    cmd_for_session = session:
      cfg.desktop.wayland.sessions."${session}".command;
    cmd0 = cmd_for_session (lib.elemAt selectedSessions 0);
    tuigreetSettings = {
      default_session = {
        command = "${
            lib.makeBinPath [ pkgs.greetd.tuigreet ]
          }/tuigreet --width 120 --remember --remember-session --sessions /etc/greetd/wayland-sessions/ --time --cmd '${cmd0}'";
        user = "greeter";
      };
    };
    cageGtkgreetSettings = {
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
  in (lib.mkIf (cfg.desktop.wayland.enable && selectedSessions != [ ]) {
    services.greetd = {
      enable = true;
      settings = if cfg.desktop.wayland.directLoginFirstSession then
                     directLaunchSettings
                else if cfg.desktop.wayland.selectedGreeter == "gtkgreet" then
                     cageGtkgreetSettings
                else if cfg.desktop.wayland.selectedGreeter == "tuigreet" then
                     tuigreetSettings
                else
                     abort "selectedGreeter is invalid";
    };

    environment.etc = {
      "greetd/environments".text =
        lib.foldr (session: str: (cmd_for_session session) + "\n" + str) "fish"
        selectedSessions;
    } // (let
      fun = session: {
        name = "greetd/wayland-sessions/${session}.desktop";
        value = { text = ''
[Desktop Entry]
Name=${session}
Comment=${session}
Exec=${cmd_for_session session}
Type=Application
         '';
       };
     };
      in builtins.listToAttrs (builtins.map fun selectedSessions));

    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          (writeShellScriptBin "regreet"
            "sudo systemctl restart greetd.service")
        ];
    }];
  });
}

