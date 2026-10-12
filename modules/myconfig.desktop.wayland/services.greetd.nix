{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig;
  user = myconfig.user;
  selectedSessions = cfg.desktop.wayland.selectedSessions;
  persistentCacheDir = "/persistent/cache";
  cmd_for_session = session: cfg.desktop.wayland.sessions."${session}".command;
  cmd0 = if selectedSessions == [ ] then "tmux" else cmd_for_session (lib.elemAt selectedSessions 0);
  sessionStarters = pkgs.symlinkJoin {
    name = "sessionStarters";
    paths =
      let
        fun = session: pkgs.writeShellScriptBin "start-${session}-session" (cmd_for_session session);
      in
      builtins.map fun selectedSessions;
  };
in
{
  imports = [
    (lib.mkIf config.services.greetd.enable {
      home-manager.sharedModules = [
        {
          home.packages = with pkgs; [
            (writeShellScriptBin "regreet" "sudo systemctl restart greetd.service")
          ];
        }
      ];
      services.greetd =
        let
          tuigreetSettings = {
            default_session = {
              command = "${pkgs.tuigreet}/bin/tuigreet --width 120 --remember --remember-session --sessions /etc/greetd/wayland-sessions/ --time --background doom --cmd '${cmd0}'";
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
        in
        {
          settings =
            if cfg.desktop.wayland.directLoginFirstSession then
              directLaunchSettings
            else if cfg.desktop.wayland.selectedGreeter == "gtkgreet" then
              cageGtkgreetSettings
            else if cfg.desktop.wayland.selectedGreeter == "tuigreet" then
              tuigreetSettings
            else
              abort "selectedGreeter is invalid";
        };
    })
  ];
  config = (
    lib.mkIf (cfg.desktop.wayland.enable && selectedSessions != [ ]) {

      system.nixos.tags = [ (lib.elemAt selectedSessions 0) ];

      home-manager.sharedModules = [ { home.packages = with pkgs; [ sessionStarters ]; } ];

      # tuigreet stores its remembered session/user state in
      # /var/cache/tuigreet/{lastsession,lastuser,...}, which lives on the
      # tmpfs root on impermanence hosts and would be wiped on every boot.
      environment.persistence."${persistentCacheDir}" =
        lib.mkIf config.myconfig.persistence.impermanence.enable
          {
            directories = [
              {
                directory = "/var/cache/tuigreet";
                user = "greeter";
                group = "greeter";
                mode = "0755";
              }
            ];
          };

      services.greetd = {
        enable = true;
      };

      environment.etc = {
        "greetd/environments".text = lib.foldr (
          session: str:
          ''
            start-${session}-session
          ''
          + str
        ) "fish" selectedSessions;
        "greetd/wayland-sessions/tmux.desktop".text = ''
          [Desktop Entry]
          Name=tmux
          Comment=tmux in foot in cage
          Exec=${pkgs.cage}/bin/cage -s -- foot-greetd 
          Type=Application
        '';
      }
      // (
        let
          fun = session: {
            name = "greetd/wayland-sessions/${session}.desktop";
            value = {
              text = ''
                [Desktop Entry]
                Name=${session}
                Comment=${session}
                Exec=${cmd_for_session session}
                Type=Application
              '';
            };
          };
        in
        builtins.listToAttrs (builtins.map fun selectedSessions)
      );
    }
  );
}
