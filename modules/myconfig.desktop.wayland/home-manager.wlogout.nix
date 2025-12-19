{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = {
    programs.wlogout = {
      enable = true;
      layout = [
        {
          label = "suspend";
          action = "systemctl suspend";
          text = "Suspend";
          keybind = "u";
        }
        {
          label = "reboot";
          action = "systemctl reboot";
          text = "Reboot";
          keybind = "r";
        }
        {
          label = "shutdown";
          action = "systemctl poweroff";
          text = "Shutdown";
          keybind = "s";
        }
      ];
      style = ''
        * {
          background-image: none;
          box-shadow: none;
        }

        window {
          background-color: rgba(12, 12, 12, 0.9);
        }

        button {
          border-radius: 0;
          border-color: black;
          text-decoration-color: #FFFFFF;
          color: #FFFFFF;
          background-color: #1E1E1E;
          border-style: solid;
          border-width: 1px;
          background-repeat: no-repeat;
          background-position: center;
          background-size: 25%;
        }

        button:focus, button:active, button:hover {
          background-color: #ee9a00;
          outline-style: none;
        }

        /* Use the packaged icons if present */
        #lock     { background-image: image(url("${config.programs.wlogout.package}/share/wlogout/icons/lock.png")); }
        #logout   { background-image: image(url("${config.programs.wlogout.package}/share/wlogout/icons/logout.png")); }
        #suspend  { background-image: image(url("${config.programs.wlogout.package}/share/wlogout/icons/suspend.png")); }
        #reboot   { background-image: image(url("${config.programs.wlogout.package}/share/wlogout/icons/reboot.png")); }
        #shutdown { background-image: image(url("${config.programs.wlogout.package}/share/wlogout/icons/shutdown.png")); }
      '';
    };
  };
}
