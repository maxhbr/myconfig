# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig;
  pkg = pkgs.master.labwc;
  user = myconfig.user;
in
{
  config = (
    lib.mkIf (cfg.desktop.wayland.enable && builtins.elem "labwc" cfg.desktop.wayland.selectedSessions)
      {
        home-manager.sharedModules = [
          {
            xdg.configFile = {
              "labwc/rc.xml" = {
                source = ./labwc/rc.xml;
                onChange = "${pkgs.psmisc}/bin/killall -s SIGHUP labwc || true";
              };
              "labwc/autostart" = {
                source =
                  (pkgs.writeShellScriptBin "autostart.sh" ''
                    set -x
                    ${cfg.desktop.wayland.autostartCommands}
                    waybarOnce labwc &disown
                    dbus-wm-environment labwc
                  '')
                  + "/bin/autostart.sh";
                executable = true;
              };
              "labwc/environment".text = ''
                XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
                XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
              '';
              "labwc/menu.xml".text = ''
                <?xml version="1.0" ?>

                <openbox_menu>
                <menu id="root-menu" label="">
                  <item label="Web browser"><action name="Execute" command="firefox" /></item>
                  <item label="Terminal"><action name="Execute" command="tfoot" /></item>
                  <item label="plain-Terminal"><action name="Execute" command="foot" /></item>
                  <item label="re-Terminal"><action name="Execute" command="tfoot-reattach" /></item>
                  <item label="Screenshot"><action name="Execute" command="grim-region" /></item>
                  <item label="Displays"><action name="Execute" command="${pkgs.wdisplays}/bin/wdisplays" /></item>
                  <menu id="labwc-menu" label="labwc">
                    <item label="Reconfigure"><action name="Reconfigure" /></item>
                    <item label="Exit"><action name="Exit" /></item>
                  </menu>
                </menu>
                </openbox_menu>
              '';
            };
            home.packages = [ pkg ];
          }
        ];
        services.xserver.windowManager.session = lib.singleton {
          name = "labwc";
          start = "${pkg}/bin/labwc";
        };
        myconfig.desktop.wayland.sessions = {
          labwc = {
            command = "${pkg}/bin/labwc";
          };
        };
      }
  );
}
