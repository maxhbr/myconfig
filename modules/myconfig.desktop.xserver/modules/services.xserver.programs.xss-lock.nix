{ pkgs, config, lib, ... }:
let
  xsecurelockmodule = let
    myXsecurelock = with pkgs;
      writeScriptBin "myXsecurelock" ''
        #!${stdenv.shell}
        export XSECURELOCK_SAVER=saver_blank
        export XSECURELOCK_PASSWORD_PROMPT=time
        ${xsecurelock}/bin/xsecurelock
      '';
  in {
    config = {
      programs.xss-lock = {
        lockerCommand = "${myXsecurelock}/bin/myXsecurelock";
        extraOptions =
          [ "-n" "${myXsecurelock}/libexec/xsecurelock/dimmer" "-l" ];
      };
      home-manager.sharedModules = [{
        services = {
          screen-locker.lockCmd = "${myXsecurelock}/bin/myXsecurelock";
          xss-lock.extraOptions =
            [ "-n" "${myXsecurelock}/libexec/xsecurelock/dimmer" "-l" ];
        };
      }];
    };
  };

  physlockmodule = { config, ... }: {
    # physlock is deprecated: https://github.com/xyb3rt/physlock
    services.physlock.enable = lib.mkForce false;
  #   config = lib.mkIf config.services.physlock.enable {
  #     services.physlock = { allowAnyUser = true; };
  #     programs.xss-lock = {
  #       lockerCommand = "${config.security.wrapperDir}/physlock";
  #     };
  #     home-manager.sharedModules = [{
  #       services = {
  #         screen-locker.lockCmd = "${config.security.wrapperDir}/physlock";
  #       };
  #     }];
  #   };
  };

  myStopScreensaver = with pkgs;
    writeScriptBin "myStopScreensaver" ''
      #!${stdenv.shell}
      printf "run: "
      while true; do
          printf "."
          sleep $((60 * 4))
          ${xdotool}/bin/xdotool key shift
      done
    '';

in {
  imports = [ physlockmodule ];
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [{
      home.packages = [ myStopScreensaver ];
      services.screen-locker = {
        enable = true;
        inactiveInterval = 60;
        xss-lock.screensaverCycle = 3600;
      };
    }];
    # programs.xss-lock = { enable = true; };
  });
}
