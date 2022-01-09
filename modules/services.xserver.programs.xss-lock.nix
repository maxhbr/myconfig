{ pkgs, config, lib, ... }:
let
  myXsecurelock = with pkgs;
    writeScriptBin "myXsecurelock" ''
      #!${stdenv.shell}
      export XSECURELOCK_SAVER=saver_blank
      export XSECURELOCK_PASSWORD_PROMPT=time
      ${xsecurelock}/bin/xsecurelock
    '';
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
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [{ home.packages = [ myStopScreensaver ]; }];
    programs.xss-lock = {
      enable = true;
      # lockerCommand = "${myXsecurelock}/bin/myXsecurelock";
      lockerCommand = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
      extraOptions =
        [ "-n" "${myXsecurelock}/libexec/xsecurelock/dimmer" "-l" ];
    };
  });
}
