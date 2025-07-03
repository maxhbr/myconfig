{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  mybench =
    with pkgs;
    writeShellScriptBin "mybench" ''
      benchDir="$HOME/Desktop/mybench/$(hostname)"
      mkdir -p $benchDir
      ${geekbench}/bin/geekbench5 | tee $benchDir/geekbench5
      ${openssl}/bin/openssl speed | tee $benchDir/sslspeed
      ${glmark2}/bin/glmark2 --fullscreen --annotate | tee $benchDir/glmark2
    '';
in
{
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        mybench
        glmark2
      ];
    };
  };
}
