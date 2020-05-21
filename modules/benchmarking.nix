{ pkgs, ... }:
let
  blender-benchmark-launcher-cli = pkgs.callPackage ../pkgs/blender-benchmark-launcher-cli {};
  blender-benchmark-launcher = pkgs.callPackage ../pkgs/blender-benchmark-launcher {};
  mybench = with pkgs; writeShellScriptBin "mybench" ''
    benchDir="$HOME/Desktop/mybench/$(hostname)"
    mkdir -p $benchDir
    ${geekbench}/bin/geekbench5 | tee $benchDir/geekbench5
    ${openssl}/bin/openssl speed | tee $benchDir/sslspeed
  '';
in {
  config = {
    home-manager.users.mhuber = {
      home.packages =
        with pkgs;
        [ mybench
        ];
    };
  };
}
