{ stdenv, pkg-config, hidapi, pkgs }:
stdenv.mkDerivation {
  name = "footswitch";
  src = builtins.fetchGit {
    url = "https://github.com/rgerganov/footswitch.git";
    rev = "7b89a3bdefbdf62bbdbffd080dfbd3936f4326b7";
    ref = "master";
  };
  buildInputs = [ pkg-config hidapi ];
  buildPhase = ''
    make
  '';
  installPhase = ''
    install -Dm755 footswitch "$out/bin/footswitch"
    install -Dm755 scythe "$out/bin/scythe"
    install -Dm644 19-footswitch.rules "$out/etc/udev/rules.d/19-footswitch.rules"
  '';

  meta = with pkgs.lib; {
    homepage = "https://github.com/rgerganov/footswitch";
    description =
      "Command line utlities for programming PCsensor and Scythe foot switches.";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = [ ];
  };
}
