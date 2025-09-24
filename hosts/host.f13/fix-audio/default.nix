let
  hm = {pkgs, ...}: {
    home.packages = let
      src = ./fix-audio.sh;
      binName = "fix-audio";
      deps = [
        bash
        pulseaudio
      ];
      # package script with dependencies
      fix-audio = runCommand "${binName}"
        {
          nativeBuildInputs = [ makeWrapper ];
          meta = {
            mainProgram = "${binName}";
          };
        }
        ''
          mkdir -p $out/bin
          install -m +x ${src} $out/bin/${binName}

          wrapProgram $out/bin/${binName} \
            --prefix PATH : ${lib.makeBinPath deps}
        ''
    in [ fix-audio ];
  };
in
{
  ...
}:
{
  config = {
    home-manager.sharedModules = [ hm ];
  };
}