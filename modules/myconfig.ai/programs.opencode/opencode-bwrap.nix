{
  lib,
  opencode,
  bubblewrap,
  writeShellApplication,
}:

writeShellApplication {
  name = "opencode-bwrap";

  runtimeInputs = [
    opencode
    bubblewrap
  ];

  text = builtins.fileContents ./opencode-bwrap;

  meta = with lib; {
    description = "Sandboxed opencode wrapper using bubblewrap";
    license = licenses.unfree;
    maintainers = [ ];
    platforms = platforms.linux;
  };
}