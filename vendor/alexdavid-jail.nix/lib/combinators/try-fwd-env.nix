{
  combinators,
  helpers,
  pkgs,
  ...
}:
let
  inherit (combinators) unsafe-add-raw-args;
in
{
  sig = "String -> Permission";
  doc = ''
    Forwards the specified environment variable to the underlying process (if set).
  '';
  impl =
    name:
    assert pkgs.lib.isValidPosixName name;
    unsafe-add-raw-args "\${${name}+--setenv ${name} \"\${${name}}\"}";
}
