{ combinators, lib, ... }:
let
  inherit (combinators) compose add-path;
in
{
  sig = "[Package] -> Permission";
  doc = ''
    Adds the packages' `bin` directory to `$PATH`.
  '';
  impl =
    pkgsToAdd:
    compose (
      builtins.map (pkg: add-path "${lib.getBin pkg}/bin") pkgsToAdd
      ++ [
        (
          state:
          state
          // {
            additionalRuntimeClosures = state.additionalRuntimeClosures ++ (map toString pkgsToAdd);
          }
        )
      ]
    );
}
