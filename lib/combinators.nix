pkgs: jail:
let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix pkgs;

  rawCombinators = lib.mapAttrs' (
    fileName: _:
    lib.nameValuePair (lib.removeSuffix ".nix" fileName) (
      import ./combinators/${fileName} {
        inherit
          combinators
          helpers
          jail
          lib
          pkgs
          ;
      }
    )
  ) (builtins.readDir ./combinators);

  combinators =
    let
      foldCombinator =
        acc: names: combinatorImpl:
        if lib.length names == 0 then
          acc
        else
          let
            name = lib.head names;
          in
          if acc ? ${name} then
            throw "duplicate combinator ${name}"
          else
            foldCombinator (acc // { ${name} = combinatorImpl; }) (lib.tail names) combinatorImpl;
    in
    lib.foldl' (
      acc: { name, value }: foldCombinator acc ([ name ] ++ value.aliases or [ ]) value.impl
    ) { } (lib.attrsToList rawCombinators);
in
{
  inherit combinators;
  docs = rawCombinators;
}
