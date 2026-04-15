{
  pkgs,
  suppressExperimentalWarnings ? false,
  ...
}:
jail:
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
        acc: names: combinator:
        if lib.length names == 0 then
          acc
        else
          let
            name = lib.head names;
            combinatorImpl =
              if combinator.experimental or false && !suppressExperimentalWarnings then
                builtins.warn "jail.nix combinator ${name} is experimental!" combinator.impl
              else
                combinator.impl;
          in
          if acc ? ${name} then
            throw "duplicate combinator ${name}"
          else
            foldCombinator (acc // { ${name} = combinatorImpl; }) (lib.tail names) combinator;
    in
    lib.foldl' (acc: { name, value }: foldCombinator acc ([ name ] ++ value.aliases or [ ]) value) { } (
      lib.attrsToList rawCombinators
    );
in
{
  inherit combinators;
  docs = rawCombinators;
}
