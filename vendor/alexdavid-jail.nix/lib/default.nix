let
  jailNix = import ./jail.nix;
in
{
  init = pkgs: jailNix { inherit pkgs; };
  extend = jailNix;

  mkOverlay =
    opts@{
      final,
      prev,
      packages ? _: { },
      ...
    }:
    let
      fwdOptsKey = key: if opts ? ${key} then { ${key} = opts.${key}; } else { };
      jail = jailNix (
        {
          pkgs = prev;
        }
        // fwdOptsKey "additionalCombinators"
        // fwdOptsKey "basePermissions"
        // fwdOptsKey "bubblewrapPackage"
      );
      wrap = prev.lib.mapAttrs (
        name: opts:
        let
          unjailed = prev.${name};
          jailed = jail name unjailed opts;
        in
        jailed // { inherit jailed unjailed; }
      );
    in
    wrap (packages jail.combinators);
}
