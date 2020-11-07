{ pkgs, lib, config, ... }: {
  config = {
    nixpkgs = {
      overlays = [
        (final: prev: let
          stpre = (prev.st.overrideAttrs (oldAttrs: rec {
            patches = oldAttrs.patches ++ [
              (prev.fetchpatch {
                url = "https://st.suckless.org/patches/solarized/st-solarized-both-0.8.4.diff";
                sha256 = "1nczrfgvl5ip95gm8qd9g3kam5xqbb5psqnpm563pbvrrw5d68hb";
              })
            ];
          })).override {conf = builtins.readFile ./st-config.h;};
        in {
          st = prev.writeShellScriptBin "st" ''
if [ $# -eq 0 ]; then
  exec ${final.tabbed}/bin/tabbed -d -c -r 2 ${stpre}/bin/st -w ’’ -e ${final.tmux}/bin/tmux
else
  exec ${stpre}/bin/st "$@"
fi
'';
          st-notabbed = stpre;
        })
      ];
    };
    environment.systemPackages = [ pkgs.st ];
  };
}
