{ pkgs, lib, config, ... }: {
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        st = (prev.st.overrideAttrs (oldAttrs: rec {
          conf = builtins.readFile ./st-config.h;
          patches = oldAttrs.patches ++ [
            (prev.fetchpatch {
              url = "https://st.suckless.org/patches/solarized/st-solarized-both-0.8.4.diff";
              sha256 = "1nczrfgvl5ip95gm8qd9g3kam5xqbb5psqnpm563pbvrrw5d68hb";
            })
          ];
        }));
      })
    ];
    # environment.systemPackages = with pkgs; [ st ];
    environment.systemPackages = with pkgs; [
      (st.override { conf = builtins.readFile ./st-config.h; })
    ];
  };
}
