{ pkgs, lib, config, ... }: {
  config = (lib.mkIf config.services.xserver.enable {
    nixpkgs = {
      overlays = [
        (final: prev:
          let
            stpre = (prev.st.overrideAttrs (oldAttrs: rec {
              patches = oldAttrs.patches ++ [
                # (prev.fetchpatch {
                #   url =
                #     "https://st.suckless.org/patches/solarized/st-solarized-both-0.8.4.diff";
                #   sha256 =
                #     "1nczrfgvl5ip95gm8qd9g3kam5xqbb5psqnpm563pbvrrw5d68hb";
                # })
                # (prev.fetchpatch {
                #   url =
                #     "https://st.suckless.org/patches/anysize/st-anysize-0.8.4.diff";
                #   sha256 =
                #     "2uxCn9HtSM44poIVOPm4YwGXynN50OIE/HcsnW2cHyo=";
                # })
              ];
            })).override { conf = builtins.readFile ./st-config.h; };
          in {
            st-notabbed = stpre;
            st-notmux = prev.writeShellScriptBin "st-notmux" ''
              if [ $# -eq 0 ]; then
                exec ${final.tabbed}/bin/tabbed -d -c -r 2 ${stpre}/bin/st
              else
                exec ${stpre}/bin/st "$@"
              fi
            '';
            st = prev.writeShellScriptBin "st" ''
              if [ $# -eq 0 ]; then
                exec ${final.tabbed}/bin/tabbed -d -c -r 2 ${stpre}/bin/st -w ’’ -e ${final.tmux}/bin/tmux
              else
                exec ${stpre}/bin/st "$@"
              fi
            '';
          })
      ];
    };
    environment.systemPackages = with pkgs; [
      tabbed
      st
      st-notmux
      (writeShellScriptBin "st-reattach" ''
        ${tmux}/bin/tmux ls |
            ${gnugrep}/bin/grep -v '(attached)' |
            cut -f 1 -d ":" |
            while read SESSION; do
                (set -x;
                 ${st-notabbed}/bin/st -e ${tmux}/bin/tmux attach -t "$SESSION" & disown)
            done
      '')
    ];
  });
}
