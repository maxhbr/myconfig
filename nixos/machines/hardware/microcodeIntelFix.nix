{ config, pkgs, ... }:

# due to: https://www.heise.de/newsticker/meldung/Bug-in-aktuellen-Intel-Prozessoren-macht-die-Runde-3755660.htm
let
version = "20170511";
in {
  nixpkgs.overlays = [ ( self: super: {
    microcodeIntel = super.microcodeIntel.overrideAttrs (oldAttrs: rec {
      inherit version;
      name = "microcode-intel-${version}";

      src = self.fetchurl {
        url = "http://downloadmirror.intel.com/26798/eng/microcode-${version}.tgz";
        sha256 = "18w1ysklvkf4l9xgnl1wvhbgr3wbdaiphv56056pafs0hwnzsxrg";
      };
    });
  })];
}
