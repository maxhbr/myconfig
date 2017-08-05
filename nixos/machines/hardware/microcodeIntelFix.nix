{ config, pkgs, ... }:

# due to: https://www.heise.de/newsticker/meldung/Bug-in-aktuellen-Intel-Prozessoren-macht-die-Runde-3755660.htm
{
  hardware.cpu.intel.updateMicrocode = true;
  # nixpkgs.overlays = [ ( self: super: {
  #   microcodeIntel = super.microcodeIntel.overrideAttrs (oldAttrs: rec {
  #     name = "microcode-intel-${version}";
  #     version = "20170511";

  #     src = self.fetchurl {
  #       url = "http://downloadmirror.intel.com/26798/eng/microcode-${version}.tgz";
  #       sha256 = "18w1ysklvkf4l9xgnl1wvhbgr3wbdaiphv56056pafs0hwnzsxrg";
  #     };
  #   });
  # })];
}
