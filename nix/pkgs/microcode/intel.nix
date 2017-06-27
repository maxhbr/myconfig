{stdenv, pkgs, fetchurl}:
pkgs.microcodeIntel.overrideDerivation (super: rec {
  name = "microcode-intel-${version}";
  version = "20170511";

  src = fetchurl {
    url = "http://downloadmirror.intel.com/26798/eng/microcode-${version}.tgz";
    sha256 = "18w1ysklvkf4l9xgnl1wvhbgr3wbdaiphv56056pafs0hwnzsxrg";
  };
})
