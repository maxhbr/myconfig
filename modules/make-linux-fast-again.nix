{ lib, ... }:

# disable spectre and meltdown mitigations in the kernel
# based on: https://github.com/nrdxp/nixflk/blob/template/modules/security/mitigations.nix

let
  cmdline = with builtins; readFile (fetchurl {
    url = "https://make-linux-fast-again.com";
    sha256 = "sha256:10diw5xn5jjx79nvyjqcpdpcqihnr3y0756fsgiv1nq7w28ph9w6";
  });
in {
  config = {
    boot.kernelParams = lib.splitString " " cmdline;
  };
}
