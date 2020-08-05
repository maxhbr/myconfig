with (import ../lib.nix);
mkHostNixops "nuc"
( {lib, ...}:
  { config =
      {
      };
    imports =
      [ (fixIp "nuc" "enp2s0")
        ../../secrets/common/wifi.QS3j.nix
      ];
  }
)
