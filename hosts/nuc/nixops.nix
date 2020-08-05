with (import ../lib.nix);
mkHostNixops "nuc"
( {lib, ...}:
  { config =
      { deployment.targetHost = lib.mkDefault "10.199.199.9";
      };
    imports =
      [ (fixIp "nuc" "enp2s0")
        ../../secrets/common/wifi.QS3j.nix
      ];
  }
)
