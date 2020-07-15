with (import ../../lib.nix);
mkHostNixops "pi4"
( {lib, ...}:
  { imports =
      [ (fixIp "workstation" "eth0")
        # ../../secrets/common/wifi.QS3j.nix
      ];
  }
)
