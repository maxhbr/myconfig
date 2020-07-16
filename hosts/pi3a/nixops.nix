with (import ../lib.nix);
mkHostNixops "pi3a"
( {lib, ...}:
  { imports =
      [ (fixIp "pi3a" "eth0")
      ];
  }
)
