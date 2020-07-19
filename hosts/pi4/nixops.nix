with (import ../lib.nix);
mkHostNixops "pi4"
( {lib, ...}:
  { imports =
      [ (fixIp "pi4" "eth0")
        (deployWireguardKeys "pi4")
        (setupNixServe
          [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
            (getSecret "vserver" "ssh/id_rsa.pub")
          ])
        (setupAsBuildMachine
          [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
          ])
      ];
  }
)
