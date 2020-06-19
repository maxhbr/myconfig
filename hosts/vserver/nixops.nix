with (import ../lib.nix);
mkHostNixops "vserver"
( {lib, ...}:
  { config =
      { deployment.targetHost = lib.mkDefault "10.199.199.1";
      };
    imports =
      [ (deployWireguardKeys "vserver")
        (deploySSHUserKeys "vserver" "rsa")
      ];
  }
)
