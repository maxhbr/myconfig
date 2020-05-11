let
  secretsDir = ./secrets;
in
rec
{ getSecret =
    hostName:
    fileName:
    builtins.readFile (secretsDir + "/${hostName}/${fileName}");
  mkHost =
    hostName:
    addConfig:
    { config, lib, ... }@args:
    { imports =
        [ (../nixos/host- + hostName)
          (secretsDir + "/${hostName}")
          (addConfig args)
          { deployment =
              let
                ipFile = (secretsDir + "/${hostName}/ip");
              in lib.mkIf (builtins.pathExists ipFile)
              { targetHost = builtins.readFile ipFile;
              };
          }
          { assertions =
              [ { assertion = config.networking.hostName == hostName;
                  message = "hostname should be set!";
                }
                # { assertion = secretsConfig.users.users.mhuber.hashedPassword != null;
                #   message = "password should be overwritten in ./nixops-secrets.nix";
                # }
              ];
          }
        ];
    };
  deployWireguardKeys = hostName:
    { deployment.keys =
        { wg-private =
            { text = getSecret hostName "wireguard-keys/private";
              user = "root";
              group = "root";
              permissions = "0400";
            };
          wg-public =
            { text = getSecret hostName "wireguard-keys/public";
              user = "root";
              group = "root";
              permissions = "0444";
            };
        };
    };
  deploySSHUserKeys = hostName: algo:
    { deployment.keys =
        { "id_${algo}" =
            { text = getSecret hostName "ssh/id_${algo}";
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0400";
            };
          "id_${algo}.pub" =
            { text = getSecret hostName "ssh/id_${algo}.pub";
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0444";
            };
        };
    };
  setupNixServe =
    keys:
    { nix.sshServe =
        { enable = true;
          inherit keys;
        };
      services.openssh.extraConfig =
        ''
        Match User nix-ssh
          AllowAgentForwarding no
          AllowTcpForwarding no
          PermitTTY no
          PermitTunnel no
          X11Forwarding no
          ForceCommand nix-store --serve
        Match All
        '';
    };
  # # generate with:
  # # $ nix-store --generate-binary-cache-key binarycache.example.com cache-priv-key.pem cache-pub-key.pem
  # # see:
  # # - https://nixos.wiki/wiki/Binary_Cache
  # deployBinaryCacheKeys = hostName:
  #   let
  #     keyName = "cache-priv-key.pem";
  #   in
  #   { deployment.keys =
  #       { "${keyName}" =
  #           { text = builtins.readFile (secretsDir + "/${hostName}/binary-cache-keys/${keyName}");
  #             user = "nix-serve";
  #             group = "nix-serve";
  #             permissions = "0400";
  #           };
  #       };
  #     # services.nix-serve =
  #     #   { enable = true;
  #     #     secretKeyFile = "/run/keys/${keyName}";
  #     #   };
  #   };
}
