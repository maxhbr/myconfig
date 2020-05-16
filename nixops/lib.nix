let
  secretsDir = ./secrets;
  importall =
    path:
    if builtins.pathExists path
      then let
          content = builtins.readDir path;
        in map (n: import (path + ("/" + n)))
          (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
              (builtins.attrNames content))
      else [];
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
        [ (../hosts + "/${hostName}")
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
        ] ++ (importall (secretsDir + "/${hostName}/imports"));
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
      users.extraUsers.nix-ssh =
        { openssh.authorizedKeys =
            { inherit keys; };
        };
    };

  setupAsBuildMachine =
    authorizedKeys:
    { users.extraUsers.nixBuild =
        { name = "nixBuild";
          useDefaultShell = true;
          openssh.authorizedKeys.keys = authorizedKeys;
        };
      nix =
        { allowedUsers = [ "nixBuild" ];
          trustedUsers = [ "nixBuild" ];
        };
    };

  setupBuildSlave =
    host:
    speedFactor:
    sshKeyText:
    publicKey:
    let
      keyName = "nixBuildPrivKey.${host}";
    in
    { deployment.keys =
        { "${keyName}" =
            { text = sshKeyText;
              user = "mhuber";
              group = "keys";
              permissions = "0400";
            };
        };
      nix.buildMachines =
        [{ hostName = host;
           system = "x86_64-linux";
           maxJobs = 6;
           supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
           mandatoryFeatures = [ ];
           sshUser = "nixBuild";
           sshKey = "/run/keys/${keyName}";
           inherit speedFactor;
        }];
      nix.distributedBuilds = true;
      # optional, useful when the builder has a faster internet connection than yours
      nix.extraOptions = ''
        builders-use-substitutes = true
      '';
      services.openssh.knownHosts =
        { "${host}" =
            { inherit publicKey;
            };
        };
      programs.ssh.extraConfig = ''
        Host builder.${host}
             HostName ${host}
             User nixBuild
             IdentitiesOnly yes
             IdentityFile /run/keys/${keyName}
             StrictHostKeyChecking accept-new
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
