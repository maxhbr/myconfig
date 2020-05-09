{ deployWireguardKeys =   secretsRoot: hostName:
    { deployment.keys =
        { wg-private =
            { text = builtins.readFile (secretsRoot + "/wireguard-keys/${hostName}/private");
              user = "root";
              group = "root";
              permissions = "0400";
            };
          wg-public =
            { text = builtins.readFile (secretsRoot + "/wireguard-keys/${hostName}/public");
              user = "root";
              group = "root";
              permissions = "0444";
            };
        };
    };
  deploySSHUserKeys = secretsRoot: hostName: algo:
    { deployment.keys =
        { "id_${algo}" =
            { text = builtins.readFile (secretsRoot + "/ssh/${hostName}/.ssh/id_${algo}");
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0400";
            };
          "id_${algo}.pub" =
            { text = builtins.readFile (secretsRoot + "/ssh/${hostName}/.ssh/id_${algo}.pub");
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0444";
            };
        };
    };
}
