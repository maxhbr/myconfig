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
              user = "mhuber";
              group = "mhuber";
              permissions = "0400";
            };
          "id_${algo}.pub" =
            { text = builtins.readFile (secretsRoot + "/ssh/${hostName}/.ssh/id_${algo}.pub");
              user = "mhuber";
              group = "mhuber";
              permissions = "0444";
            };
        };
      systemd.services."id_${algo}-home-deploy" =
        { after = [ "id_${algo}-key.service" "id_${algo}.pub-key.service" ];
          wants = [ "id_${algo}-key.service" "id_${algo}.pub-key.service" ];
          script = ''
            mkdir -p /home/mhuber/.ssh
            chown mhuber:mhuber /home/mhuber/.ssh
            ln -s /run/keys/id_${algo} /home/mhuber/.ssh/id_${algo}
            ln -s /run/keys/id_${algo}.pub /home/mhuber/.ssh/id_${algo}.pub
          '';
        };
    };
}
