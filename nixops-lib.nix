{ deployWireguardKeys = hostName:
    { deployment.keys =
        { wg-private =
            { text = builtins.readFile ./wireguard-keys/ + "${hostName}/private";
              user = "root";
              group = "root";
              permissions = "0400";
            };
          wg-public =
            { text = builtins.readFile ./wireguard-keys/ + "${hostName}/public";
              user = "root";
              group = "root";
              permissions = "0444";
            };
        };
    };
  deploySSHUserKeys = hostName: algo:
    { deployment.keys =
        { "${algo}" =
            { text = builtins.readFile ./wireguard-keys/ + "${hostName}/.ssh/${algo}";
              user = "mhuber";
              group = "mhuber";
              permissions = "0400";
            };
          "${algo}.pub" =
            { text = builtins.readFile ./ssh/ + "${hostName}/.ssh/${algo}.pub";
              user = "mhuber";
              group = "mhuber";
              permissions = "0444";
            };
        };
      systemd.services."${algo}-home-deploy" =
        { after = [ "${algo}-key.service" "${algo}.pub-key.service" ];
          wants = [ "${algo}-key.service" "${algo}.pub-key.service" ];
          script = ''
            mkdir -p /home/mhuber/.ssh
            chown mhuber:mhuber /home/mhuber/.ssh
            ln -s /run/keys/${algo} /home/mhuber/.ssh/${algo}
            ln -s /run/keys/${algo}.pub /home/mhuber/.ssh/${algo}.pub
          '';
        };
    };
};
