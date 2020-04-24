let
  secrets = import ./vserver-nixops-secrets.nix;
in {
  network.description = "vserver";
  vserver =
    { config, pkgs, ... }:
    (import ./default.nix { inherit config pkgs; }) //
    { config =
        { deployment =
            { inherit (secrets) targetHost;
              targetEnv = "none";
            };
          users.extraUsers.root = {
            openssh.authorizedKeys.keys = config.users.extraUsers.mhuber.openssh.authorizedKeys.keys;
          };
        };
    };
}
