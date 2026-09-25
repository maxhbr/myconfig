# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  ...
}:

{
  config = {
    myconfig.gitolite.enable = true;

    # gitolite shares the host sshd (port 22) with regular admin SSH,
    # so the firewall cannot scope it. Deny the `gitolite` user for
    # clients outside the wg0 subnet (10.199.199.0/24, hosts/metadata.json)
    # instead; admin logins over other interfaces stay possible.
    services.openssh.extraConfig = ''
      Match User gitolite Address *,!10.199.199.0/24
        DenyUsers gitolite
    '';
  };
}
