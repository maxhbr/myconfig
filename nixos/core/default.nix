# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ hostName, hostId, otherImports ? [], ... }:

{
  imports = otherImports ++ [
    ./base.nix
    ../roles
  ] ++ (if builtins.pathExists (../machines + "/${hostName}.nix")
        then [(../machines + "/${hostName}.nix")]
        else []);

  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";

  boot.kernel.sysctl = {
    # "fs.inotify.max_user_watches" = 524288;
    "vm.swappiness" = 1;
  };
}
