# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
{ config =
    { networking =
        { nat =
            { enable = true;
              internalInterfaces = ["ve-+"];
              externalInterface = "wlp82s0"; #"ens1u2u3u4u4";
            };
          networkmanager.unmanaged = [ "interface-name:ve-*" ];
        };
    };
}
