# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:
let
  path = ./extrahosts;
in
{
  config =
    if builtins.pathExists path then
      {
        networking.extraHosts = builtins.readFile path;
      }
    else
      { };
}
