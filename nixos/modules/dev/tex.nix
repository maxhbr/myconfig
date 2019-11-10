# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      (pkgs.texLiveAggregationFun {
        paths = [
          pkgs.texLive pkgs.texLiveExtra
          pkgs.texLiveBeamer
          pkgs.texLiveCMSuper
        ];
      })
    ];
  };
}
