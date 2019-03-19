# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs }:
{
  tex = pkgs.texLiveAggregationFun { paths = [pkgs.texLive pkgs.texLiveExtra pkgs.texLiveBeamer pkgs.texLiveCMSuper]; };
}
