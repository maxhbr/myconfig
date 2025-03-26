# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, inputs, ... }: {
  imports = [
    ../../hardware/eGPU.nix
  ];

  config = {
    services.ollama = {
      enable = true;
      # listenAddress = "0.0.0.0:11434";
      acceleration = "cuda";
      # environmentVariables = {
      #   OLLAMA_LLM_LIBRARY = "cpu";
      #   HIP_VISIBLE_DEVICES = "0,1";
      # };
    };
  };
}
