# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  host = myconfig.metadatalib.getWgIp config.networking.hostName;
in 
{
  services.qdrant = {
    enable = true;
    settings = {
      storage = {
        storage_path = "/var/lib/qdrant/storage";
        snapshots_path = "/var/lib/qdrant/snapshots";
      };
      service = {
        inherit host;
        http_port = 6333;
      };
      telemetry_disabled = true;
    };
  };

  environment.persistence = lib.mkMerge [
    {
      "/persistent/cache" = {
        directories = [
          "/var/lib/qdrant"
        ];
      };
    }
  ];
}
