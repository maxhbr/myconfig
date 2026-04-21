# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  ...
}:
{
  options.myconfig.ai.localModels = with lib; mkOption {
    type = types.listOf (
      types.submodule {
        options = {
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Provider alias for the local server instance (defaults to '<host>:<port>')";
          };
          models = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = "Model names served by this local server instance (defaults to [name] or ['<host>:<port>'])";
          };
          port = mkOption {
            type = types.int;
            description = "Port the local server is listening on";
          };
          host = mkOption {
            type = types.str;
            default = "localhost";
            description = "Host the local server is listening on";
          };
        };
      }
    );
    default = [ ];
    description = "List of local model server instances (e.g. llama-cpp) available for AI tools";
  };
}
