# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  config,
  ...
}:
{
  options.myconfig.ai.localModels =
    with lib;
    mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Provider alias for the local server instance (defaults to '<host>:<port>')";
            };
            models = mkOption {
              type = types.listOf (
                types.oneOf [
                  types.str
                  (types.submodule {
                    options = {
                      name = mkOption {
                        type = types.str;
                        description = "Model name";
                      };
                      kind = mkOption {
                        type = types.nullOr (
                          types.enum [
                            "base"
                            "variant"
                            "alias"
                          ]
                        );
                        default = null;
                        description = ''
                          Derived classification of this model entry:
                            - "base":    an unpacked top-level model entry (as declared by
                                         the user in `myconfig.ai.llama-cpp.models`).
                            - "variant": an entry synthesized from a `variants.<n>` block;
                                         its `name` is `<base>-<n>`.
                            - "alias":   an alternative name for a base/variant model, with
                                         no llama-cpp entry of its own.
                            - null:      classification unknown (e.g. for upstream provider
                                         lists that don't carry this information).
                          This field is computed by the publisher (router.nix /
                          llama-swap.nix) and should not be set by hand.
                        '';
                      };
                    };
                  })
                ]
              );
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
  config = lib.mkIf (config.myconfig.ai.localModels != [ ]) {
    systemd.tmpfiles.rules = [
      "d /run/myconfig 0755 root root - -"
      (
        let
          localModelsJson = builtins.toJSON config.myconfig.ai.localModels;
        in
        "f /run/myconfig/localModels.json 0644 root root - ${localModelsJson}"
      )
    ];
  };
}
