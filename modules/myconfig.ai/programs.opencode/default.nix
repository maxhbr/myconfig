{
  config,
  lib,
  pkgs,
  ...
}:
let
  osconfig = config;
in
{
  options.myconfig = with lib; {
    ai.opencode = {
      enable = mkEnableOption "myconfig.ai.opencode";
      localModels = mkOption {
        type = types.listOf (
          types.submodule {
            options = {
              name = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Model name for the llama-cpp server instance (defaults to 'llama-cpp-<port>')";
              };
              port = mkOption {
                type = types.int;
                description = "Port the llama-cpp server is listening on";
              };
              host = mkOption {
                type = types.str;
                default = "localhost";
                description = "Host the llama-cpp server is listening on";
              };
            };
          }
        );
        default = [ ];
        description = "List of local llama-cpp server instances to configure as opencode providers";
      };
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          callLib = file: import file { inherit lib pkgs; };
          opencodeBwrap = callLib ../fns/sandboxed-app.nix {
            name = "opencode";
            pkg = config.programs.opencode.package;
            writableDirs = [
              ".config/opencode"
              ".config/mcp"
            ];
          };
        in
        {
          programs.opencode = {
            enable = true;
            enableMcpIntegration = true;
            web.enable = true;
            settings = {
              "autoupdate" = false;
              "share" = "disabled";
              "permission" = {
                "bash" = "ask";
                "edit" = "ask";
              };
              "provider" = lib.mkMerge [
                (lib.mkIf (osconfig.myconfig.ai.opencode.localModels != [ ]) (
                  builtins.listToAttrs (
                    lib.map (
                      model:
                      let
                        modelName =
                          if model.name != null then model.name else "localhost:${toString model.port}";
                      in
                      {
                        name = "local-${modelName}";
                        value = {
                          "npm" = "@ai-sdk/openai-compatible";
                          "name" = "provider:${modelName}";
                          "options" = {
                            "baseURL" = "http://${model.host}:${toString model.port}/v1";
                          };
                          "models" = {
                            "${modelName}" = {
                              "name" = modelName;
                            };
                          };
                        };
                      }
                    ) osconfig.myconfig.ai.opencode.localModels
                  )
                ))
                (lib.mkIf osconfig.services.litellm.enable (
                  let
                    opencodeModels = builtins.listToAttrs (
                      lib.map (model: {
                        name = model.model_name;
                        value = {
                          "name" = model.model_name;
                        };
                      }) osconfig.services.litellm.settings.model_list
                    );
                  in
                  {
                    "litellm" = {
                      "npm" = "@ai-sdk/openai-compatible";
                      "name" = "LiteLLM";
                      "options" = {
                        "baseURL" =
                          "http://${osconfig.services.litellm.host}:${toString osconfig.services.litellm.port}/v1";
                      };
                      "models" = opencodeModels;
                    };
                  }
                ))
                (lib.mkIf osconfig.services.ollama.enable (
                  let
                    ollamaModels = builtins.listToAttrs (
                      lib.map (model: {
                        name = model;
                        value = {
                          "name" = model;
                        };
                      }) osconfig.services.ollama.loadModels
                    );
                  in
                  {
                    "ollama" = {
                      "npm" = "@ai-sdk/openai-compatible";
                      "name" = "Ollama";
                      "options" = {
                        "baseURL" = "http://${osconfig.services.ollama.host}:${toString osconfig.services.ollama.port}/v1";
                      };
                      "models" = ollamaModels;
                    };
                  }
                ))
              ];
              "disabled_providers" = [
                "opencode"
              ];
              ## TODO: overwriting with `lib.mkForce` does not work here
              #   permission = lib.mkForce {
              #     "bash" = {
              #       "*" = "ask";
              #       "head *" = "allow";
              #       "go build *" = "allow";
              #       "go test *" = "allow";
              #       "go generate *" = "allow";
              #       "go fmt *" = "allow";
              #       "go vet *" = "allow";
              #       "npm run dev *" = "allow";
              #       "npm run build *" = "allow";
              #       "npm run lint *" = "allow";
              #       "npm test *" = "allow";
              #       "ls *" = "allow";
              #       "grep *" = "allow";
              #       "rg *" = "allow";
              #       "find *" = "allow";
              #       "mkdir *" = "allow";
              #     };
              #     "edit" = "ask";
              #   };
            };
            agents = {
              code-reviewer = ''
                # Code Reviewer Agent

                You are a senior software engineer specializing in code reviews.
                Focus on code quality, security, and maintainability.

                ## Guidelines
                - Review for potential bugs and edge cases
                - Check for security vulnerabilities
                - Ensure code follows best practices
                - Suggest improvements for readability and performance
              '';
            };
            commands = {
              commit = ''
                # Commit Command

                Create a git commit with proper message formatting.
                Usage: /commit [message]
              '';
              run = ''
                # Run Command

                Run the provided command as is
                Usage: /run command
              '';
            };
          };
          myconfig.persistence.directories = [ ".local/share/opencode" ];
          home.packages = [
            opencodeBwrap
            (pkgs.writeShellApplication {
              name = "opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "opencode-worktree";
              runtimeInputs = with pkgs; [
                git
                coreutils
              ];
              text = ''
                if [ ! -d .git ]; then
                  echo "Error: Not in a git repository root"
                  exit 1
                fi

                timestamp=$(date +%s)
                dirname=$(basename "$(pwd)")
                worktree_name="''${dirname}-opencode-''${timestamp}"
                branch_name="opencode-''${timestamp}"

                git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
                cd "../''${worktree_name}" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
