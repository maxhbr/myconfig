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
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
    myconfig.ai.skills.playwright.enable = lib.mkDefault true;
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
          programs.mcp.enable = true;
          programs.opencode = {
            enable = true;
            enableMcpIntegration = true;
            web.enable = true;

            settings = {
              "autoupdate" = false;
              "share" = "disabled";
              "permission" = lib.mkForce {
                "bash" = {
                  "*" = "ask";
                  "head *" = "allow";
                  "tail *" = "allow";
                  "git add *" = "allow";
                  "git status *" = "allow";
                  "git diff *" = "allow";
                  "nix flake check *" = "allow";
                  "nix flake *" = "allow";
                  "nix fmt" = "allow";
                  "nix fmt *" = "allow";
                  "go build *" = "allow";
                  "go test *" = "allow";
                  "go generate *" = "allow";
                  "go fmt *" = "allow";
                  "go vet *" = "allow";
                  "npm run dev *" = "allow";
                  "npm run build *" = "allow";
                  "npm run lint *" = "allow";
                  "npm test *" = "allow";
                  "ls *" = "allow";
                  "grep *" = "allow";
                  "rg *" = "allow";
                  "find *" = "allow";
                  "mkdir *" = "allow";
                };
                "edit" = "ask";
              };
              "provider" = lib.mkMerge [
                (lib.mkIf (osconfig.myconfig.ai.localModels != [ ]) (
                  builtins.listToAttrs (
                    lib.map (
                      model:
                      let
                        hostPort = "${model.host}:${toString model.port}";
                        providerName = if model.name != null then model.name else hostPort;
                        modelNames = if model.models != [ ] then model.models else [ providerName ];
                      in
                      {
                        name = "local-${providerName}";
                        value = {
                          "npm" = "@ai-sdk/openai-compatible";
                          "name" = "${hostPort}";
                          "options" = {
                            "baseURL" = "http://${hostPort}/v1";
                          };
                          "models" = builtins.listToAttrs (
                            lib.map (modelName: {
                              name = modelName;
                              value = {
                                "name" = modelName;
                              };
                            }) modelNames
                          );
                        };
                      }
                    ) osconfig.myconfig.ai.localModels
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
                (lib.mkIf osconfig.services.llama-swap.enable (
                  let
                    llamaSwapModels = builtins.listToAttrs (
                      lib.map (model: {
                        name = model;
                        value = {
                          "name" = model;
                        };
                      }) (builtins.attrNames osconfig.services.llama-swap.settings.models)
                    );
                  in
                  {
                    "llama-swap" = {
                      "npm" = "@ai-sdk/openai-compatible";
                      "name" = "llama-swap";
                      "options" = {
                        "baseURL" = "http://localhost:${toString osconfig.services.llama-swap.port}/v1";
                      };
                      "models" = llamaSwapModels;
                    };
                  }
                ))
              ];
              "disabled_providers" = [
                "opencode"
              ];
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
            pkgs.opencode-desktop
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
