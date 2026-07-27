{
  config,
  lib,
  pkgs,
  jail,
  ...
}:
let
  osconfig = config;

  callLib = file: import file { inherit lib pkgs; };
  callJailLib =
    file:
    import file {
      inherit
        lib
        pkgs
        jail
        osconfig
        ;
    };
  jail-app = callJailLib ../fns/jail-app.nix;
  mkWorkmuxWorktree = callLib ../fns/workmux-worktree.nix;

  # Make the `workmux` binary available inside the sandboxes (for the
  # `workmux set-window-status` status hooks and `workmux merge`/`remove` from
  # a worktree pane) whenever workmux is enabled.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.workmux.enable osconfig.myconfig.ai.workmux.package;

  # home-manager uses `useGlobalPkgs`, so `pkgs.opencode` is the same package
  # `programs.opencode.package` defaults to. Building the wrappers at the
  # NixOS scope lets us register the workmux named agents from here.
  opencodeBwrap = callLib ../fns/sandboxed-app.nix {
    name = "opencode";
    pkg = pkgs.opencode;
    writableDirs = [
      ".config/opencode"
      ".config/mcp"
    ];
    extraRuntimeInputs = workmuxDevTools;
  };
  # `jailed-opencode` is an alternative to `opencodeBwrap` that uses the
  # jail.nix library instead of a hand-rolled bubblewrap wrapper. See
  # `../fns/jail-app.nix` for the shared defaults.
  jailed-opencode = jail-app {
    name = "jailed-opencode";
    pkg = pkgs.opencode;
    userDataDirs = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      ".config/mcp"
    ];
    extraDevTools = workmuxDevTools;
  };
  # Worktree variant of `jailed-opencode`: additionally binds the linked main
  # repository read-only and remounts its shared `.git` read-write, resolved
  # at runtime from the WORKTREE_* env vars set by the workmux launcher.
  jailed-opencode-worktree-inner = jail-app {
    name = "jailed-opencode-worktree-inner";
    pkg = pkgs.opencode;
    userDataDirs = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      ".config/mcp"
    ];
    extraDevTools = workmuxDevTools;
    extraReadOnlyEnvPaths = [ "WORKTREE_MAIN_REPO" ];
    extraReadWriteEnvPaths = [ "WORKTREE_GIT_DIR" ];
  };
  opencodeWorktree = mkWorkmuxWorktree {
    name = "opencode-worktree";
    agentName = "opencode";
    agentType = "opencode";
    innerPkg = opencodeBwrap;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };
  jailedOpencodeWorktree = mkWorkmuxWorktree {
    name = "jailed-opencode-worktree";
    agentName = "jailed-opencode";
    agentType = "opencode";
    innerPkg = jailed-opencode-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };

  # Build a lookup: model name (raw or provider-prefixed) -> contextWindow.
  contextWindowLookup = lib.listToAttrs (
    lib.concatMap (
      provider:
      let
        hostPort = "${provider.host}:${toString provider.port}";
        providerName = if provider.name != null then provider.name else hostPort;
        rawModels = if provider.models != [ ] then provider.models else [ ];
      in
      lib.concatMap (
        m:
        if builtins.isAttrs m && m.contextWindow != null then
          [
            {
              name = m.name;
              value = m.contextWindow;
            }
            {
              name = "${providerName}:${m.name}";
              value = m.contextWindow;
            }
          ]
        else
          [ ]
      ) rawModels
    ) osconfig.myconfig.ai.localModels
  );
in
{
  options.myconfig = with lib; {
    ai.opencode = {
      enable = mkEnableOption "myconfig.ai.opencode";
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
    myconfig.ai.skills.playwright.enable = lib.mkDefault true;
    myconfig.ai.workmux.agents.opencode = opencodeWorktree.agent;
    myconfig.ai.workmux.agents.jailed-opencode = jailedOpencodeWorktree.agent;
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
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
                  "git log *" = "allow";
                  "git diff *" = "allow";
                  "nix flake check *" = "allow";
                  "nix flake *" = "allow";
                  "nix fmt" = "allow";
                  "nix fmt *" = "allow";
                  "nix build --dry-run *" = "allow";
                  "nix-instantiate *" = "allow";
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
                        # localModels may contain plain strings or
                        # `{ name, kind ? null }` submodules (`kind` is one
                        # of "base"/"variant"/"alias"/null, computed by the
                        # publisher); flatten both shapes to a string list
                        # for the opencode `models` map. opencode treats
                        # every name the same, so the kind tag is ignored
                        # here.
                        rawModels = if model.models != [ ] then model.models else [ providerName ];
                        modelNames = lib.map (m: if builtins.isAttrs m then m.name else m) rawModels;
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
                            lib.map (
                              modelName:
                              let
                                cw = contextWindowLookup.${modelName} or null;
                              in
                              {
                                name = modelName;
                                value = {
                                  "name" = modelName;
                                }
                                // lib.optionalAttrs (cw != null) {
                                  "contextWindowSize" = cw;
                                };
                              }
                            ) modelNames
                          );
                        };
                      }
                    ) osconfig.myconfig.ai.localModels
                  )
                ))
                (lib.mkIf osconfig.services.litellm.enable (
                  let
                    opencodeModels = builtins.listToAttrs (
                      lib.map (
                        model:
                        let
                          cw = contextWindowLookup.${model.model_name} or null;
                        in
                        {
                          name = model.model_name;
                          value = {
                            "name" = model.model_name;
                          }
                          // lib.optionalAttrs (cw != null) {
                            "contextWindowSize" = cw;
                          };
                        }
                      ) osconfig.services.litellm.settings.model_list
                    );
                    # `host` may be a wildcard (e.g. "0.0.0.0") for external
                    # exposure; rewrite to localhost for in-host clients.
                    litellmHost =
                      if osconfig.services.litellm.host == "0.0.0.0" then "localhost" else osconfig.services.litellm.host;
                  in
                  {
                    "litellm" = {
                      "npm" = "@ai-sdk/openai-compatible";
                      "name" = "LiteLLM";
                      "options" = {
                        "baseURL" = "http://${litellmHost}:${toString osconfig.services.litellm.port}/v1";
                      };
                      "models" = opencodeModels;
                    };
                  }
                ))
                (lib.mkIf osconfig.services.llama-swap.enable (
                  let
                    llamaSwapModels = builtins.listToAttrs (
                      lib.map (
                        model:
                        let
                          cw = contextWindowLookup.${model} or null;
                        in
                        {
                          name = model;
                          value = {
                            "name" = model;
                          }
                          // lib.optionalAttrs (cw != null) {
                            "contextWindowSize" = cw;
                          };
                        }
                      ) (builtins.attrNames osconfig.services.llama-swap.settings.models)
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
            tui = {
              "theme" = "system";
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
            jailed-opencode
            (pkgs.writeShellApplication {
              name = "opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe opencodeBwrap} "$@"
              '';
            })
            (pkgs.writeShellApplication {
              name = "jailed-opencode-tmp";
              runtimeInputs = with pkgs; [ coreutils ];
              text = ''
                cd "$(mktemp -d)" && exec ${lib.getExe jailed-opencode} "$@"
              '';
            })
            opencodeWorktree.wrapper
            jailedOpencodeWorktree.wrapper
          ];
        }
      )
    ];
  };
}
