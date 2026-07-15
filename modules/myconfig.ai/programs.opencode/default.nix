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
          jail,
          ...
        }:
        let
          callLib = file: import file { inherit lib pkgs; };
          callJailLib = file: import file { inherit lib pkgs jail; };

          # Visually flag *un-jailed* opencode sessions. As with pi, the jailed
          # (sandboxed) `jailed-opencode*` wrappers are the safe default and
          # keep the user's normal theme untouched; everything else (bare
          # `opencode`, the `opencode-bwrap`/`-tmp`/`-worktree` wrappers) gets a
          # red editor/prompt border as a warning that it is running without the
          # jail.
          #
          # opencode has no runtime theme-switch API (unlike pi's extension), so
          # the theme is chosen statically in `~/.config/opencode/tui.json`. We
          # make the *default* theme the red-bordered `myconfig-unjailed` theme,
          # then let the jailed wrappers override it back to the normal theme via
          # `OPENCODE_CONFIG_CONTENT` (an inline config that takes precedence
          # over the on-disk config). This keeps the jail's on-disk config
          # (which it bind-mounts read-write) unmodified.
          #
          # The theme is mostly left to the terminal defaults (like the built-in
          # `system` theme) and only recolors the border tokens. It is declared
          # via `programs.opencode.themes` below.
          unjailedThemeName = "myconfig-unjailed";
          unjailedBorderColor = "#cc2222";

          # The theme opencode should fall back to when *not* un-jailed. This is
          # the same value the user would otherwise get from `settings.theme`
          # ("system"); the jailed wrappers inject it via OPENCODE_CONFIG_CONTENT
          # so the jail does not need to touch its bind-mounted config.
          normalThemeName = "system";

          # Wrap a jailed opencode binary so it forces the normal theme back on,
          # overriding the un-jailed default baked into tui.json. Uses
          # OPENCODE_CONFIG_CONTENT (inline config, higher precedence than the
          # on-disk config) so nothing on disk is modified.
          withNormalTheme =
            drv:
            pkgs.symlinkJoin {
              inherit (drv) name;
              paths = [ drv ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                for f in $out/bin/*; do
                  wrapProgram "$f" \
                    --set OPENCODE_CONFIG_CONTENT '{"theme":"${normalThemeName}"}'
                done
              '';
              # `symlinkJoin` does not copy `meta`; preserve `mainProgram` so
              # `lib.getExe` (used by the `-tmp`/`-worktree` wrappers) resolves.
              meta = (drv.meta or { }) // {
                mainProgram = drv.meta.mainProgram or drv.name;
              };
            };

          opencodeBwrap = callLib ../fns/sandboxed-app.nix {
            name = "opencode";
            pkg = config.programs.opencode.package;
            writableDirs = [
              ".config/opencode"
              ".config/mcp"
            ];
          };
          jail-app = callJailLib ../fns/jail-app.nix;
          # `jailed-opencode` is an alternative to `opencodeBwrap` that uses
          # the jail.nix library instead of a hand-rolled bubblewrap wrapper.
          # See `../fns/jail-app.nix` for the shared defaults.
          jailed-opencode = withNormalTheme (jail-app {
            name = "jailed-opencode";
            pkg = config.programs.opencode.package;
            userDataDirs = [
              ".config/opencode"
              ".local/share/opencode"
              ".local/state/opencode"
              ".config/mcp"
            ];
          });
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
            # Default the TUI to the red-bordered un-jailed warning theme. The
            # jailed wrappers override this back to the normal theme at runtime
            # via OPENCODE_CONFIG_CONTENT (see `withNormalTheme`), so only
            # un-jailed sessions show the warning.
            tui.theme = unjailedThemeName;
            # Custom theme: inherit terminal colors for everything (like the
            # built-in `system` theme) but paint the borders red as an
            # un-jailed warning.
            themes.${unjailedThemeName} = {
              "$schema" = "https://opencode.ai/theme.json";
              theme = {
                border = unjailedBorderColor;
                borderActive = unjailedBorderColor;
                borderSubtle = unjailedBorderColor;
              };
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
            (pkgs.writeShellApplication {
              name = "jailed-opencode-worktree";
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
                cd "../''${worktree_name}" && exec ${lib.getExe jailed-opencode} "$@"
              '';
            })
          ];
        }
      )
    ];
  };
}
