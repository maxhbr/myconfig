{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
in
{
  imports = [
    (
      {
        config,
        lib,
        pkgs,
        myconfig,
        ...
      }:
      {
        config = lib.mkIf config.programs.fish.enable {
          environment = {
            shells = [
              "${pkgs.fish}/bin/fish"
              "/run/current-system/sw/bin/fish"
            ];
          };
          programs.bash = {
            # launches fish unless the parent process is already fish:
            interactiveShellInit = ''
              if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
              then
                shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
                exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
              fi
            '';
          };
          home-manager.sharedModules = [
            (
              { config, lib, ... }:
              {
                config = lib.mkIf config.programs.fish.enable {
                  myconfig.persistence.directories = [ ".local/share/fish" ];
                  home.packages = with pkgs; [ grc ];
                  programs.fish = {
                    shellAliases = { };
                    shellAbbrs = {
                      rm = "rm -I";
                      cp = "cp -i";
                      mv = "mv -vi";
                      ducks = "du -chs *";
                      ff = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type f -iname ";
                      ffd = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type d -iname ";
                      ffa = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -iname ";
                      ffg = "find . -type f -print0 | xargs -0 grep -lI";
                    };
                    functions = {
                      __fish_command_not_found_handler = {
                        body = "__fish_default_command_not_found_handler $argv[1]";
                        onEvent = "fish_command_not_found";
                      };
                      __list_dir_handler = {
                        body = ''
                          # echo content after cd / z / or any pwd change
                          set linesInLs (ls -1GF | ${pkgs.coreutils}/bin/wc -l)
                          set linesInTerminal (${pkgs.ncurses}/bin/tput lines)
                            if [  -lt  ]; then
                              ls -GF
                            fi
                        '';
                        onVariable = "PWD";
                      };
                      whichf = "readlink -f (which $argv)";
                      cdtemp = "cd (mktemp -d); pwd";
                      cptemp = ''
                        set f (readlink -f $1)
                        cd (mktemp -d)
                        cp -r $f ./
                        pwd
                      '';
                      mvtemp = ''
                        set f (readlink -f $1)
                        cd (mktemp -d)
                        mv $f ./
                        pwd
                      '';
                    };
                    shellInit = "";
                    loginShellInit = "";
                    interactiveShellInit = ''
                      set -U fish_greeting

                      # see: https://fishshell.com/docs/current/#command-line-editor
                      function hybrid_bindings --description "Vi-style bindings that inherit emacs-style bindings in all modes"
                          for mode in default insert visual
                              fish_default_key_bindings -M $mode
                          end
                          fish_vi_key_bindings --no-erase
                      end
                      set -g fish_key_bindings hybrid_bindings
                      set -l nix_shell_info (
                        if test -n "$IN_NIX_SHELL"
                          echo -n "<nix-shell> "
                        end
                      )
                      set -U sponge_successful_exit_codes (seq 0 255)
                    '';
                    plugins = (
                      map
                        (name: {
                          inherit name;
                          src = pkgs.fishPlugins."${name}".src;
                        })
                        [
                          "colored-man-pages" # Fish shell plugin to colorize man pages
                          "done"
                          "grc" # grc Colourizer for some commands on Fish shell
                          "sponge" # keeps your fish shell history clean from typos, incorrectly used commands and everything you don't want to store due to privacy reasons
                          "z" # Pure-fish z directory jumping
                        ]
                    );
                  };
                  xdg.configFile = {
                    "fish/functions" = {
                      source = lib.cleanSourceWith {
                        src = lib.cleanSource ./functions/.;
                      };
                      recursive = true;
                    };
                    "fish/functions/fish_mode_prompt.fish" = {
                      source = "${pkgs.fishPlugins.hydro}/share/fish/vendor_functions.d/fish_mode_prompt.fish";
                    };
                    "fish/functions/fish_prompt.fish" = {
                      source = "${pkgs.fishPlugins.hydro}/share/fish/vendor_functions.d/fish_prompt.fish";
                    };
                    "fish/functions/fish_title.fish" = {
                      source = "${pkgs.fishPlugins.hydro}/share/fish/vendor_functions.d/fish_title.fish";
                    };
                    "fish/conf.d/hydro.fish" = {
                      source = "${pkgs.fishPlugins.hydro}/share/fish/vendor_conf.d/hydro.fish";
                    };
                  };
                };
              }
            )
          ];
        };
      }
    )
  ];
  config = {
    programs.fish = {
      enable = true;
    };
    home-manager.sharedModules = [
      (
        { ... }:
        {
          programs.fish = {
            enable = true;
          };
        }
      )
    ];
  };
}
