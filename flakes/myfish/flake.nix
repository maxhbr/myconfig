{
  description = "my fish configuration";

  inputs = {
    fasd.url = "github:oh-my-fish/plugin-fasd";
    fasd.flake = false;
    foreign-env.url = "github:oh-my-fish/plugin-foreign-env";
    foreign-env.flake = false;
    tmux.url = "github:oh-my-fish/plugin-tmux";
    tmux.flake = false;
    z.url = "github:jethrokuan/z";
    z.flake = false;
    fzf.url = "github:jethrokuan/fzf";
    fzf.flake = false;
    done.url = "github:franciscolourenco/done";
    done.flake = false;
    fish-async-prompt.url = "github:acomagu/fish-async-prompt";
    fish-async-prompt.flake = false;
    fish-ssh-agent.url = "github:danhper/fish-ssh-agent";
    fish-ssh-agent.flake = false;
    # sashimi.url = "github:isacikgoz/sashimi";
    # sashimi.flake = false;
    agnoster.url = "github:hauleth/agnoster";
    agnoster.flake = false;
    bax.url = "github:jorgebucaran/bax.fish";
    bax.flake = false;
  };

  outputs = { self, ... }@inputs: {
    nixosModule = { pkgs, ... }: {
      imports = [ ./historybackup.nix ];
      config = {
        programs.fish = { enable = true; };
        environment = {
          shells =
            [ "${pkgs.fish}/bin/fish" "/run/current-system/sw/bin/fish" ];
        };
        home-manager.sharedModules = [({
          imports = [ ./ex.hm.nix ];
          config = {
            home.packages = with pkgs; [ fasd fzf ];
            programs.fish = {
              enable = true;
              shellAliases = { };
              shellAbbrs = {
                rm = "rm -I";
                cp = "cp -i";
                mv = "mv -vi";
                ag = "rg";
                grep = "rg";
                ducks = "du -chs *";
                ff =
                  "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type f -iname ";
                ffd =
                  "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type d -iname ";
                ffa =
                  "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -iname ";
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
              '';
              plugins = [
                {
                  name = "fasd";
                  src = inputs.fasd;
                }
                {
                  name = "foreign-env";
                  src = inputs.foreign-env;
                }
                {
                  name = "tmux";
                  src = inputs.tmux;
                }
                {
                  name = "z";
                  src = inputs.z;
                }
                {
                  name = "fzf";
                  src = inputs.fzf;
                }
                {
                  name = "done";
                  src = inputs.done;
                }
                {
                  name = "fish-async-prompt";
                  src = inputs.fish-async-prompt;
                }
                {
                  name = "fish-ssh-agent";
                  src = inputs.fish-ssh-agent;
                }
              ];
            };
            home.file = {
              ".config/fish/functions/fish_prompt.fish".source = inputs.agnoster
                + "/fish_prompt.fish";
              ".config/fish/functions/bax.fish".source = inputs.bax
                + "/bax.fish";
            };
          };
        })];
      };
    };
  };
}
