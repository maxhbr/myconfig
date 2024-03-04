{ inputs, pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  imports = [
    ({ config, lib, pkgs, myconfig, ... }: {
      config = lib.mkIf config.programs.fish.enable {
        systemd = {
          timers.fish-history-backup-timer = {
            wantedBy = [ "timers.target" ];
            partOf = [ "fish-history-backup-timer.service" ];
            timerConfig.OnCalendar = "hourly";
          };
          services.fish-history-backup-timer = {
            serviceConfig.Type = "oneshot";
            script = ''
              historyfile=/home/${myconfig.user}/.local/share/fish/fish_history
              backupdir="$historyfile"_backups
              backup=$backupdir/$(date '+%Y-%V').fish_history.gz
              if [[ ! -f $backup ]]; then
                mkdir -p $backupdir
                echo "Time: $(date)." >> $backupdir/fish-history-backup-timer.log
                ${pkgs.gzip}/bin/gzip -k $historyfile
                mv $historyfile.gz $backup
                chown ${myconfig.user}:${myconfig.user} $backup
              fi
            '';
          };
        };
      };
    })
    ({ config, lib, pkgs, myconfig, ... }: {
      config = lib.mkIf config.programs.fish.enable {
        environment = {
          shells =
            [ "${pkgs.fish}/bin/fish" "/run/current-system/sw/bin/fish" ];
        };
        home-manager.sharedModules = [
          ({ config, lib, ... }: {
            config = lib.mkIf config.programs.fish.enable {
              home.packages = with pkgs;
                [
                ];
              programs.fish = {
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
                  ex = ''
                    for file in $argv
                          if test -f $file
                              set opt ( echo "$file" | tr '[:upper:]' '[:lower:]' )
                              switch $opt
                                  case '*.tar.bz2'
                                      tar xjf $file
                                  case '*.tar.gz'
                                      tar xzf $file
                                  case '*.tar.xz'
                                      tar xvfJ $file
                                  case '*.xz'
                                      ${pkgs.xz}/bin/xz -d $file
                                  case '*.tar.lzma'
                                      tar --lzma -xvf $file
                                  case '*.bz2'
                                      ${pkgs.bzip2}/bin/bunzip2 $file
                                  case '*.rar'
                                      unrar e $file
                                  case '*.gz'
                                      ${pkgs.gzip}/bin/gunzip $file
                                  case '*.tar'
                                      tar xf $file
                                  case '*.tbz2'
                                      tar xjf $file
                                  case '*.tgz'
                                      tar xzf $file
                                  case '*.zip'
                                      ${pkgs.unzip}/bin/unzip $file
                                  case '*.Z'
                                      uncompress $file
                                  case '*.7z'
                                      ${pkgs.p7zip}/bin/7z x $file
                                  case '*.jar'
                                      ${pkgs.unzip}/bin/unzip $file
                                  case '*.war'
                                      ${pkgs.unzip}/bin/unzip $file
                                  case '*.ear'
                                      ${pkgs.unzip}/bin/unzip $file
                                  case '*.deb'
                                      ${pkgs.binutils}/bin/ar xv $file
                                  case '*'
                                      echo "'$file' of type '$opt' cannot be extracted via ex(), more info:"
                                      file $file
                              end
                          else
                              echo "'$file' is not a valid file"
                          end
                      end
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
                  # {
                  #   name = "fzf";
                  #   src = inputs.fzf;
                  # }
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
                ".config/fish/functions/fish_prompt.fish".source =
                  inputs.agnoster + "/fish_prompt.fish";
                ".config/fish/functions/bax.fish".source = inputs.bax
                  + "/bax.fish";
              };
            };
          })
        ];
      };
    })
  ];
  config = {
    programs.fish = { enable = true; };
    home-manager.sharedModules =
      [ ({ ... }: { programs.fish = { enable = true; }; }) ];
  };
}
