# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  myconfig,
  ...
}:
let
  user = myconfig.user;
in
{
  imports = [
    {
      services.udisks2.enable = true;
      environment.systemPackages = with pkgs; [
        udisks2
        # udiskie
        bashmount
      ];
      environment = {
        shellAliases = {
          # mountall = "udiskie -a";
          pmount = "udisksctl mount -b";
        };
      };
    }
  ];
  config = {
    home-manager.users."${user}" = {
      home.file = {
        ".aliasrc".source = ./aliasrc;
        ".bashrc" = {
          text = ''
            #
            # ~/.bashrc
            #

            # If not running interactively, don't do anything
            [[ $- != *i* ]] && return

            ###############################################################################
            [[ -f ~/.aliasrc ]] && source ~/.aliasrc
            [[ -f ~/.aliasrc-private ]] && source ~/.aliasrc-private
            [[ -d $HOME/bin ]] && {
                export PATH=$HOME/bin:$PATH
                [[ -d $HOME/bin/stolen ]] && export PATH=$PATH:$HOME/bin/stolen
                [[ -d $HOME/bin/docker ]] && export PATH=$PATH:$HOME/bin/docker
            }
            [[ -d $HOME/.perl/bin ]] && export PATH=$HOME/.perl/bin:$PATH
            [[ -d $HOME/.cabal/bin ]] && export PATH=$HOME/.cabal/bin:$PATH
            [[ -d $HOME/.local/bin ]] && export PATH=$HOME/.local/bin:$PATH

            ###############################################################################
            export PROMPT_COMMAND='history -a'
          '';
        };
        ".agignore" = {
          text = ''
            /.git/
          '';
        };
      };
    };
    environment = {
      systemPackages = with pkgs; [
        elinks
        w3m
        man-pages
        file
        # renameutils # qmv
        pv
        entr

        # admin:
        btop
        iftop
        iptraf-ng
        iotop
        bmon
        nmon
        ps_mem
        s-tui
        # (writeShellScriptBin "usbtop" ''
        #   sudo modprobe usbmon
        #   sudo ${usbtop}/bin/usbtop
        # '')
        pwgen
        usbutils
        tcpdump
        fuse

        (writeScriptBin "myspeedtest.sh" (builtins.readFile ./bin/myspeedtest.sh))
        (writeScriptBin "startServer.py" ''
          #!${pkgs.python3}/bin/python
          import http.server

          class MyHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
              def end_headers(self):
                  self.send_my_headers()
                  http.server.SimpleHTTPRequestHandler.end_headers(self)

              def send_my_headers(self):
                  self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
                  self.send_header("Pragma", "no-cache")
                  self.send_header("Expires", "0")

          if __name__ == '__main__':
              http.server.test(HandlerClass=MyHTTPRequestHandler)
        '')
        (writeShellScriptBin "mountExfat" ''
          src="$(readlink -f $1)"
          if [[ ! -e "$src" ]]; then
            echo "$src is not a file"
            exit 1
          fi
          if [[ "$src" != "/dev/"* ]]; then
            echo "$src is not in /dev/"
            exit 2
          fi
          target="''${src/\/dev//media}"
          echo $target
          sudo mkdir -p "$target"
          sudo mount \
              -o nonempty \
              -o uid=$(id -u) \
              -o gid=$(id -g) \
              -t exfat \
              "$src" "$target"
        '')
      ];
      interactiveShellInit = ''
        hgrep() { history | egrep "$@"; }
      '';
      shellAliases = {
        ps = "${pkgs.procs}/bin/procs";
        ranger = "lf";
        r = "lf";
        webserver = "python -m http.server 8000";
      };
    };

    security.sudo.extraConfig = ''
      ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl suspend
      ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl reboot
      ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl poweroff
    '';
  };
}
