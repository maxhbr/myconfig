{ pkgs, ... }:
let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  upg-pull = pkgs.writeShellScriptBin "upg-pull" ''
    set -e
    if [ "$(id -u)" -ne "1000" ]; then
      echo "you should run this script as the user, which owns $0"
      exec sudo su -c "$0" "$(id -nu 1000)"
    fi
    set -x
    myconfigDir=$HOME/myconfig
    if [[ -d "$myconfigDir" ]]; then
      if [[ ! -d "$myconfigDir/.git" ]]; then
        exit 1
      fi
      cd "$myconfigDir"
      BRANCH=$(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)
      if [[ "$BRANCH" == "master" ]]; then
        if [ -z "$(${pkgs.git}/bin/git diff-index --name-only HEAD --)" ]; then
          ${pkgs.git}/bin/git pull origin master
        fi
      fi
    else
      ${pkgs.git}/bin/git clone https://github.com/maxhbr/myconfig "$myconfigDir"
    fi
  '';
  mk-upg-script = name: args:
    pkgs.writeShellScriptBin name ''
      exec /home/mhuber/myconfig/rebuild.sh ${args} "$@"
    '';
in {
  imports = [ ./user.myconfig.nix ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ unstable.nixfmt ];
    };
    environment = {
      systemPackages = with pkgs; [
        upg-pull
        (mk-upg-script "upg" "")
        (mk-upg-script "upg-fast" "--fast")
        (mk-upg-script "upg-dry" "--dry-run")
        (mk-upg-script "upg-workstation" "--fast --target workstation")
        (mk-upg-script "upg-nas" "--fast --target nas")
        (mk-upg-script "upg-vserver" "--fast --target vserver")
        nixos-2003-small.nixops
      ];
      shellAliases = {
        upg-get-hostId = ''
          cksum /etc/machine-id | while read c rest; do printf "%x" $c; done'';
      };
    };
    boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];
  };
}
