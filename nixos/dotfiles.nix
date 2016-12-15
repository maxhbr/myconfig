{ config, lib, pkgs, ... }:

with lib;
rec {
  system.activationScripts =
  {
    dotfiles = stringAfter [ "users" ]
    ''
    dotfiles="/etc/nixos/dotfiles"

    user=$(stat -c '%U' "$dotfiles/README.md")
    userGroup=$(stat -c '%G' "$dotfiles/README.md")

    userDir="/home/$user"
    if [ ! -d $userDir ]; then
        echo "user dir does not exist"
        exit 1
    fi

    ################################################################################
    cd "$dotfiles"
    dirs=$(find . -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
    for dir in $dirs; do
        cd "$dotfiles/$dir"
        # I only want to have linked folders, no linked files
        find . -mindepth 1 -type d \
             -exec mkdir -p "$userDir/"{} \; \
             -exec chown $user:$userGroup "$userDir/"{} \;
        ${pkgs.stow}/bin/stow -t $userDir -d $dotfiles $dir
    done
    ''; 
  };
}
