{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      oh-my-zsh
    ];
    shells = ["/run/current-system/sw/bin/zsh"];
  };
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
  export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/

  # # Customize your oh-my-zsh options here
  # ZSH_THEME="agnoster"
  # plugins=(git)

  # source $ZSH/oh-my-zsh.sh
'';

    promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
  };
}
