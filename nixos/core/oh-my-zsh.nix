{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      oh-my-zsh
      unstable.zsh-syntax-highlighting
    ];
    shells = ["/run/current-system/sw/bin/zsh"];
  };
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
source ${pkgs.unstable.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
'';

    promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
  };
}
