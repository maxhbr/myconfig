{ config, lib, pkgs, ... }:

{
  programs.astroid =
    lib.mkIf config.programs.astroid.enable { externalEditor = "emacs %1"; };
}
