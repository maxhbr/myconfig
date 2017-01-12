{ config, pkgs, pathExists, ... }:

let
  extraHosts = "${builtins.readFile ../static/extrahosts}";
in {
  networking.extraHosts = "${extraHosts}";
}
