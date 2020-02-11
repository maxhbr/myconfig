# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

let
  xmobarrc = ./xmobarrc;
  xmobarrcTop = ./xmobarrc.top;

  isvpn = with pkgs; writeScriptBin "isvpn" ''
#!${stdenv.shell}
delimiter=$1
startcol=$2
endcol=$3

pre="$delimiter $startcol"
post="$endcol "

if ${nettools}/bin/ifconfig tun0 &> /dev/null; then
  echo -n "$pre"'VPN'"$post"
fi
  '';
  xmobarXmonad = with pkgs; writeScriptBin "xmobarXmonad" ''
#!${stdenv.shell}
set -ex
export PATH=$PATH:${isvpn}/bin/
${xmobar}/bin/xmobar ${xmobarrc}
  '';
  xmobarDmesg = with pkgs; writeScriptBin "xmobarDmesg" ''
#!${stdenv.shell}
set -o pipefail
set -ex
fun () {
  ${utillinux}/bin/dmesg -w -T | ${xmobar}/bin/xmobar ${xmobarrcTop}
}
fun &disown
  '';
in pkgs.buildEnv {
  name = "my-xmobar";
  extraOutputsToInstall = ["bin"];
  paths = [
    pkgs.xmobar
    xmobarXmonad
    xmobarDmesg
  ];
}
