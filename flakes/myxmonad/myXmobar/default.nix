# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv, xmobarrc ? ./xmobarrc
, xmobarrcTop ? ./xmobarrc.top, my-mute-telco }:

let
  isvpn = with pkgs;
    writeShellScriptBin "isvpn" ''
      delimiter=$1
      startcol=$2
      endcol=$3

      pre="$delimiter $startcol"
      post="$endcol "

      if ${nettools}/bin/ifconfig tun0 &> /dev/null; then
        echo -n "$pre"'VPN'"$post"
      fi
    '';
  isBtBlocked = with pkgs;
    writeShellScriptBin "isBtBlocked" ''
      delimiter=$1
      startcol=$2
      endcol=$3

      export LC_ALL=C

      pre="$delimiter $startcol"
      post="$endcol "
      btState=$(${pkgs.utillinux}/bin/rfkill -J | ${pkgs.jq}/bin/jq -r '.""|.[] | select (."id"==0) | ."soft"')
      if [[ "$btState" == "blocked" ]]; then
        echo -n "''${pre}¬BT''${post}"
      fi
    '';

  hasXssLock = with pkgs;
    writeShellScriptBin "hasXssLock" ''
      delimiter=$1
      startcol=$2
      endcol=$3

      pre="$delimiter $startcol"
      post="$endcol "

      if ! ${procps}/bin/pgrep xss-lock &> /dev/null; then
        echo -n "$pre"'¬XSS-LOCK'"$post"
      fi
    '';
  xmobarXmonad = with pkgs;
    writeShellScriptBin "xmobarXmonad" ''
      set -e
      export PATH=$PATH:${isvpn}/bin/:${hasXssLock}/bin/
      pidfile=/tmp/xmobarXmonad.pid
      if [[ -f $pidfile ]]; then
        kill $(cat $pidfile) || true
        rm $pidfile
      fi
      set -x
      ${xmobar}/bin/xmobar ${xmobarrc} &
      echo $! > $pidfile
    '';
  xmobarDmesg = with pkgs;
    writeShellScriptBin "xmobarDmesg" ''
      set -o pipefail
      set -ex
      fun () {
        ${utillinux}/bin/dmesg -w -T | ${xmobar}/bin/xmobar ${xmobarrcTop}
      }
      fun &disown
    '';
in pkgs.buildEnv {
  name = "my-xmobar";
  extraOutputsToInstall = [ "bin" ];
  paths = [ pkgs.xmobar xmobarXmonad xmobarDmesg my-mute-telco isBtBlocked ];
}
