# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  tex = (pkgs.texLiveAggregationFun {
    paths = [
      pkgs.texLive pkgs.texLiveExtra
      pkgs.texLiveBeamer
      pkgs.texLiveCMSuper
    ];
  })
in
{
  imports = [
    ./dev.nix
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        tex
      ];
      home.file = {
        ".latexmkrc" = {
          text = ''
$pdflatex = 'pdflatex -interaction=nonstopmode --shell-escape';
$pdf_mode=1;

$dvi_previewer = "start xdvi";

# # $pdf_previewer = "zathura -l error -s -x 'myTexWrapper.sh %{line} \"%{input}\"' %O %S"
# $pdf_previewer = "zathura -l error -x 'myTexWrapper.sh %{line} \"%{input}\"' %O %S"

$pdf_previewer='start llpp %S';
$pdf_update_method=2;
          '';
        };
      };
    };
  };
}
