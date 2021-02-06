#!/usr/bin/env bash
nix-shell --keep-failed -E "with import <nixpkgs> {}; callPackage ./default.nix {}"
