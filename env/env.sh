# stolen From: https://github.com/sdiehl/dotfiles/blob/master/arch/env.sh
# 
#   In work!
#

# mkdir /home/mhuber

pacman -Sy

# Network
pacman -S dhcpcd
pacman -S dhclient
pacman -S wireless_tools
pacman -S wpa_supplicant
pacman -S wget
pacman -S wireshark-cli
pacman -S wireshark-gtk

# Laptop
pacman -S cpufrequtils
pacman -S laptop-mode-tools
pacman -S xbindkeys
pacman -S xf86-input-evdev
pacman -S xf86-input-synaptics
pacman -S powertop

# Drivers
pacman -S xf86-video-intel

# System
pacman -S gcc
pacman -S patch
pacman -S htop

# Shell
pacman -S zsh
pacman -S grml-zsh-config

# Sound
pacman -S alsa-lib
pacman -S alsa-oss
pacman -S alsa-plugins
pacman -S alsa-utils

# Base
pacman -S git
pacman -S gvim
pacman -S rxvt-unicode

#git clone https://github.com/maximilianhuber/myconfig

## Math
#pacman -S mpfr
#pacman -S gmp
#pacman -S cloog
#pacman -S isl
#pacman -S blas
#pacman -S lapack
#pacman -S arpack
#pacman -S pari
#pacman -S atlas-lapack
#pacman -S openblas
#pacman -S cblas
#pacman -S yacas
##pacman -S gap-math

# Haskell
pacman -S ghc
pacman -S haskell-x11
pacman -S haskell-x11-xft
pacman -S cabal-install
#pacman -S xmonad
#pacman -S xmonad-contrib
pacman -S xmobar
pacman -S dmenu
#pacman -S dzen2
cabal install xmonad
xmonad --recompile

# Essential Haskell
#cabal install parsec
#cabal install containers
#cabal install monad-loops
#cabal install directory
#cabal install filepath
#cabal install mtl
#cabal install transformers
#cabal install text
#cabal install stm
#cabal install pandoc
#cabal install lens
#cabal install categories
#cabal install diagrams
#cabal install network
#cabal install HTTP
#cabal install unix
#cabal install aeson
#cabal install hashable
#cabal install cereal
#cabal install criterion
#cabal install process
#cabal install quickcheck
#cabal install smallcheck
##cabal install smartcheck
#cabal install hashtable
#cabal install warp
#cabal install happy
#cabal install pretty
#cabal install pretty-show
#cabal install cloud
#cabal install hmatrix
#cabal install vect
#cabal install repa
#cabal install accelerate
#cabal install pipes
#cabal install uniplate
cabal install pointfree
#cabal install pointful
#cabal install pandoc
#cabal install hakyll
cabal install hlint
cabal install hdevtools

## Python
#pacman -S python2
#pacman -S python2-pip
#pacman -S python2-virtualenv

##pip install virtualenv
#pip install virtualenvwrapper

## Numeric libs in site-packages
#pacman -S python2-numpy
#pacman -S python2-scipy
#pacman -S python2-matplotlib

#pacman -S pypy

# Xorg
pacman -S xorg-server
pacman -S xorg-init
pacman -S xorg-xinput

## Desktop
pacman -S chromium
pacman -S tmux
pacman -S feh
pacman -S unclutter
#pacman -S epdfview

# Media
#pacman -S mpd
#pacman -S ncmcpp
pacman -S ffmpeg
#pacman -S vlc

# Latex
pacman -S texlive-bin
pacman -S texlive-core
pacman -S texinfo
pacman -S texlive-latexextra
pacman -S texlive-pictures
pacman -S texlive-pstricks
pacman -S ghostscript

pacman -S gimp
pacmas -S geeqie

## Dev
#pacman -S zeromq
#pacman -S openmpi
#pacman -S gdb
#pacman -S libevent
#pacman -S libev
#pacman -S gc
#pacman -S nginx
#pip install cython

pacman -S lua
#pacman -S go
#pacman -S scala
#pacman -S rust
#pacman -S openjdk6
#pacman -S julia-git
#pacman -S jsoftware

## PLT
#pacman -S ocaml
#pacman -S coq
#pacman -S twelf
#pacman -S pure
#pacman -S maude
#pacman -S erlang
#pacman -S strategoxt
#pacman -S libaterm
#pacman -S antlr2
#cabal install agda

# Compression
pacman -S xz
pacman -S tar
pacman -S gzip
pacman -S bzip2
pacman -S unrar

# Codecs
pacman -S mpg123

# Fonts
pacman -S ttf-bitstream-vera
pacman -S ttf-dejavu
pacman -S ttf-freefont
# pacman -S ttf-inconsolata-g
# pacman -S ttf-liberation
# pacman -S monaco-linux-font
