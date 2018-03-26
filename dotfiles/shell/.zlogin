# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# my login manager
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
[[ -z $DISPLAY && $XDG_VTNR -eq 2 ]] && exec tmux
[[ -z $DISPLAY && $XDG_VTNR -eq 6 ]] && exec startx
