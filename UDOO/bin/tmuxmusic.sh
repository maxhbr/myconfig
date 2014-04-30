#!/bin/bash

cd /mnt/media/music
tmux new-session -d -s music -n 'ncmpcpp' 'ncmpcpp'
 
#tmux new-window -t music:1 -n 'ncmpcpp' 'ncmpcpp'
#tmux select-window -t music:1
