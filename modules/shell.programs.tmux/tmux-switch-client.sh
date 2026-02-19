#!/usr/bin/env bash
# Source: https://mskelton.dev/bytes/cycle-tmux-sessions-in-order
set -euo pipefail

# Parse arguments
arg="${1:-next}"

# Check if argument is a direction indicator or a session name
case "$arg" in
	n|next)
		direction=1
		;;
	p|prev)
		direction=0
		;;
	l|list)
		# List all sessions, sorted by creation time
		tmux list-sessions -F "#{session_created} #{session_name}" | sort -n | awk '{print $2}'
		exit 0
		;;
	*)
		# Treat as session name and switch directly
		tmux switch-client -t "$arg"
		exit 0
		;;
esac

# List all sessions, sorted by creation time
sessions=$(tmux list-sessions -F "#{session_created} #{session_name}" | sort -n | awk '{print $2}')
# Get the current session name
current_session=$(tmux display-message -p '#{session_name}')
# Get the total number of sessions
total_sessions=$(echo "$sessions" | wc -l)
# Get the current session index
current_index=$(echo "$sessions" | grep -n "^$current_session$" | cut -d: -f1)
# Get the next session index based on the direction, loop around if necessary
if [[ "$direction" == "1" ]]; then
	next_index=$((current_index % total_sessions + 1))
else
	next_index=$(((current_index - 2 + total_sessions) % total_sessions + 1))
fi
# Find the session by index and switch to it
tmux switch-client -t "$(echo "$sessions" | sed -n "${next_index}p")"
