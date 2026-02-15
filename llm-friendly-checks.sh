#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

log_error() {
    echo -e "$(tput setaf 1)error: $1$(tput sgr0)" >&2
}

reduce_logging_for_llm() {
  local logfile="$(mktemp)"
  >&2 echo "(log will be abbreviated, cat $logfile to get the full log)"
  tee "$logfile" |
    awk '
    BEGIN {
        first_lines = 10
        last_lines = 20
        total_lines = 0
    }
    {
        total_lines++
        if (total_lines <= first_lines) {
            first[NR] = $0
        }
        if (total_lines > (first_lines + 1)) {
            last[NR] = $0
        }
    }
    END {
        for (i = 1; i <= total_lines; i++) {
            if (i <= first_lines) {
                print first[i]
            } else if (i == (first_lines + 1) && total_lines > (first_lines + last_lines)) {
                print "..."
            } else if (i > (total_lines - last_lines + 1)) {
                print last[i]
            }
        }
    }
    '
}

check_formatting() {
    >&2 echo "+ ./nixfmtall.sh --check"
    if ! ./nixfmtall.sh --check; then
        log_error "Formatting check failed. Run './nixfmtall.sh' to fix."
        exit 1
    fi
}

check_flake() {
    >&2 echo "+ nix flake check"
    if ! nix flake check 2>&1 | reduce_logging_for_llm; then
        log_error "nix flake check failed."
        exit 1
    fi
}

build_host() {
    local host="$1"
    >&2 echo "+ nix build \".#nixosConfigurations.$host.config.system.build.toplevel\""
    if ! nix build ".#nixosConfigurations.$host.config.system.build.toplevel" 2>&1 | reduce_logging_for_llm; then
        log_error "Build failed for host: $host"
        exit 1
    fi
}

main() {
    check_formatting
    check_flake

    if [[ -n "${1:-}" ]]; then
        build_host "$1"
    fi

    >&2 echo "All checks passed"
}

main "$@"
