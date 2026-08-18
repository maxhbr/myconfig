# Rendered by myconfig.ai.microvm.guestShellConvenience from the host's
# modules/programs.fish config (host-coupled parts dropped: no
# any-nix-shell, no persistence directory).

set -g fish_greeting

# Vi-style bindings that inherit emacs-style bindings in all modes.
function hybrid_bindings --description "Vi-style bindings that inherit emacs-style bindings in all modes"
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set -g fish_key_bindings hybrid_bindings
set -U sponge_successful_exit_codes (seq 0 255)

# Abbreviations from the host config.
abbr rm 'rm -I'
abbr cp 'cp -i'
abbr mv 'mv -vi'
abbr ducks 'du --one-file-system -chs -- *'
abbr ff "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type f -iname "
abbr ffd "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type d -iname "
abbr ffa "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -iname "
abbr ffg 'find . -type f -print0 | xargs -0 grep -lI'

# Host helper functions.
function whichf
    readlink -f (which $argv)
end
function cdtemp
    cd (mktemp -d); pwd
end
function cptemp
    set f (readlink -f $argv[1])
    cd (mktemp -d)
    cp -r $f ./
    pwd
end
function mvtemp
    set f (readlink -f $argv[1])
    cd (mktemp -d)
    mv $f ./
    pwd
end
