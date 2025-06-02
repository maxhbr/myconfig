# partially copied from: https://github.com/glasserc/etc/blob/master/dot/config/fish/functions/fish_right_prompt.fish

function fish_right_prompt
    set default_host "f13"
    set current_host (hostname)

    if test "$current_host" != "$default_host"
        echo -n -s ' ' (set_color yellow)"$current_host"(set_color normal)
    end

    show_nix_status
end


# This is somewhat incomplete -- ideally we'd show information about
# what kind of shell we were in -- but there appear to be upstream
# political obstacles to getting better nix-shell support for non-bash
# shells. See https://github.com/NixOS/nix/issues/498 for an overview
# and https://github.com/chisui/zsh-nix-shell for an example of what a
# zsh user does by way of contrast.
function show_nix_status
    if set -q IN_NIX_SHELL
        set -l letter N
        set -l color blue
        switch $IN_NIX_SHELL
            case impure
                set color green
        end
        echo -n -s ' ' (set_color $color) $letter (set_color normal)
    end
end
