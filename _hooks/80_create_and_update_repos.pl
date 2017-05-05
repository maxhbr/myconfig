#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use File::Basename qw( dirname );
use Cwd qw( abs_path );
use File::Path qw( make_path );
use Term::ANSIColor qw( colored );

my %toLink = (
    'https://github.com/syl20bnr/spacemacs'                => '~/.emacs.d',
    # 'https://github.com/robbyrussell/oh-my-zsh.git'        => '~/.oh-my-zsh',
    # 'https://github.com/zsh-users/zsh-syntax-highlighting' => '~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting',
    # 'https://github.com/AndreaCrotti/yasnippet-snippets'   => '~/.emacs.d/snippets',
    # 'https://github.com/jwiegley/use-package'              => '~/.emacs.d/use-package',
    # 'https://github.com/monky-hs/monky'                    => '~/.xmonad/monky',
    'https://github.com/jrosdahl/maildirproc'              => '~/Mail/maildirproc'
    );

while ( my ($url, $target) = each(%toLink) ) {
    print "update: @{[colored(['bold green'], $target,'')]}\n";
    $target = (glob($target))[0];
    if ( -d dirname($target) ){
        if ( !-d "$target/.git" ){
            chdir($target);
            system("git init");
            system("git","remote","add","origin",$url);
            system("git","fetch");
            system("git","checkout","-t","origin/master");
        }else{
            chdir($target);
            system("git","pull");
        }
    }else{
        print "the parent directory of $target does not exist\n";
    }
}
