#!/usr/bin/env nix-shell
#! nix-shell -i perl -p perl
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

use strict;
use warnings;
use File::Basename qw( dirname );
use Cwd qw( abs_path );
use File::Path qw( make_path );
use Term::ANSIColor qw( colored );

my %toLink = (# 'https://github.com/syl20bnr/spacemacs'   => '~/.emacs.d',
              'https://github.com/chisui/zsh-nix-shell' => '~/.zsh-nix-shell'
             # , 'https://github.com/jrosdahl/maildirproc' => '~/Mail/maildirproc'
    );

while ( my ($url, $target) = each(%toLink) ) {
    print "update: @{[colored(['bold green'], $target,'')]}\n";
    $target = (glob($target))[0];
    if ( -d dirname($target) ){
        if ( !-d "$target/.git" ){
            mkdir($target,0755);
            chdir($target);
            system("git init");
            system("git","remote","add","origin",$url);
            system("git","fetch","origin","master");
            system("git","checkout","-t","origin/master");
        }else{
            chdir($target);
            system("git","pull", "origin", "master");
        }
    }else{
        print "the parent directory of $target does not exist\n";
    }
}
