#!/usr/bin/env perl
#
#  written by maximilian-huber.de
use strict;
use warnings;
use File::Basename qw( dirname );
use Cwd qw( abs_path );
use Term::ANSIColor;

my %toLink = (
    '/zsh/zsh/' => '~/.zsh',
    '/emacs/emacs.d/snippets' => '~/.emacs.d/snippets',
    '/emacs/emacs.d/use-package' => '~/.emacs.d/use-package',
    );

my $baseDir = abs_path("@{[dirname($0)]}/..");

chdir $baseDir;

while ( my ($key, $value) = each(%toLink) ) {
    $value = "@{[glob($value)]}";
    $key = "$baseDir$key";
    if ( !-d $value && -d $key ) {
        print "symlink: @{[colored(['bold green'], \"$key => $value\n\",'')]}\n";
        symlink($key, $value);
    }
}
