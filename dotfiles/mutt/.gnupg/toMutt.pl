#!/usr/bin/env perl
#
#  written by maximilian-huber.de
use strict;
use warnings;

my $gpgConf = glob('~/.gnupg/gpg.conf');
my $target = glob('~/.gnupg/gpg_groups.mutt');

if (open(my $fh, '<:encoding(UTF-8)', $gpgConf)) {
    if (open(my $oh, '>', $target)) {
        while (my $line = <$fh>) {
            if ($line =~ /^group/) {
                my ($mail,$id) = (split /[<>=\n]/, $line)[1,3];
                $mail =~ s/\./\\./g;
                print $oh "crypt-hook '^$mail\$' '0x$id'\n";
            }
        }
        close $fh;
        close $oh;
    }
}
