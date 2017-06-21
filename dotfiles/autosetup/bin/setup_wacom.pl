#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;

################################################################################
sub setupWacom{
    my @wacomDevices = `xsetwacom --list devices | sed -e 's#.*id: \\(\\)#\\1#' | awk '{print \$1;}'`;
    chomp @wacomDevices;
    my $primaryOutput = `xrandr | grep primary | cut -d" " -f1`

    foreach my $wacomDevice (@wacomDevices) {
        system("xsetwacom --set $wacomDevice MapToOutput ${primaryOutput}");
    }
}
################################################################################
setupWacom();
