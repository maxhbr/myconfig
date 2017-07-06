#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Getopt::Long qw( GetOptions );
use Term::ANSIColor qw( colored );
use Scalar::Util qw( looks_like_number );

################################################################################
my $alsaOutput = "";

GetOptions(
    'setSound=s'  => \$alsaOutput,
) or die "Usage: $0 \n\t[--setSound=CardName/CardNumber]\n";
################################################################################
# alsa:
my @alsaOutputs = ("BT600" => "on", "USB" => "on", "CODEC" => "on", "PCH" => "off");

sub setupSound{
    sub configureSoundCard{
        # parameters are:
        #   name of device
        my $num = `cat /proc/asound/cards | grep -m1 $_[0] | cut -d' ' -f2`;
        $num = '0' if ! looks_like_number($num);
        $num = $_[0] if looks_like_number($_[0]);

        print "set alsa device to $_[0] == $num";
        if (open(ASOUNDRC, ">@{[glob(\"~/.asoundrc\")]}")) {
            print ASOUNDRC "#generated via ~/bin/myautosetup.pl\n";
            print ASOUNDRC "#Device is: $_[0]\n";
            print ASOUNDRC "defaults.ctl.card $num\n";
            print ASOUNDRC "defaults.pcm.card $num\n";
            print ASOUNDRC "defaults.timer.card $num\n";
            # the following needs the package alsaequal
            if ( -e "/usr/lib/alsa-lib/libasound_module_ctl_equal.so"){
                print ASOUNDRC "ctl.equal { type equal; }\n";
                print ASOUNDRC "pcm.plugequal { type equal; slave.pcm \"plughw:${num},0\"; }\n";
                print ASOUNDRC "pcm.!default { type plug; slave.pcm plugequal; }\n";
            }
            close ASOUNDRC;
        }
    }

    sleep(1);
    if ($alsaOutput eq ""){
        my $asoundCards = `cat /proc/asound/cards`;
        while (@alsaOutputs) {
            my ($output, $defaultState) = splice @alsaOutputs, 0, 2;
            if($asoundCards =~ /$output/) {
                configureSoundCard($output);
                system("amixer -q set Master $defaultState");
                last;
            }
        }
    }else{
        configureSoundCard($alsaOutput);
    }
}
################################################################################
setupSound();
