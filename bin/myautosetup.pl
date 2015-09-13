#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Getopt::Long qw( GetOptions );
use Term::ANSIColor qw( colored );
use Scalar::Util qw( looks_like_number );

################################################################################
##  config                                                                    ##
################################################################################
my $lvdsOutput = "eDP1";
my @mainOutputs = ("DP1", "HDMI1", "HDMI2");
my @dockedOutputs = ("DP2-1", "DP2-2", "DP2-3");
my $background = "/home/mhuber/Bilder/background/BACKGROUND.png";

################################################################################
##  prepare                                                                   ##
################################################################################
my $docked = 0;
my $rotate = "normal";
my $noXrandr = 0;
my $alsaOutput = "";

GetOptions(
    'rotate'     => \$rotate,
    'docked'     => \$docked,
    'unDocked'   => sub { $docked = 0 },
    'noXrandr'   => \$noXrandr,
    'setSound=s' => \$alsaOutput,
) or die "Usage: $0 \n\t[--rotate=rotation]\n\t[--setSound=CardName/CardNumber]\n\t[--docked/--unDocked]\n\t[--noXrandr]\n";

my $acPresent = `acpi -a | grep -c on-line`;
my $xrandr = `xrandr`;

foreach my $output (@dockedOutputs) {
    $docked = 1 if $xrandr =~ /$output connected/;
}

################################################################################
##  subroutines                                                               ##
################################################################################
{
    my $xrandrCmd = "xrandr";
    my @otherOutputs = ();
    push @otherOutputs, @mainOutputs;
    push @otherOutputs, @dockedOutputs;

    sub addToXrandrCmd {
        my $output = shift;
        my $params = shift;

        $xrandrCmd .= " --output $output $params";

        @otherOutputs = grep { $_ ne $output } @otherOutputs;
    }
    sub runXrandrCmd {
        foreach my $output (@otherOutputs) {
            addToXrandrCmd($output,"--rotate normal --off");
        }
        system($xrandrCmd) if !$noXrandr;
    }

    addToXrandrCmd("VIRTUAL1","--rotate normal --off");
}
sub configureSoundCard{
    # parameters are:
    #   name of device
    my $num = `cat /proc/asound/cards | grep -m1 $_[0] | cut -d' ' -f2`;
    $num = '1' if looks_like_number($num);
    $num = $_[0] if looks_like_number($_[0]);

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
sub configureSoundCardIfNotPredefined{
    $alsaOutput eq "" ?
        configureSoundCard($_[0]) :
        configureSoundCard($alsaOutput);
}

################################################################################
sub undockedConfig{
    sub toggleMainOutputs{
        addToXrandrCmd($lvdsOutput,"--mode 1920x1080 --pos 0x0 --rotate normal --primary");
        foreach my $output (@mainOutputs) {
            if ($xrandr =~ /$output connected \(/){
                addToXrandrCmd($output,"--auto --above $lvdsOutput");
            }elsif ($xrandr =~ /$output connected/){
                addToXrandrCmd($output,"--rotate normal --off");
            }
        }
        runXrandrCmd();
    }

    toggleMainOutputs();
    system("xset dpms 300 600 900");
    $acPresent ? system("xbacklight =70") : system("xbacklight =100");

    #sound
    sleep(1);
    configureSoundCardIfNotPredefined("PCH");
    system("amixer -q set Master off");
}

################################################################################
sub dockedConfig{
    sub setupDockedOutputs{
        addToXrandrCmd($lvdsOutput,"--mode 1920x1080 --pos 0x0 --rotate normal");
        foreach my $output (@dockedOutputs) {
            if ($xrandr =~ /$output connected/){
                addToXrandrCmd($output,"--mode 1920x1080 --same-as $lvdsOutput --rotate $rotate --primary");
                # $cmd .= " --output $output --mode 1920x1080 --same-as eDP1 --rotate $rotate";
                # --output $DOCKED_OUTPUT --primary --mode 1920x1080 --rotate "$rotate" \
            }
        }
        runXrandrCmd();
    }

    setupDockedOutputs();
    system("xset dpms 900 1800 2700");
    system("xbacklight =100");

    #sound
    sleep(1);
    configureSoundCardIfNotPredefined("Device");
}

################################################################################
sub commonConfig{
    system("feh --bg-center \"$background\"");
}

################################################################################
##  main                                                                      ##
################################################################################
$docked ? dockedConfig() : undockedConfig();
commonConfig();
