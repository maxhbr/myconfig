#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Getopt::Long qw( GetOptions );
use Term::ANSIColor qw( colored );
use Scalar::Util qw( looks_like_number );
use threads;

################################################################################
##  config                                                                    ##
################################################################################
my $lvdsOutput = "eDP1";
my @mainOutputs = ("DP1", "DP1-8", "HDMI1", "HDMI2");
my @dockedOutputs = ("DP2-1", "DP2-2", "DP2-3");
my $background = "/home/mhuber/Bilder/background/BACKGROUND.png";

################################################################################
##  prepare                                                                   ##
################################################################################
my $docked = 0;
my $rotate = "normal";
my $noXrandr = 0;
my $alsaOutput = "";
my $primaryCountStart = 0;

GetOptions(
    'rotate=s'    => \$rotate,
    'docked'      => \$docked,
    'unDocked'    => sub { $docked = 0 },
    'noXrandr'    => \$noXrandr,
    'primOutNr=i' => \$primaryCountStart,
    'setSound=s'  => \$alsaOutput,
) or die "Usage: $0 \n\t[--rotate=rotation]\n\t[--primOutNr=0]\n\t[--setSound=CardName/CardNumber]\n\t[--docked/--unDocked]\n\t[--noXrandr]\n";

my $acPresent = `acpi -a | grep -c on-line`;
my $xrandr = `xrandr`;

foreach my $output (@dockedOutputs) {
    $docked = 1 if $xrandr =~ /$output connected/;
}

################################################################################
##  subroutines                                                               ##
################################################################################
{ # clojure
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
        addToXrandrCmd($lvdsOutput,"--primary") if($xrandrCmd !~ /--primary/);
        foreach my $output (@otherOutputs) {
            addToXrandrCmd($output,"--rotate normal --off")
                if ($xrandr =~ /$output/);
        }
        system($xrandrCmd) if !$noXrandr;
    }
    addToXrandrCmd("VIRTUAL1","--rotate normal --off");
}
{ # clojure
    my $primaryCount = $primaryCountStart;
    sub isPrimary {
        return ($primaryCount-- == 0) ? "--primary" : "";
    }
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

################################################################################
sub undockedConfig{
    sub toggleMainOutputs{
        addToXrandrCmd($lvdsOutput,"--mode 1920x1080 --pos 0x0 --rotate normal @{[isPrimary()]}");
        my $defaultPosition = "--right-of"; # --above
        foreach my $output (@mainOutputs) {
            if ($xrandr =~ /$output connected \(/ ||
                $xrandr =~ /$output connected \w \(/){
                addToXrandrCmd($output,"--auto $defaultPosition $lvdsOutput --rotate $rotate @{[isPrimary()]}");
            }elsif ($xrandr =~ /$output connected/){
                addToXrandrCmd($output,"--rotate normal --off");
            }
        }
        runXrandrCmd();
    }

    toggleMainOutputs();
    system("xset dpms 300 600 900");
    $acPresent ? system("xbacklight =70") : system("xbacklight =100");
}

################################################################################
sub dockedConfig{
    sub setupDockedOutputs{
        addToXrandrCmd($lvdsOutput,"--mode 1920x1080 --pos 0x0 --rotate normal");
        
        my $modifier = ($xrandr =~ /1920x1080\+1920\+0/) ? "--same-as" : "--right-of";

        foreach my $output (@dockedOutputs) {
            if ($xrandr =~ /$output connected/){
                addToXrandrCmd($output,"--mode 1920x1080 $modifier $lvdsOutput --rotate $rotate --primary");
            }
        }
        runXrandrCmd();
    }

    setupDockedOutputs();
    system("xset dpms 900 1800 2700");
    system("xbacklight =100");
}

################################################################################
sub commonConfig{
    system("feh --bg-center \"$background\"");
}

################################################################################
sub setupSound{
    sub configureSoundCardIfNotPredefined{
        $alsaOutput eq "" ?
            configureSoundCard($_[0]) :
            configureSoundCard($alsaOutput);
    }
    sleep(1);
    configureSoundCardIfNotPredefined("PCH");
    if (! $docked){
        system("amixer -q set Master off");
    }
}

################################################################################
##  main                                                                      ##
################################################################################
$docked ? dockedConfig() : undockedConfig();
commonConfig();
setupSound();
