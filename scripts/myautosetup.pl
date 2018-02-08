#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Getopt::Long qw( GetOptions );
use Term::ANSIColor qw( colored );
use Scalar::Util qw( looks_like_number );
use Digest::MD5 qw(md5_base64);
use threads;

################################################################################
{ # pidfile handling
    my $pidfile = "/tmp/myautosetup.pl.pid";

    my $pid = $$;
    if (-f $pidfile) {
        my $oldpid;
        open(my $fh, '<', $pidfile) or die "cannot open pidfile ($pidfile)";
        {
            local $/;
            $oldpid = <$fh>;
        }
        close($fh);
        chomp $oldpid;
        if (kill(0, $oldpid)) {
            die "Already running as $oldpid!";
        } else {
            unlink($pidfile);
        }
    }

    open(my $fh, '>', $pidfile);
    print $fh $pid;
    close $fh;

    sub cleanupPidfile {
        unlink($pidfile)
    }
}

################################################################################
my $rotate = "normal";
my $noXrandr = 0;
my $sameAs = 0;
my $allOff = 0;
my $onlyIfChanged = 0;

my $alsaOutput = "";

GetOptions(
    'rotate=s'      => \$rotate,
    'noXrandr'      => \$noXrandr,
    'setSound=s'    => \$alsaOutput,
    'sameAs'        => \$sameAs,
    'allOff'        => \$allOff,
    'onlyIfChanged' => \$onlyIfChanged,
    ) or die "Usage: $0 \n\t[--rotate=rotation]\n\t[--setSound=CardName/CardNumber]\n\t[--noXrandr|--sameAs]\n";

my $acPresent = `acpi -a | grep -c on-line`;
my $lidState = `cat /proc/acpi/button/lid/LID/state`;
# my $wifiState = `nmcli dev wifi`;
# my $atWork = $wifiState =~ /TNG/;

################################################################################

sub call{
    print "$_[0]\n";
    system($_[0]);
}

################################################################################

my $xrandr = `xrandr`;

{ # xrandr env hash
    my $envHashFile = "/tmp/myautosetup.pl.envHash";

    sub getCalculatedEnvHash {
        my ( $xrandrOutput ) = @_;
        $xrandrOutput = `xrandr` if (! defined $xrandrOutput);
        return md5_base64("$xrandrOutput $lidState");
    }

    sub getOldEnvHash {
        my $oldEnvHash = "";
        if (-f $envHashFile) {
            if(open my $file, '<', $envHashFile) {
                $oldEnvHash = <$file>;
                chomp $oldEnvHash;
                close $file;
            }
        }
        return $oldEnvHash;
    }

    sub isEnvUnchanged {
        return getCalculatedEnvHash($xrandr) eq getOldEnvHash();
    }

    sub writeCurHashToFile {
        my $envHash = getCalculatedEnvHash();
        open(my $file, '>', $envHashFile);
        print $file $envHash;
        close $file;
    }
}

if ($onlyIfChanged && isEnvUnchanged()) {
    print "nothing to do\n";
    exit 0;
}

################################################################################

{ # xrandr
    my $lvdsOutput = "eDP1";
    my @outputs = (
        "DP1", "DP1-8",
        "DP2", "DP2-2", "DP2-3", "DP2-8", "DP2-1",
        "DP3-1", "DP3-2", "DP3-3",
        "HDMI1", "HDMI2", "HDMI3"
        );
    my %resolutions = (
        $lvdsOutput => "2560x1440",
        );

    my $primaryOutput = $lvdsOutput;
    my @activeOutputs = ();
    my @otherOutputs = ();
    push @otherOutputs, @outputs;

    if($lidState =~ /open/) {
        push @activeOutputs, $lvdsOutput;
    }

    sub addAllOutputs {
        foreach my $output (@outputs) {
            if ($xrandr =~ /\W$output connected/){
                push @activeOutputs, $output;
                @otherOutputs = grep { $_ ne $output } @otherOutputs;
                if($primaryOutput eq $lvdsOutput){
                    $primaryOutput = $output;
                }
            }
        }
    }

    sub toggleAllOutputs {
        foreach my $output (@outputs) {
            if ($xrandr =~ /\W$output connected/ &&
                $xrandr !~ /\W$output connected \(/ &&
                $xrandr !~ /\W$output connected \w \(/ ){
                return;
            }
        }
        addAllOutputs();
    }

    sub turnOthersOff{
        my $xrandrCmd = "xrandr --auto";
        foreach my $output (@otherOutputs) {
            if ($xrandr =~ /\W$output /){
                $xrandrCmd .= " --output $output --rotate normal --scale 1x1 --off";
            }
        }
        call($xrandrCmd);
    }

    sub setupLeftToRight{
        turnOthersOff();

        my $lastOutput = "";
        foreach my $output (@activeOutputs) {
            my $xrandrCmd = "xrandr --output $output";
            $xrandrCmd .= ($output eq $primaryOutput) ? " --primary" : "";
            $xrandrCmd .= (defined $resolutions{$output}) ? " --mode $resolutions{$output}" : " --auto";
            $xrandrCmd .= ($lastOutput eq "") ? " --pos 0x0" : " --right-of $lastOutput";
            $xrandrCmd .= " --rotate normal --scale 1x1";
            call($xrandrCmd);
            $lastOutput = $output;
        }
    }

    sub setupSameAsLVDS{
        turnOthersOff();

        $primaryOutput = $lvdsOutput;
        foreach my $output (@activeOutputs) {
            my $xrandrCmd = "xrandr --output $output";
            $xrandrCmd .= ($output eq $primaryOutput) ? " --primary" : "";
            $xrandrCmd .= (defined $resolutions{$output}) ? " --mode $resolutions{$output}" : " --auto";
            $xrandrCmd .= " --pos 0x0";
            $xrandrCmd .= " --rotate normal --scale-from $resolutions{$lvdsOutput}";
            call($xrandrCmd);
        }
    }

    sub getPrimaryOutput{
        return $primaryOutput;
    }
}
sub setupX{
    if(!$allOff) {
        if($lidState =~ /open/ || ! $sameAs){
            toggleAllOutputs();
        }else{
            addAllOutputs();
        }
    }
    if($sameAs){
        setupSameAsLVDS();
    }else{
        setupLeftToRight();
    }

    writeCurHashToFile();
    system("xmonad --restart");
}

################################################################################

sub setupAlsa{
    my @alsaOutputs = ("BT600" => "on", "USB" => "on", "CODEC" => "on", "PCH" => "off");
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

sub setupBacklight{
    $acPresent ? call("xbacklight =70") : call("xbacklight =100");
}

################################################################################

sub setupWacom{
    my @wacomDevices = `xsetwacom --list devices | sed -e 's#.*id: \\(\\)#\\1#' | awk '{print \$1;}'`;
    chomp @wacomDevices;
    foreach my $wacomDevice (@wacomDevices) {
        my $wacomCmd = "xsetwacom --set $wacomDevice MapToOutput @{[getPrimaryOutput()]}";
        call($wacomCmd);
    }
}

################################################################################

sub setupBackgroundAndUI{
    my $background = "/home/mhuber/.background-image";
    call("feh --bg-scale \"$background\"");

    call("xrdb -merge ~/.Xresources");
}

################################################################################

setupX() if !$noXrandr;
# setupAlsa(); # deprecated in favor of pulseaudio
setupWacom() if !$noXrandr; # needs `setupX()` to be run before the call here
setupBacklight();
setupBackgroundAndUI();

################################################################################
#TODO:
# system("setxkbmap -layout de,de -variant neo,nodeadkeys -option grp:shifts_toggle -option grp_led:scroll -option altwin:swap_lalt_lwin")
# system("xset dpms 300 600 900");

################################################################################

cleanupPidfile();
