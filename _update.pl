#!/usr/bin/env perl
#
# parses the files in the folder "_files/" and copys all listed files, which are
# changed, to the git repo.
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Cwd qw( abs_path );
use File::Basename qw( basename fileparse dirname );
use File::Compare qw( compare );
use File::Copy qw( copy );
use File::Path qw( make_path );
use Getopt::Long qw(GetOptions);
use Sys::Hostname qw( hostname );
use Term::ANSIColor qw( colored );

my %givenOutDirs = (
    't450s' => './', # default host is t450s
    );
my $updateFiles  = 1; # default: 1
my $useGit       = 1; # default: 1
my $doHooks      = 1; # default: 1
my $forceUpdates = 0; # default: 0

################################################################################
##  prepare                                                                   ##
################################################################################
GetOptions(
    'updateFiles|u'    => \$updateFiles,
    'noUpdateFiles'    => sub { $updateFiles = 0 },
    'useGit|g'         => \$useGit,
    'noGit'            => sub { $useGit = 0 },
    'doHooks|h'        => \$doHooks,
    'noHooks'          => sub { $doHooks = 0 },
    'forceUpdates|s'   => \$forceUpdates,
    'noForceUpdates|s' => sub { $forceUpdates = 0 },
) or die "Usage: $0 [--forceUpdates|-f] [--noGit] [--noHooks]\n";

chdir dirname($0);
my $myhome = glob('~');

my $outDir = "HOST:@{[hostname()]}";
while ( my ($key, $value) = each(%givenOutDirs) ) {
    if ( $key eq hostname() ) {$outDir = $value;}
}
$outDir = abs_path($outDir);

################################################################################
##  subroutines                                                               ##
################################################################################
sub update{
    if ( !-d $outDir ) {
        make_path $outDir or
            die colored(['red'], "Failed to create: $outDir","");
    }
    my $rdme = getTargetName("","/README.md",0);
    if(!-e $rdme){
        open README, ">$rdme";
        print README "# myconfig for the host: @{[hostname()]}\n";
        close README;
        system("git", "add", $rdme) if $useGit;
    }
    ############################################################################
    # subroutines
    sub getTargetName{
        # parameters are:
        #   toppic
        #   path of src file
        #   bool, if meta
        my $toppic = ($_[0] eq "root") ? "" : "/$_[0]";
        if ($_[2]) {$toppic = "$toppic/.meta";}
        my $abs_path = abs_path($_[1]);
        sub trimHome{
            if ($_[0] =~ /^$myhome\/\./){
                return("/@{[substr($_[0],length($myhome) + 2)]}");
            } elsif ($_[0] =~ /^$myhome/){
                return(substr($_[0],length($myhome)));
            } else {return($_[0]);}
        }
        return("$outDir$toppic@{[trimHome($abs_path)]}");
    }
    sub writeToppicReadme{
        # parameters are
        #   topic
        my $rdme = getTargetName($_[0],"/README.md",0);
        if(!-e $rdme){
            open README, ">$rdme";
            print README "# myconfig for the toppic: $_[0]\n";
            close README;
            system("git", "add", $rdme) if $useGit;
        }
    }
    sub savePermissions{
        # parameters are:
        #   path of src file
        #   path of meta file
        my @stat = stat($_[0]);
        my($filename, $dir, $suffix) = fileparse($_[1]);
        if ( !-d $dir ) {
            make_path $dir or die colored(['red'], "Failed to create: $dir","");
        }
        open MDATA, ">$_[1]";
        print MDATA "$_[0]\n";
        print MDATA "@{[sprintf \"%04o\", $stat[2] & 07777]}\n";
        print MDATA "$stat[4]\n";
        print MDATA "$stat[5]\n";
        close MDATA;
        system("git", "add", $_[1]) if $useGit;
    }
    sub updateFile{
        # parameters are:
        #   toppic
        #   path of src file
        my $target = getTargetName($_[0],$_[1],0);
        if(compare($_[1],$target) != 0 || $forceUpdates) {
            my($tFilename, $tDir, $suffix) = fileparse($target);
            print "update: @{[colored(['bold green'], $tFilename,'')]} (of toppic: $_[0])\n";
            if ( !-d $tDir ) {make_path $tDir or die "Failed to create: $tDir";}
            copy($_[1],$target) or die colored(['red'], "Copy failed: $!", "");
            system("git", "add", $target) if $useGit;
            savePermissions($_[1],getTargetName($_[0],$_[1],1));
        }
    }

    ############################################################################
    # do everything
    foreach (glob('_files/*')) {
        my $filesFile = $_;
        my @curToppicParts = split /@/, basename($filesFile);
        if (@curToppicParts > 1 && !($curToppicParts[1] eq hostname())) {next;}
        my $curToppic = $curToppicParts[0];
        print "update toppic: @{[colored(['bold green'], $curToppic,'')]}\n";
        writeToppicReadme($curToppic);
        if (open(my $fh, '<:encoding(UTF-8)', $filesFile)) {
            while (my $file = <$fh>) {
                if ($file =~ /^#/) { next; }
                foreach (glob($file)) {
                    updateFile($curToppic,$_) if -r "$_";
                }
            }
            system("git", "commit"
                   , "-m automatic commit for $curToppic", "-e") if $useGit;
        } else {
            warn "Could not open file '$filesFile' $!";
        }
    }
}

sub runHooks{
    # parameters
    #   name of hooks to run
    print "run hooks: @{[colored(['bold green'], $_[0],'')]}\n";
    my $hookDir = "_hooks-$_[0]";
    if ( -d $hookDir ) {
        foreach (glob("$hookDir/*")) {system($_) if -x $_;}
    }
}

################################################################################
##  run                                                                       ##
################################################################################
system("git", "commit", "-a", "-m automatic commit bevore update", "-e")
    if $useGit;

runHooks("bevore") if $doHooks;
update() if $updateFiles;
runHooks("after") if $doHooks;

print colored(['bold green'], "git push","\n") if $useGit;
system("git", "push") if $useGit;
