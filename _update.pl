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
use Getopt::Long qw( GetOptions );
use Sys::Hostname qw( hostname );
use Term::ANSIColor qw( colored );

my %predefinedOutDirs = (
    # desktops:
    't450s' => './',
    'nixos' => './',
    );
my $updateFiles  = 1; # default: 1
my $useGit       = 1; # default: 1
my $noPush       = 0; # default: 1
my $doHooks      = 1; # default: 1
my $forceUpdates = 0; # default: 0
my $dryRun       = 0; # default: 0

################################################################################
##  prepare                                                                   ##
################################################################################
GetOptions(
    'updateFiles|u'    => \$updateFiles,
    'noUpdateFiles'    => sub { $updateFiles = 0 },
    'useGit|g'         => \$useGit,
    'noPush'           => \$noPush,
    'noGit'            => sub { $useGit = 0 },
    'doHooks|h'        => \$doHooks,
    'noHooks'          => sub { $doHooks = 0 },
    'forceUpdates|s'   => \$forceUpdates,
    'noForceUpdates|s' => sub { $forceUpdates = 0 },
    'dryRun'           => \$dryRun,
) or die "Usage: $0 [--dryRun] [--forceUpdates|-f] [--noGit] [--noHooks]\n";

if ($dryRun) {$useGit = 0; $doHooks = 0;}
if ($useGit == 0) {$noPush = 1;}

chdir dirname($0);
my $myhome = (glob('~'))[0]; # TODO: use a clojure/closure?

my $outDir = "HOST:@{[hostname()]}";
while ( my ($key, $value) = each(%predefinedOutDirs) ) {
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
    my $rdme = getTargetName("root","/README.md",0);
    if(!-e $rdme && open(README, ">$rdme")) {
        print README "# myconfig for the host: @{[hostname()]}\n";
        close README;
        system("git", "add", $rdme) if $useGit;
    }
    ############################################################################
    # subroutines
    sub getTargetName{
        # parameters are:
        #   topic
        #   path of src file
        #   bool, if meta
        my $topic = ($_[0] eq "root") ? "" : "/GRP:$_[0]";
        if ($_[2]) {$topic = "$topic/.meta";}
        my $abs_path = abs_path($_[1]);
        sub expandHome{
            if ($_[0] =~ /^$myhome\/\./){
                return("/@{[substr($_[0],length($myhome) + 2)]}");
            } elsif ($_[0] =~ /^$myhome/){
                return(substr($_[0],length($myhome)));
            } else {return($_[0]);}
        }
        return("$outDir$topic@{[expandHome($abs_path)]}");
    }
    sub writeTopicReadme{
        # parameters are
        #   topic
        my $rdme = getTargetName($_[0],"/README.md",0);
        if(!-e $rdme && open(README, ">$rdme")) {
            print README "# myconfig for the topic: $_[0]\n";
            close README;
            system("git", "add", $rdme) if $useGit;
        }
    }
    sub savePermissions{
        # parameters are:
        #   path of src file
        #   path of meta file
        sub tightenHome{
            if ($_[0] =~ /^$myhome/){
                return '~' . substr($_[0],length($myhome));
            }else{return $_[0];}
        }

        my @stat = stat($_[0]);
        my($filename, $dir, $suffix) = fileparse($_[1]);
        if ( !-d $dir ) {
            make_path $dir or die colored(['red'], "Failed to create: $dir","");
        }
        if (open(MDATA, ">$_[1]")) {
            print MDATA tightenHome($_[0]) . "\n";
            print MDATA sprintf("%04o", $stat[2] & 07777) . "\n";
            print MDATA $stat[4] . "\n";
            print MDATA $stat[5] . "\n";
            print MDATA hostname() . "\n";
            close MDATA;
            system("git", "add", $_[1]) if $useGit;
        }
    }
    sub updateFile{
        # parameters are:
        #   topic
        #   path of src file

        sub isLastUpdatedByCurrentHost{
            # parameters are:
            #   path of mdata
            if (-f $_[0] && open(my $fh, '<:encoding(UTF-8)', $_[0])){
                chomp(my @mdata = <$fh>);
                close $fh;
                return $mdata[4] eq hostname();
            }
            return 1;
        }

        my $target = getTargetName($_[0],$_[1],0);
        my $mtarget = getTargetName($_[0],$_[1],1);
        if(!(-f $target) || compare($_[1],$target) != 0 || $forceUpdates) {
            my($tFilename, $tDir, $suffix) = fileparse($target);
            print "update: @{[colored(['bold green'], $tFilename,'')]} (of topic: $_[0])\n";
            if (!-d $tDir && !$dryRun) {
                make_path $tDir or die colored(['red'], "Failed to create: $tDir");
            }
            if (isLastUpdatedByCurrentHost($mtarget) || $forceUpdates){
                copy($_[1],$target) or die colored(['red'], "Copy failed: $!", "")
                    if !$dryRun;
            }else{
                system("touch",$target)
                    if !$dryRun;
                system("meld", $_[1], $target)
                    if !$dryRun;
            }
            system("git", "add", $target) if $useGit;
            savePermissions($_[1],$mtarget)
                if !$dryRun;
        }
    }
    sub isOneOfTheHosts{
        # parameters are:
        #   hostlist, i.e. "host1,host2,host3"
        my @hosts = split /,/, $_[0];
        for( @hosts ){
            return 1 if hostname() eq $_;
        }
        return 0;
    }

    ############################################################################
    # do everything
    foreach my $filesFile (glob('_files/*')) {
        my @curTopicParts = split /@/, basename($filesFile);
        #TODO: komma seperated hosts
        if (@curTopicParts > 1 && !(isOneOfTheHosts($curTopicParts[1]))) { next; }
        my $curTopic = $curTopicParts[0];
        print "update topic: @{[colored(['bold green'], $curTopic,'')]}\n";
        writeTopicReadme($curTopic);
        if (open(my $fh, '<:encoding(UTF-8)', $filesFile)) {
            while (my $file = <$fh>) {
                if ($file =~ /^#/) { next; }
                foreach (glob($file)) {
                    updateFile($curTopic,$_) if -r "$_";
                }
            }
            close $fh;
            system("git", "commit"
                   , "-m automatic commit for $curTopic", "-e")
                if $useGit;
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
system("git", "commit", "-a", "-m automatic commit before update", "-e")
    if $useGit;

runHooks("before") if $doHooks;
update() if $updateFiles;
runHooks("after") if $doHooks;

print colored(['bold green'], "git push","\n")
    if $noPush == 0;
system("git", "push") if $noPush == 0;
