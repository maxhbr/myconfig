#!/usr/bin/env perl
#
# parses the files in the folder "_files/" and copys all listed files, which are
# changed, to the git repo.
#
#  written by maximilian-huber.de

use strict;
use warnings;
use Cwd qw( abs_path );
use File::Basename qw( fileparse );
use File::Compare qw( compare );
use Term::ANSIColor qw( colored );

my $forceRun = 0; # default: 0
my $dryRun   = 0; # default: 0

################################################################################
##  prepare                                                                   ##
################################################################################
if ($#ARGV != 0) {
    print "\nUsage: $0 Path/to/group\n";
    exit;
}
my $base = abs_path($ARGV[0]);
my $mbase = "$base/.meta";
die colored(['red'], "given directory contains no meta information!\n","")
    if (!-d $mbase);

my $myhome = glob('~');

################################################################################
##  subroutines                                                               ##
################################################################################
sub getSubPath{
    if ($_[0] !~ /^$base/){
        die colored(['red'], "path has not the correct prefix","");
    }
    if ($_[0] =~ /^$mbase/){
        return substr($_[0],length($mbase));
    }else{
        return substr($_[0],length($base));
    }
}
sub metaFromPath{return $mbase . getSubPath($_[0]);}
sub filenameFromPath{return $base . getSubPath($_[0]);}
sub showSubPath{
    my($filename, $dir, $suffix) = fileparse($_[0]);
    return $dir . colored(['bold'],$filename,"");
}
sub prompt_yn {
    sub prompt {
        my ($query) = @_;
        local $| = 1;
        print $query;
        chomp(my $answer = <STDIN>);
        return $answer;
    }
    my ($query) = @_;
    my $answer = prompt("$query [yN]: ");
    return lc($answer) eq 'y';
}

################################################################################
##  run                                                                       ##
################################################################################
foreach my $mfile (glob("$mbase/* $mbase/**/*")) {
    my $file = filenameFromPath($mfile);
    if (-f $file && open(my $fh, '<:encoding(UTF-8)', $mfile)){
        chomp(my @mdata = <$fh>);
        close $fh;
        my $target = (glob($mdata[0]))[0];
        if(!-f $target){
            print showSubPath($target) .
                " @{[colored(['yellow'],'does not exist yet','')]}\n";
        }elsif(compare($file,$target) != 0) {
            print showSubPath($target) .
                " is @{[colored(['red'],'different','')]}\n";
        }else{
            print showSubPath($target) .
                " is @{[colored(['green'], 'already installed','')]}\n";
            next if !$forceRun;
        }
        if (!$dryRun && prompt_yn "do you want to install it?"){
            # TODO:
            die colored(['bold red'], "not implemented yet", "\n");
        }
    }
}
