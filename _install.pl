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
# use File::Copy qw( copy );
# use File::Path qw( make_path );
# use Sys::Hostname qw( hostname );
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

################################################################################
##  subroutines                                                               ##
################################################################################
sub getSubPath{
    if ($_[0] !~ /^$base/){
        die colored(['red'], "path has not the correct prefix","");
    }
    return substr($_[0],length($base));
}
sub showSubPath{
    my($filename, $dir, $suffix) = fileparse(getSubPath($_[0]));
    return $dir . colored(['bold'],$filename,"");
}
sub metaFromPath{
    return $mbase . getSubPath($_[0]);
}

################################################################################
##  run                                                                       ##
################################################################################
foreach my $file (glob("$base/**/*")) {
    my $mfile = metaFromPath($file);
    if (-f $file && -e $mfile && open(my $fh, '<:encoding(UTF-8)', $mfile)){
        my @mdata = <$fh>;
        chomp $mdata[0];
        if(!-f $mdata[0]){
            print showSubPath($file) . " @{[colored(['yellow'],'does not exist yet','')]}\n";
        }else{
            if(compare($file,$mdata[0]) != 0) {
                print showSubPath($file) . " @{[colored(['red'],'is different','')]}\n";
            }else{
                print showSubPath($file) . " is @{[colored(['green'], 'already installed','')]}\n";
                next if !$forceRun;
            }
        }
        print "do you want to install it? [yN]\n" if !$dryRun;
    }
}
