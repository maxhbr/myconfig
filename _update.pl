#!/usr/bin/env perl
#
# parses the files in the folder "_files/" and copys all listed files, which are
# changed, to the git repo.
#
#  written by maximilian-huber.de

use strict;
use warnings;
use File::Path qw( make_path );
use File::Basename qw( basename fileparse );
use File::Copy qw( copy );
use File::Compare qw( compare );
use Cwd qw( abs_path );
use Sys::Hostname qw( hostname );

my $defaultHostname = "t450s";
my $defaultOut = "./";
my $updateFiles = 1;
my $useGit = 1;
my $doMore = 1;

my %toLink = (
    '/zsh/zsh/' => '~/.zsh',
    '/emacs/emacs.d/snippets' => '~/.emacs.d/snippets',
    '/emacs/emacs.d/use-package' => '~/.emacs.d/use-package',
    );

################################################################################
my $myhome = glob('~');
my $outDir = abs_path("@{[hostname() eq $defaultHostname ? $defaultOut : hostname()]}");

system("git", "commit", "-a", "-m \"automatic commit bevore update\"", "-e")
    if $useGit;
sub update{
    if ( !-d $outDir ) {make_path $outDir or die "Failed to create: $outDir";}
    ############################################################################
    # functions
    sub getTargetName{
        # parameters are:
        #   toppic
        #   path of src file
        #   bool, if ...
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
    sub savePermissions{
        # parameters are:
        #   path of src file
        #   path of meta file
        my @stat = stat($_[0]);
        my($filename, $dir, $suffix) = fileparse($_[1]);
        if ( !-d $dir ) {make_path $dir or die "Failed to create: $dir";}
        open MDATA, ">$_[1]";
        print MDATA "$_[0]\n";
        print MDATA "@{[sprintf \"%04o\", $stat[2] & 07777]}\n";
        print MDATA "$stat[4]\n";
        print MDATA "$stat[5]\n";
        close MDATA;
    }
    sub updateFile{
        # parameters are:
        #   toppic
        #   path of src file
        my $target = getTargetName($_[0],$_[1],0);
        if(compare($_[1],$target) != 0) {
            my $mtarget = getTargetName($_[0],$_[1],1);
            my($filename, $dir, $suffix) = fileparse($target);
            print "$filename\n";
            if ( !-d $dir ) {make_path $dir or die "Failed to create: $dir";}
            copy($_[1],$target) or die "Copy failed: $!";
            savePermissions($_[1],$mtarget);
            system("git", "add", $target) if $useGit;
            system("git", "add", $mtarget) if $useGit;
        }
    }

    ############################################################################
    # do everything
    foreach (glob('_files/*')) {
        my $filesFile = $_;
        my @curToppicParts = split /@/, basename($filesFile);
        if (@curToppicParts > 1 && !($curToppicParts[1] eq hostname())) {next;}
        my $curToppic = $curToppicParts[0];
        if (open(my $fh, '<:encoding(UTF-8)', $filesFile)) {
            while (my $file = <$fh>) {
                if ($file =~ /^#/) { next; }
                foreach (glob($file)) {
                    updateFile($curToppic,$_) if -r "$_";
                }
            }
            system("git", "commit"
                   , "-m \"automatic commit for $curToppic\"", "-e") if $useGit;
        } else {
            warn "Could not open file '$filesFile' $!";
        }
    }
}
update() if $updateFiles;

sub moreToDo{
    system("git", "submodule", "foreach", "git", "pull");
    while ( my ($key, $value) = each(%toLink) ) {
        $value = "@{[glob($value)]}";
        $key = "@{[abs_path($defaultOut)]}$key";
        if ( !-d $value && -d $key ) {
            print "$key => $value\n";
            symlink($key, $value);
        }
    }
}
moreToDo() if $doMore;
