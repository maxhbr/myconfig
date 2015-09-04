#!/usr/bin/env perl
#
#  written by maximilian-huber.de

use strict;
use warnings;
use File::Basename qw( dirname );
use Cwd qw( abs_path );
use Term::ANSIColor;

chdir(abs_path("@{[dirname($0)]}/.."));

print "update: @{[colored(['bold green'], 'submodules','')]}\n";
system("git", "submodule", "foreach", "git", "pull");
