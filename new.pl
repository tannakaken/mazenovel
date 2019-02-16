use strict;
use warnings;
use File::Copy;

my @file = glob "./novels/*";
my $len = @file;
my $target = "./novels/novel".($len+1).".txt";
my $source = $file[int(rand($len))];
copy($source, $target) or die "error: $!";
print $target, "\n";
