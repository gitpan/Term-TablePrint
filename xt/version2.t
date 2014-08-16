use 5.010000;
use strict;
use warnings;
use Time::Piece;
use Test::More tests => 3;


my $v            = -1;
my $v_pod        = -1;
my $v_changes    = -1;
my $release_date = -1;


open my $fh1, '<', 'lib/Term/TablePrint.pm' or die $!;
while ( my $line = <$fh1> ) {
    if ( $line =~ /^our\ \$VERSION\ =\ '(\d\.\d\d\d(?:_\d\d)?)';/ ) {
        $v = $1;
    }
    if ( $line =~ /^=pod/ .. $line =~ /^=cut/ ) {
        if ( $line =~ /^\s*Version\s+(\S+)/ ) {
            $v_pod = $1;
        }
    }
}
close $fh1;


open my $fh_ch, '<', 'Changes' or die $!;
while ( my $line = <$fh_ch> ) {
    if ( $line =~ /^\s*(\d\.\d\d\d(?:_\d\d)?)\s+(\d\d\d\d-\d\d-\d\d)\s*\Z/ ) {
        $v_changes = $1;
        $release_date = $2;
        last;
    }
}
close $fh_ch;


my $t = localtime;
my $today = $t->ymd;


is( $v,            $v_pod,         'Version in POD Term::TablePrint OK' );
is( $v,            $v_changes,     'Version in "Changes" OK' );
is( $release_date, $today,         'Release date in Changes is date from today' );


