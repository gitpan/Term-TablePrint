package Term::TablePrint;

use warnings;
use strict;
use 5.10.1;

our $VERSION = '0.002_01';
use Exporter 'import';
our @EXPORT_OK = qw( print_table );

use Carp          qw( croak );
use List::Util    qw( sum );
use Scalar::Util  qw( looks_like_number );

use Term::Choose       qw( choose );
use Term::ProgressBar;
use Term::Size::Any    qw( chars );
use Text::LineFold;
use Unicode::GCString;

no warnings 'utf8';

use constant CLEAR_SCREEN   => "\e[1;1H\e[0J";

sub new {
    return bless {}, shift if @_ == 1;
    return bless $_[1], $_[0] if eval { $_[1]->{db_browser_mode} };
    my ( $class, $opt ) = @_;
    my $arg = scalar( @_ ) - 1;
    croak "new: called with $arg arguments - 0 or 1 arguments expected." if $arg > 1;
    croak "new: The (optional) argument is not a HASH reference." if ref $opt ne 'HASH';
    my $self = bless {}, $class;
    $self->__validate_options( $opt );
    return $self;
}

sub __validate_options {
    my ( $self, $opt ) = @_;
    return if ! defined $opt;
    for my $key ( qw( progress max_rows min_col_width tab_width ) ) { # choose_columns
        next if ! defined $opt->{$key};
        croak "print_table: '$opt->{$key}' is not a valid value for option '$key'." if $opt->{$key} !~ /^\d+\z/;
        $self->{$key} = $opt->{$key};
    }
    for my $key ( qw( table_expand binary_filter ) ) { #  header_row
        next if ! defined $opt->{$key};
        croak "print_table: '$opt->{$key}' is not a valid value for option '$key'." if $opt->{$key} !~ /^[01]\z/;
        $self->{$key} = $opt->{$key};
    }
    for my $key ( qw( mouse ) ) {
        next if ! defined $opt->{$key};
        croak "print_table: '$opt->{$key}' is not a valid value for option '$key'." if $opt->{$key} !~ /^[01234]\z/;
        $self->{$key} = $opt->{$key};
    }
}

sub __set_defaults {
    my ( $self ) = @_;
    $self->{progress_bar} = 20000 if ! exists $self->{progress_bar};
    $self->{max_rows}     = 50000 if ! exists $self->{max_rows};
    $self->{tab_width}      //= 2;
    $self->{min_col_width}  //= 30;
    $self->{table_expand}   //= 1;
    $self->{binary_filter}  //= 0;
    $self->{undef}          //= '';
    $self->{mouse}          //= 0;
    $self->{binary_string}  //= 'BNRY';
    #$self->{header_row}     //= 1;
    #$self->{choose_columns} //= 0;
    $self->{thsd_sep} = ',';
}


sub print_table {
    if ( ref $_[0] ne 'Term::TablePrint' ) {
        return Term::TablePrint->new( $_[1] )->print_table( $_[0] );
    }
    my ( $self, $a_ref, $opt ) = @_;
    if ( ! $self->{db_browser_mode} ) {
        my $arg = scalar( @_ ) - 1;
        croak "print_table: called with $arg arguments - 1 or 2 arguments expected." if $arg < 1 || $arg > 2;
        croak "print_table: Required an ARRAY reference as the first argument."      if ref $a_ref  ne 'ARRAY';
        croak "print_table: The (optional) second argument is not a HASH reference." if defined $opt && ref $opt ne 'HASH';
        $self->{backup_opt} = { map{ $_ => $self->{$_} } keys %$opt } if defined $opt;
        $self->__validate_options( $opt );
        $self->__set_defaults();
        #if ( $self->{choose_columns} ) {
        #    my @i = choose( $a_ref->[0], { prompt => 'Choose: ', index => 1 } );
        #    for my $i ( 0 .. $#$a_ref ) {
        #        $a_ref->[$i] = [ @{$a_ref->[$i]}[sort @i] ];
        #    }
        #}
    }
    my $gcs_bnry = Unicode::GCString->new( $self->{binary_string} );
    $self->{binary_length} = $gcs_bnry->columns;
    if ( defined $self->{progress_bar} ) {
        say 'Computing: ...';
        if ( @$a_ref * @{$a_ref->[0]} > $self->{progress_bar} ) {
            $self->{show_progress} = 1;
        }
    }
    $self->__calc_col_width( $a_ref );
    $self->__inner_print_tbl( $a_ref );
    if ( $self->{backup_opt} ) {
        my $backup_opt = delete $self->{backup_opt};
        for my $key ( keys %$backup_opt ) {
            $self->{$key} = $backup_opt->{$key};
        }
    }
}


sub __inner_print_tbl {
    my ( $self, $a_ref ) = @_;
    my ( $term_width ) = _term_size();
    my $width_cols = $self->__calc_avail_width( $a_ref, $term_width );
    return if ! $width_cols;
    my ( $list, $len ) = $self->__trunk_col_to_avail_width( $a_ref, $width_cols );
    if ( $self->{max_rows} && @$list > $self->{max_rows} ) {
        my $reached_limit = 'REACHED LIMIT "MAX_ROWS": ' . _insert_sep( $self->{max_rows}, $self->{thsd_sep} );
        my $gcs = Unicode::GCString->new( $reached_limit );
        if ( $gcs->columns > $term_width ) {
            $reached_limit = _unicode_sprintf( $term_width, 'REACHED LIMIT', 0 );
        }
        push @$list, $reached_limit;
    }
    my $old_row = 0;
    my ( $width ) = _term_size();
    while ( 1 ) {
        if ( ( _term_size() )[0] != $width ) {
            ( $width ) = _term_size();
            $self->__inner_print_tbl( $a_ref );
            return;
        }
        my $row = choose(
            $list,
            { prompt => '', index => 1, default => $old_row, ll => $len, layout => 3, clear_screen => 1, mouse => $self->{mouse} }
        );
        return if ! defined $row;
        return if $row == 0;
        if ( ! $self->{table_expand} ) {
            $old_row = 0;
            next;
        }
        if ( $old_row == $row ) {
            $old_row = 0;
            next;
        }
        $old_row = $row;
        my $row_data = $self->__single_row( $a_ref, $row, $self->{longest_col_name} + 1 );
        choose(
            $row_data,
            { prompt => '', layout => 3, clear_screen => 1, mouse => $self->{mouse} }
        );
    }
}


sub __single_row {
    my ( $self, $a_ref, $row, $length_key ) = @_;
    my ( $term_width ) = _term_size();
    $length_key = int( $term_width / 100 * 33 ) if $length_key > int( $term_width / 100 * 33 );
    my $separator = ' : ';
    my $gcs = Unicode::GCString->new( $separator );
    my $length_sep = $gcs->columns;
    #$length_key = length( scalar @{$a_ref->[0]} ) if ! $self->{header_row};
    my $col_max = $term_width - ( $length_key + $length_sep + 1 );
    my $line_fold = Text::LineFold->new(
        Charset=> 'utf8',
        OutputCharset => '_UNICODE_',
        Urgent => 'FORCE' ,
        ColMax => $col_max,
    );
    my $row_data = [ ' Close with ENTER' ];
    for my $col ( 0 .. $#{$a_ref->[0]} ) {
        push @{$row_data}, ' ';
        my $key = $a_ref->[0][$col];
        #$key = $col + 1 if ! $self->{header_row};
        my $sep = $separator;
        if ( ! defined $a_ref->[$row][$col] || $a_ref->[$row][$col] eq '' ) {
            push @{$row_data}, sprintf "%*.*s%*s%s", $length_key, $length_key, $key, $length_sep, $sep, '';
        }
        else {
            my $text = $line_fold->fold( $a_ref->[$row][$col], 'PLAIN' );
            for my $line ( split /\R+/, $text ) {
                push @{$row_data}, sprintf "%*.*s%*s%s", $length_key, $length_key, $key, $length_sep, $sep, $line;
                $key = '' if $key;
                $sep = '' if $sep;
            }
        }
    }
    return $row_data;
}


sub __calc_col_width {
    my ( $self, $a_ref ) = @_;
    my $binray_regexp = qr/[\x00-\x08\x0B-\x0C\x0E-\x1F]/;
    $self->{longest_col_name} = 0;
    my $normal_row = 0;
    #$normal_row = 1 if ! $self->{header_row};
    $self->{width_cols} = [ ( 1 ) x @{$a_ref->[0]} ];
    #$self->{width_head} = [ ( 1 ) x @{$a_ref->[0]} ] if ! $self->{header_row};
    my @col_idx = ( 0 .. $#{$a_ref->[0]} );
    for my $row ( @$a_ref ) {
        for my $i ( @col_idx ) {
            $row->[$i] //= $self->{undef};
            if ( ref $row->[$i] ) {
                $row->[$i] = $self->__handle_reference( $row->[$i] );
            }
            my $width;
            if ( $self->{binary_filter} && substr( $row->[$i], 0, 100 ) =~ $binray_regexp ) {
                $row->[$i] = $self->{binary_string};
                $width     = $self->{binary_length};
            }
            else {
                $row->[$i] =~ s/^\p{Space}+//;
                $row->[$i] =~ s/\p{Space}+\z//;
                #$row->[$i] =~ s/(?<=\P{Space})\p{Space}+/ /g;
                $row->[$i] =~ s/\p{Space}+/ /g;
                $row->[$i] =~ s/\p{C}//g;
                my $gcs = Unicode::GCString->new( $row->[$i] );
                $width = $gcs->columns;
            }
            if ( $normal_row ) {
                $self->{width_cols}[$i] = $width if $width > $self->{width_cols}[$i];
                ++$self->{not_a_number}[$i] if $row->[$i] && ! looks_like_number $row->[$i];
            }
            else {
                # column name
                $self->{width_head}[$i] = $width;
                $self->{longest_col_name} = $width if $width > $self->{longest_col_name};
                $normal_row = 1 if $i == $#$row;
            }
        }
    }
}


sub __calc_avail_width {
    my ( $self, $a_ref, $term_width ) = @_;
    my $width_head = [ @{$self->{width_head}} ];
    my $width_cols = [ @{$self->{width_cols}} ];
    my $avail_width = $term_width - $self->{tab_width} * $#$width_cols;
    my $sum = sum( @$width_cols );
    if ( $sum < $avail_width ) {
        # auto cut
        HEAD: while ( 1 ) {
            my $count = 0;
            for my $i ( 0 .. $#$width_head ) {
                if ( $width_head->[$i] > $width_cols->[$i] ) {
                    ++$width_cols->[$i];
                    ++$count;
                    last HEAD if ( $sum + $count ) == $avail_width;
                }
            }
            last HEAD if $count == 0;
            $sum += $count;
        }
        return $width_head, $width_cols;
    }
    elsif ( $sum > $avail_width ) {
        my $minimum_with = $self->{min_col_width} || 1;
        if ( @$width_head > $avail_width ) {
            say 'Terminal window is not wide enough to print this table.';
            choose(
                [ 'Press ENTER to show the column names.' ],
                { prompt => '', clear_screen => 0, mouse => $self->{mouse} }
            );
            my $prompt = 'Reduce the number of columns".' . "\n";
            $prompt .= 'Close with ENTER.';
            choose(
                $a_ref->[0],
                { prompt => $prompt, clear_screen => 0, mouse => $self->{mouse} }
            );
            return;
        }
        my @width_cols_tmp = @$width_cols;
        my $percent = 0;

        MIN: while ( $sum > $avail_width ) {
            ++$percent;
            my $count = 0;
            for my $i ( 0 .. $#width_cols_tmp ) {
                next if $minimum_with >= $width_cols_tmp[$i];
                if ( $minimum_with >= _minus_x_percent( $width_cols_tmp[$i], $percent ) ) {
                    $width_cols_tmp[$i] = $minimum_with;
                }
                else {
                    $width_cols_tmp[$i] = _minus_x_percent( $width_cols_tmp[$i], $percent );
                }
                ++$count;
            }
            $sum = sum( @width_cols_tmp );
            $minimum_with-- if $count == 0;
            #last MIN if $minimum_with == 0;
        }
        my $rest = $avail_width - $sum;
        if ( $rest ) {

            REST: while ( 1 ) {
                my $count = 0;
                for my $i ( 0 .. $#width_cols_tmp ) {
                    if ( $width_cols_tmp[$i] < $width_cols->[$i] ) {
                        $width_cols_tmp[$i]++;
                        $rest--;
                        $count++;
                        last REST if $rest == 0;
                    }
                }
                last REST if $count == 0;
            }
        }
        $width_cols = [ @width_cols_tmp ] if @width_cols_tmp;
    }
    return $width_cols;
}


sub __trunk_col_to_avail_width {
    my ( $self, $a_ref, $width_cols ) = @_;
    my $total = $#{$a_ref};                   #
    my $next_update = 0;                      #
    my $c = 0;                                #
    my $progress;                             #
    if ( $self->{show_progress} ) {           #
        local $| = 1;                         #
        print CLEAR_SCREEN;                   #
        $progress = Term::ProgressBar->new( { #
            name => 'Computing',              #
            count => $total,                  #
            remove => 1 } );                  #
        $progress->minor( 0 );                #
    }
    #unshift @$a_ref, [ map { '=' x $width_cols->[$_] } 0 .. $#$width_cols ] if ! $self->{header_row};
    my $list;
    my $tab = ' ' x $self->{tab_width};
    for my $row ( @$a_ref ) {
        my $str = '';
        for my $i ( 0 .. $#$width_cols ) {
            $str .= _unicode_sprintf( $width_cols->[$i], $row->[$i], $self->{not_a_number}[$i] ? 0 : 1 );
            $str .= $tab if $i != $#$width_cols;
        }
        push @$list, $str;
        if ( $self->{show_progress} ) {                                      #
            my $is_power = 0;                                                #
            for ( my $i = 0; 2 ** $i <= $c; ++$i ) {                         #
                $is_power = 1 if 2 ** $i == $c;                              #
            }                                                                #
            $next_update = $progress->update( $c ) if $c >= $next_update;    #
            ++$c;                                                            #
        }                                                                    #
    }
    $progress->update( $total ) if $self->{show_progress} && $total >= $next_update; #
    my $len = sum( @$width_cols, $self->{tab_width} * $#{$width_cols} );
    return $list, $len;
}


sub __handle_reference {
    my ( $self, $ref ) = @_;
    if ( ref $ref eq 'ARRAY' ) {
        return 'ref: [' . join( ',', map { '"' . $_ . '"' } @$ref ) . ']';
    }
    elsif ( ref $ref eq 'SCALAR' ) {
        return 'ref: \\' . $$ref;
    }
    elsif ( ref $ref eq 'HASH' ) {
        return 'ref: {' . join( ',', map { $_ . '=>"' . $ref->{$_} . '"' } keys %$ref ) . '}';
    }
    elsif ( ref $ref eq 'Regexp' ) {
        return 'ref: qr/' . $ref . '/';
    }
    elsif ( ref $ref eq 'VSTRING' ) {
        return 'ref: \v' . join '.', unpack 'C*', $$ref;
    }
    elsif ( ref $ref eq 'GLOB' ) {
        return 'ref: \\' . $$ref;
    }
    else {
        return 'ref: ' . ref( $ref );
    }
}



sub _minus_x_percent {
    my ( $value, $percent ) = @_;
    my $new = int( $value - ( $value / 100 * $percent ) );
    return $new > 0 ? $new : 1;
}


sub _term_size {
    my ( $width, $heigth ) = chars();
    return $width - 1, $heigth if $^O eq 'MSWin32';
    return $width, $heigth;
}


sub _insert_sep {
    my ( $number, $separator ) = @_;
    return if ! defined $number;
    return $number if ! $separator;
    $number =~ s/(\d)(?=(?:\d{3})+\b)/$1$separator/g;
    return $number;
}


sub _unicode_sprintf {
    my ( $avail_width, $unicode, $right_justify ) = @_;
    my $gcs = Unicode::GCString->new( $unicode );
    my $colwidth = $gcs->columns;
    if ( $colwidth > $avail_width ) {
        my $pos = $gcs->pos;
        $gcs->pos( 0 );
        my $cols = 0;
        my $gc;
        while ( defined( $gc = $gcs->next ) ) {
            if ( $avail_width < ( $cols += $gc->columns ) ) {
                my $ret = $gcs->substr( 0, $gcs->pos - 1 );
                $gcs->pos( $pos );
                return $ret->as_string;
            }
        }
    }
    elsif ( $colwidth < $avail_width ) {
        if ( $right_justify ) {
            $unicode = " " x ( $avail_width - $colwidth ) . $unicode;
        }
        else {
            $unicode = $unicode . " " x ( $avail_width - $colwidth );
        }
    }
    return $unicode;
}


1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Term::TablePrint - Print a table on the terminal.

=head1 VERSION

Version 0.002_01

=cut

=head1 SYNOPSIS

    my $table = [ [ 'id', 'name' ],
                  [    1, 'Ruth' ],
                  [    2, 'John' ],
                  [    3, 'Mark' ],
                  [    4, 'Nena' ], ];

    use Term::TablePrint qw( print_table );

    print_table( $table );

    # or OO style:

    use Term::TablePrint;

    my $pt = Term::TablePrint->new();
    $pt->print_table( $table );

=head1 DESCRIPTION

Print a table to the terminal.

C<print_table> provides a cursor. The row on which the cursor is located is highlighted.

The user can scroll through the table with the cursor up/down keys - see L<USAGE>.

If the table has more rows than the terminal, the table is divided up on several sides automatically.

If the cursor reaches the end of a side, the next page is shown automatically (until the last site is reached).

Also if the cursor reaches the topmost line, the previous side is shown automatically if it is not already the first
site.

If the terminal is too narrow to print the table, the columns are adjusted to the available breadth automatically.

If the option "table_expand" is enabled and the highlighted row is selected, each column of that row is output in its
one line preceded by the column name. This might be useful if the columns were cut due to the too low terminal breadth.

To get a proper output, C<print_table> uses the C<columns> method from L<Unicode::GCString> to calculate the string
length.

=head1 METHODES

=head2 new

Returns an object. As an argument it can be passed  a reference to a hash which holds the options as pairs of
"option-name" and "option-value".

    my $tp = Term::TablePrint->new( [ \%options ] );

=head2 print_table

Prints the table passed with the first argument.

    $tp->print_table( $array_ref, [ \%options ] );

The first argument must be a reference to an array of arrays. The first array of the arrays holds the column names. The
following arrays are the table rows where the elements of these arrays are the field values.

As a second and optional argument can be passed a reference to a hash which holds the options as pairs of
"option-name" and "option-value".

=head1 SUBROUTINES

=head2 print_table

Prints the table passed with the first argument.

    print_table( $array_ref, [ \%options ] );

The first argument must be a reference to an array of arrays. The first array of the arrays holds the column names. The
following arrays are the table rows where the elements of these arrays are the field values.

As a second and optional argument can be passed a reference to a hash which holds the options as pairs of
"option-name" and "option-value".

=head1 USAGE

=head2 Keys to move around

=over

=item *

the C<ArrowDown> key (or the C<j> key) to move down and  C<ArrowUp> key (or the C<k> key) to move up.

=item *

the C<PageUp> key (or C<Ctrl-B>) to go back one page, the C<PageDown> key (or C<Ctrl-F>) to go forward one page.

=item *

the C<Home> key (or C<Ctrl-A>) to jump to the first row of the table, the C<End> key (or C<Ctrl-E>) to jump to the last
row of the table.

=item *

the C<Enter/Return> key to close the table or to print the highlighted row if "table_expand" is enabled.

=back

With the option "table_expand" disabled:

- Pressing ENTER jumps to the head of the table.

- Selecting the head of the table closes the table.

With the option "table_expand" enabled:

- If one selects a row twice in succession, the pointer jumps to the head of the table.

- Selecting the head of the table closes the table.

- If the width of the window is changed the user can rewrite the screen by choosing a row.

=head2 OPTIONS

Defaults may change in a future release.

=head3 tab_width

Set the number of spaces between columns.

Default: 2

=head3 colwidth

Set the width the columns should have at least when printed.

Default: 30

=head3 undef

Set the string that will be shown on the screen instead of an undefined field.

Default: '' (empty string)

=head3 max_rows

Set the maximum number of printed table rows.

To disable the automatic limit set I<max rows> to C<undef> (--).

Default: 50_000

=head3 progress_bar

Set the progress bar threshold. If the number of fields (rows x columns) is higher than the threshold a progress bar is
shown while preparing the data for the output.

Default: 20_000

=head3 table_expand

"table_expand" set to 1 enables printing the chosen table row by pressing the Enter key.

Default: 1

=head3 mouse

Set the "mouse" mode (see L<Term::Choose/OPTIONS/mouse>).

Default: 0

=head3 binary_filter

Print "BNRY" instead of arbitrary binary data.

If the data matches the repexp C</[\x00-\x08\x0B-\x0C\x0E-\x1F]/> it is considered arbitrary binary data.

Printing arbitrary binary data could break the output.

Default: 0

=head1 REQUIREMENTS

See L<Term::Choose/REQUIREMENTS>

=head2 Perl version

Requires Perl version 5.10.1 or greater.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Term::TablePrint

=head1 AUTHOR

Matthäus Kiem <cuer2s@gmail.com>

=head1 CREDITS

Thanks to the L<Perl-Community.de|http://www.perl-community.de> and the people form
L<stackoverflow|http://stackoverflow.com> for the help.

=head1 LICENSE AND COPYRIGHT

Copyright 2012-2014 Matthäus Kiem.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl 5.10.0. For
details, see the full text of the licenses in the file LICENSE.

=cut
