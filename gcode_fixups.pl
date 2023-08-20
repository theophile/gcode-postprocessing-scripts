#!/usr/bin/perl -i
use strict;
use warnings;

my $z = 0;

while ( my $line = <> ) {

    # If line contains FILAMENT_COLOR_HEX, excise the hash character
    $line =~ tr/\#//d if ( $line =~ m/\bFILAMENT_COLOR_HEX\b/ );

    # If line contains a Z parameter, save the value
    $z = $1 if $line =~ m/Z\s*(\d*(\.\d+)?)/;

    # If the line starts with the FILAMENT_CHANGE macro, look ahead
    # until we find a travel move, then store the X and Y parameters
    # so we can tell the FILAMENT_CHANGE macro where we'll be going
    if ( $line =~ m/^FILAMENT_CHANGE/ ) {
        my @inner_lines = ();
        my ( $x, $y, $f ) = ( '', '', '' );
        while ( my $inner_line = <> ) {
            if ( $inner_line =~ /G1 X([^\s]+) Y([^\s]+) F([^\s]+)/ ) {
                ( $x, $y, $f ) = ( $1, $2, $3 );

                # Add the Z parameter to the travel line as a redundancy
                push( @inner_lines, "G1 X$x Y$y Z$z F$f\n" );
                last;
            }

            # Remove retraction moves because we'll already be retracted
            # after the FILAMENT_CHANGE macro
            push( @inner_lines, $inner_line )
              unless ( $inner_line =~ /^G1 E-\d+(\.\d+)?.*/ );
        }

        # Append the X, Y, and Z coordinates of the first post-toolchange
        # travel move to the FILAMENT_CHANGE macro line
        $line =~ s/$/ NEXT_X=$x NEXT_Y=$y NEXT_Z=$z/
          if ( defined $x && defined $y && defined $z );
        print $line, join( "", @inner_lines );
    }
    else {
        print $line or die $!;
    }
}
