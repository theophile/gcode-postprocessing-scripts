#!/usr/bin/perl -i
use strict;
use warnings;

# Empty hash to store names and extruder numbers of filaments used
my %filament_list;

# Subroutine to extract filament name and extruder number from a line
# of gcode and store it in the %filament_list hash
sub extract_filament_data {
    my $line = $_[0];
    my ($filament_name, $filament_position) = (undef, undef);
    
    $filament_name = $1 if $line =~ /FILAMENT_ID="([^"]+)"/;
    $filament_position = $1 if $line =~ /POSITION=([^\s]+)/;

    if (defined $filament_name && defined $filament_position) {
        $filament_list{$filament_name} = $filament_position;
    }
}

my $z = 0;

while ( my $line = <> ) {

    # If line contains FILAMENT_COLOR_HEX, excise the hash character
    $line =~ tr/\#//d if ( $line =~ m/\bFILAMENT_COLOR_HEX\b/ );

    # If line contains a Z parameter, save the value
    $z = $1 if $line =~ m/^G1.*Z\s*(\d*(\.\d+)?)/;

    # If the line starts with the FILAMENT_CHANGE macro, look ahead
    # until we find a travel move, then store the X and Y parameters
    # so we can tell the FILAMENT_CHANGE macro where we'll be going
    
    if ( $line =~ m/^FILAMENT_CHANGE/ ) {

        # If we hit a FILAMENT_CHANGE macro, find and store the filament 
        # name...
        extract_filament_data( $line );

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
    elsif ( $line =~ m/^PRINT_START/ ) {
        extract_filament_data( $line );
        print $line;
    }

    # Reformat the filament_settings_id line to make it a comma-separated
    # list of only the filaments used in the print, sorted by extruder no.
    elsif ( $line =~ /^; filament_settings_id/ ) {
        my %reversed_filament_list = reverse %filament_list;
        my @names_array;
        for my $i (sort { $a <=> $b } keys %reversed_filament_list) {
            push( @names_array, $reversed_filament_list{$i} );
        }
        print '; filament_settings_id = ' . join(", ", @names_array) . "\n";
    }

    # Reformat the filament_type line to make it a comma-separated list of
    # types of only the filaments used in the print, sorted by extruder no.
    elsif ( $line =~ /^; filament_type = (.*)$/ ) {
        my @values         = split( /;/, $1 );
        my @newvalues;
        for my $i (sort { $a <=> $b } values %filament_list) {
            push( @newvalues, $values[$i] );
        }
        print '; filament_type = '. join( ", ", @newvalues ) . "\n";
    }
    else {
        print $line or die $!;
    }
   
}
