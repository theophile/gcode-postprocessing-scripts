#!/usr/bin/perl -i
use strict;
use warnings;
use List::Util 'any';

my $input_file = shift @ARGV;

if ( !defined $input_file ) {
    die "Usage: $0 input_file\n";
}

open my $in, '<', $input_file or die "Cannot open input file $input_file: $!";

# Skip to end of the file
seek( $in, -30216, 2 );

# Check if more than one extruder uses filament. If not, exit the script.
while ( my $line = <$in> ) {
    if ( $line =~ /^; filament used \[mm\] = (.*)$/ ) {
        my @values         = split( /, /, $1 );
        my $non_zero_count = grep { $_ > 0 } @values;
        if ( $non_zero_count > 1 ) {
            print "Toolchanges detected. Running script...\n";
        }
        else {
            print "No toolchanges detected. Exiting.\n";
            exit;
        }
        last;
    }
}

# Go back to the beginning of the file
seek( $in, 0, 0 );

# Read all lines into the @lines array
my @lines = <$in>;
close $in;

# Empty hash to store names of filaments used
my %filament_list;

my %entity_roles = (

    # Catch gap-fill moves so they stay with whatever feature type they follow
    'Gap fill'   => 'follow',
    'Gap infill' => 'follow',
    'Ironing'    => 'follow',
    'Thin wall'  => 'follow',

    # Solid infill, perimeters, and any other "TYPE" marked not OK to purge into
    'Bottom surface'        => 'nopurge',
    'Bridge'                => 'nopurge',
    'Bridge infill'         => 'nopurge',
    'Custom'                => 'nopurge',
    'External perimeter'    => 'nopurge',
    'Inner wall'            => 'nopurge',
    'Multiple'              => 'nopurge',
    'Outer wall'            => 'nopurge',
    'Overhang perimeter'    => 'nopurge',
    'Overhang wall'         => 'nopurge',
    'Perimeter'             => 'nopurge',
    'Solid infill'          => 'nopurge',
    'Internal solid infill' => 'nopurge',
    'Top solid infill'      => 'nopurge',
    'Top surface'           => 'nopurge',
    'Undefined'             => 'nopurge',
    'Unknown'               => 'nopurge',

    # Supports, internal infill, and wipe tower marked as OK to purge into
    'Brim'                       => 'purge',
    'Internal bridge infill'     => 'purge',
    'Internal bridge'            => 'purge',
    'Internal infill'            => 'purge',
    'Prime tower'                => 'purge',
    'Skirt'                      => 'purge',
    'Skirt/Brim'                 => 'purge',
    'Sparse infill'              => 'purge',
    'Support'                    => 'purge',
    'Support interface'          => 'purge',
    'Support material'           => 'purge',
    'Support material interface' => 'purge',
    'Support transition'         => 'purge',
    'Wipe tower'                 => 'purge'
);

our ( $current_block, $unretract_line ) = ( 0, 0 );
our ( @lines_buffer,  @lines_array );

# Subroutine to determine whether the input line is a retract move
sub is_retract_move {

    # If the line is a G1 command with only a negative E value and an
    # F value (and no X, Y, X), assume this is a retract move
    return $_[0] =~ /^G1 E-[0-9]*(?:\.[0-9]+)? F[0-9]*(?:\.[0-9]+)?$/;
}

# Subroutine to determine whether the input line is an unretract move
sub is_unretract_move {

    # If the line is a G1 command with only a positive E value and an
    # F value (and no X, Y, X), assume this is an unretract move
    return $_[0] =~ /^G1 E[0-9]*(?:\.[0-9]+)? F[0-9]*(?:\.[0-9]+)?$/;
}

# Subroutine to determine whether the input line is a travel move
sub is_travel_move {

    # If the line is a G1 command with an X, Y, or Z coordinate but
    # no E value, assume this is a travel move
    return $_[0] =~ /^G1\s(?!.*E)\s*[XYZ]\S+\b/;
}

# Subroutine to determine whether the input line is an extrusion move
sub is_extrusion_move {

    # If the line is a G1 command with an X and/or Y coordinate and
    # also an E value, assume this is a non-retraction extrusion move
    return $_[0] =~ /^G1\s(?:.*[XY]\S+.*E|.*E.*[XY]\S+)/;
}

# Subroutine to determine whether new feature block is starting
sub flag_type {
    my $type = $_[0];
    return 0 if ( $type eq 'follow' );

    my $mark_new_block = 0;

    if ( $current_block ne $type ) {
        $mark_new_block = 1;
        $current_block  = $type;
    }

    return $mark_new_block;
}

# Subroutine to parse gcode lines and detect change of feature types
sub handle_type_line {
    my $i           = $_[0];
    my $line        = $lines[$i];
    my ($line_type) = $line =~ /;TYPE:(.+)\n/;

    if ( $line =~ /^FILAMENT_CHANGE/ ) {

        # If we hit a FILAMENT_CHANGE macro, find and store the filament 
        # name...
        if ($line =~ /FILAMENT_ID="([^"]+)"/) {
            my $filament_name = $1;
            $filament_list{$filament_name} = 1;
        }

        # ...and then return this line number as a trigger to the pre-
        # toolchange analysis
        return ( 0, $i );
    }

    if ( defined $line_type && exists $entity_roles{$line_type} ) {
        return ( flag_type( $entity_roles{$line_type} ), 0 );
    }

    return ( 0, 0 );
}

# Subroutine to close out current line buffer and start a new one
sub start_new_line_buffer {

    my @temp_array = ();

    # If the last line of the current line buffer is an unretract move,
    # assume the line before that is a travel move to wherever the next
    # block starts, so we cut off the last two lines from the current
    # line buffer and use them to start the next one, so that wherever
    # the next block ends up in the output, it will start with the proper
    # travel move. Then append "RETRACTED" tag to the end of the current
    # line buffer so when stitching the blocks together later, we can make
    # sure to unretract.
    if ( is_unretract_move( $lines_buffer[-1] ) ) {
        @temp_array = splice( @lines_buffer, -2 );
        push( @lines_buffer, ";RETRACTED\n" );
    }

    # If the last line of the current line buffer is NOT an unretract
    # move, then it must be the travel move to the next feature block,
    # so cut only the last line from the current line buffer and use it
    # to start the next line buffer, and append the "RETRACTED" tag to
    # the end of the current line buffer so we know how to handle later.
    else {
        @temp_array = splice( @lines_buffer, -1 );
        push( @lines_buffer, ";NOT RETRACTED\n" );
    }

    # Add the current line buffer to the @lines_array
    push( @lines_array, [@lines_buffer] );

    # Replace the current lines buffer with the contents of the temp array
    # we just started
    @lines_buffer = splice(@temp_array);

    # Create a gcode comment at the beginning of the new lines buffer
    # to identify it (e.g. ";purge\n")
    unshift( @lines_buffer, ";${current_block}\n" );
}

# Subroutine to return the total amount of filament extruded between
# the two input lines
sub total_extruded {
    my ( $start, $end ) = @_;
    my $sum = 0;
    for my $idx ( $start .. $end ) {
        $sum += $1 if ( $lines[$idx] =~ /^G1.*?E(-?[0-9]*(?:\.\d+)?)/ );
    }
    return $sum;
}

# Subroutine to append a "MODEL_PURGE" parameter to the input line (first arg)
# with the parameter's value being the total amount of filament purged (second arg)
sub append_purge_vol {
    my ( $idx, $purge_total ) = @_;

    # Limit to three decimal places (round down)
    $purge_total =~ s/\.(\d{3})\d+$/.$1/;
    my $new_element = $lines[$idx];

    if ( $new_element !~ /MODEL_PURGE=/ ) {
        $new_element =~ s/$/ MODEL_PURGE=$purge_total/;
    }

    # If the given line already has a "MODEL_PURGE" parameter, simply add the
    # input amount to the already existing value
    else {
        $new_element =~ s/MODEL_PURGE=(\d+)/MODEL_PURGE=($1 + $purge_total)/;
    }
    $lines[$idx] = $new_element;
}

# Subroutine to find the next line after the input line (first arg) on which the
# FILAMENT_CHANGE or PRINT_END macro occurs, or which is marked as the beginning
# of a layer change
sub get_post_range_end {
    my $range_end_line = 0;
    my @range_end_patterns = (
        '^;TOOLCHANGE',
        '^;LAYER_CHANGE',
        '^PRINT_END',
        '^; stop printing object',
        '^; Filament-specific end gcode'
    );
    
    for ( my $idx = ( $_[0] + 1 ) ; $idx <= $#lines ; $idx++ ) {
        if ( any { $lines[$idx] =~ /$_/ } @range_end_patterns ) {
            $range_end_line = $idx;
            last;
        }
    }

    return $range_end_line - 1;
}

# Subroutine to find where in the @lines array to insert the reordered blocks
sub get_insertion_point {
    my ( $start_index, $current_operation ) = @_;
    my $insertion_point = $start_index;

    # If we're parsing the pre-toolchange gcode, start at the input line (first
    # arg) and look backwards for the first G1 command with an X, Y, or Z
    # coordinate but no E value (meaning it is a travel-only move) and set
    # $insertion_point to that line number. Then break the loop.
    if ( $current_operation eq 'pre' ) {
        for ( my $idx = $start_index ; $idx >= 0 ; $idx-- ) {
            if ( is_travel_move( $lines[$idx] ) ) {
                $insertion_point = $idx;
                last;
            }
        }
    }

    # If we're parsing the post-toolchange gcode, use the input line as
    # $insertion_point unless it's the line with the FILAMENT_CHANGE
    # command, in which case set $insertion_point to the next line.
    elsif ( $current_operation eq 'post' ) {
        $insertion_point += 1
          if ( $lines[$start_index] =~ m/^FILAMENT_CHANGE/ );
    }

    return $insertion_point;
}

# Subroutine to make sure retractions and unretractions are in the right
# places after the FILAMENT_CHANGE macro
sub clean_up_retractions {
    my $filament_change_line = $_[0];
    my ( $extrusion_move, $retract_move, $unretract_move, $travel_move ) =
      ( 0, 0, 0, 0 );

    for my $idx ( $filament_change_line .. $#lines ) {
        $retract_move   = $idx if is_retract_move( $lines[$idx] );
        $unretract_move = $idx if is_unretract_move( $lines[$idx] );
        $travel_move    = $idx if is_travel_move( $lines[$idx] );
        if ( is_extrusion_move( $lines[$idx] ) ) {
            $extrusion_move = $idx;
            last;
        }
    }

    # If there's a retraction move in the post-toolchange gcode before the
    # first extrusion move, remove it because we'll already be retracted
    # upon returning from the FILAMENT_CHANGE
    if ($retract_move) {
        splice( @lines, $retract_move, 1 );
        $unretract_move-- if $unretract_move;
        $travel_move--    if $travel_move;
    }

    # If there's a travel move without an unretraction, add an unretraction
    # to make sure the nozzle is reprimed after returning from the
    # FILAMENT_CHANGE
    splice( @lines, $travel_move + 1, 0, $unretract_line )
      if $travel_move && !$unretract_move;
}

# Subroutine to parse and rearrange the pre-toolchange gcode
sub pre_change_rearrange {
    my ( $start_index, $end_index, $purge_total ) = @_;

    # Calculate the total filament extruded during the "purge" features
    # preceding the FILAMENT_CHANGE
    $purge_total += total_extruded( $start_index, $end_index );

    # Append the MODEL_PURGE parameter to the current filament change
    # line with the calculated purge volume
    append_purge_vol( $end_index, $purge_total );

    my ( $first_line_to_move, $last_line_to_move );

    # Start at the current FILAMENT_CHANGE macro line and begin parsing
    # each line working backwards
    for ( my $idx = $end_index ; $idx >= 0 ; $idx-- ) {

        # If the line begins with "; Filament-specific," this is the
        # beginning of the code block that immediately precedes the
        # toolchange, so mark it as the beginning of the block we're
        # going to relocate, then break the loop
        if ( $lines[$idx] =~ /^; Filament-specific/ ) {
            $first_line_to_move = $idx;
            last;
        }
    }

    # Start at the current FILAMENT_CHANGE macro line and begin parsing
    # each line working forward
    for my $idx ( $end_index .. scalar(@lines) ) {

        # If the line is a retraction move after the FILAMENT_CHANGE,
        # assume it is the retraction line after the toolchange
        # immediately preceding the travel move to the first feature
        # printed after the toolchange. Mark this line as the last one
        # in the block that we're going to move.
        if ( is_retract_move( $lines[$idx] ) ) {
            $last_line_to_move = $idx;
            last;
        }
    }

    # Error handling. Kill the script with an error if we couldn't find
    # appropriate start and end points for moving the toolchange block.
    die "Couldn't find filament-specific gcode tag. Exiting..."
      unless defined $first_line_to_move;
    die "Couldn't find post-toolchange retraction line. Exiting..."
      unless defined $last_line_to_move;

    # Use get_insertion_point subroutine to find where to insert the
    # toolchange block. This should be the last travel-only move before
    # the toolchange, so that after we return following the
    # FILAMENT_CHANGE macro, we'll travel to the correct point to resume
    # the print.
    my $insertion_point = get_insertion_point( $start_index, 'pre' );

    # Cut the toolchange block out of the @lines array then reinsert it
    # right before the $insertion_point
    my $range            = $last_line_to_move - $first_line_to_move;
    my @elements_to_move = splice( @lines, $first_line_to_move, $range );
    splice( @lines, $insertion_point, 0, @elements_to_move );

    my $filament_change_line;
    for my $idx ( $insertion_point .. scalar(@lines) ) {
        if ( $lines[$idx] =~ /^FILAMENT_CHANGE/ ) {
            $filament_change_line = $idx;
            last;
        }
    }
    clean_up_retractions($filament_change_line);
}

# Subroutine to parse and rearrange the post-toolchange gcode
sub post_change_rearrange {
    $current_block = "POST TOOLCHANGE";

    my ( $start_index, $end_index ) = @_;

    @lines_buffer = ();
    @lines_array  = ();
    my $start_new_array = 0;

    # Mark the first @lines_buffer as a post-toolchange block
    unshift( @lines_buffer, ";${current_block}\n" );

    # Loop through every line in the @lines array between $start_index
    # (first arg) and $end_index (second arg)
    for my $index ( $start_index .. $end_index ) {
        my $line = $lines[$index];

        # Run each line through the handle_type_line subroutine to mark
        # the start of a new feature-type block
        ( $start_new_array, undef ) = handle_type_line($index);

        # If the handle_type_line subroutine detected a new block type,
        # run the start_new_line_buffer subroutine
        start_new_line_buffer() if ($start_new_array);

        # Add the current line to the lines buffer
        push( @lines_buffer, $line );
    }

    # After finishing the loop, mark the last block before the filament
    # change as not retracted because there's a manual retraction in the
    # FILAMENT_CHANGE macro before the move to the purge bucket
    push( @lines_buffer, ";NOT RETRACTED\n" );
    push( @lines_array,  [@lines_buffer] );

    # Now that all the pre-toolchange lines have been processed, we should
    # have a @lines_array array in which all the elements are arrays that
    # were constructed as a lines buffer before being capped off by the
    # start_new_line_buffer subroutine.

    my ( @intro_lines, @purge_lines, @nonpurge_lines );

    # Now loop over each array in @lines_array
    for ( my $i = 0 ; $i < scalar @lines_array ; $i++ ) {
        my @this_array = @{ $lines_array[$i] };

        # The first line of each sub-array should be a gcode comment
        # identifying the block type
        my $id_line = $this_array[0];

        # Depending on whether this array is the initial post-toolchange
        # gcode, a purge block, or a non-purge block, move the lines in this
        # array to the appropriate new array
        my %line_groups = (
            '^;POST TOOLCHANGE' => \@intro_lines,
            '^;purge'           => \@purge_lines,
            '^;nopurge'         => \@nonpurge_lines
        );

        for my $pattern ( keys %line_groups ) {
            if ( $id_line =~ /$pattern/ ) {
                push @{ $line_groups{$pattern} }, [@this_array];
                last;   # Only one pattern should match, so we can exit the loop
            }
        }
    }

    # If after all that there aren't any features we can purge into, bail out
    # now without rearranging anything
    return 0 if ( !@purge_lines );

    # Clear the lines array so we can put the sub-arrays back into it in the
    # new sequence
    @lines_array = ();

    # Start by moving each @intro_lines array to the new @lines_array
    for my $each_array (@intro_lines) {
        push( @lines_array, [@$each_array] );
    }

    my $purge_total = 0;

    # Next, for all arrays in @purge_lines, keep a running total of all
    # extrusion moves and then move each @purge_lines array to the new
    # @lines_array
    for my $each_array (@purge_lines) {
        for my $purge_line (@$each_array) {
            $purge_total += $1 if $purge_line =~ /^G1.*?E(-?[0-9]*(?:\.\d+)?)/;
        }
        push( @lines_array, [@$each_array] );
    }

    # Finally, move each @nonpurge_lines array to the new @lines_array
    for my $each_array (@nonpurge_lines) {
        push( @lines_array, [@$each_array] );
    }

    # Now @lines_array has all the lines in the correct order but it's still
    # an array of arrays. We did it this way so we can make sure we stitch
    # the blocks together properly before losing track of where the block
    # boundaries were.

    my @reordered_lines = ();
    for ( my $i = 0 ; $i <= $#lines_array ; $i++ ) {
        my @this_array = @{ $lines_array[$i] };

        # For each sub-array in @lines_array except the last one, check to see
        # if this sub-array ends in the "RETRACTED" tag. If so, check to see if
        # the next sub-array begins with an unretract line. If it doesn't, add
        # the unretract line to the head of the next sub-array.
        unless ( $i == $#lines_array ) {
            my @next_array = @{ $lines_array[ $i + 1 ] };

            if ( ( $this_array[-1] =~ /^;RETRACTED/ )
                && !( is_unretract_move( $next_array[2] ) ) )
            {
                splice( @next_array, 2, 0, $unretract_line );

                # @next_array is just a copy so write the new contents to the
                # actual next @lines_array element
                $lines_array[ $i + 1 ] = [@next_array];
            }

            # If this sub-array is not retracted at the end but the next array
            # begins with an unretract move, invert that unretract move to turn
            # it into the corresponding retraction move and add it to the end of
            # this sub-array
            if ( $this_array[-1] eq ";NOT RETRACTED\n"
                && ( is_unretract_move( $next_array[2] ) ) )
            {
                my $retract_line =
                  ( $next_array[2] =~ s/E([0-9]*(?:\.[0-9]+)?)/E-$1/r );
                splice( @this_array, -1, 0, $retract_line );
            }
        }

        # After processing each sub-array in @lines_array, move all the lines to
        # the new @reordered_lines array
        for my $element (@this_array) {
            push( @reordered_lines, $element );
        }
    }

    # Now we should have a @reordered_lines array containing all of the lines
    # between $start_index and $end_index that have been rearranged and stitched
    # together properly

    # Call get_insertion_point subroutine to determine where in @lines to insert
    # the contents of @reordered_lines
    my $insertion_point = get_insertion_point( $start_index, 'post' );

    # Replace all of the lines in @lines between $insertion_point and $end_index
    # with the contents of @reordered_lines
    my $range = ( $end_index - $insertion_point + 1 );
    splice( @lines, $insertion_point, $range, @reordered_lines );

    return $purge_total;
}

###
# MAIN CODE EXECUTION STARTS HERE
###

my (
    $pre_purge_line, $pre_nonpurge_line,
    $mark_new_block, $filament_change_line
) = ( 0, 0, 0, 0 );

# Loop over every line in @lines in order
for ( my $i = 0 ; $i <= $#lines ; $i++ ) {
    my $line = $lines[$i];

    # If we hit the PRINT_END macro, signal that this will be the last time
    # through the loop
    last if $line =~ /^PRINT_END/;

    # If the line is an unretract move, save it for later use
    $unretract_line = $line if ( is_unretract_move($line) );

    # Run each line through the handle_type_line subroutine to mark
    # the start of a new feature-type block and flag when to break
    # out of this loop.
    my ( $mark_new_block, $filament_change_line ) = handle_type_line($i);

    # If this line starts a new block, and if the block type is a purge
    # block, update $pre_purge_line with the current line number. Otherwise
    # update $pre_nonpurge_line with the current line number.
    if ($mark_new_block) {
        ( $current_block eq "purge" )
          ? $pre_purge_line = $i
          : $pre_nonpurge_line = $i;
    }

    # If we've reached a FILAMENT_CHANGE macro
    if ($filament_change_line) {

        my $purge_total = 0;

        # Find where the next FILAMENT_CHANGE, layer change, or PRINT_END
        # occurs so we know the range to analyze for post-toolchange
        # resequencing
        my $post_range_end = get_post_range_end($i);

        # Run the post_change_rearrange subroutine to resequence the gcode after
        # the FILAMENT_CHANGE here, and get the total filament to be purged
        $purge_total += post_change_rearrange( ( $filament_change_line + 1 ),
            ($post_range_end) );

        # If $pre_nonpurge_line is lower than $pre_purge_line, that means the
        # last features to print before the FILAMENT_CHANGE can be safely purged
        # into without having to rearrange the print sequence, so we can simply
        # move the FILAMENT_CHANGE macro to before these final "purgeable"
        # features
        if ( $pre_nonpurge_line < $pre_purge_line ) {

            pre_change_rearrange( ($pre_purge_line),
                ($filament_change_line), ($purge_total) );
        }

        # If we didn't move the FILAMENT_CHANGE macro, add the model purge value
        # to the line and then clean up the initial lines after the macro
        else {
            append_purge_vol( $filament_change_line, $purge_total )
              if $purge_total;
            clean_up_retractions($filament_change_line);
        }

        # Clear the variables before restarting the loop
        (
            $pre_purge_line, $pre_nonpurge_line,
            $mark_new_block, $filament_change_line
        ) = ( 0, 0, 0, 0 );
    }
}

# Create a comma-separated list of the names of filaments used in this print
my $key_list = '';
foreach my $key (keys %filament_list) {
    $key_list .= ',' if $key_list ne '';
    $key_list .= $key;
}

open my $out, '>', $input_file
  or die "Cannot open output file $input_file: $!";

for my $line (@lines) {

    # Append the filament list to the PRINT_START command
    $line =~ s/\n$/ FILAMENT_LIST="$key_list"\n/ if $line =~ /^PRINT_START/;

    # Write the rearranged lines back to the input file, making sure we've
    # stripped out all the ID tags we added along the way
    print $out $line
      unless ( $line =~
        m/(^;NOT RETRACTED|^;RETRACTED|^;POST TOOLCHANGE|^;purge|^;nopurge)/);
}

close $out;
