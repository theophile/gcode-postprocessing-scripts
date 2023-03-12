# Gcode Postprocessing Scripts
This is a collection of my personal gcode postprocessing scripts I use with PrusaSlicer/SuperSlicer. Most of them are specific to my printer setup and various custom Klipper macros I use so they probably can't be used with a different setup without some modification and tweaking. To that end, I have heavily commented the scripts to hopefully make it easier to follow what the scripts are doing and facilitate modifications.

## purge_to_infill.pl
My printer is a heavily modified Ender 3 that has a 6-in-1-out hotend system, with a side-mounted purge bucket for filament changes. Multimaterial prints are great but I've always hated how much filament is wasted purging out the old filament during filament changes. PrusaSlicer/SuperSlicer have had a "purge to infill" feature for a while now, but it has never worked for me. I created this script to get the behavior I've been wanting.

Filament changes are handled entirely in the Klipper firmware by a gcode macro `FILAMENT_CHANGE` that takes various parameters. This script reads down the gcode file in search of that `FILAMENT_CHANGE` macro. Whenever it finds one, it performs a two-stage analysis. 

In the first stage, the script analyzes the gcode after the `FILAMENT_CHANGE` macro looking for blocks of gcode representing print features such as perimeters, supports, infill, etc. Whenever it finds features that will not be visible on the finished print (generally supports and internal infill), it resequences the gcode so that those features are grouped together and printed immediately after the `FILAMENT_CHANGE`. It then adds up the total amount of filament that will be extruded during the printing of those invisible features.

In the second stage, the script analyzes the gcode before the `FILAMENT_CHANGE` macro to determine whether the last feature or features to be printed before the `FILAMENT_CHANGE` are "invisible" features. If so, the script relocates the `FILAMENT_CHANGE` macro (and relevant toolchange gcode on either side of it) to right before those "invisible" features. It then adds up the total amount of filament that will be extruded during the printing of *these* invisible features, and adds that to the total derived during stage 1.

The total filament to be purged into these "invisible" features is then added to the `FILAMENT_CHANGE` command as the value of the `MODEL_PURGE` parameter. This tells Klipper how much filament will be purged into "invisible" features when the print resumes, and the firmware then deducts that amount from the volume of filament that otherwise would be purged into the purge bucket and wasted. 

## gcode_fixups.pl
This is a comparatively simple script that performs three main functions:
1. My `PRINT_START` and `FILAMENT_CHANGE` macros take a parameter `FILAMENT_COLOR_HEX`, the value of which is the hexidecimal color code of the filament that is about to be loaded. Unfortunately, the slicer includes the customary hash symbol (#) before the 6-digit color code, which breaks the macro because Klipper interprets everything after that symbol as a comment. So when this script finds a line containing the `FILAMENT_COLOR_HEX` parameter, it strips the hash symbol out of that line.
2. To reduce zits and surface artifacts, my `FILAMENT_CHANGE` macro lifts the print head before traveling to the purge bucket and leaves it raised until after traveling back from the bucket. To make sure that oozed filament is not inadvertently wiped on a surface feature, it is best to send the print head to wherever the next print move will begin after the filament change before lowering the print head back to the print. So this script looks ahead and finds the first travel move after the `FILAMENT_CHANGE`, then it passes the X, Y, and Z coordinates of that location to the `FILAMENT_CHANGE` macro as parameters named `NEXT_X`, `NEXT_Y`, and `NEXT_Z`, so the macro can send the printhead to the ideal location after the filament change has completed. As an additional redundancy to protect against layer shifts and errors, the script also adds the Z coordinate to the travel move in the gcode.
3. To reduce ooze, my `FILAMENT_CHANGE` macro includes a retraction move right before the printhead travels back to the print. If the gcode already contains a retraction move, this will result in a double retracted state that the single unretraction after the travel move won't be able to overcome. So this script removes any retraction lines it finds between a `FILAMENT_CHANGE` command and the first travel move afterwards.

In my slicer, I have it configured to run `purge_to_infill.pl` first, then `gcode_fixups.pl`.