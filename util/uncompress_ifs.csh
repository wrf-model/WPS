#!/bin/csh
#
# A simple uncompress script for NCAR's derecho to convert
# files that use ccsds compression to a standard grib2 which
# can be processed by ungrib. Uses grib_set from eccodes.
#
#  
set echo

module load ncarenv/23.09  intel/2024.2.1
module load eccodes/2.36.0

set workdir = ./
cd $workdir

# Create a file list of the downloaded ifs files

set files = `ls -l 20*oper-fc.grib2 | awk '{print $9}' `

foreach f ( $files )
# file names are of the following form 20241012000000-0h-oper-fc.grib2
 set fname = `echo $f | cut -c1-10`
 set h = `echo $f | cut -c19-19`
 if ( `echo $f | cut -c19-19` == 'h' ) then
   set fhr = `echo $f | cut -c16-18`
 else if ( `echo $f | cut -c18-18` == 'h' ) then
   set fhr = '0'`echo $f | cut -c16-17`
 else
   set fhr = '00'`echo $f | cut -c16-16`
 endif
 set newname = 'ifs.'${fname}.f${fhr}.grib2

 cp ${f} ${fname}_ccsds.grb2         # work on a copy

 grib_set -r -w packingType=grid_ccsds -s packingType=grid_simple ${fname}_ccsds.grb2 ${newname}

 if ( $status == 0 ) /bin/rm ${fname}_ccsds.grb2   # if successful delete the compressed file copy

end
