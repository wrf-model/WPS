#!/bin/csh

#	Make sure we are in the right spot.

if ( ( -d WPS ) || ( -d WRFV2 ) ) then
	echo "cleaning up regression stuff"
else
	echo "need to be in the right dir"
	exit ( 1 ) 
endif

rm WPS/*print* >& /dev/null
rm WPS/*.log >& /dev/null
rm WPS/GRIBFILE.* >& /dev/null
rm WPS/PFILE* >& /dev/null
rm WPS/geo_* >& /dev/null
rm WPS/met_* >& /dev/null
rm WPS/Vtable >& /dev/null
rm -rf WPS/TEMPORARY_STORAGE >& /dev/null
rm -rf WPS/foodir >& /dev/null

rm WRFV2/test/em_real/*print* >& /dev/null
rm WRFV2/test/em_real/met_* >& /dev/null
rm WRFV2/test/em_real/wrfi* >& /dev/null
rm WRFV2/test/em_real/wrfb* >& /dev/null
rm WRFV2/test/em_real/wrfo* >& /dev/null
rm -rf WRFV2/test/em_real/TEMPORARY_STORAGE >& /dev/null
rm -rf WRFV2/test/em_real/RIP >& /dev/null

rm foo >& /dev/null
