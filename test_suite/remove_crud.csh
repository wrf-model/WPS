#!/bin/csh

if ( ! -d TEST_001.source=GFS_g1 ) then
	echo "I must be in the wrong place"
	exit ( 1 ) 
else
	find . -name \*.ras -exec rm {} \;
	find . -name \*.cgm -exec rm {} \;
	find . -name wrfi\* -exec rm {} \;
	find . -name wrfo\* -exec rm {} \;
	find . -name wrfb\* -exec rm {} \;
	find . -name core\* -exec rm {} \;
endif
