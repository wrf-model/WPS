#!/bin/csh

unalias cp rm ls

if ( ( ! -d WPS ) || ( ! -d WRFV2 ) ) then
	clear
	echo " "
	echo " "
	echo "This test is run from a directory where"
	echo "both WPS and WRFV2 exist"
	echo " "
	echo " "
	exit ( 1 ) 
endif

set TOP_DIR = `pwd`

#	WRFV2 build

clear
if ( `uname` == Linux ) then
	echo 1. starting WRFV2 build - takes about 7 minutes
else if ( `uname` == AIX ) then
	echo 1. starting WRFV2 build - takes about 20 minutes
else
	echo 1. starting WRFV2 build
endif
echo "     start: " `date`
pushd WRFV2 >& /dev/null

#	We at least want to be able to do nesting.

if ( `uname` == Linux ) then
	echo 2 | ./configure >& /dev/null
else if ( `uname` == AIX ) then
	echo 9 | ./configure >& /dev/null
else
	echo need info on this `uname` arch
	exit
endif

./compile em_real >&! build.log
if ( ( -e main/wrf.exe ) && \
     ( -e main/real.exe ) && \
     ( -e main/ndown.exe ) ) then
	echo "        WRFV2 build OK"
else
	echo " "
	echo " "
	echo "WRFV2 build failed"
	echo "Look at $TOP_DIR/WRFV2/build.log"
	echo " "
	echo " "
	exit ( 2 ) 
endif
echo "     end:   " `date`
popd >& /dev/null

#	WPS build

echo " "
echo 2. starting WPS build - takes about 1 minute
echo "     start: " `date`
pushd WPS >& /dev/null
echo 2 | ./configure >& /dev/null
./compile wps >&! build.log
if ( ( -e geogrid.exe ) && \
     ( -e metgrid.exe ) && \
     ( -e ungrib.exe ) ) then
	echo "        WPS build OK"
else
	echo " "
	echo " "
	echo "WPS build failed"
	echo "Look at $TOP_DIR/WPS/build.log"
	echo " "
	echo " "
	exit ( 3 ) 
endif
echo "     end:   " `date`
popd >& /dev/null

#	WPS TESTS

echo " "
echo 3. starting WPS tests
echo "     start: " `date`
pushd WPS >& /dev/null
set count = 0

#	Loop over all of the tests

if ( ${#argv} == 0 ) then
	set all_tests = `ls -1 test_suite`
else	
	set all_tests = $*
endif
foreach test_num ( $all_tests ) 

	#	Set the env from the helpful env file

	source test_suite/${test_num}/the_env
	
	#	Copy in the WPS namelist.

	cp test_suite/${test_num}/namelist.wps .

	#	Tell folks what we are doing.

	@ count ++
	echo "        Test #${count}"
	echo "        share:   " $share
	echo "        geogrid: " $geogrid
	echo "        ungrib:  " $ungrib
	echo "        metgrid: " $metgrid

	#	Set the path to the static geog data in the namelist.

	if      ( -d /bouleau/users/duda/GEOG ) then
	#	Michael is a no-op
	else if ( -d /standalone/users/gill/DATA/GEOG ) then
		echo ' geog_data_path = "/standalone/users/gill/DATA/GEOG"' >! foodir
	else if ( -d /data3a/mp/gill/DATA/GEOG ) then
		echo ' geog_data_path = "/data3a/mp/gill/DATA/GEOG"' >! foodir
	else if ( -d /mmm/users/gill/DATA/GEOG ) then
		echo ' geog_data_path = "/mmm/users/gill/DATA/GEOG"' >! foodir
	else
		echo " "
		echo " "
		echo "Could not find the static data for geogrid"
		echo " "
		echo " "
		exit ( 4 ) 
	endif
	sed -e '/geog_data_path/d' namelist.wps >! .foo
	sed -e '/stand_lon/r foodir' .foo >! namelist.wps

	#	Clean up the ol temp file we made.

	rm .foo

	#	Echo what is happening for each program, also there is
	#	start/ending couplet so that approx timings are possible.

	echo "        geogrid.exe share=${share} geogrid=$geogrid"
	echo "           start: " `date`
	geogrid.exe >&! geogrid.print.share=${share}.geogrid=$geogrid
	grep -i success geogrid.print.share=${share}.geogrid=$geogrid >& /dev/null
	set ok = $status
	if ( $ok != 0 ) then
		echo " "
		echo " "
		echo "Failed to run geogrid.exe"
		echo " "
		echo " "
		exit ( 5 ) 
	endif
	echo "           end:   " `date`

	#	Now we slide over to the ungrib and metgrid programs.  We need to get the
	#	location of the met data.  This is not a nml var, but an argument
	#	to a script. 

	if      ( -d /standalone/users/gill/DATA/WPS_regression_data ) then
		set datadir = /standalone/users/gill/DATA/WPS_regression_data
	else if ( -d /data3a/mp/gill/DATA/WPS_regression_data ) then
		set datadir = /data3a/mp/gill/DATA/WPS_regression_data
	else if ( -d /mmm/users/gill/DATA/WPS_regression_data ) then
		set datadir = /mmm/users/gill/DATA/WPS_regression_data
	else
		echo " "
		echo " "
		echo "Could not find the met data for ungrib.exe"
		echo " "
		echo " "
		exit ( 6 ) 
	endif

	#	List of all of the input data (met) files.  The g1/g2 means Grib Edition
	#	1 vs Edition 2.  Each has the associated Vtable, where the difference
	#	between dir and the Vtable name is the extra "_g1" or "_g2".

	set source = ( `ls -1 $datadir/$test_num` )

	#	Loop of the data sources, one run each of ungrib and one of metgrid.

	foreach data_source ( $source )

		#	The incremented counter, to keep track of which data source
		#	we are running, and so that we are using the right Vtable.

		set Vtable = `echo $data_source | cut -d "_" -f1`
		cp ungrib/Variable_Tables/Vtable.$Vtable Vtable
		./link_grib.csh $datadir/$test_num/$data_source/*

		#	Run ungrib, the grib decoder.
		
		echo "           ungrib.exe share=${share} ungrib=$ungrib source=$data_source"
		echo "              start: " `date`
		ungrib.exe >&! ungrib.print.share=${share}.ungrib=${ungrib}.source=$data_source
#		grep -i success ungrib.print.share=${share}.ungrib=$ungrib.source=$data_source >& /dev/null
		grep -i Bandimere ungrib.print.share=${share}.ungrib=$ungrib.source=$data_source >& /dev/null
		set ok = $status
		if ( $ok != 0 ) then
			echo " "
			echo " "
			echo "Failed to run ungrib.exe"
			echo " "
			echo " "
			exit ( 6 ) 
		endif
		echo "              end:   " `date`

		#	Now with the geogrid and ungrib progs done, we can move to metgrid.

		echo "           metgrid.exe share=${share} metgrid=${metgrid} source=$data_source"
		echo "              start: " `date`
		metgrid.exe >&! metgrid.print.share=${share}.metgrid=${metgrid}.source=$data_source
		grep -i success metgrid.print.share=${share}.metgrid=${metgrid}.source=$data_source >& /dev/null
		set ok = $status
		if ( $ok != 0 ) then
			echo " "
			echo " "
			echo "Failed to run metgrid.exe"
			echo " "
			echo " "
			exit ( 7 ) 
		endif
		echo "              end:   " `date`

		#	Save the data.

		if ( ! -d TEMPORARY_STORAGE ) then
			mkdir TEMPORARY_STORAGE
		endif
		if ( -d TEMPORARY_STORAGE/${test_num}.source=${data_source} ) then
			rm -rf TEMPORARY_STORAGE/${test_num}.source=${data_source}
		endif
		mkdir TEMPORARY_STORAGE/${test_num}.source=${data_source}
		mv FILE* TEMPORARY_STORAGE/${test_num}.source=${data_source}
		mv met_* TEMPORARY_STORAGE/${test_num}.source=${data_source}

	end

	#	Save the static data for this location.

	if ( -d TEMPORARY_STORAGE/${test_num}.location ) then
		rm -rf TEMPORARY_STORAGE/${test_num}.location
	endif
	mkdir TEMPORARY_STORAGE/${test_num}.location
	mv geo_* TEMPORARY_STORAGE/${test_num}.location
end
echo "     end:   " `date`
popd >& /dev/null
