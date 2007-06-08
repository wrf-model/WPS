#!/bin/csh

#BSUB -P 64000400			# proj account to charge	
#BSUB -R "span[ptile=2]"                # how many tasks per node (up to 8)
#BSUB -n 1                              # number of total tasks
#BSUB -o WPS.out                        # output filename (%J to add job id)
#BSUB -e WPS.err                        # error filename
#BSUB -J WPS.test                       # job name
#BSUB -q share                          # queue
#BSUB -W 3:00                           # wallclock time

########	CHANGE THIS DIRECTORY	#######
if ( `uname` == AIX ) then
	if ( -d /ptmp/gill/WPS_reg ) then
		cd /ptmp/gill/WPS_reg
	else
		echo "/ptmp/gill/WPS_reg does not exist - stopping"
		exit 1
	endif
endif
########	CHANGE THIS DIRECTORY	#######

unalias cp rm ls
unalias popd

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

set NUM_FRAMES = 12
set TOP_DIR = `pwd`
set PLOTS_ONLY = TRUE
set PLOTS_ONLY = FALSE

#	WRFV2 build

clear

if ( $PLOTS_ONLY == FALSE ) then
	
	if ( `uname` == Linux ) then
		echo 1. starting WRFV2 build - takes about 7 minutes
	else if ( `uname` == AIX ) then
		echo "1. starting WRFV2 build - takes 20 (bs) to 35 (bv) minutes"
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
	     ( -e main/real.exe ) ) then
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

endif

#	WPS TESTS

echo " "
echo 3. starting WPS tests
echo "     start: " `date`
pushd WPS >& /dev/null
set tcount = 0

#	Loop over all of the tests

if ( ${#argv} == 0 ) then
	set all_tests = ( `ls -1 test_suite` )
else	
	set all_tests = ( $* )
endif
foreach test_num ( $all_tests ) 

	#	Set the env from the helpful env file

	source test_suite/${test_num}/the_env
	
	#	Copy in the WPS namelist.

	cp test_suite/${test_num}/namelist.wps .

	#	Tell folks what we are doing.

	@ tcount ++
	echo "        Test #${tcount}"
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
	else if ( -d /mmm/users/wrfhelp/WPS_GEOG ) then
		echo ' geog_data_path = "/mmm/users/wrfhelp/WPS_GEOG"' >! foodir
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

	if ( $PLOTS_ONLY == FALSE ) then
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
	endif

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

	set source = ( `ls -1 $datadir/$test_num/DATA` )

	#	Loop of the data sources, one run each of ungrib and one of metgrid.

	foreach data_source ( $source )

		#	The sources are just directories in the TEST_001, etc dirs.  Also
		#	in there is a namelist file.  That is not a valid source of data,
		#	so we skip it an move on.

		if ( ( $data_source == namelist.input ) || ( $data_source == rip_test.in ) ) then
			goto skipped_namelist_as_a_directory
		endif

		#	The incremented counter, to keep track of which data source
		#	we are running, and so that we are using the right Vtable.

		set Vtable = `echo $data_source | cut -d "_" -f1`
		cp ungrib/Variable_Tables/Vtable.$Vtable Vtable
		./link_grib.csh $datadir/$test_num/DATA/$data_source/*

		if ( $PLOTS_ONLY == FALSE ) then

			#	Run ungrib, the grib decoder.
			
			echo "           ungrib.exe share=${share} ungrib=$ungrib source=$data_source"
			echo "              start: " `date`
			ungrib.exe >&! ungrib.print.share=${share}.ungrib=${ungrib}.source=$data_source
			grep -i success ungrib.print.share=${share}.ungrib=$ungrib.source=$data_source >& /dev/null
	#		grep -i Bandimere ungrib.print.share=${share}.ungrib=$ungrib.source=$data_source >& /dev/null
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
	
			#	## Run the real/wrf combo on this data generated. ##
	
			#	Get to the WRF dir, just up and over a bit.
	
			pushd ../WRFV2/test/em_real >& /dev/null
	
				#	We need the data we just made from metgrid to be the input for real.
		
				ln -sf ../../../WPS/TEMPORARY_STORAGE/${test_num}.source=${data_source}/met_* .
		
				#	Manufacture the namelist.  A template is in the data dir, just edit the 
				#	number of metgrid levels.
		
				cp $datadir/$test_num/namelist.input namelist.input.template
				set NUM_METGRID_LEVELS = `ncdump -h met_em.d02.* | grep -i num_metgrid_levels | grep = | awk '{print $3}'`
				m4 -DNUM_METGRID_LEVELS=${NUM_METGRID_LEVELS} namelist.input.template >! namelist.input
		
				#	The real portion.
		
				echo "           real.exe share=${share} metgrid=${metgrid} source=$data_source"
				echo "              start: " `date`
				real.exe >&! real.print.share=${share}.metgrid=${metgrid}.source=$data_source
				grep -i success real.print.share=${share}.metgrid=${metgrid}.source=$data_source >& /dev/null
				set ok = $status
				if ( $ok != 0 ) then
					echo " "
					echo " "
					echo "Failed to run real.exe"
					echo " "
					echo " "
					exit ( 8 ) 
				endif
				echo "              end:   " `date`
		
				#	The wrf portion.
		
				echo "           wrf.exe share=${share} metgrid=${metgrid} source=$data_source"
				echo "              start: " `date`
				wrf.exe >&! wrf.print.share=${share}.metgrid=${metgrid}.source=$data_source
				grep -i success wrf.print.share=${share}.metgrid=${metgrid}.source=$data_source >& /dev/null
				set ok = $status
				if ( $ok != 0 ) then
					echo " "
					echo " "
					echo "Failed to run wrf.exe"
					echo " "
					echo " "
					exit ( 9 ) 
				endif
				echo "              end:   " `date`
	
				#	Save the model IC, BC and forecast data.
	
				if ( ! -d TEMPORARY_STORAGE ) then
					mkdir TEMPORARY_STORAGE
				endif
				if ( -d TEMPORARY_STORAGE/${test_num}.source=${data_source} ) then
					rm -rf TEMPORARY_STORAGE/${test_num}.source=${data_source}
				endif
				mkdir TEMPORARY_STORAGE/${test_num}.source=${data_source}
				mv wrfi* wrfb* wrfo* namelist.input TEMPORARY_STORAGE/${test_num}.source=${data_source}
				rm met_*
	
			#	Get back out to the WPS dir.
	
			popd >& /dev/null

		endif

#######

		pushd ../WRFV2/test/em_real >& /dev/null
		
		#	Handle the plots with RIP.
		
		echo "           ripdp_wrf and rip share=${share} metgrid=${metgrid} source=$data_source"
		echo "              start: " `date`
		if ( ! -d RIP ) then
			mkdir RIP
		endif
		rm -rf RIP/${test_num}_${data_source}* >& /dev/null
		ripdp_wrf RIP/${test_num}_${data_source} \
		          all \
		          TEMPORARY_STORAGE/${test_num}.source=${data_source}/wrfo* >&! \
		          ripdp_wrf.print.share=${share}.metgrid=${metgrid}.source=$data_source
		grep -i vladimir ripdp_wrf.print.share=${share}.metgrid=${metgrid}.source=$data_source >& /dev/null
		set ok = $status
		if ( $ok != 0 ) then
			echo " "
			echo " "
			echo "Failed to run ripdp_wrf"
			echo " "
			echo " "
			exit ( 10 ) 
		endif

		#	... and RIP.

		cp ${datadir}/${test_num}/rip_test.in test.in
		rip RIP/${test_num}_${data_source} test.in >&! \
		          rip.print.share=${share}.metgrid=${metgrid}.source=$data_source
		grep -i vladimir rip.print.share=${share}.metgrid=${metgrid}.source=$data_source >& /dev/null
		set ok = $status
		if ( $ok != 0 ) then
			echo " "
			echo " "
			echo "Failed to run rip"
			echo " "
			echo " "
			exit ( 11 ) 
		endif

		#	Split plots into CGM pieces.

		if ( -e med.info ) then
			rm med.info
		endif
		touch med.info

		set count = 0

		echo "read test.cgm" >> med.info
		while ( $count < $NUM_FRAMES ) 
			@ count ++
			if ( $count < 10 ) then
				set index = 0$count
			else
				set index =  $count
			endif
			echo "${index} , ${index} w plot${index}.cgm" >> med.info
		end
		echo "quit" >> med.info

		#	Run med to split frames.

		med -f med.info >& /dev/null

		#	Convert to a more traditional form, we like gif right now.

		set count = 0
		while ( $count < $NUM_FRAMES ) 
			@ count ++
			if ( $count < 10 ) then
				set index = 0$count
			else
				set index =  $count
			endif
			ctrans -d sun plot${index}.cgm > plot${index}.ras
			convert plot${index}.ras ${test_num}_${data_source}_${index}.gif
			convert ${test_num}_${data_source}_${index}.gif -resize 10% \
				${test_num}_${data_source}_${index}_small.gif
		end

		mv test.cgm plot*.cgm plot*.ras *.gif \
		TEMPORARY_STORAGE/${test_num}.source=${data_source}
		rm med.info

		#	Build singleton web pages for each image

		set count = 0
		while ( $count < $NUM_FRAMES ) 
			@ count ++
			if ( $count < 10 ) then
				set index = 0$count
			else
				set index =  $count
			endif
			cat >&! TEMPORARY_STORAGE/${test_num}.source=${data_source}/plot_${index}.html << EOF
<HTML>
<BODY>
<img src="${test_num}_${data_source}_${index}.gif">
</BODY>
</HTML>
EOF
		end

		popd >& /dev/null

		#	Put the pre-built web page on top of the WRF fcst plots.

		if ( ! -e ../WRFV2/test/em_real/TEMPORARY_STORAGE/wps_reg.html ) then
			cp util/wps_reg.html ../WRFV2/test/em_real/TEMPORARY_STORAGE
		endif
		echo "              end:   " `date`

#########
		skipped_namelist_as_a_directory:

	end

	#	Save the static data for this location.

	if ( $PLOTS_ONLY == FALSE ) then
		if ( -d TEMPORARY_STORAGE/${test_num}.location ) then
			rm -rf TEMPORARY_STORAGE/${test_num}.location
		endif
		mkdir TEMPORARY_STORAGE/${test_num}.location
		mv geo_* TEMPORARY_STORAGE/${test_num}.location
	endif
end
echo "     end:   " `date`
popd >& /dev/null
