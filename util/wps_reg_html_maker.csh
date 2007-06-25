#!/bin/csh

#	Script to build the WPS Regression Test Page

#	For saftey purposes, unalias any silly things users have for
#	these commonly cobbled up commands.

unalias cp rm ln cd popd

#	Where should we be expecting to find data?

if ( -d /mmm/users/gill/DATA/WPS_regression_data/TEST_001 ) then
	set DATA_DIR = /mmm/users/gill/DATA/WPS_regression_data
else if ( -d /standalone/users/gill/DATA/WPS_regression_data/TEST_001 ) then
        set DATA_DIR = /standalone/users/gill/DATA/WPS_regression_data
else if ( -d /stink/gill/DATA/WPS_regression_data/TEST_001 ) then
        set DATA_DIR = /stink/gill/DATA/WPS_regression_data
else if ( -d /data3a/mp/gill/DATA/WPS_regression_data/TEST_001 ) then
        set DATA_DIR = /data3a/mp/gill/DATA/WPS_regression_data
else
	echo " "
	echo "Could not find the WPS Regression Data anywhere"
	echo " "
	exit ( 1 ) 
endif

#	First, get rid of any old versions laying around.

set HTML = Awps_reg.html

if ( -e $HTML ) then
	/bin/rm $HTML
endif

#	Top portion of the web page, all that header and title stuff.

echo "<HTML>" > $HTML
echo "<HEAD>" >> $HTML
echo "<TITLE> WPS, ARW Real, and WRF Forecast Regression Test Status Page for `uname`</title>" >> $HTML
echo " " >> $HTML
echo "</HEAD>" >> $HTML
echo " " >> $HTML
echo "<BODY bgcolor='#FFFFFF'>" >> $HTML
echo " " >> $HTML
echo "<CENTER>" >> $HTML
echo "<H2><B>WPS, ARW Real, and WRF Forecast Regression Test Status Page for `uname`</B> </H2><BR>" >> $HTML
echo "</CENTER>" >> $HTML
echo " " >> $HTML
echo "<center>" >> $HTML
echo "<table BORDER='0' CELLPADDING='5' WIDTH='70%' CELLSPACING='5'>" >> $HTML
echo "<tr>" >> $HTML
echo "<td WIDTH='25%' BGCOLOR='#FFFFFF' ALIGN='left' VALIGN='top'><b></b></td>" >> $HTML
echo "<td WIDTH='25%' BGCOLOR='#FFFFFF' ALIGN='left' VALIGN='top'><b>Constants</b></td>" >> $HTML
echo "<td WIDTH='25%' BGCOLOR='#FFFFFF' ALIGN='left' VALIGN='top'><b>0 h FCST</b></td>" >> $HTML
echo "<td WIDTH='25%' BGCOLOR='#FFFFFF' ALIGN='left' VALIGN='top'><b>6 h FCST</b></td>" >> $HTML
echo "</tr>" >> $HTML


set TNUM = ( `ls -1 WPS/test_suite` )

foreach n ( $TNUM )

	set CASE_NAME = ( `grep geogrid WPS/test_suite/${n}/the_env | awk '{print $3}'` )

	echo " " >> $HTML
	echo "<tr>" >> $HTML
	echo "<td BGCOLOR='#FFAAAA'><b>${CASE_NAME}</b></td>" >> $HTML
	echo "<td BGCOLOR='#FFFFFF'>       </td>" >> $HTML
	echo "<td BGCOLOR='#FFFFFF'>       </td>" >> $HTML
	echo "<td BGCOLOR='#FFFFFF'>       </td>" >> $HTML
	echo "</tr>" >> $HTML

	set ALL_ICs = ( `ls -1 ${DATA_DIR}/${n}/DATA` )
	foreach IC ( $ALL_ICs ) 

		set flnm = ${n}_${IC}
		echo " " >> $HTML
		echo "<tr>" >> $HTML
		echo "<td><b>CASE NAME ${CASE_NAME}<br>${IC}</b></td>" >> $HTML
		echo "<td BGCOLOR='#FFFFFF'>" >> $HTML
		echo "<dl>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_01.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_01_small.gif'>LU</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_02.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_02_small.gif'>Topo</a>" >> $HTML
		echo "</dl>" >> $HTML
		echo "</td>" >> $HTML
		echo " " >> $HTML
		echo "<td BGCOLOR='#FFFFFF'>" >> $HTML
		echo "<dl>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_03.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_03_small.gif'>SLP</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_04.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_04_small.gif'>850</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_05.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_05_small.gif'>700</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_06.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_06_small.gif'>500</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_07.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_07_small.gif'>300</a>" >> $HTML
		echo "</dl>" >> $HTML
		echo "</td>" >> $HTML
		echo " " >> $HTML
		echo "<td BGCOLOR='#FFFFFF'>" >> $HTML
		echo "<dl>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_08.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_08_small.gif'>SLP</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_09.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_09_small.gif'>850</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_10.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_10_small.gif'>700</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_11.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_11_small.gif'>500</a>" >> $HTML
		echo "<dt><a href='./${n}.source=${IC}/plot_12.html' target='_new'><img src='./${n}.source=${IC}/${flnm}_12_small.gif'>300</a>" >> $HTML
		echo "</dl>" >> $HTML
		echo "</td>" >> $HTML
		echo "</tr>" >> $HTML
	end
end

echo "</table>" >> $HTML
echo "</BODY>" >> $HTML
echo "</HTML>" >> $HTML
