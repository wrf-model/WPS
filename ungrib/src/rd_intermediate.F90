!  NRCM helper, WPS toy code

PROGRAM rd_intermediate

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   CHARACTER ( LEN =132 )            :: flnm

   INTEGER                           :: ifv

   CHARACTER ( LEN = 24 )            :: hdate
   CHARACTER ( LEN =  9 )            :: field
   CHARACTER ( LEN = 25 )            :: units
   CHARACTER ( LEN = 46 )            :: desc

   REAL                              :: level
   REAL                              :: lat1, lon1
   REAL                              :: deltalat, deltalon
   REAL                              :: xfcst, dy, dx, lov, truelat1, truelat2

   INTEGER                           :: idim, jdim
   INTEGER                           :: llflag, ierr, iop
   INTEGER                           :: grid_wind

   CHARACTER ( LEN =  8 )            :: map_start
   CHARACTER ( LEN = 32 )            :: source

   REAL                              :: scr

   !  Get the input file name from the command line.

   CALL getarg ( 1 , flnm  )

   IF ( flnm(1:1) .EQ. ' ' ) THEN
      print *,'USAGE: rd_intermediate.exe FILE:2006-07-31_00'
      STOP
   END IF

   !  This is the input to open.

   OPEN ( UNIT   =  13           , &
          FILE   =  flnm         , &
          FORM   = 'UNFORMATTED' , &
          STATUS = 'OLD'         , &
          IOSTAT =  iop            )

   IF ( iop .NE. 0) then
      print *, 'File = ',TRIM(flnm)
      print *, 'Status = ',iop
      print *, 'Problem with input file, I can''t open it'
      STOP 
   END IF

   !  Loop over all of the fields in the file.

   all_fields : DO

      !  The format version.

      READ ( 13, IOSTAT=ierr ) ifv

      IF ( ierr .NE. 0 ) THEN
         EXIT all_fields
      END IF


      !  There are a few recognized format versions:
      !     3 = MM5 pregrid  intermediate format
      !     4 = SI  gribprep intermediate format
      !     5 = WPS ungrib   intermediate format
       
      IF ( ifv .EQ. 3 ) THEN

         READ ( 13 )  hdate, xfcst, field, units, desc, level,idim, jdim, llflag
         write (*,FMT='("date = ",A," FCST = ",F8.4," FIELD = ",A," UNITS = ",A,/&
               &" DESCRIPTION = ",A," LEVEL = ",F8.0,/,&
               &" i,j dims = ",2i4," FLAG = ",I2)' ) &
               hdate, xfcst, field, units, desc, level,idim, jdim, llflag
         
         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  lat1, lon1, dy, dx
               WRITE (*,FMT='(A,/,4g12.6)') 'lat1, lon1, dy, dx = ',lat1, lon1, dy, dx
            CASE (1)
               READ ( 13 )  lat1, lon1, dy, dx, truelat1
               WRITE (*,FMT='(A,/,5g12.6)') 'lat1, lon1, dy, dx, truelat1 = ', lat1, lon1, dy, dx, truelat1
            CASE (3)
               READ ( 13 )  lat1, lon1, dx, dy, lov, truelat1, truelat2
               WRITE (*,FMT='(A,/,7g12.6)') 'lat1, lon1, dx, dy, lov, truelat1, truelat2 = ',lat1, lon1, dx, dy, lov, truelat1, truelat2
            CASE (5)
               READ ( 13 )  lat1, lon1, dx, dy, lov, truelat1
               WRITE (*,FMT='(A,/,6g12.6)') 'lat1, lon1, dx, dy, lov, truelat1 = ',lat1, lon1, dx, dy, lov, truelat1
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT

      ELSE IF ( ifv .EQ. 4) THEN

         READ ( 13 )  hdate, xfcst, source, field, units, desc, level, idim, jdim, llflag
         write (*,FMT='("date = ",A," FCST = ",F8.4,&
               &" SOURCE = ",A," FIELD = ",A," UNITS = ",A,/&
               &" DESCRIPTION = ",A," LEVEL = ",F8.0,/,&
               &" i,j dims = ",2i4," FLAG = ",I2)' ) &
               hdate, xfcst, source, field, units, desc, level,idim, jdim, llflag

         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  map_start, lat1, lon1, dy, dx
               WRITE (*,FMT='(A,/,A,1x,4g12.6)') 'map_start, lat1, lon1, dy, dx = ',map_start, lat1, lon1, dy, dx
            CASE (1)
               READ ( 13 )  map_start, lat1, lon1, dy, dx, truelat1
               WRITE (*,FMT='(A,/,A,1x,5g12.6)') 'map_start, lat1, lon1, dy, dx, truelat1 = ' ,map_start, lat1, lon1, dy, dx, truelat1
            CASE (3)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               WRITE (*,FMT='(A,/,A,1x,7g12.6)') 'map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2 = ' , map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
            CASE (5)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1
               WRITE (*,FMT='(A,/,A,1x,6g12.6)') 'map_start, lat1, lon1, dx, dy, lov, truelat1 = ', map_start, lat1, lon1, dx, dy, lov, truelat1
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT

      ELSE IF ( ifv .EQ. 5) THEN

         READ ( 13 )  hdate, xfcst, source, field, units, desc, level, idim, jdim, llflag
         write (*,FMT='("date = ",A," FCST = ",F8.4,&
               &" SOURCE = ",A," FIELD = ",A," UNITS = ",A,/&
               &" DESCRIPTION = ",A," LEVEL = ",F8.0,/,&
               &" i,j dims = ",2i4," FLAG = ",I2)' ) &
               hdate, xfcst, source, field, units, desc, level,idim, jdim, llflag

         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  map_start, lat1, lon1, dy, dx
               WRITE (*,FMT='(A,/,A,1x,4g12.6)') 'map_start, lat1, lon1, dy, dx = ',map_start, lat1, lon1, dy, dx
            CASE (1)
               READ ( 13 )  map_start, lat1, lon1, dy, dx, truelat1
               WRITE (*,FMT='(A,/,A,1x,5g12.6)') 'map_start, lat1, lon1, dy, dx, truelat1 = ' ,map_start, lat1, lon1, dy, dx, truelat1
            CASE (3)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               WRITE (*,FMT='(A,/,A,1x,7g12.6)') 'map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2 = ' , map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
            CASE (5)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1
               WRITE (*,FMT='(A,/,A,1x,6g12.6)') 'map_start, lat1, lon1, dx, dy, lov, truelat1 = ', map_start, lat1, lon1, dx, dy, lov, truelat1
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT
         READ ( 13 )  grid_wind
	 WRITE (*,FMT='(A,i4)') 'grid_wind = ', grid_wind

      ELSE
         print*, 'Unknown ifv: ', ifv
         STOP
      END IF

      READ ( 13 )  scr
      print *,'data = ',scr
      print *,' '


   END DO all_fields

   !  We have processed all of the input data, close both files.

   CLOSE ( 13 )

   print *,'SUCCESSFUL COMPLETION OF PROGRAM RD_INTERMEDIATE'
   STOP

END PROGRAM rd_intermediate
