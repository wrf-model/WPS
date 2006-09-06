!  Program to modify levels in the intermediate format.  Two input
!  files come in on the command line: input file and output file.
!  An additional namelist file is used to select which pressure levels
!  are to be kept.

!  NRCM helper, WPS toy code

PROGRAM mod_levs_prog

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   CHARACTER ( LEN =132 )            :: flnm, flnm2

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

   REAL, ALLOCATABLE, DIMENSION(:,:) :: scr2d
   REAL                              :: scr

   !  The namelist has a pressure array that we want.

   LOGICAL                           :: keep_this_one
   INTEGER                           :: l , max_pres_keep
   INTEGER , PARAMETER               :: num_pres_lev = 1000
   REAL, DIMENSION(num_pres_lev)     :: press_pa = -1.
   NAMELIST /mod_levs/ press_pa

   INTEGER , EXTERNAL :: lenner

   !  Open up the file with the pressure levels to process.

   OPEN ( UNIT   =  10            , &
          FILE   = 'namelist.wps' , &
          STATUS = 'OLD'          , &
          FORM   = 'FORMATTED'    , & 
          IOSTAT =  iop              )

   IF (iop .NE. 0) then
      print *, 'Problem with namelist.input file, I can''t open it'
      STOP 
   END IF

   !  Input the pressure levels requested.

   READ ( 10 , mod_levs ) 

   CLOSE ( 10 ) 

   !  How many pressure levels were asked for?

   DO l = 1 , num_pres_lev
      IF ( press_pa(l) .EQ. -1. ) THEN
         max_pres_keep = l-1
         EXIT
      END IF
   END DO

   !  Get the two files: input and output.

   CALL getarg ( 1 , flnm  )

   IF ( flnm(1:1) .EQ. ' ' ) THEN
      print *,'USAGE: mod_levs.exe FILE:2006-07-31_00 new_FILE:2006-07-31_00'
      STOP
   END IF

   CALL getarg ( 2 , flnm2 )

   l = lenner(flnm)
   IF ( flnm2(1:1) .EQ. ' ' ) THEN
      flnm2(5:l+4) = flnm(1:l)
      flnm2(1:4) = 'new_'
   END IF

   !  These are the input and output files, respectively.

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

   OPEN ( UNIT   =  14           , &
          FILE   =  flnm2        , &
          FORM   = 'UNFORMATTED' , &
          STATUS = 'UNKNOWN'     , &
          IOSTAT =  iop            )

   IF (iop .NE. 0) then
      print *, 'File = ',TRIM(flnm2)
      print *, 'Status = ',iop
      print *, 'Problem with output file, I can''t open it'
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

         READ ( 13 )  hdate, xfcst, field, units, desc, level,&
              idim, jdim, llflag
         
         !  Is this a level that we want?

         keep_this_one = .FALSE.
         DO l = 1 , max_pres_keep
            IF ( level .EQ. press_pa(l) ) THEN
               keep_this_one = .TRUE.
               EXIT
            END IF
         END DO 

         IF ( keep_this_one ) THEN
            WRITE ( 14 ) ifv
            WRITE ( 14 ) hdate, xfcst, field, units, desc, level,&
                         idim, jdim, llflag
else
print *,'                   blowing off level ',level,' Pa'
         END IF

         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  lat1, lon1, dy, dx
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) lat1, lon1, dy, dx
               END IF
            CASE (1)
               READ ( 13 )  lat1, lon1, dy, dx, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) lat1, lon1, dy, dx, truelat1
               END IF
            CASE (3)
               READ ( 13 )  lat1, lon1, dx, dy, lov, truelat1, truelat2
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) lat1, lon1, dx, dy, lov, truelat1, truelat2
               END IF
            CASE (5)
               READ ( 13 )  lat1, lon1, dx, dy, lov, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) lat1, lon1, dx, dy, lov, truelat1
               END IF
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT

      ELSE IF ( ifv .EQ. 4) THEN

         READ ( 13 )  hdate, xfcst, source, field, units, desc, level,&
              idim, jdim, llflag
         
         !  Is this a level that we want?

         keep_this_one = .FALSE.
         DO l = 1 , max_pres_keep
            IF ( level .EQ. press_pa(l) ) THEN
               keep_this_one = .TRUE.
               EXIT
            END IF
         END DO 

         IF ( keep_this_one ) THEN
            WRITE ( 14 ) ifv
            WRITE ( 14 ) hdate, xfcst, source, field, units, desc, level,&
                         idim, jdim, llflag
else
print *,'blowing off level ',level,' Pa'
         END IF

         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  map_start, lat1, lon1, dy, dx
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dy, dx
               END IF
            CASE (1)
               READ ( 13 )  map_start, lat1, lon1, dy, dx, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dy, dx, truelat1
               END IF
            CASE (3)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               END IF
            CASE (5)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dx, dy, lov, truelat1
               END IF
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT

      ELSE IF ( ifv .EQ. 5) THEN

         READ ( 13 )  hdate, xfcst, source, field, units, desc, level,&
              idim, jdim, llflag
         
         !  Is this a level that we want?

         keep_this_one = .FALSE.
         DO l = 1 , max_pres_keep
            IF ( level .EQ. press_pa(l) ) THEN
               keep_this_one = .TRUE.
               EXIT
            END IF
         END DO 

         IF ( keep_this_one ) THEN
            WRITE ( 14 ) ifv
            WRITE ( 14 ) hdate, xfcst, source, field, units, desc, level,&
                         idim, jdim, llflag
else
print *,'blowing off level ',level,' Pa'
         END IF

         SELECT CASE ( llflag )
            CASE (0)
               READ ( 13 )  map_start, lat1, lon1, dy, dx
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dy, dx
               END IF
            CASE (1)
               READ ( 13 )  map_start, lat1, lon1, dy, dx, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dy, dx, truelat1
               END IF
            CASE (3)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dx, dy, lov, truelat1, truelat2
               END IF
            CASE (5)
               READ ( 13 )  map_start, lat1, lon1, dx, dy, lov, truelat1
               IF ( keep_this_one ) THEN
                  WRITE ( 14 ) map_start, lat1, lon1, dx, dy, lov, truelat1
               END IF
            CASE default
               print *, 'Unknown flag for ifv = ',ifv,', llflag = ', llflag
               STOP
         END SELECT
	 READ ( 13 ) grid_wind
	 WRITE ( 14 ) grid_wind

      ELSE
         print*, 'Unknown ifv: ', ifv
         STOP
      END IF

      IF ( keep_this_one ) THEN

         !  Read, write and de-allocate.

         ALLOCATE ( scr2d ( idim , jdim ) )

         READ ( 13 )  scr2d
         WRITE ( 14 ) scr2d

         DEALLOCATE ( scr2d )
         print *, 'Processed ',field, level,' for time ',hdate
      ELSE
         READ ( 13 )  scr
      END IF


   END DO all_fields

   !  We have processed all of the input data, close both files.

   CLOSE ( 13 )
   CLOSE ( 14 )

   print *,'SUCCESSFUL COMPLETION OF PROGRAM MOD_LEVS'
   STOP

END PROGRAM mod_levs_prog
   
INTEGER FUNCTION lenner ( string ) 
   CHARACTER ( LEN = 132 ) ::  string
   INTEGER :: l
   DO l = 132 , 1 , -1
      IF ( ( ( string(l:l) .GE. 'A' ) .AND. ( string(l:l) .LE. 'Z' ) ) .OR. &
           ( ( string(l:l) .GE. 'a' ) .AND. ( string(l:l) .LE. 'z' ) ) .OR. &
           ( ( string(l:l) .GE. '0' ) .AND. ( string(l:l) .LE. '9' ) ) ) THEN
         lenner = l
         EXIT
      END IF
   END DO
END FUNCTION lenner
