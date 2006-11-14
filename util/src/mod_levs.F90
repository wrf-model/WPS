!  Program to modify levels in the intermediate format.  Two input
!  files come in on the command line: input file and output file.
!  An additional namelist file is used to select which pressure levels
!  are to be kept.

!  NRCM helper, WPS toy code

PROGRAM mod_levs_prog

   USE module_debug
   USE read_met_module
   USE write_met_module
   USE misc_definitions_module

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   CHARACTER ( LEN =132 )            :: flnm, flnm2

   INTEGER :: istatus, iop, version, nx, ny, iproj
   integer :: idum, ilev
   REAL :: xfcst, xlvl, startlat, startlon, starti, startj, &
           deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius
   REAL, POINTER, DIMENSION(:,:) :: slab
   LOGICAL :: is_wind_grid_rel

   CHARACTER ( LEN = 24 )            :: hdate
   CHARACTER ( LEN =  9 )            :: field
   CHARACTER ( LEN = 25 )            :: units
   CHARACTER ( LEN = 46 )            :: desc
   CHARACTER ( LEN = 32 )            :: map_source

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

   CALL set_debug_level(WARN)

   CALL read_met_init(TRIM(flnm), .true., '0000-00-00_00', istatus)

   IF ( istatus == 0 ) THEN

      CALL write_met_init(TRIM(flnm2), .true., '0000-00-00_00', istatus)

      IF ( istatus == 0 ) THEN

         CALL read_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                           iproj, startlat, startlon, starti, startj, deltalat, &
                           deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, &
                           nx, ny, map_source, &
                           slab, is_wind_grid_rel, istatus)

         DO WHILE (istatus == 0)
   
   
            keep_this_one = .FALSE.
            DO l = 1 , max_pres_keep
               IF ( xlvl .EQ. press_pa(l) ) THEN
                  keep_this_one = .TRUE.
                  EXIT
               END IF
            END DO 

            IF (keep_this_one) THEN
               CALL write_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                                      iproj, startlat, startlon, starti, startj, deltalat, &
                                      deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, &
                                      nx, ny, map_source, &
                                      slab, is_wind_grid_rel, istatus)
            ELSE
               CALL mprintf(.true.,STDOUT,'Deleting level %f Pa',f1=xlvl)
            END IF

            CALL mprintf(.true.,STDOUT,'Processed %s at level %f for time %s', &
                         s1=field, f1=xlvl, s2=hdate)
            IF (ASSOCIATED(slab)) DEALLOCATE(slab)
   
            CALL read_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                                iproj, startlat, startlon, starti, startj, deltalat, &
                                deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, &
                                nx, ny, map_source, &
                                slab, is_wind_grid_rel, istatus)
         END DO

         CALL write_met_close()

      ELSE

         print *, 'File = ',TRIM(flnm2)
         print *, 'Problem with output file, I can''t open it'
         STOP

      END IF

      CALL read_met_close()
 
   ELSE

      print *, 'File = ',TRIM(flnm)
      print *, 'Problem with input file, I can''t open it'
      STOP

   END IF

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
