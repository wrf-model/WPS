PROGRAM rd_intermediate

   USE module_debug
   USE misc_definitions_module
   USE read_met_module

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   INTEGER :: istatus, version, nx, ny, iproj
   REAL :: xfcst, xlvl, startlat, startlon, starti, startj, &
           deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, &
           earth_radius
   REAL, POINTER, DIMENSION(:,:) :: slab
   LOGICAL :: is_wind_grid_rel

   CHARACTER ( LEN =132 )            :: flnm

   CHARACTER ( LEN = 24 )            :: hdate
   CHARACTER ( LEN =  9 )            :: field
   CHARACTER ( LEN = 25 )            :: units
   CHARACTER ( LEN = 46 )            :: desc
   CHARACTER ( LEN = 32 )            :: map_source

   !  Get the input file name from the command line.
   CALL getarg ( 1 , flnm  )

   IF ( flnm(1:1) == ' ' ) THEN
      print *,'USAGE: rd_intermediate.exe <filename>'
      print *,'       where <filename> is the name of an intermediate-format file'
      STOP
   END IF

   CALL set_debug_level(WARN)

   CALL read_met_init(trim(flnm), .true., '0000-00-00_00', istatus)

   IF ( istatus == 0 ) THEN

      CALL  read_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                          iproj, startlat, startlon, starti, startj, deltalat, &
                          deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, &
                          nx, ny, map_source, &
                          slab, is_wind_grid_rel, istatus)

      DO WHILE (istatus == 0)

         CALL mprintf(.true.,STDOUT, '================================================')
         CALL mprintf(.true.,STDOUT, 'FIELD = %s', s1=field)
         CALL mprintf(.true.,STDOUT, 'UNITS = %s DESCRIPTION = %s', s1=units, s2=desc)
         CALL mprintf(.true.,STDOUT, 'DATE = %s FCST = %f', s1=hdate, f1=xfcst)
         CALL mprintf(.true.,STDOUT, 'SOURCE = %s', s1=map_source)
         CALL mprintf(.true.,STDOUT, 'LEVEL = %f', f1=xlvl)
         CALL mprintf(.true.,STDOUT, 'I,J DIMS = %i, %i', i1=nx, i2=ny)
         CALL mprintf(.true.,STDOUT, 'IPROJ = %i', i1=iproj) 

         SELECT CASE ( iproj )
            CASE (PROJ_LATLON)
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=starti, f2=startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=startlat, f2=startlon)
               CALL mprintf(.true.,STDOUT,'  DLAT, DLON = %f, %f', f1=deltalat, f2=deltalon)
            CASE (PROJ_MERC)
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=starti, f2=startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=startlat, f2=startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=dx, f2=dy)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=truelat1)
            CASE (PROJ_LC)
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=starti, f2=startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=startlat, f2=startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=dx, f2=dy)
               CALL mprintf(.true.,STDOUT,'  STAND_LON = %f', f1=xlonc)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=truelat1)
               CALL mprintf(.true.,STDOUT,'  TRUELAT2 = %f', f1=truelat2)
            CASE (PROJ_GAUSS)
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=starti, f2=startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=startlat, f2=startlon)
               CALL mprintf(.true.,STDOUT,'  NLATS, DLON = %f, %f', f1=dy, f2=deltalon)
            CASE (PROJ_PS)
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=starti, f2=startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=startlat, f2=startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=dx, f2=dy)
               CALL mprintf(.true.,STDOUT,'  STAND_LON = %f', f1=xlonc)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=truelat1)
            CASE default
               CALL mprintf(.true.,ERROR, '  Unknown iproj %i for version %i', i1=iproj, i2=version)
         END SELECT
         CALL mprintf(.true.,STDOUT,'DATA(1,1)=%f',f1=slab(1,1))
         CALL mprintf(.true.,STDOUT,'')

         IF (ASSOCIATED(slab)) DEALLOCATE(slab)

         CALL  read_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                             iproj, startlat, startlon, starti, startj, deltalat, &
                             deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, &
                             nx, ny, map_source, &
                             slab, is_wind_grid_rel, istatus)
      END DO

      CALL read_met_close()

   ELSE
      print *, 'File = ',TRIM(flnm)
      print *, 'Problem with input file, I can''t open it'
      STOP 
   END IF

   print *,'SUCCESSFUL COMPLETION OF PROGRAM RD_INTERMEDIATE'
   STOP

END PROGRAM rd_intermediate
