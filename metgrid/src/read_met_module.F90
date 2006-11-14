module read_met_module

   use constants_module
   use module_debug
   use misc_definitions_module

   ! State variables?
   integer :: input_unit
   character (len=128) :: filename
 
   contains
 
   subroutine read_met_init(fg_source, source_is_constant, datestr, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      logical, intent(in) :: source_is_constant
      character (len=*) :: fg_source
      character (len=*) :: datestr
  
      ! Local variables
      integer :: io_status
      logical :: is_used

      istatus = 0
    
      !  1) BUILD FILENAME BASED ON TIME 
      filename = ' '
      if (.not. source_is_constant) then 
         write(filename, '(a)') trim(fg_source)//':'//trim(datestr)
      else
         write(filename, '(a)') trim(fg_source)
      end if
  
      !  2) OPEN FILE
      do input_unit=10,100
         inquire(unit=input_unit, opened=is_used)
         if (.not. is_used) exit
      end do 
      call mprintf((input_unit > 100),ERROR,'In read_met_init(), couldn''t find an available Fortran unit.')
      open(unit=input_unit, file=trim(filename), status='old', form='unformatted', iostat=io_status)

      if (io_status > 0) istatus = 1

      return
  
 
   end subroutine read_met_init
 
 
   subroutine read_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                          iproj, startlat, startlon, starti, startj, deltalat, &
                          deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius, nx, ny, map_source, &
                          slab, is_wind_grid_rel, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus, version, nx, ny, iproj
      real, intent(out) :: xfcst, xlvl, startlat, startlon, starti, startj, &
                           deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius
      real, pointer, dimension(:,:) :: slab
      logical, intent(out) :: is_wind_grid_rel
      character (len=9), intent(out) :: field
      character (len=24), intent(out) :: hdate
      character (len=25), intent(out) :: units
      character (len=32), intent(out) :: map_source
      character (len=46), intent(out) :: desc
  
      ! Local variables
      character (len=8) :: startloc
  
      istatus = 1
  
      !  1) READ FORMAT VERSION
      read(unit=input_unit,err=1001,end=1001) version
  
      ! PREGRID
      if (version == 3) then

         read(unit=input_unit) hdate, xfcst, field, units, desc, xlvl, nx, ny, iproj
         map_source = ' '

         if (field == 'HGT      ') field = 'GHT      '

         starti = 1.0
         startj = 1.0
     
         ! Cylindrical equidistant
         if (iproj == 0) then
            iproj = PROJ_LATLON
            read(unit=input_unit,err=1001,end=1001) startlat, startlon, deltalat, deltalon
     
         ! Mercator
         else if (iproj == 1) then
            iproj = PROJ_MERC
            read(unit=input_unit,err=1001,end=1001) startlat, startlon, dx, dy, truelat1
     
         ! Lambert conformal
         else if (iproj == 3) then
            iproj = PROJ_LC
            read(unit=input_unit,err=1001,end=1001) startlat, startlon, dx, dy, xlonc, truelat1, truelat2
     
         ! Polar stereographic
         else if (iproj == 5) then
            iproj = PROJ_PS
            read(unit=input_unit,err=1001,end=1001) startlat, startlon, dx, dy, xlonc, truelat1

         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s', &
                         i1=iproj, s1=filename)
     
         end if
     
         earth_radius = EARTH_RADIUS_M / 1000.

#if (defined _GEOGRID) || (defined _METGRID)
         dx = dx * 1000.
         dy = dy * 1000.

         if (xlonc > 180.) xlonc = xlonc - 360.
         if (startlon > 180.) startlon = startlon - 360.
  
         if (startlat < -90.) startlat = -90.
         if (startlat > 90.) startlat = 90.
#endif
     
         is_wind_grid_rel = .true.
     
         allocate(slab(nx, ny))
         read(unit=input_unit,err=1001,end=1001) slab
     
         istatus = 0 
    
      ! GRIB_PREP
      else if (version == 4) then
  
         read(unit=input_unit) hdate, xfcst, map_source, field, units, desc, xlvl, nx, ny, iproj
  
         if (field == 'HGT      ') field = 'GHT      '
  
         ! Cylindrical equidistant
         if (iproj == 0) then
            iproj = PROJ_LATLON
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, deltalat, deltalon

         ! Mercator
         else if (iproj == 1) then
            iproj = PROJ_MERC
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, truelat1

         ! Lambert conformal
         else if (iproj == 3) then
            iproj = PROJ_LC
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, xlonc, truelat1, truelat2

         ! Polar stereographic
         else if (iproj == 5) then
            iproj = PROJ_PS
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, xlonc, truelat1
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s', &
                         i1=iproj, s1=filename)
     
         end if
  
         if (startloc == 'CENTER  ') then
            starti = real(nx)/2.
            startj = real(ny)/2.
         else if (startloc == 'SWCORNER') then
            starti = 1.0
            startj = 1.0
         end if

         earth_radius = EARTH_RADIUS_M / 1000.

#if (defined _GEOGRID) || (defined _METGRID)
         dx = dx * 1000.
         dy = dy * 1000.

         if (xlonc > 180.) xlonc = xlonc - 360.
         if (startlon > 180.) startlon = startlon - 360.
  
         if (startlat < -90.) startlat = -90.
         if (startlat > 90.) startlat = 90.
#endif
         
         is_wind_grid_rel = .true.
      
         allocate(slab(nx, ny))
         read(unit=input_unit,err=1001,end=1001) slab
      
         istatus = 0

      ! WPS
      else if (version == 5) then
  
         read(unit=input_unit) hdate, xfcst, map_source, field, units, desc, xlvl, nx, ny, iproj
  
         if (field == 'HGT      ') field = 'GHT      '
  
         ! Cylindrical equidistant
         if (iproj == 0) then
            iproj = PROJ_LATLON
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, deltalat, deltalon, earth_radius

         ! Mercator
         else if (iproj == 1) then
            iproj = PROJ_MERC
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, truelat1, earth_radius

         ! Lambert conformal
         else if (iproj == 3) then
            iproj = PROJ_LC
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, xlonc, truelat1, truelat2, earth_radius

         ! Gaussian
         else if (iproj == 4) then
            iproj = PROJ_GAUSS
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, deltalat, deltalon, earth_radius

         ! Polar stereographic
         else if (iproj == 5) then
            iproj = PROJ_PS
            read(unit=input_unit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, xlonc, truelat1, earth_radius
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s', &
                         i1=iproj, s1=filename)
     
         end if
  
         if (startloc == 'CENTER  ') then
            starti = real(nx)/2.
            startj = real(ny)/2.
         else if (startloc == 'SWCORNER') then
            starti = 1.0
            startj = 1.0
         end if

#if (defined _GEOGRID) || (defined _METGRID)
         dx = dx * 1000.
         dy = dy * 1000.

         if (xlonc > 180.) xlonc = xlonc - 360.
         if (startlon > 180.) startlon = startlon - 360.
         
         if (startlat < -90.) startlat = -90.
         if (startlat > 90.) startlat = 90.
#endif
 
         read(unit=input_unit,err=1001,end=1001) is_wind_grid_rel
      
         allocate(slab(nx, ny))
         read(unit=input_unit,err=1001,end=1001) slab
      
         istatus = 0

      else
         call mprintf(.true.,ERROR,'Didn''t recognize format of data in %s.', s1=filename)
      end if
  
      return
 
   1001 return
 
   end subroutine read_next_met_field
 
 
   subroutine read_met_close()
 
      implicit none
  
      close(unit=input_unit)
      filename = 'UNINITIALIZED_FILENAME'
  
   end subroutine read_met_close

end module read_met_module
