module write_met_module

   use module_debug
   use misc_definitions_module

   ! State variables?
   integer :: output_unit
   character (len=128) :: met_out_filename
 
   contains
 
   subroutine write_met_init(fg_source, source_is_constant, datestr, istatus)
 
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
      met_out_filename = ' '
      if (.not. source_is_constant) then 
         write(met_out_filename, '(a)') trim(fg_source)//':'//trim(datestr)
      else
         write(met_out_filename, '(a)') trim(fg_source)
      end if
  
      !  2) OPEN FILE
      do output_unit=10,100
         inquire(unit=output_unit, opened=is_used)
         if (.not. is_used) exit
      end do 
      call mprintf((output_unit > 100),ERROR,'In write_met_init(), couldn''t find an available Fortran unit.')
      open(unit=output_unit, file=trim(met_out_filename), status='unknown', form='unformatted', iostat=io_status)

      if (io_status > 0) istatus = 1

      return
  
 
   end subroutine write_met_init
 
 
   subroutine write_next_met_field(version, field, hdate, xfcst, xlvl, units, desc, &
                          iproj, startlat, startlon, starti, startj, deltalat, deltalon, &
                          dx, dy, xlonc, truelat1, truelat2, earth_radius, nx, ny, map_source, &
                          slab, is_wind_grid_rel, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: version, nx, ny, iproj
      integer, intent(out) :: istatus
      real, intent(in) :: xfcst, xlvl, startlat, startlon, starti, startj, &
                           deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, earth_radius
      real, dimension(nx,ny) :: slab
      logical, intent(in) :: is_wind_grid_rel
      character (len=9), intent(in) :: field
      character (len=24), intent(in) :: hdate
      character (len=25), intent(in) :: units
      character (len=32), intent(in) :: map_source
      character (len=46), intent(in) :: desc
  
      ! Local variables
      real :: local_dx, local_dy
      character (len=8) :: startloc
      character (len=9) :: local_field
  
      istatus = 1
  
      !  1) WRITE FORMAT VERSION
      write(unit=output_unit) version

      local_field = field
      if (field == 'GHT      ') local_field = 'HGT      '

#if (defined _GEOGRID) || (defined _METGRID)
      local_dx = dx / 1000.
      local_dy = dy / 1000.
#endif

      ! PREGRID
      if (version == 3) then

         ! Cylindrical equidistant
         if (iproj == PROJ_LATLON) then
            write(unit=output_unit) hdate, xfcst, local_field, units, desc, xlvl, nx, ny, 0
            write(unit=output_unit) startlat, startlon, deltalat, deltalon
     
         ! Mercator
         else if (iproj == PROJ_MERC) then
            write(unit=output_unit) hdate, xfcst, local_field, units, desc, xlvl, nx, ny, 1
            write(unit=output_unit) startlat, startlon, dx, dy, truelat1
     
         ! Lambert conformal
         else if (iproj == PROJ_LC) then
            write(unit=output_unit) hdate, xfcst, local_field, units, desc, xlvl, nx, ny, 3
            write(unit=output_unit) startlat, startlon, dx, dy, xlonc, truelat1, truelat2
     
         ! Polar stereographic
         else if (iproj == PROJ_PS) then
            write(unit=output_unit) hdate, xfcst, local_field, units, desc, xlvl, nx, ny, 5
            write(unit=output_unit) startlat, startlon, dx, dy, xlonc, truelat1

         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', i1=iproj,s1=met_out_filename)
     
         end if
     
         write(unit=output_unit) slab
     
         istatus = 0 
    
      ! GRIB_PREP
      else if (version == 4) then

         if (starti == 1.0 .and. startj == 1.0) then
            startloc='SWCORNER'
         else
            startloc='CENTER  '
         end if

#if (defined _GEOGRID) || (defined _METGRID)
      local_dx = dx / 1000.
      local_dy = dy / 1000.
#endif
  
         ! Cylindrical equidistant
         if (iproj == PROJ_LATLON) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 0
            write(unit=output_unit) startloc, startlat, startlon, deltalat, deltalon

         ! Mercator
         else if (iproj == PROJ_MERC) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 1
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, truelat1

         ! Lambert conformal
         else if (iproj == PROJ_LC) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 3
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, xlonc, truelat1, truelat2

         ! Polar stereographic
         else if (iproj == PROJ_PS) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 5
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, xlonc, truelat1
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', i1=iproj,s1=met_out_filename)
     
         end if
  
         write(unit=output_unit) slab
      
         istatus = 0

      ! WPS
      else if (version == 5) then

         if (starti == 1.0 .and. startj == 1.0) then
            startloc='SWCORNER'
         else
            startloc='CENTER  '
         end if

#if (defined _GEOGRID) || (defined _METGRID)
      local_dx = dx / 1000.
      local_dy = dy / 1000.
#endif
  
         ! Cylindrical equidistant
         if (iproj == PROJ_LATLON) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 0
            write(unit=output_unit) startloc, startlat, startlon, deltalat, deltalon, earth_radius

         ! Mercator
         else if (iproj == PROJ_MERC) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 1
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, truelat1, earth_radius

         ! Lambert conformal
         else if (iproj == PROJ_LC) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 3
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, xlonc, truelat1, truelat2, earth_radius

         ! Gaussian
         else if (iproj == PROJ_GAUSS) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 4
            write(unit=output_unit) startloc, startlat, startlon, deltalat, deltalon, earth_radius

         ! Polar stereographic
         else if (iproj == PROJ_PS) then
            write(unit=output_unit) hdate, xfcst, map_source, local_field, units, desc, xlvl, nx, ny, 5
            write(unit=output_unit) startloc, startlat, startlon, dx, dy, xlonc, truelat1, earth_radius
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', i1=iproj,s1=met_out_filename)
     
         end if
  
         write(unit=output_unit) is_wind_grid_rel

         write(unit=output_unit) slab
      
         istatus = 0

      else
         call mprintf(.true.,ERROR,'Didn''t recognize format number %i.', i1=version)
      end if
  
      return
 
   end subroutine write_next_met_field
 
 
   subroutine write_met_close()
 
      implicit none
  
      close(unit=output_unit)
      met_out_filename = 'UNINITIALIZED_FILENAME'
  
   end subroutine write_met_close

end module write_met_module
