!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module gridinfo_module

   use misc_definitions_module
   use module_debug
 
   ! Parameters
   integer, parameter :: MAX_DOMAINS = 21
 
   ! Variables
   integer :: iproj_type, n_domains, io_form_output, dyn_opt
   integer, dimension(MAX_DOMAINS) :: parent_grid_ratio, parent_id, ixdim, jydim
   real :: known_lat, known_lon, stand_lon, truelat1, truelat2, known_x, known_y, &
           dxkm, dykm, phi, lambda 
   real, dimension(MAX_DOMAINS) :: parent_ll_x, parent_ll_y, parent_ur_x, parent_ur_y
   character (len=128) :: geog_data_root, opt_output_from_geogrid_path, opt_geogrid_tbl_path

   character (len=128), dimension(MAX_DOMAINS) :: geog_data_res 
   character (len=1) :: gridtype
   logical :: do_tiled_output
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_grid_params
   !
   ! Purpose: This subroutine retrieves all parameters regarding the model domains
   !    to be processed by geogrid.exe. This includes map parameters, domain
   !    size and location, and nest information. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_grid_params()
 
      implicit none
  
      ! Local variables
      integer :: i, j, max_dom, funit, io_form_geogrid
      real :: dx, dy
      integer, dimension(MAX_DOMAINS) :: i_parent_start, j_parent_start, &
                           s_we, e_we, s_sn, e_sn
      integer :: start_year, start_month, start_day, start_hour, &
                 end_year,   end_month,   end_day,   end_hour, &
                 interval_seconds
      character (len=128) :: map_proj
      character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
      character (len=3) :: wrf_core
      logical :: is_used

      namelist /share/ wrf_core, max_dom, start_date, end_date, &
                        start_year, end_year, start_month, end_month, &
                        start_day, end_day, start_hour, end_hour, &
                        interval_seconds, &
                        io_form_geogrid, opt_output_from_geogrid_path
      namelist /geogrid/ parent_id, parent_grid_ratio, &
                         i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
                         map_proj, known_x, known_y, known_lat, known_lon, &
                         truelat1, truelat2, stand_lon, dx, dy, &
                         geog_data_res, geog_data_root, opt_geogrid_tbl_path
  
      ! Set defaults for namelist variables
      io_form_geogrid = -1
      wrf_core = '   '
      geog_data_root = 'NOT_SPECIFIED'
      known_x = NAN
      known_y = NAN
      do i=1,MAX_DOMAINS
         geog_data_res(i) = 'default'
      end do
      opt_output_from_geogrid_path = './'
      opt_geogrid_tbl_path = 'geogrid/'
      
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
      read(funit,share)
      read(funit,geogrid)
      close(funit)

      dxkm = dx
      dykm = dy

      ! Convert wrf_core to uppercase letters
      do i=1,3
         if (ichar(wrf_core(i:i)) >= 97) wrf_core(i:i) = char(ichar(wrf_core(i:i))-32)
      end do

      ! Before doing anything else, we must have a valid grid type 
      gridtype = ' '
      if (wrf_core == 'ARW') then
         gridtype = 'C'
         dyn_opt = 2
      else if (wrf_core == 'NMM') then
         gridtype = 'E'
         dyn_opt = 4
      end if
  
      call mprintf(gridtype /= 'C' .and. gridtype /= 'E', ERROR, &
                   'A valid wrf_core must be specified in the namelist. '// &
                   'Currently, only "ARW" and "NMM" are supported.')

      call mprintf(max_dom > MAX_DOMAINS, ERROR, &
                   'In namelist, max_dom must be <= %i. To run with more'// &
                   ' than %i domains, increase the MAX_DOMAINS parameter.', &
                   i1=MAX_DOMAINS, i2=MAX_DOMAINS)

      ! Every domain must have a valid parent id
      do i=2,max_dom
         call mprintf(parent_id(i) <= 0 .or. parent_id(i) >= i, ERROR, &
                      'In namelist, the parent_id of domain %i must be in '// &
                      'the range 1 to %i.', i1=i, i2=i-1)
      end do
  
      ! Check for valid geog_data_root
      j=1
      do i=1,len(geog_data_root)
         geog_data_root(j:j) = geog_data_root(i:i)
         if (geog_data_root(i:i) /= ' ') j = j + 1
      end do
      if (geog_data_root(1:1) == ' ') then
         call mprintf(.true.,ERROR,'In namelist, geog_data_root must be specified.')
      end if
      j = len_trim(geog_data_root)
      if (j >= 128) then
         call mprintf(.true.,ERROR, &
                      'In namelist, geog_data_root must be strictly less '// &
                      'than 128 characters in length.')
      else
         if (geog_data_root(j:j) /= '/') then
            geog_data_root(j+1:j+1) = '/'
         end if
      end if

      ! Paths need to end with a /
      j = len_trim(opt_geogrid_tbl_path)
      if (opt_geogrid_tbl_path(j:j) /= '/') then
         opt_geogrid_tbl_path(j+1:j+1) = '/'
      end if

      j = len_trim(opt_output_from_geogrid_path)
      if (opt_output_from_geogrid_path(j:j) /= '/') then
         opt_output_from_geogrid_path(j+1:j+1) = '/'
      end if
  
      ! Handle IOFORM+100 to do tiled IO
      if (io_form_geogrid > 100) then
         do_tiled_output = .true.
         io_form_geogrid = io_form_geogrid - 100
      else
         do_tiled_output = .false.
      end if
  
      ! Check for valid io_form_geogrid
      if ( &
#ifdef IO_BINARY
          io_form_geogrid /= BINARY .and. &
#endif
#ifdef IO_NETCDF
          io_form_geogrid /= NETCDF .and. &
#endif
#ifdef IO_GRIB1
          io_form_geogrid /= GRIB1 .and. &
#endif
          .true. ) then
         write(6,*) ' '
         write(6,*) 'Error: No valid value for io_form_geogrid was specified in the namelist.'  
         write(6,*) '       Valid io_form_geogrid values are:'
#ifdef IO_BINARY
         write(6,*) '       ',BINARY,' (=BINARY)'
#endif
#ifdef IO_NETCDF
         write(6,*) '       ',NETCDF,' (=NETCDF)'
#endif
#ifdef IO_GRIB1
         write(6,*) '       ',GRIB1,' (=GRIB1)'
#endif
         write(6,*) ' '
         stop
      end if
      io_form_output = io_form_geogrid
  
      ! Convert map_proj to uppercase letters
      do i=1,len(map_proj)
         if (ichar(map_proj(i:i)) >= 97) map_proj(i:i) = char(ichar(map_proj(i:i))-32)
      end do
  
      ! Assign parameters to module variables
      if ((index(map_proj, 'LAMBERT') /= 0) .and. &
          (len_trim(map_proj) == len('LAMBERT'))) then
         iproj_type = PROJ_LC 
  
      else if ((index(map_proj, 'MERCATOR') /= 0) .and. &
               (len_trim(map_proj) == len('MERCATOR'))) then
         iproj_type = PROJ_MERC 
  
      else if ((index(map_proj, 'POLAR') /= 0) .and. &
               (len_trim(map_proj) == len('POLAR'))) then
         iproj_type = PROJ_PS 
  
      else if ((index(map_proj, 'ROTATED_LL') /= 0) .and. &
               (len_trim(map_proj) == len('ROTATED_LL'))) then
         iproj_type = PROJ_ROTLL 
  
      else
         call mprintf(.true.,ERROR,&
                      'In namelist, invalid map_proj specified. Valid '// &
                      'projections are "lambert", "mercator", "polar", '// &
                      'and "rotated_ll".')
      end if
  
      n_domains = max_dom
  
      do i=1,n_domains
         ixdim(i) = e_we(i) - s_we(i) + 1
         jydim(i) = e_sn(i) - s_sn(i) + 1
      end do
  
      if (gridtype == 'E') then
         phi = dykm*real(jydim(1)-1)/2.
         lambda = dxkm*real(ixdim(1)-1)
      end if

      ! If the user hasn't supplied a known_x and known_y, assume the center of domain 1
      if (known_x == NAN) known_x = ixdim(1) / 2.
      if (known_y == NAN) known_y = jydim(1) / 2.
  
      ! Checks specific to E grid
      if (gridtype == 'E') then
  
         ! E grid supports only the rotated lat/lon projection
         if (iproj_type /= PROJ_ROTLL) then
            call mprintf(.true., WARN, &
                         'For the NMM core, projection type must be rotated '// &
                         'lat/lon (map_proj=rotated_ll)')
            call mprintf(.true.,WARN,'Projection will be set to rotated_ll')
            iproj_type = PROJ_ROTLL
         end if
   
         ! Nesting is not currently supported in E grid
         call mprintf((n_domains > 1), ERROR, &
                      'Nesting for NMM core is not currently supported by '// &
                      'this program. Only a single domain must be specified '// &
                      'in the namelist.')
   
         do i=1,n_domains
            call mprintf(mod(jydim(i),2) /= 1, ERROR, &
                         'For the NMM core, the number of rows must be odd for grid %i.', i1=i)
         end do
   
      ! Checks specific to C grid
      else if (gridtype == 'C') then
  
         ! C grid does not support the rotated lat/lon projection
         call mprintf((iproj_type == PROJ_ROTLL), ERROR, &
                      'Rotated lat/lon projection is not supported for the ARW core. '// &
                      'Valid projecitons are "lambert", "mercator", and "polar".')
      end if
  
      do i=1,n_domains
         parent_ll_x(i) = real(i_parent_start(i))
         parent_ll_y(i) = real(j_parent_start(i))
         parent_ur_x(i) = real(i_parent_start(i))+real(ixdim(i))/real(parent_grid_ratio(i))-1.
         parent_ur_y(i) = real(j_parent_start(i))+real(jydim(i))/real(parent_grid_ratio(i))-1.
      end do
  
      return
  
 1000 call mprintf(.true.,ERROR,'Error opening file namelist.wps')
 
   end subroutine get_grid_params
  
end module gridinfo_module
