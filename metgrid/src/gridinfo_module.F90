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
   integer :: interval_seconds, max_dom, io_form_input, io_form_output
   character (len=128) :: opt_output_from_geogrid_path, &
                          opt_output_from_metgrid_path, opt_metgrid_tbl_path 
   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date, fg_name, &
                          constants_name
   logical :: do_tiled_input, do_tiled_output, opt_ignore_dom_center, debug_print
   character (len=1) :: gridtype
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_namelist_params
   !
   ! Purpose: Read namelist parameters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_namelist_params()
 
      implicit none
  
      ! Local variables
      integer :: i, io_form_geogrid, io_form_metgrid
      integer, dimension(MAX_DOMAINS) :: start_year, start_month, start_day, start_hour, &
                                         end_year, end_month, end_day, end_hour 
      integer :: funit
      logical :: is_used
      character (len=3) :: wrf_core
      namelist /share/ wrf_core, max_dom, start_date, end_date, &
                        start_year, end_year, start_month, end_month, &
                        start_day, end_day, start_hour, end_hour, &
                        interval_seconds, &
                        io_form_geogrid, opt_output_from_geogrid_path, debug_print
      namelist /metgrid/ io_form_metgrid, fg_name, constants_name, opt_output_from_metgrid_path, &
                         opt_metgrid_tbl_path, opt_ignore_dom_center 
        
      ! Set defaults
      io_form_geogrid = -1
      io_form_metgrid = -1
      max_dom = -1
      wrf_core = '   '
      do i=1,MAX_DOMAINS
         fg_name(i) = '*'
         constants_name(i) = '*'
         start_date(i) = '0000-00-00_00:00:00'
         end_date(i) = '0000-00-00_00:00:00'
      end do
      opt_output_from_geogrid_path = './'
      opt_output_from_metgrid_path = './'
      opt_metgrid_tbl_path = 'metgrid/'
      opt_ignore_dom_center = .false.
      start_year = 0
      start_month = 0
      start_day = 0
      start_hour = 0
      end_year = 0
      end_month = 0
      end_day = 0
      end_hour = 0
      interval_seconds = 86400
  
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
      read(funit,share)
      read(funit,metgrid)
      close(funit)

      ! Convert wrf_core to uppercase letters
      do i=1,3
         if (ichar(wrf_core(i:i)) >= 97) wrf_core(i:i) = char(ichar(wrf_core(i:i))-32)
      end do

      ! Before doing anything else, we must have a valid grid type 
      gridtype = ' '
      if (wrf_core == 'ARW') then
         gridtype = 'C'
      else if (wrf_core == 'NMM') then
         gridtype = 'E'
      end if

      call mprintf(gridtype /= 'C' .and. gridtype /= 'E', ERROR, &
                   'A valid wrf_core must be specified in the namelist. '// &
                   'Currently, only "ARW" and "NMM" are supported.')

      call mprintf(max_dom > MAX_DOMAINS, ERROR, &
                   'In namelist, max_dom must be <= %i. To run with more'// &
                   ' than %i domains, increase the MAX_DOMAINS parameter.', &
                   i1=MAX_DOMAINS, i2=MAX_DOMAINS)
  
      ! Handle IO_FORM+100
      if (io_form_geogrid > 100) then
         io_form_geogrid = io_form_geogrid - 100
         do_tiled_input = .true.
      else
         do_tiled_input = .false.
      end if
      if (io_form_metgrid > 100) then
         io_form_metgrid = io_form_metgrid - 100
         do_tiled_output = .true.
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
      io_form_input = io_form_geogrid
  
      ! Check for valid io_form_metgrid
      if ( &
#ifdef IO_BINARY
          io_form_metgrid /= BINARY .and. &
#endif
#ifdef IO_NETCDF
          io_form_metgrid /= NETCDF .and. &
#endif
#ifdef IO_GRIB1
          io_form_metgrid /= GRIB1 .and. &
#endif
          .true. ) then
         write(6,*) ' '
         write(6,*) 'Error: No valid value for io_form_metgrid was specified in the namelist.'
         write(6,*) '       Valid io_form_metgrid values are:'
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
      io_form_output = io_form_metgrid
  
      if (start_date(1) == '0000-00-00_00:00:00') then
         do i=1,max_dom
            ! Build starting date string
            write(start_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a6)') &
               start_year(i),'-',start_month(i),'-',start_day(i),' ',start_hour(i),':00:00'
     
            ! Build ending date string
            write(end_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a6)') &
               end_year(i),'-',end_month(i),'-',end_day(i),' ',end_hour(i),':00:00'
         end do
      end if
  
      ! Paths need to end with a /
      i = len_trim(opt_metgrid_tbl_path)
      if (opt_metgrid_tbl_path(i:i) /= '/') then
         opt_metgrid_tbl_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_geogrid_path)
      if (opt_output_from_geogrid_path(i:i) /= '/') then
         opt_output_from_geogrid_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_metgrid_path)
      if (opt_output_from_metgrid_path(i:i) /= '/') then
         opt_output_from_metgrid_path(i+1:i+1) = '/'
      end if
  
      return
  
 1000 call mprintf(.true.,ERROR,'Error opening file namelist.wps')
 
   end subroutine get_namelist_params
  
end module gridinfo_module
