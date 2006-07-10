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
   character (len=128) :: opt_output_from_gridgen_path, &
                          opt_output_from_hinterp_path, opt_hinterp_tbl_path 
   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date, fg_name, &
                          constants_name
   logical :: do_tiled_input, do_tiled_output
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_namelist_params
   !
   ! Purpose: Read namelist parameters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_namelist_params()
 
      implicit none
  
      ! Local variables
      integer :: i
      integer, dimension(MAX_DOMAINS) :: start_year, start_month, start_day, start_hour, &
                                         end_year, end_month, end_day, end_hour 
      integer :: funit
      logical :: is_used
      namelist /hinterp/ max_dom, start_date, start_year, start_month, start_day, start_hour, &
                         end_date, end_year, end_month, end_day, end_hour, interval_seconds, &
                         io_form_input, io_form_output, fg_name, constants_name, &
                         opt_output_from_gridgen_path, opt_output_from_hinterp_path, &
                         opt_hinterp_tbl_path 
        
      ! Set defaults
      io_form_input = -1
      io_form_output = -1
      do i=1,MAX_DOMAINS
         fg_name(i) = '*'
         constants_name(i) = '*'
         start_date(i) = '0000-00-00_00:00:00'
         end_date(i) = '0000-00-00_00:00:00'
      end do
      opt_output_from_gridgen_path = './'
      opt_output_from_hinterp_path = './'
      opt_hinterp_tbl_path = './'
  
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.wrfsi',status='old',form='formatted',err=1000)
      read(funit,hinterp)
      close(funit)
  
      ! Handle IO_FORM+100
      if (io_form_input > 100) then
         io_form_input = io_form_input - 100
         do_tiled_input = .true.
      else
         do_tiled_input = .false.
      end if
      if (io_form_output > 100) then
         io_form_output = io_form_output - 100
         do_tiled_output = .true.
      else
         do_tiled_output = .false.
      end if
  
      ! Check for valid io_form_input
      if ( &
#ifdef IO_BINARY
          io_form_input /= BINARY .and. & 
#endif
#ifdef IO_NETCDF
          io_form_input /= NETCDF .and. & 
#endif
#ifdef IO_GRIB1
          io_form_input /= GRIB1 .and. & 
#endif
          .true. ) then
         write(6,*) ' '
         write(6,*) 'Error: No valid value for io_form_input was specified in the namelist.'
         write(6,*) '       Valid io_form_input values are:'
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
  
      ! Check for valid io_form_output
      if ( &
#ifdef IO_BINARY
          io_form_output /= BINARY .and. &
#endif
#ifdef IO_NETCDF
          io_form_output /= NETCDF .and. &
#endif
#ifdef IO_GRIB1
          io_form_output /= GRIB1 .and. &
#endif
          .true. ) then
         write(6,*) ' '
         write(6,*) 'Error: No valid value for io_form_output was specified in the namelist.'
         write(6,*) '       Valid io_form_output values are:'
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
      i = len_trim(opt_hinterp_tbl_path)
      if (opt_hinterp_tbl_path(i:i) /= '/') then
         opt_hinterp_tbl_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_gridgen_path)
      if (opt_output_from_gridgen_path(i:i) /= '/') then
         opt_output_from_gridgen_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_hinterp_path)
      if (opt_output_from_hinterp_path(i:i) /= '/') then
         opt_output_from_hinterp_path(i+1:i+1) = '/'
      end if
  
      return
  
 1000 call mprintf(.true.,ERROR,'Error opening file namelist.wrfsi')
 
   end subroutine get_namelist_params
  
end module gridinfo_module
