module coefficients

   integer :: n_levels
   real, allocatable, dimension(:) :: a, b

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: read_coeffs
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine read_coeffs()
      
      implicit none

      integer :: i, nlvl, istatus

      open(21,file='ecmwf_coeffs',form='formatted',status='old',iostat=istatus)
  
      n_levels = 0

      if (istatus /= 0) then
         write(6,*) 'ERROR: Error opening ecmwf_coeffs' 
         return
      end if

      read(21,'(i5)',iostat=istatus) nlvl
      do while (istatus == 0)
         n_levels = n_levels + 1
         read(21,'(i5)',iostat=istatus) nlvl
      end do
      
      rewind(21)

      allocate(a(n_levels))
      allocate(b(n_levels))

      write(6,*) ' '
      write(6,*) 'Coefficients for each level:',n_levels
      do i=1,n_levels
         read(21,'(i5,5x,f12.6,2x,f12.10)',iostat=istatus) nlvl, a(i), b(i)
         write(6,'(i5,5x,f12.6,2x,f12.10)') nlvl, a(i), b(i)
      end do
      write(6,*) ' '

      close(21)
      
   end subroutine read_coeffs


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: cleanup_coeffs
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine cleanup_coeffs()

      implicit none

      n_levels = 0
      if (allocated(a)) deallocate(a)
      if (allocated(b)) deallocate(b)

   end subroutine cleanup_coeffs

end module coefficients


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calc_ecmwf_p
!
! The purpose of this program is to compute a 3d pressure field for ECMWF 
!    model-level data sets; the code works in the WPS intermediate file format, 
!    reading a PSFC field from intermediate files, the A and B coefficients 
!    from a text file, ecmwf_coeffs, and writes the pressure data to an 
!    intermediate file.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program calc_ecmwf_p

   use date_pack
   use gridinfo_module
   use read_met_module
   use write_met_module
   use misc_definitions_module
   use module_debug
   use coefficients

   implicit none

   ! Local variables
   integer :: i, idiff, n_times, t, istatus, fg_idx
   character (len=19) :: valid_date, temp_date
   character (len=128) :: input_name
   type (met_data) :: psfc_data, p_data


   !
   ! Setup (read namelist and check on time range)
   !
   call get_namelist_params()

   call set_debug_level(WARN)

   ! Compute number of times that we will process
   call geth_idts(end_date(1), start_date(1), idiff)
   call mprintf((idiff < 0),ERROR,'Ending date is earlier than starting date in namelist for domain %i.', i1=1)

   n_times = idiff / interval_seconds

   ! Check that the interval evenly divides the range of times to process
   call mprintf((mod(idiff, interval_seconds) /= 0),WARN, &
                'In namelist, interval_seconds does not evenly divide '// &
                '(end_date - start_date) for domain %i. Only %i time periods '// &
                'will be processed.', i1=1, i2=n_times)

   fg_idx = 1

   input_name = fg_name(fg_idx)

   !
   ! Get coefficients for model level pressures
   !
   call read_coeffs()


   !
   ! Loop over all prefixes listed in namelist for fg_name
   !
   do while (input_name /= '*')

      !
      ! Loop over all times to be processed for this domain
      !
      do t=0,n_times

         ! Get the date string for the current time
         call geth_newdate(valid_date, trim(start_date(1)), t*interval_seconds)
         temp_date = ' '
         write(temp_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)

         ! Initialize the module for reading in the met fields
         call read_met_init(trim(input_name), .false., temp_date(1:13), istatus)

         if (istatus == 0) then
            call mprintf(.true.,STDOUT,'Reading from %s at time %s', s1=input_name, s2=temp_date(1:13))

            ! Process all fields and levels from the current file; read_next_met_field()
            !   will return a non-zero status when there are no more fields to be read.
            do while (istatus == 0)

               call read_next_met_field(psfc_data, istatus)

               if (istatus == 0) then

                  ! If we have found the PSFC field, we can quit reading from this file
                  if (trim(psfc_data%field) == 'PSFC' .and. psfc_data%xlvl == 200100.) then
                     p_data = psfc_data
                     p_data%field = 'PRES'
                     call mprintf(.true.,STDOUT,'Found PSFC field in %s:%s',s1=input_name,s2=temp_date(1:13))
                     exit
                  end if
                  
                  if (associated(psfc_data%slab)) deallocate(psfc_data%slab)
   
               end if
   
            end do
   
            call read_met_close()

            ! Now write out, for each level, the pressure field
            if (trim(psfc_data%field) == 'PSFC') then

               allocate(p_data%slab(p_data%nx,p_data%ny))

               call write_met_init('PRES', .false., temp_date(1:13), istatus)

               do i = 1, n_levels

                  p_data%xlvl = real(i)
                  p_data%slab = a(i) + psfc_data%slab * b(i)

                  call write_next_met_field(p_data, istatus) 

               end do

               call write_met_close()
  
               deallocate(p_data%slab)

            end if

         else
            call mprintf(.true.,ERROR,'Problem opening %s at time %s', s1=input_name, s2=temp_date(1:13))
         end if

      end do 

      fg_idx = fg_idx + 1
      input_name = fg_name(fg_idx)

   end do 

   call cleanup_coeffs()

   stop

end program calc_ecmwf_p
