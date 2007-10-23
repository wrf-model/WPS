module coefficients

   integer :: n_levels
   real, allocatable, dimension(:) :: a, b

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: read_coeffs
   !
   ! Notes: Obtain table of coefficients for input by this routine from
   !        http://www.ecmwf.int/products/data/technical/model_levels/index.html
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

      read(21,*,iostat=istatus) nlvl
      do while (istatus == 0)
         n_levels = n_levels + 1
         read(21,*,iostat=istatus) nlvl
      end do
      
      rewind(21)

      n_levels = n_levels - 1

      allocate(a(0:n_levels))
      allocate(b(0:n_levels))

      write(6,*) ' '
      write(6,*) 'Coefficients for each level:',n_levels
      do i=0,n_levels
         read(21,*,iostat=istatus) nlvl, a(i), b(i)
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

   use coefficients
   use date_pack
   use gridinfo_module
   use misc_definitions_module
   use module_debug
   use read_met_module
   use stringutil
   use write_met_module

   implicit none

   ! Local variables
   integer :: i, idiff, n_times, t, istatus, fg_idx
   real :: a_full, b_full
   character (len=19) :: valid_date, temp_date
   character (len=128) :: input_name
   real, allocatable, dimension(:,:) :: psfc
   real, allocatable, dimension(:,:,:) :: tt, qv
   logical :: is_psfc
   type (met_data) :: ecmwf_data, p_data, rh_data


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

         is_psfc = .false.

         if (istatus == 0) then
            call mprintf(.true.,STDOUT,'Reading from %s at time %s', s1=input_name, s2=temp_date(1:13))

            ! Process all fields and levels from the current file; read_next_met_field()
            !   will return a non-zero status when there are no more fields to be read.
            do while (istatus == 0)

               call read_next_met_field(ecmwf_data, istatus)

               if (istatus == 0) then

                  ! Have we found either PSFC or LOGSFP?
                  if ((trim(ecmwf_data%field) == 'PSFC' .and. ecmwf_data%xlvl == 200100.) &
                      .or. trim(ecmwf_data%field) == 'LOGSFP') then
                     p_data = ecmwf_data
                     p_data%field = 'PRESSURE '
                     p_data%desc  = 'Pressure'

                     rh_data = ecmwf_data
                     rh_data%field = 'RH       '
                     rh_data%units = '%'
                     rh_data%desc  = 'Relative humidity'

                     if (.not. allocated(psfc)) then
                        allocate(psfc(ecmwf_data%nx,ecmwf_data%ny))
                     end if
                     call mprintf(.true.,STDOUT,'Found %s field in %s:%s', &
                                  s1=ecmwf_data%field, s2=input_name, s3=temp_date(1:13))

                     is_psfc   = .true.
                     if (trim(ecmwf_data%field) == 'LOGSFP') then
                        psfc(:,:) = exp(ecmwf_data%slab(:,:))
                     else
                        psfc(:,:) = ecmwf_data%slab(:,:)
                     end if

                  ! Have we found temperature?
                  else if (trim(ecmwf_data%field) == 'TT') then

                     if (.not. allocated(tt)) then
                        allocate(tt(ecmwf_data%nx,ecmwf_data%ny,n_levels+1))  ! Extra level is for surface
                     end if

                     if (nint(ecmwf_data%xlvl) >= 1 .and. &
                         nint(ecmwf_data%xlvl) <= n_levels) then 
                        tt(:,:,nint(ecmwf_data%xlvl)) = ecmwf_data%slab
                     else if (nint(ecmwf_data%xlvl) == 200100) then
                        tt(:,:,n_levels+1) = ecmwf_data%slab
                     end if

                  ! Have we found specific humidity?
                  else if (trim(ecmwf_data%field) == 'SPECHUMD') then

                     if (.not. allocated(qv)) then
                        allocate(qv(ecmwf_data%nx,ecmwf_data%ny,n_levels+1))  ! Extra level is for surface
                     end if

                     if (nint(ecmwf_data%xlvl) >= 1 .and. &
                         nint(ecmwf_data%xlvl) <= n_levels) then 
                        qv(:,:,nint(ecmwf_data%xlvl)) = ecmwf_data%slab
                     else if (nint(ecmwf_data%xlvl) == 200100) then
                        qv(:,:,n_levels+1) = ecmwf_data%slab
                     end if

                  end if
                  
                  if (associated(ecmwf_data%slab)) deallocate(ecmwf_data%slab)
   
               end if
   
            end do
   
            call read_met_close()


            ! Now write out, for each level, the pressure field
            if (is_psfc) then

               allocate(p_data%slab(p_data%nx,p_data%ny))
               allocate(rh_data%slab(rh_data%nx,rh_data%ny))

               call write_met_init(trim(get_path(input_name))//'PRES', .false., temp_date(1:13), istatus)

               if (allocated(tt) .and. allocated(qv)) then
                  p_data%xlvl  = 200100.
                  p_data%slab  = psfc
! Surface RH should be computed from surface DEWPT by ungrib
!                  rh_data%xlvl = 200100.
!                  call calc_rh(tt(:,:,n_levels+1), qv(:,:,n_levels+1), psfc, rh_data%slab, rh_data%nx, rh_data%ny)
!                  call write_next_met_field(rh_data, istatus) 
                  call write_next_met_field(p_data, istatus) 
               else
                  call mprintf(.true.,WARN,'Either TT or SPECHUMD not found. No RH will be computed.')
               end if

               do i = 1, n_levels

                  a_full = 0.5 * (a(i-1) + a(i))   ! A and B are dimensioned (0:n_levels)
                  b_full = 0.5 * (b(i-1) + b(i))

                  p_data%xlvl = real(i)
                  p_data%slab = a_full + psfc * b_full

                  if (allocated(tt) .and. allocated(qv)) then
                     rh_data%xlvl = real(i)
                     call calc_rh(tt(:,:,i), qv(:,:,i), p_data%slab, rh_data%slab, rh_data%nx, rh_data%ny)
                     call write_next_met_field(rh_data, istatus) 
                  end if

                  call write_next_met_field(p_data, istatus) 

               end do

               call write_met_close()
  
               deallocate(p_data%slab)
               deallocate(rh_data%slab)

            end if

            if (allocated(tt))   deallocate(tt)
            if (allocated(qv))   deallocate(qv)
            if (allocated(psfc)) deallocate(psfc)

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


subroutine calc_rh(t, qv, p, rh, nx, ny)

   implicit none

   ! Arguments
   integer, intent(in) :: nx, ny
   real, dimension(nx, ny), intent(in)  :: t, qv, p
   real, dimension(nx, ny), intent(out) :: rh

   ! Constants
   real, parameter :: svp1=0.6112
   real, parameter :: svp2=17.67
   real, parameter :: svp3=29.65
   real, parameter :: svpt0=273.15
   real, parameter :: eps = 0.622

   ! Local variables
   integer :: i, j
   real :: es, qs

   do j=1,ny
   do i=1,nx
      es=svp1*10.*exp(svp2*(t(i,j)-svpt0)/(t(i,j)-svp3))
      qs=eps*es/(p(i,j)/100.-es)
      rh(i,j) = 100.0 * qv(i,j) / qs
   end do
   end do

end subroutine calc_rh
