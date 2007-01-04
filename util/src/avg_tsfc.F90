program avg_tsfc

   use date_pack
   use gridinfo_module
   use read_met_module
   use write_met_module
   use misc_definitions_module
   use module_debug

   implicit none

   ! Local variables
   integer :: idiff, n_times, t, istatus, fg_idx, discardtimes
   integer :: met_map_proj, version, nx, ny, t_nx, t_ny, t_met_map_proj
   real :: xfcst, xlvl, startlat, startlon, starti, startj, deltalat, deltalon, earth_radius
   real :: t_startlat, t_startlon, t_starti, t_startj, t_deltalat, t_deltalon, t_earth_radius
   real :: met_dx, met_dy
   real :: t_met_dx, t_met_dy
   real :: met_cen_lon, met_truelat1, met_truelat2
   real :: t_met_cen_lon, t_met_truelat1, t_met_truelat2
   real, pointer, dimension(:,:) :: slab
   real, pointer, dimension(:,:) :: mean
   logical :: is_rotated
   character (len=9) :: short_fieldnm
   character (len=19) :: valid_date, temp_date
   character (len=24) :: hdate
   character (len=25) :: units, t_units
   character (len=32) :: map_src, t_map_src
   character (len=46) :: desc, t_desc
   character (len=128) :: input_name

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

   discardtimes = mod(idiff+interval_seconds,86400) / interval_seconds

   do while (input_name /= '*')

      ! Loop over all times to be processed for this domain
      do t=0,n_times-discardtimes

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


               call read_next_met_field(version, short_fieldnm, hdate, xfcst, xlvl, units, desc, &
                                   met_map_proj, startlat, startlon, starti, startj, deltalat, &
                                   deltalon, met_dx, met_dy, met_cen_lon, met_truelat1, met_truelat2, &
                                   earth_radius, nx, ny, &
                                   map_src, slab, is_rotated, istatus)

               if (istatus == 0) then

                  if (trim(short_fieldnm) == 'TT' .and. xlvl == 200100.) then
                     t_units = units
                     t_desc  = desc
                     if (.not. associated(mean)) then
                        t_nx = nx
                        t_ny = ny
                        t_startlat = startlat
                        t_startlon = startlon
                        t_starti = starti
                        t_startj = startj
                        t_deltalat = deltalat
                        t_deltalon = deltalon
                        t_met_dx = met_dx
                        t_met_dy = met_dy
                        t_met_dy = met_dy
                        t_met_map_proj = met_map_proj
                        t_met_cen_lon = met_cen_lon
                        t_met_truelat1 = met_truelat1
                        t_met_truelat2 = met_truelat2
                        t_earth_radius = earth_radius
                        t_map_src = map_src
                        allocate(mean(nx,ny))
                        mean = 0.
                     end if

! BUG: Should check here whether projection matches from previous read of the TT field
                     if (t_nx /= nx .or. t_ny /= ny) &
                        call mprintf(.true.,ERROR,'Mismatch in Tsfc field dimensions in file %s', &
                                     s1=trim(input_name)//':'//temp_date(1:13))
                      
                     mean = mean + slab
                  end if
   
                  if (associated(slab)) deallocate(slab)
   
               end if
   
            end do
   
            call read_met_close()

         else
            call mprintf(.true.,ERROR,'Problem opening %s at time %s', s1=input_name, s2=temp_date(1:13))
         end if

      end do 

      if (associated(mean)) then
         mean = mean /real(n_times-discardtimes+1)

         call write_met_init('TAVGSFC', .true., temp_date(1:13), istatus)
         hdate = '0000-00-00_00:00:00     '
         xfcst = 0.
         call write_next_met_field(version, 'TAVGSFC  ', hdate, xfcst, 200100., t_units, t_desc, &
                             t_met_map_proj, t_startlat, t_startlon, t_starti, t_startj, t_deltalat, &
                             t_deltalon, t_met_dx, t_met_dy, t_met_cen_lon, t_met_truelat1, t_met_truelat2, &
                             t_earth_radius, t_nx, t_ny, &
                             t_map_src, mean, is_rotated, istatus)
         call write_met_close()
  
         deallocate(mean)
      end if

      fg_idx = fg_idx + 1
      input_name = fg_name(fg_idx)

   end do 

   stop

end program avg_tsfc
