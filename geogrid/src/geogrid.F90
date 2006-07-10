!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Program: geogrid
!
! Written by Michael G. Duda
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program geogrid

   use gridinfo_module
   use llxy_module
   use module_debug
   use parallel_module
   use process_tile_module
   use source_data_module

   implicit none

   ! Local variables
   integer :: i
   logical :: ew_extra_col, sn_extra_row

   ! Prepare anything necessary to do parallel processing of domains 
   ! The parallel module should be initialized before any other calls take place
   call parallel_start()

   call mprintf(.true.,LOGFILE,' *** Starting program geogrid.exe *** ')
  
   ! Have the gridinfo module retrieve description of the grid setup
   call get_grid_params()

   ! Get information about the source data to be processed
   call get_datalist()

   ! Tell the llxy module that it can now compute parameters necessary to do 
   !   transformations for any nest 
   call compute_nest_locations()

   ! Process all requested domains 
   do i=1,n_domains
      call mprintf(.true.,STDOUT,'Processing domain %i of %i', i1=i, i2=n_domains)
      call mprintf(.true.,LOGFILE,'Processing domain %i of %i', i1=i, i2=n_domains)
  
      ! Get information about the source data we will use for this nest
      call get_source_params(geog_data_res(i))
  
      ! Set transformations in llxy module to be with respect to current nest
      call select_domain(i)
 
      ! Determine which range of indices we will work on
      call parallel_get_tile_dims(ixdim(i), jydim(i))
  
      if (gridtype == 'C') then
         if (my_x == nproc_x-1) then ! One more column for U points
            ew_extra_col = .true.
         else
            ew_extra_col = .false.
         end if
  
         if (my_y == nproc_y-1) then ! One more row for V points
            sn_extra_row = .true.
         else
            sn_extra_row = .false.
         end if
  
      else if (gridtype == 'E') then
         sn_extra_row = .false.  
         ew_extra_col = .false.  

      end if
  
      ! Process fields for a tile of the current nest
      call process_tile(i, gridtype, dyn_opt, &
                        1,       ixdim(i), 1,       jydim(i), &
                        my_minx, my_maxx,  my_miny, my_maxy, &   ! These come from parallel_module
                        ew_extra_col, sn_extra_row)
   end do

   ! Free up memory used by list of source data to be processed
   call datalist_destroy()
 
   ! Clean up parallel stuff
   call parallel_finish()
 
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   call mprintf(.true.,STDOUT,'!  Successful completion of geogrid.        !')
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')

   call mprintf(.true.,LOGFILE,' *** Successful completion of program geogrid.exe *** ')
 
   stop

end program geogrid
