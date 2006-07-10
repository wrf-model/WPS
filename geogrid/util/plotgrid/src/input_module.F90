module input_module

   use gridinfo_module
   use misc_definitions_module
   use module_debug
#ifdef IO_BINARY
   use module_internal_header_util
#endif
   use parallel_module
   use queue_module
 
   type (queue) :: unit_desc
 
   ! WRF I/O API related variables
   integer :: handle
 
   integer :: num_calls
 
   character (len=1) :: internal_gridtype
 
   contains
 
 
   subroutine input_init(nest_number, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: nest_number
      integer, intent(out) :: istatus
  
      include 'wrf_io_flags.h'
      include 'wrf_status_codes.h'
  
      ! Local variables
      integer :: i, ipx, ipy
      integer :: comm_1, comm_2
      character (len=128) :: coption, input_fname
      logical :: supports_training, supports_3d_fields
  
      istatus = 0
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
#ifdef IO_BINARY
         if (io_form_input == BINARY) call ext_int_ioinit('sysdep info', istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) call ext_ncd_ioinit('sysdep info', istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) call ext_gr1_ioinit('sysdep info', istatus)
#endif
         call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')
     
         comm_1 = 1
         comm_2 = 1
         input_fname = ' '
#ifdef IO_BINARY
         if (io_form_input == BINARY) input_fname = trim(opt_output_from_gridgen_path)//'domain  .int'
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) input_fname = trim(opt_output_from_gridgen_path)//'domain  .nc'
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) input_fname = trim(opt_output_from_gridgen_path)//'domain  .grib'
#endif
         i = len_trim(opt_output_from_gridgen_path)
         write(input_fname(i+7:i+8),'(i2.2)') nest_number
         if (nprocs > 1 .and. do_tiled_input) then
            write(input_fname(len_trim(input_fname)+1:len_trim(input_fname)+6), '(a2,i4.4)') &
                            '.p', my_proc_id
         end if
     
         istatus = 0
#ifdef IO_BINARY
         if (io_form_input == BINARY) &
            call ext_int_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) &
            call ext_ncd_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) &
            call ext_gr1_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
         call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_open_for_read')
     
         call q_init(unit_desc)
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)
  
      num_calls = 0
 
   end subroutine input_init
 
 
   subroutine read_next_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                          start_mem_k, end_mem_k, cname, cunits, cdesc, memorder, &
                          stagger, dimnames, real_array, int_array, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: start_mem_i, end_mem_i, start_mem_j, end_mem_j, start_mem_k, end_mem_k
      real, pointer, dimension(:,:,:) :: real_array
      integer, pointer, dimension(:,:,:) :: int_array
      character (len=*), intent(out) :: cname, memorder, stagger, cunits, cdesc
      character (len=128), dimension(3) :: dimnames
      integer, intent(inout) :: istatus
  
      include 'wrf_io_flags.h'
      include 'wrf_status_codes.h'
  
      ! Local variables
      integer :: i, ndim, wrftype
      integer :: sm1, em1, sm2, em2, sm3, em3, sp1, ep1, sp2, ep2, sp3, ep3
      integer, dimension(3) :: domain_start, domain_end
      real, pointer, dimension(:,:,:) :: real_domain
      integer, pointer, dimension(:,:,:) :: int_domain
      character (len=20) :: datestr
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if (num_calls == 0) then
#ifdef IO_BINARY
            if (io_form_input == BINARY) call ext_int_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) call ext_ncd_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) call ext_gr1_get_next_time(handle, datestr, istatus)
#endif
         end if
     
         num_calls = num_calls + 1
   
#ifdef IO_BINARY
         if (io_form_input == BINARY) call ext_int_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) call ext_ncd_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) call ext_gr1_get_next_var(handle, cname, istatus) 
#endif
      end if
  
      if (nprocs > 1 .and. .not. do_tiled_input) call parallel_bcast_int(istatus)
      if (istatus /= 0) return
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         istatus = 0
#ifdef IO_BINARY
         if (io_form_input == BINARY) &
            call ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) &
            call ext_ncd_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) &
            call ext_gr1_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
#endif
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), problems with ext_pkg_get_var_info()')
     
         start_mem_i = domain_start(1) 
         start_mem_j = domain_start(2) 
         end_mem_i = domain_end(1)
         end_mem_j = domain_end(2)
         if (ndim == 3) then
            start_mem_k = domain_start(3) 
            end_mem_k = domain_end(3) 
         else
            domain_start(3) = 1
            domain_end(3) = 1
            start_mem_k = 1
            end_mem_k = 1
         end if
     
         nullify(real_domain)
         nullify(int_domain)
     
         if (wrftype == WRF_REAL) then
            allocate(real_domain(start_mem_i:end_mem_i, start_mem_j:end_mem_j, start_mem_k:end_mem_k))
#ifdef IO_BINARY
            if (io_form_input == BINARY) then
               call ext_int_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               call ext_ncd_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               call ext_gr1_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
         else if (wrftype == WRF_INTEGER) then
            allocate(int_domain(start_mem_i:end_mem_i, start_mem_j:end_mem_j, start_mem_k:end_mem_k))
#ifdef IO_BINARY
            if (io_form_input == BINARY) then
               call ext_int_read_field(handle, '0000-00-00_00:00:00', cname, int_domain, WRF_INTEGER, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               call ext_ncd_read_field(handle, '0000-00-00_00:00:00', cname, int_domain, WRF_INTEGER, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               call ext_gr1_read_field(handle, '0000-00-00_00:00:00', cname, int_domain, WRF_INTEGER, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
         end if
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), got error code %i.', i1=istatus)
     
         if (io_form_input == BINARY) then
            qd = q_remove(unit_desc)
            cunits = qd%units
            cdesc = qd%description
            stagger = qd%stagger
         else
            cunits = ' '
            cdesc = ' '
            stagger = ' '
        
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               call ext_ncd_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_ncd_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_ncd_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               call ext_gr1_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_gr1_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_gr1_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
#endif
         end if
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)

      if (nprocs > 1 .and. .not. do_tiled_input) then
         call parallel_bcast_char(cname, len(cname))
         call parallel_bcast_char(cunits, len(cunits))
         call parallel_bcast_char(cdesc, len(cdesc))
         call parallel_bcast_char(memorder, len(memorder))
         call parallel_bcast_char(stagger, len(stagger))
         call parallel_bcast_char(dimnames(1), 128)
         call parallel_bcast_char(dimnames(2), 128)
         call parallel_bcast_char(dimnames(3), 128)
         call parallel_bcast_int(wrftype)
         call parallel_bcast_int(domain_start(3))
         call parallel_bcast_int(domain_end(3))
   
         sp1 = my_minx
         ep1 = my_maxx - 1
         sp2 = my_miny
         ep2 = my_maxy - 1
         sp3 = domain_start(3)
         ep3 = domain_end(3)
   
         if (internal_gridtype == 'C') then
            if (my_x /= nproc_x - 1 .or. stagger == 'U') then
               ep1 = ep1 + 1
            end if
            if (my_y /= nproc_y - 1 .or. stagger == 'V') then
               ep2 = ep2 + 1
            end if
         else if (internal_gridtype == 'E') then
            ep1 = ep1 + 1
            ep2 = ep2 + 1
         end if
   
         sm1 = sp1
         em1 = ep1
         sm2 = sp2
         em2 = ep2
         sm3 = sp3
         em3 = ep3
   
         start_mem_i = sm1
         end_mem_i = em1
         start_mem_j = sm2
         end_mem_j = em2
         start_mem_k = sm3
         end_mem_k = em3
   
         if (wrftype == WRF_REAL) then
   
            allocate(real_array(sm1:em1,sm2:em2,sm3:em3))
            nullify(int_array)
            if (my_proc_id /= IO_NODE) then
               allocate(real_domain(1,1,1))
               domain_start(1) = 1
               domain_start(2) = 1
               domain_start(3) = 1
               domain_end(1) = 1
               domain_end(2) = 1
               domain_end(3) = 1
            end if
            call scatter_whole_field_r(real_array, &
                                      sm1, em1, sm2, em2, sm3, em3, &
                                      sp1, ep1, sp2, ep2, sp3, ep3, &
                                      real_domain, &
                                      domain_start(1), domain_end(1), domain_start(2), domain_end(2), domain_start(3), domain_end(3))
            deallocate(real_domain)
   
         else if (wrftype == WRF_INTEGER) then
   
            allocate(int_array(sm1:em1,sm2:em2,sm3:em3))
            nullify(real_array)
            if (my_proc_id /= IO_NODE) then
               allocate(int_domain(1,1,1))
               domain_start(1) = 1
               domain_start(2) = 1
               domain_start(3) = 1
               domain_end(1) = 1
               domain_end(2) = 1
               domain_end(3) = 1
            end if
            call scatter_whole_field_i(int_array, &
                                      sm1, em1, sm2, em2, sm3, em3, &
                                      sp1, ep1, sp2, ep2, sp3, ep3, &
                                      int_domain, &
                                      domain_start(1), domain_end(1), domain_start(2), domain_end(2), domain_start(3), domain_end(3))
            deallocate(int_domain)
  
        end if    
  
      else
  
         real_array => real_domain
         int_array => int_domain
      end if
 
   end subroutine read_next_field
 
   
   subroutine read_global_attrs(title, start_date, grid_type, dyn_opt, &
                                west_east_dim, south_north_dim, bottom_top_dim, &
                                we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                                sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                                map_proj, is_water, &
                                is_ice, grid_id, parent_id, i_parent_start, j_parent_start, &
                                i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon, &
                                stand_lon, truelat1, truelat2, parent_grid_ratio, corner_lats, corner_lons)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, map_proj, is_water, &
                 we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                 sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                 is_ice, grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio
      real, intent(out) :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2
      real, dimension(16), intent(out) :: corner_lats, corner_lons
      character (len=128), intent(out) :: title, start_date, grid_type
  
      ! Local variables
      integer :: outcount, istatus, i
      integer :: is_urban, isoilwater
      character (len=128) :: cunits, cdesc, cstagger, mminlu
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            istatus = 0
            do while (istatus == 0) 
               cunits = ' '
               cdesc = ' '
               call ext_int_get_var_ti_char(handle, 'units', 'VAR', cunits, istatus)
         
               if (istatus == 0) then
                  call ext_int_get_var_ti_char(handle, 'description', 'VAR', cdesc, istatus)
          
                  if (istatus == 0) then
                     call ext_int_get_var_ti_char(handle, 'stagger', 'VAR', cdesc, istatus)
         
                     qd%units = cunits
                     qd%description = cdesc
                     qd%stagger = cstagger
                     call q_insert(unit_desc, qd)
        
                  end if
       
               end if
            end do
         end if
#endif
     
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            call ext_int_get_dom_ti_char(handle, 'TITLE', &
                                            title, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_char(handle, 'SIMULATION_START_DATE', &
                                            start_date, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'WEST-EAST_GRID_DIMENSION', &
                                            west_east_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'SOUTH-NORTH_GRID_DIMENSION', &
                                            south_north_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'BOTTOM-TOP_GRID_DIMENSION', &
                                            bottom_top_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_UNSTAG', &
                                            we_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_UNSTAG', &
                                            we_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_STAG', &
                                            we_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_STAG', &
                                            we_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_UNSTAG', &
                                            sn_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_UNSTAG', &
                                            sn_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_STAG', &
                                            sn_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_STAG', &
                                            sn_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_char(handle, 'GRIDTYPE', &
                                            grid_type, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_real(handle, 'DX', &
                                            dx, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'DY', &
                                            dy, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'DYN_OPT', &
                                            dyn_opt, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'CEN_LAT', &
                                            cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'CEN_LON', &
                                            cen_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'TRUELAT1', &
                                            truelat1, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'TRUELAT2', &
                                            truelat2, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'MOAD_CEN_LAT', &
                                            moad_cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_real(handle, 'STAND_LON', &
                                            stand_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_real(handle, 'corner_lats', &
                                            corner_lats, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_real(handle, 'corner_lons', &
                                            corner_lons, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'MAP_PROJ', &
                                            map_proj, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_char(handle, 'MMINLU', &
                                            mminlu, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'ISWATER', &
                                            is_water, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'ISICE', &
                                            is_ice, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'ISURBAN', &
                                            is_urban, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_int_get_dom_ti_integer(handle, 'ISOILWATER', &
                                            isoilwater, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'grid_id', &
                                            grid_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'parent_id', &
                                            parent_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'i_parent_start', &
                                            i_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'j_parent_start', &
                                            j_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'i_parent_end', &
                                            i_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'j_parent_end', &
                                            j_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_int_get_dom_ti_integer(handle, 'parent_grid_ratio', &
                                            parent_grid_ratio, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
         end if
#endif
     
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) then
            call ext_ncd_get_dom_ti_char(handle, 'TITLE', &
                                            title, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_char(handle, 'SIMULATION_START_DATE', &
                                            start_date, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'WEST-EAST_GRID_DIMENSION', &
                                            west_east_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'SOUTH-NORTH_GRID_DIMENSION', &
                                            south_north_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'BOTTOM-TOP_GRID_DIMENSION', &
                                            bottom_top_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_UNSTAG', &
                                            we_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_UNSTAG', &
                                            we_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_STAG', &
                                            we_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_STAG', &
                                            we_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_UNSTAG', &
                                            sn_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_UNSTAG', &
                                            sn_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_STAG', &
                                            sn_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_STAG', &
                                            sn_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_char(handle, 'GRIDTYPE', &
                                            grid_type, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_real(handle, 'DX', &
                                            dx, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'DY', &
                                            dy, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'DYN_OPT', &
                                            dyn_opt, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'CEN_LAT', &
                                            cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'CEN_LON', &
                                            cen_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'TRUELAT1', &
                                            truelat1, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'TRUELAT2', &
                                            truelat2, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'MOAD_CEN_LAT', &
                                            moad_cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_real(handle, 'STAND_LON', &
                                            stand_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_real(handle, 'corner_lats', &
                                            corner_lats, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_real(handle, 'corner_lons', &
                                            corner_lons, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'MAP_PROJ', &
                                            map_proj, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_char(handle, 'MMINLU', &
                                            mminlu, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'ISWATER', &
                                            is_water, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'ISICE', &
                                            is_ice, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'ISURBAN', &
                                            is_urban, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_ncd_get_dom_ti_integer(handle, 'ISOILWATER', &
                                            isoilwater, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'grid_id', &
                                            grid_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'parent_id', &
                                            parent_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'i_parent_start', &
                                            i_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'j_parent_start', &
                                            j_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'i_parent_end', &
                                            i_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'j_parent_end', &
                                            j_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_ncd_get_dom_ti_integer(handle, 'parent_grid_ratio', &
                                            parent_grid_ratio, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
         end if
#endif
     
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) then
     
            call ext_gr1_get_dom_ti_char(handle, 'TITLE', &
                                            title, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_char(handle, 'SIMULATION_START_DATE', &
                                            start_date, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'WEST-EAST_GRID_DIMENSION', &
                                            west_east_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'SOUTH-NORTH_GRID_DIMENSION', &
                                            south_north_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'BOTTOM-TOP_GRID_DIMENSION', &
                                            bottom_top_dim, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_UNSTAG', &
                                            we_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_UNSTAG', &
                                            we_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_START_STAG', &
                                            we_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'WEST-EAST_PATCH_END_STAG', &
                                            we_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_UNSTAG', &
                                            sn_patch_s, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_UNSTAG', &
                                            sn_patch_e, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_START_STAG', &
                                            sn_patch_s_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'SOUTH-NORTH_PATCH_END_STAG', &
                                            sn_patch_e_stag, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_char(handle, 'GRIDTYPE', &
                                            grid_type, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_real(handle, 'DX', &
                                            dx, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'DY', &
                                            dy, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'DYN_OPT', &
                                            dyn_opt, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'CEN_LAT', &
                                            cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'CEN_LON', &
                                            cen_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'TRUELAT1', &
                                            truelat1, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'TRUELAT2', &
                                            truelat2, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'MOAD_CEN_LAT', &
                                            moad_cen_lat, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_real(handle, 'STAND_LON', &
                                            stand_lon, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_real(handle, 'corner_lats', &
                                            corner_lats, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_real(handle, 'corner_lons', &
                                            corner_lons, &
                                            16, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'MAP_PROJ', &
                                            map_proj, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_char(handle, 'MMINLU', &
                                            mminlu, &
                                            istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'ISWATER', &
                                            is_water, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'ISICE', &
                                            is_ice, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'ISURBAN', &
                                            is_urban, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      
            call ext_gr1_get_dom_ti_integer(handle, 'ISOILWATER', &
                                            isoilwater, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'grid_id', &
                                            grid_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'parent_id', &
                                            parent_id, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'i_parent_start', &
                                            i_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'j_parent_start', &
                                            j_parent_start, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'i_parent_end', &
                                            i_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'j_parent_end', &
                                            j_parent_end, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
        
            call ext_gr1_get_dom_ti_integer(handle, 'parent_grid_ratio', &
                                            parent_grid_ratio, &
                                            1, outcount, istatus)
            call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
         end if
#endif
      end if
  
      if (nprocs > 1 .and. .not. do_tiled_input) then
  
         call parallel_bcast_char(title, len(title))
         call parallel_bcast_char(start_date, len(start_date))
         call parallel_bcast_char(grid_type, len(grid_type))
         call parallel_bcast_int(west_east_dim)
         call parallel_bcast_int(south_north_dim)
         call parallel_bcast_int(bottom_top_dim)
         call parallel_bcast_int(we_patch_s)
         call parallel_bcast_int(we_patch_e)
         call parallel_bcast_int(we_patch_s_stag)
         call parallel_bcast_int(we_patch_e_stag)
         call parallel_bcast_int(sn_patch_s)
         call parallel_bcast_int(sn_patch_e)
         call parallel_bcast_int(sn_patch_s_stag)
         call parallel_bcast_int(sn_patch_e_stag)
         call parallel_bcast_real(dx)
         call parallel_bcast_real(dy)
         call parallel_bcast_int(dyn_opt)
         call parallel_bcast_real(cen_lat)
         call parallel_bcast_real(cen_lon)
         call parallel_bcast_real(truelat1)
         call parallel_bcast_real(truelat2)
         call parallel_bcast_real(moad_cen_lat)
         call parallel_bcast_real(stand_lon)
         do i=1,16
            call parallel_bcast_real(corner_lats(i))
            call parallel_bcast_real(corner_lons(i))
         end do
         call parallel_bcast_int(map_proj)
         call parallel_bcast_char(mminlu, len(mminlu))
         call parallel_bcast_int(is_water)
         call parallel_bcast_int(is_ice)
         call parallel_bcast_int(is_urban)
         call parallel_bcast_int(isoilwater)
         call parallel_bcast_int(grid_id)
         call parallel_bcast_int(parent_id)
         call parallel_bcast_int(i_parent_start)
         call parallel_bcast_int(i_parent_end)
         call parallel_bcast_int(j_parent_start)
         call parallel_bcast_int(j_parent_end)
         call parallel_bcast_int(parent_grid_ratio)
      end if
  
      internal_gridtype = grid_type
 
   end subroutine read_global_attrs
 
 
   subroutine input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            call ext_int_ioclose(handle, istatus)
            call ext_int_ioexit(istatus)
         end if
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) then
            call ext_ncd_ioclose(handle, istatus)
            call ext_ncd_ioexit(istatus)
         end if
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) then
            call ext_gr1_ioclose(handle, istatus)
            call ext_gr1_ioexit(istatus)
         end if
#endif
      end if
 
      call q_destroy(unit_desc)
 
   end subroutine input_close
 
end module input_module
