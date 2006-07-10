program plotgrid

   use input_module

   implicit none

   integer :: n, i, j, nx, ny
   integer :: istatus, start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
              start_mem_k, end_mem_k, dyn_opt, &
              west_east_dim, south_north_dim, bottom_top_dim, map_proj, is_water, &
              is_ice, grid_id, parent_id, i_parent_start, j_parent_start, &
              i_parent_end, j_parent_end, parent_grid_ratio, &
              we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
              sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag
   real :: width, height
   real :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2
   real :: start_r, start_g, start_b, end_r, end_g, end_b
   real :: ll_lat, ll_lon, ur_lat, ur_lon
   real :: left, right, bottom, top, maxter, minter
   real, dimension(16) :: corner_lats, corner_lons
   integer, allocatable, dimension(:,:) :: lu
   real, allocatable, dimension(:,:) :: xlat, xlon, ter
   real, dimension(122000) :: rwrk
   integer, pointer, dimension(:,:,:) :: int_array
   real, pointer, dimension(:,:,:) :: real_array
   character (len=3) :: memorder
   character (len=25) :: units
   character (len=46) :: desc
   character (len=128) :: cname, stagger, cunits, cdesc, title, startdate, grid_type
   character (len=128), dimension(3) :: dimnames

   call opngks

   call gopwk(13, 41, 3)

   call gscr(1, 0, 1.00, 1.00, 1.00)
   call gscr(1, 1, 0.00, 0.00, 0.00)

   call gscr(1, 2, 0.25, 0.25, 0.25)
   call gscr(1, 3, 1.00, 1.00, 0.50)
   call gscr(1, 4, 0.50, 1.00, 0.50)
   call gscr(1, 5, 1.00, 1.00, 0.00)
   call gscr(1, 6, 1.00, 1.00, 0.00)
   call gscr(1, 7, 0.50, 1.00, 0.50)
   call gscr(1, 8, 1.00, 1.00, 0.50)
   call gscr(1, 9, 0.50, 1.00, 0.50)
   call gscr(1,10, 0.50, 1.00, 0.50)
   call gscr(1,11, 1.00, 1.00, 0.50)
   call gscr(1,12, 0.00, 1.00, 0.00)
   call gscr(1,13, 0.00, 0.50, 0.00)
   call gscr(1,14, 0.00, 1.00, 0.00)
   call gscr(1,15, 0.00, 0.50, 0.00)
   call gscr(1,16, 0.00, 1.00, 0.00)
   call gscr(1,17, 0.50, 0.50, 1.00)
   call gscr(1,18, 0.00, 1.00, 0.00)
   call gscr(1,19, 0.00, 1.00, 0.00)
   call gscr(1,20, 0.75, 0.75, 0.75)
   call gscr(1,21, 0.75, 0.75, 0.75)
   call gscr(1,22, 0.00, 0.50, 0.00)
   call gscr(1,23, 0.75, 0.75, 0.75)
   call gscr(1,24, 0.75, 0.75, 0.75)
   call gscr(1,25, 1.00, 1.00, 1.00)
   call gscr(1,26, 1.00, 1.00, 1.00)
   call gscr(1,27, 1.00, 1.00, 1.00)
   call gscr(1,28, 1.00, 1.00, 1.00)
   call gscr(1,29, 1.00, 1.00, 1.00)
   call gscr(1,30, 1.00, 1.00, 1.00)
   call gscr(1,31, 0.00, 0.00, 0.50)
   call gscr(1,32, 0.00, 0.00, 0.50)
   call gscr(1,33, 0.00, 0.00, 0.50)
   call gscr(1,34, 0.00, 0.00, 0.50)

   start_r = 0.00
   end_r   = 0.50
   start_g = 1.00
   end_g   = 0.25
   start_b = 0.00
   end_b   = 0.00
   do i=36,76
     call gscr(1,i,start_r+((end_r-start_r)/50.)*real(i-36),start_g+((end_g-start_g)/50.)*real(i-36),start_b+((end_b-start_b)/50.)*real(i-36))
   end do

   start_r = 0.50
   end_r   = 1.00
   start_g = 0.25
   end_g   = 1.00
   start_b = 0.00
   end_b   = 1.00
   do i=77,126
     call gscr(1,i,start_r+((end_r-start_r)/50.)*real(i-77),start_g+((end_g-start_g)/50.)*real(i-77),start_b+((end_b-start_b)/50.)*real(i-77))
   end do

   call get_namelist_params()

   do n=1,max_dom
      call input_init(n, istatus)
      if (istatus /= 0) then
         write(6,*) ' '
         write(6,*) 'Error: Could not open domain01 file.'
         write(6,*) ' '
         stop
      end if

      call read_global_attrs(title, startdate, grid_type, dyn_opt, &
                             west_east_dim, south_north_dim, bottom_top_dim, &
                             we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                             sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                             map_proj, is_water, &
                             is_ice, grid_id, parent_id, i_parent_start, j_parent_start, &
                             i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon, &
                             stand_lon, truelat1, truelat2, parent_grid_ratio, corner_lats, corner_lons)

      istatus = 0
      do while (istatus == 0)
        call read_next_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                             start_mem_k, end_mem_k, cname, cunits, cdesc, &
                             memorder, stagger, dimnames, real_array, int_array, &
                             istatus)
        if (istatus == 0) then

          if (index(cname, 'XLAT_M') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(xlat(nx,ny))
             xlat = real_array(:,:,1)
          else if (index(cname, 'XLONG_M') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(xlon(nx,ny))
             xlon = real_array(:,:,1)
          else if (index(cname, 'LU_INDEX') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(lu(nx,ny))
             lu = int_array(:,:,1)
          end if
        end if
      end do

      call input_close()

      ll_lat = xlat(1,1)
      ll_lon = xlon(1,1)
      ur_lat = xlat(nx,ny)
      ur_lon = xlon(nx,ny)

      if (n == 1) then
         left = 0.0
         right = 1.0
         bottom = 0.0
         top = 1.0
         call mappos(left,right,bottom,top)

         call mapstc('OU','CO')

         call maproj('LC', truelat1, stand_lon, truelat2)
!         call maproj('ST', cen_lat, cen_lon, stand_lon)
         call mapset('CO', ll_lat, ll_lon, ur_lat, ur_lon)
         call mapint()
      end if

      call maptrn(ll_lat, ll_lon, left, bottom)
      call maptrn(ur_lat, ur_lon, right, top)

      width = 1.02*(right-left)/real(nx)
      height = 1.02*(top-bottom)/real(ny)

      do j=1,ny 
         do i=1,nx 
            call map_square(xlat(i,j), xlon(i,j), width, height, lu(i,j)+1)
         end do
      end do

      if (n > 1) then
         call gsplci(0)
         call lined(left-width/2.,bottom-height/2.,left-width/2.,top+height/2.)
         call lined(left-width/2.,top+height/2.,right+width/2.,top+height/2.)
         call lined(right+width/2.,top+height/2.,right+width/2.,bottom-height/2.)
         call lined(right+width/2.,bottom-height/2.,left-width/2.,bottom-height/2.)
         call sflush()
         call gsplci(1)
      end if

      deallocate(xlat)
      deallocate(xlon)
      deallocate(lu)
   end do

   call mplndr('Earth..3',4)
!RANGS   call mdrgol(3,rwrk,122000)

   call frame()

   do n=1,max_dom
      call input_init(n, istatus)
      if (istatus /= 0) then
         write(6,*) ' '
         write(6,*) 'Error: Could not open domain01 file.'
         write(6,*) ' '
         stop
      end if

      call read_global_attrs(title, startdate, grid_type, dyn_opt, &
                             west_east_dim, south_north_dim, bottom_top_dim, &
                             we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                             sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                             map_proj, is_water, &
                             is_ice, grid_id, parent_id, i_parent_start, j_parent_start, &
                             i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon, &
                             stand_lon, truelat1, truelat2, parent_grid_ratio, corner_lats, corner_lons)

      istatus = 0
      do while (istatus == 0)
        call read_next_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                             start_mem_k, end_mem_k, cname, cunits, cdesc, &
                             memorder, stagger, dimnames, real_array, int_array, &
                             istatus)
        if (istatus == 0) then

          if (index(cname, 'XLAT_M') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(xlat(nx,ny))
             xlat = real_array(:,:,1)
          else if (index(cname, 'XLONG_M') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(xlon(nx,ny))
             xlon = real_array(:,:,1)
          else if (index(cname, 'HGT_M') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(ter(nx,ny))
             ter = real_array(:,:,1)
          else if (index(cname, 'LU_INDEX') /= 0) then
             nx = end_mem_i - start_mem_i + 1
             ny = end_mem_j - start_mem_j + 1
             allocate(lu(nx,ny))
             lu = int_array(:,:,1)
          end if
        end if
      end do

      call input_close()

      ll_lat = xlat(1,1)
      ll_lon = xlon(1,1)
      ur_lat = xlat(nx,ny)
      ur_lon = xlon(nx,ny)

      if (n == 1) then
         left = 0.0
         right = 1.0
         bottom = 0.0
         top = 1.0
         call mappos(left,right,bottom,top)

         call mapstc('OU','CO')

         call maproj('LC', truelat1, stand_lon, truelat2)
!         call maproj('ST', cen_lat, cen_lon, stand_lon)
         call mapset('CO', ll_lat, ll_lon, ur_lat, ur_lon)
         call mapint()

         maxter = -10000.
         minter = 10000.
         do j=1,ny 
            do i=1,nx 
               if (ter(i,j) > maxter) maxter = ter(i,j)
               if (ter(i,j) < minter) minter = ter(i,j)
            end do
         end do
         maxter = 3348.42
      end if

      call maptrn(ll_lat, ll_lon, left, bottom)
      call maptrn(ur_lat, ur_lon, right, top)

      width = 1.02*(right-left)/real(nx)
      height = 1.02*(top-bottom)/real(ny)

      do j=1,ny 
         do i=1,nx 
            ter(i,j) = ((ter(i,j)-minter) * 99.)/(maxter-minter) + 26.
            if (lu(i,j) ==  16) then
               call map_square(xlat(i,j), xlon(i,j), width, height, 17)
            else if (lu(i,j) ==  1) then
               call map_square(xlat(i,j), xlon(i,j), width, height, 2)
            else
               call map_square(xlat(i,j), xlon(i,j), width, height, nint(ter(i,j)))
            end if
         end do
      end do

      if (n > 1) then
         call gsplci(0)
         call lined(left-width/2.,bottom-height/2.,left-width/2.,top+height/2.)
         call lined(left-width/2.,top+height/2.,right+width/2.,top+height/2.)
         call lined(right+width/2.,top+height/2.,right+width/2.,bottom-height/2.)
         call lined(right+width/2.,bottom-height/2.,left-width/2.,bottom-height/2.)
         call sflush()
         call gsplci(1)
      end if

      deallocate(xlat)
      deallocate(xlon)
      deallocate(ter)
      deallocate(lu)
   end do

   call mplndr('Earth..3',4)
!RANGS   call mdrgol(3,rwrk,122000)

   call gclwk(13)

   call clsgks


   stop

end program plotgrid


subroutine map_square(rlat, rlon, width, height, colr)

    implicit none

    ! Arguments
    real :: rlat, rlon, width, height
    integer :: colr

    ! Local variables
    real :: u, v
    real, dimension(4) :: xra, yra
    real, dimension(2000) :: dst
    integer, dimension(3000) :: ind

    call maptrn(rlat, rlon, u, v)

    xra(1) = u-(width/2.)
    xra(2) = u+(width/2.)
    xra(3) = u+(width/2.)
    xra(4) = u-(width/2.)

    yra(1) = v-(height/2.)
    yra(2) = v-(height/2.)
    yra(3) = v+(height/2.)
    yra(4) = v+(height/2.)

    call sfsgfa(xra, yra, 4, dst, 2000, ind, 3000, colr)

end subroutine map_square
