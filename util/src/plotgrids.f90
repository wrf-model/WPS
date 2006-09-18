program plotgrids

   use map_utils
 
   implicit none

   ! Parameters
   integer, parameter :: MAX_DOMAINS = 21

   ! Variables
   integer :: iproj_type, n_domains, io_form_output, dyn_opt
   integer :: i, j, max_dom, funit, io_form_geogrid
   integer :: interval_seconds

   integer, dimension(MAX_DOMAINS) :: parent_grid_ratio, parent_id, ixdim, jydim
   integer, dimension(MAX_DOMAINS) :: i_parent_start, j_parent_start, &
                        s_we, e_we, s_sn, e_sn, &
                        start_year, start_month, start_day, start_hour, &
                        end_year,   end_month,   end_day,   end_hour

   real :: known_lat, known_lon, stand_lon, truelat1, truelat2, known_x, known_y, &
           dxkm, dykm, phi, lambda, ref_lat, ref_lon, ref_x, ref_y
   real :: dx, dy
   real :: ri, rj, rlats, rlons, rlate, rlone
   real :: polat , rot
   real :: rparent_gridpts
   real :: xa,xb,ya,yb,xxa,xxy,yya,yyb
   real :: xs, xe, ys, ye
   integer :: jproj, jgrid, jlts, iusout, idot, ier
   integer :: ltype , idom

   real, dimension(MAX_DOMAINS) :: parent_ll_x, parent_ll_y, parent_ur_x, parent_ur_y

   character (len=128) :: geog_data_path, opt_output_from_geogrid_path, opt_geogrid_tbl_path
   character (len=128), dimension(MAX_DOMAINS) :: geog_data_res 
   character (len=128) :: map_proj
   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
   character (len=3) :: wrf_core
   character (len=1) :: gridtype

   logical :: do_tiled_output
   integer :: debug_level
   logical :: is_used

   type (proj_info) :: map_projection

   namelist /share/ wrf_core, max_dom, start_date, end_date, &
                     start_year, end_year, start_month, end_month, &
                     start_day, end_day, start_hour, end_hour, &
                     interval_seconds, &
                     io_form_geogrid, opt_output_from_geogrid_path, debug_level
   namelist /geogrid/ parent_id, parent_grid_ratio, &
                      i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
                      map_proj, ref_x, ref_y, ref_lat, ref_lon, &
                      truelat1, truelat2, stand_lon, dx, dy, &
                      geog_data_res, geog_data_path, opt_geogrid_tbl_path
  
   ! Set defaults for namelist variables
   debug_level = 0
   io_form_geogrid = 2
   wrf_core = 'ARW'
   max_dom = 1
   geog_data_path = 'NOT_SPECIFIED'
   ref_x = NAN
   ref_y = NAN
   ref_lat = NAN
   ref_lon = NAN
   dx = 10000.
   dy = 10000.
   map_proj = 'Lambert'
   truelat1 = NAN
   truelat2 = NAN
   stand_lon = NAN
   do i=1,MAX_DOMAINS
      geog_data_res(i) = 'default'
      parent_id(i) = 1
      parent_grid_ratio(i) = INVALID
      s_we(i) = 1
      e_we(i) = INVALID
      s_sn(i) = 1
      e_sn(i) = INVALID
      start_year(i) = 0
      start_month(i) = 0
      start_day(i) = 0
      start_hour(i) = 0
      end_year(i) = 0
      end_month(i) = 0
      end_day(i) = 0
      end_hour(i) = 0
      start_date(i) = '0000-00-00_00:00:00'
      end_date(i) = '0000-00-00_00:00:00'
   end do
   opt_output_from_geogrid_path = './'
   opt_geogrid_tbl_path = 'geogrid/'
   interval_seconds = INVALID
   
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

   known_lat = ref_lat
   known_lon = ref_lon
   known_x = ref_x
   known_y = ref_y

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

   if (gridtype /= 'C' .and. gridtype /= 'E') then
      write(6,*) 'A valid wrf_core must be specified in the namelist. '// &
                 'Currently, only "ARW" and "NMM" are supported.'
      stop
   end if

   if (max_dom > MAX_DOMAINS) then
      write(6,*) 'In namelist, max_dom must be <= ',MAX_DOMAINS,'. To run with more'// &
                ' than ',MAX_DOMAINS,' domains, increase the MAX_DOMAINS parameter.'
      stop
   end if

   ! Every domain must have a valid parent id
   do i=2,max_dom
      if (parent_id(i) <= 0 .or. parent_id(i) >= i) then
         write(6,*) 'In namelist, the parent_id of domain ',i,' must be in '// &
                   'the range 1 to ',i-1
          stop
      end if
   end do

   ! Convert map_proj to uppercase letters
   do i=1,len(map_proj)
      if (ichar(map_proj(i:i)) >= 97) map_proj(i:i) = char(ichar(map_proj(i:i))-32)
   end do

   ! Assign parameters to module variables
   if ((index(map_proj, 'LAMBERT') /= 0) .and. &
       (len_trim(map_proj) == len('LAMBERT'))) then
      iproj_type = PROJ_LC 
      rot=truelat1
      polat=truelat2
      jproj = 3

   else if ((index(map_proj, 'MERCATOR') /= 0) .and. &
            (len_trim(map_proj) == len('MERCATOR'))) then
      iproj_type = PROJ_MERC 
      rot=0.
      polat=0.
      jproj = 9

   else if ((index(map_proj, 'POLAR') /= 0) .and. &
            (len_trim(map_proj) == len('POLAR'))) then
      iproj_type = PROJ_PS 
      rot=0.
      polat=SIGN(90., ref_lat)
      jproj = 1

   else if ((index(map_proj, 'ROTATED_LL') /= 0) .and. &
            (len_trim(map_proj) == len('ROTATED_LL'))) then
      iproj_type = PROJ_ROTLL 

   else
         write(6,*) 'In namelist, invalid map_proj specified. Valid '// &
                    'projections are "lambert", "mercator", "polar", '// &
                    'and "rotated_ll".'
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

   ! Checks specific to C grid
   if (gridtype == 'C') then

      ! C grid does not support the rotated lat/lon projection
      if (iproj_type == PROJ_ROTLL) then
         write(6,*) 'Rotated lat/lon projection is not supported for the ARW core. '// &
                    'Valid projecitons are "lambert", "mercator", and "polar".'
         stop
      end if

      ! Check that nests have an acceptable number of grid points in each dimension
      do i=2,n_domains
         rparent_gridpts = real(ixdim(i)-1)/real(parent_grid_ratio(i))
         if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
            write(6,*) 'For nest ',i,' (e_we-s_we+1) must be one greater than an '// &
                       'interger multiple of the parent_grid_ratio.'
            stop
         end if
         rparent_gridpts = real(jydim(i)-1)/real(parent_grid_ratio(i))
         if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
            write(6,*) 'For nest ',i,' (e_sn-s_sn+1) must be one greater than an '// &
                       'interger multiple of the parent_grid_ratio.'
            stop
         end if
      end do
   end if

   do i=1,n_domains
      parent_ll_x(i) = real(i_parent_start(i))
      parent_ll_y(i) = real(j_parent_start(i))
      parent_ur_x(i) = real(i_parent_start(i))+real(ixdim(i))/real(parent_grid_ratio(i))-1.
      parent_ur_y(i) = real(j_parent_start(i))+real(jydim(i))/real(parent_grid_ratio(i))-1.
   end do

   call map_init(map_projection)

   call map_set(iproj_type, map_projection, &
                lat1=known_lat, &
                lon1=known_lon, &
                knowni=known_x, &
                knownj=known_y, &
                dx=dx, &
                stdlon=stand_lon, &
                truelat1=truelat1, &
                truelat2=truelat2, &
                ixdim=ixdim(1), &
                jydim=jydim(1))

   call ij_to_latlon(map_projection, 1., 1., rlats, rlons)
   call ij_to_latlon(map_projection, real(e_we(1)), real(e_sn(1)) , rlate, rlone)

   call opngks

   ! Set some colors
   call gscr(1, 0, 1.00, 1.00, 1.00)
   call gscr(1, 1, 0.00, 0.00, 0.00)

   ! Do not grind them with details
   jgrid=10
   jlts=-2
   iusout=1
   idot=0

   call supmap(jproj,polat,stand_lon,rot,&
               rlats,rlons,rlate,rlone, &
               jlts,jgrid,iusout,idot,ier) 

   call setusv('LW',1000)
   call perim(e_we(1)-1,1,e_sn(1)-1,1)
   call getset(xa,xb,ya,yb,xxa,xxy,yya,yyb,ltype)
   call set   (xa,xb,ya,yb, &
         1.,real(e_we(1)),1.,real(e_sn(1)),ltype)

   do idom = 2 , max_dom
      call getxy ( xs, xe, ys, ye, &
                   idom , max_dom , &
                   e_we , e_sn , &
                   parent_id , parent_grid_ratio , &
                   i_parent_start , j_parent_start )
      call line ( xs , ys , xe , ys )
      call line ( xe , ys , xe , ye )
      call line ( xe , ye , xs , ye )
      call line ( xs , ye , xs , ys )
   end do

   call frame

   call clsgks

   stop

1000 write(6,*) 'Error opening namelist.wps'
   stop
  
end program plotgrids

subroutine getxy ( xs, xe, ys, ye, &
                   dom_id , num_domains , &
                   e_we , e_sn , &
                   parent_id , parent_grid_ratio , &
                   i_parent_start , j_parent_start )

   implicit none

   integer , intent(in) :: dom_id
   integer , intent(in) :: num_domains
   integer , intent(in) , dimension(num_domains):: e_we , e_sn , &
                                                   parent_id , parent_grid_ratio , &
                                                   i_parent_start , j_parent_start
   real , intent(out) :: xs, xe, ys, ye


   !  local vars

   integer :: idom

   xs = 0.
   xe = e_we(dom_id) -1
   ys = 0.
   ye = e_sn(dom_id) -1

   idom = dom_id
   compute_xy : DO

      xs = (i_parent_start(idom) + xs  -1 ) / &    
           real(parent_grid_ratio(parent_id(idom)))
      xe = xe / real(parent_grid_ratio(idom))

      ys = (j_parent_start(idom) + ys  -1 ) / &    
           real(parent_grid_ratio(parent_id(idom)))
      ye = ye / real(parent_grid_ratio(idom))

      idom = parent_id(idom)
      if ( idom .EQ. 1 ) then
         exit compute_xy
      end if

   END DO compute_xy

   xs = xs + 1
   xe = xs + xe
   ys = ys + 1
   ye = ys + ye

end subroutine getxy
