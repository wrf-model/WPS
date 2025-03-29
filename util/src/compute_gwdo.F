!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module gridinfo_module

  ! Parameters
  integer, parameter :: MAX_DOMAINS = 21

  ! Variables
  integer :: iproj_type, n_domains, io_form_output, dyn_opt
  integer, dimension(MAX_DOMAINS) :: parent_grid_ratio, parent_id, ixdim, jydim
  integer, dimension(MAX_DOMAINS) :: subgrid_ratio_x, subgrid_ratio_y
  real :: known_lat, known_lon, pole_lat, pole_lon, stand_lon, truelat1, truelat2, &
          known_x, known_y, dxkm, dykm, phi, lambda, ref_lat, ref_lon, ref_x, ref_y, &
          dlatdeg, dlondeg
  real, dimension(MAX_DOMAINS) :: parent_ll_x, parent_ll_y, parent_ur_x, parent_ur_y
  character (len=256) :: geog_data_path, opt_output_from_geogrid_path, opt_geogrid_tbl_path

  character (len=128), dimension(MAX_DOMAINS) :: geog_data_res 
  character (len=1) :: gridtype
  logical :: do_tiled_output
  logical, dimension(MAX_DOMAINS) :: grid_is_active
  integer :: debug_level

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
    integer :: i, j, max_dom, funit, io_form_geogrid, interval_seconds
    real :: dx, dy, rparent_gridpts
    integer, dimension(MAX_DOMAINS) :: i_parent_start, j_parent_start, &
              s_we, e_we, s_sn, e_sn, &
              start_year, start_month, start_day, start_hour, start_minute, start_second, &
              end_year,   end_month,   end_day,   end_hour,   end_minute,   end_second
    character (len=128) :: map_proj
    character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
    character (len=3) :: wrf_core
    logical :: is_used, nest_outside
    logical, dimension(MAX_DOMAINS) :: active_grid
    logical :: nocolons

    namelist /share/ wrf_core, max_dom, start_date, end_date, &
          start_year, end_year, start_month, end_month, &
          start_day, end_day, start_hour, end_hour, &
          start_minute, end_minute, start_second, end_second, &
          interval_seconds, &
          io_form_geogrid, opt_output_from_geogrid_path, &
          debug_level, active_grid, &
          subgrid_ratio_x, subgrid_ratio_y, &
          nocolons
    namelist /geogrid/ parent_id, parent_grid_ratio, &
            i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
            map_proj, ref_x, ref_y, ref_lat, ref_lon, &
            pole_lat, pole_lon, truelat1, truelat2, stand_lon, &
            dx, dy, geog_data_res, geog_data_path, opt_geogrid_tbl_path

    ! Set defaults for namelist variables
    debug_level = 0
    io_form_geogrid = 2
    wrf_core = 'ARW'
    max_dom = 1
    geog_data_path = 'NOT_SPECIFIED'
    ref_x = 1.e20
    ref_y = 1.e20
    ref_lat = 1.e20
    ref_lon = 1.e20
    dx = 1.e20
    dy = 1.e20
    map_proj = 'Lambert'
    pole_lat = 90.0
    pole_lon = 0.0
    truelat1 = 1.e20
    truelat2 = 1.e20
    stand_lon = 1.e20
    do i=1,MAX_DOMAINS
      geog_data_res(i) = 'default'
      parent_id(i) = 1
      parent_grid_ratio(i) = 1e9
      s_we(i) = 1
      e_we(i) = 1e9
      s_sn(i) = 1
      e_sn(i) = 1e9
      start_year(i) = 0
      start_month(i) = 0
      start_day(i) = 0
      start_hour(i) = 0
      start_minute(i) = 0
      start_second(i) = 0
      end_year(i) = 0
      end_month(i) = 0
      end_day(i) = 0
      end_hour(i) = 0
      end_minute(i) = 0
      end_second(i) = 0
      start_date(i) = '0000-00-00_00:00:00'
      end_date(i) = '0000-00-00_00:00:00'
      active_grid(i) = .true.
      subgrid_ratio_x(i) = 1
      subgrid_ratio_y(i) = 1
    end do
    opt_output_from_geogrid_path = './'
    opt_geogrid_tbl_path = 'geogrid/'
    interval_seconds = 1e9
    nocolons = .false.

    ! Read parameters from Fortran namelist
    do funit=10,100
      inquire(unit=funit, opened=is_used)
      if (.not. is_used) exit
    end do
    open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
    read(funit,share)
    read(funit,geogrid)
    close(funit)

    n_domains = max_dom

    return

    1000 write( *, * ) 'Error opening file namelist.wps'

  end subroutine get_grid_params

end module gridinfo_module

module nc_helpers
contains
  subroutine nf90_check_error( stat )
    use netcdf
    implicit none
    integer, intent( in ) :: stat
    if ( stat /= nf90_noerr ) then
      ! We had an error
      write( 0, * )  __FILE__, ":", __LINE__, " : ", trim( nf90_strerror( stat ) )
      stop 1
    end if
  end subroutine nf90_check_error
end module nc_helpers

module source_data_module
  type :: source_index_raw
    character (len=32) :: projection       = "required"
    character (len=32) :: source_type      = "required"
    logical            :: signed           = .false.
    character (len=32) :: units            = "required"
    character (len=64) :: description      = "required"
    real               :: dx               = -1.0       !< required
    real               :: dy               = -1.0       !< required
    real               :: known_x          =  1.0
    real               :: known_y          =  1.0
    real               :: known_lat        = -1.0       !< required
    real               :: known_lon        = -1.0       !< required
    real               :: stdlon           = -2.0       !< not required
    real               :: truelat1         = -2.0       !< not required
    real               :: truelat2         = -2.0       !< not required
    integer            :: wordsize         = -1         !< required
    integer            :: tile_x           = -1         !< required
    integer            :: tile_y           = -1         !< required
    integer            :: tile_z           = -2         !< not required
    integer            :: tile_z_start     = -2         !< not required
    integer            :: tile_z_end       = -2         !< not required
    integer            :: category_min     = -2         !< not required
    integer            :: category_max     = -2         !< not required
    integer            :: tile_bdr         =  0
    real               :: missing_value    = -2.0       !< not required
    real               :: scale_factor     =  1.0
    character (len=32) :: row_order        = "bottom_top"
    character (len=32) :: endian           = "big"
    integer            :: iswater          = 16
    integer            :: islake           = -1         !< i.e. no separate inland water category (does not mean required)
    integer            :: isice            = 24
    integer            :: isurban          =  1
    integer            :: isoilwater       = 14
    character (len=32) :: mminlu           = "USGS"
    integer            :: filename_digits  =  5
  end type source_index_raw

contains

  subroutine read_index_file( source_path, index_data )
    use, intrinsic :: iso_fortran_env

    implicit none

    character (len=*), intent( in ) :: source_path
    character (len=256)             :: index_path, line_buffer, key, var
    logical                         :: is_used, kv_exist
    integer                         :: iostatus, funit

    type( source_index_raw ), intent( inout ) :: index_data

    write( index_path, "(a)" ) trim(source_path) // 'index'
    write( *, * ) "Reading 'index' file at ", index_path

    !
    ! Open the index file for the data source for this field, and read in metadata specs
    !
    do funit=10,100
      inquire(unit=funit, opened=is_used)
      if (.not. is_used) exit
    end do
    open(funit,file=trim(index_path),form='formatted',status='old',iostat=iostatus)

    ! TODO add optional to control when not found
    if (iostatus /= 0 ) then
      write( *, * ) 'Could not open ', trim(index_path)
    end if

    do
      read( funit, '(a)', iostat=iostatus ) line_buffer
      if ( iostatus /= 0 ) then
        if ( iostatus == iostat_end ) exit
      end if

      ! Process line
      ! write( *, * ) "Line : ", trim(line_buffer)
      kv_exist = get_kv_pair( line_buffer, key, var )
      if ( kv_exist ) then
        ! write( *, * ) "Found kv ", key, " = ", var
        if ( key == "projection"      ) index_data % projection      = var
        if ( key == "source_type"     ) index_data % source_type     = var
        if ( key == "signed" .and. var == "yes" ) index_data % signed = .true.
        if ( key == "units"           ) index_data % units           = var
        if ( key == "description"     ) index_data % description     = var
        if ( key == "dx"              ) read( var, * ) index_data % dx
        if ( key == "dy"              ) read( var, * ) index_data % dy
        if ( key == "known_x"         ) read( var, * ) index_data % known_x
        if ( key == "known_y"         ) read( var, * ) index_data % known_y
        if ( key == "known_lat"       ) read( var, * ) index_data % known_lat
        if ( key == "known_lon"       ) read( var, * ) index_data % known_lon
        if ( key == "stdlon"          ) read( var, * ) index_data % stdlon
        if ( key == "truelat1"        ) read( var, * ) index_data % truelat1
        if ( key == "truelat2"        ) read( var, * ) index_data % truelat2
        if ( key == "wordsize"        ) read( var, * ) index_data % wordsize
        if ( key == "tile_x"          ) read( var, * ) index_data % tile_x
        if ( key == "tile_y"          ) read( var, * ) index_data % tile_y
        if ( key == "tile_z"          ) read( var, * ) index_data % tile_z
        if ( key == "tile_z_start"    ) read( var, * ) index_data % tile_z_start
        if ( key == "tile_z_end"      ) read( var, * ) index_data % tile_z_end
        if ( key == "category_min"    ) read( var, * ) index_data % category_min
        if ( key == "category_max"    ) read( var, * ) index_data % category_max
        if ( key == "tile_bdr"        ) read( var, * ) index_data % tile_bdr
        if ( key == "missing_value"   ) read( var, * ) index_data % missing_value
        if ( key == "scale_factor"    ) read( var, * ) index_data % scale_factor
        if ( key == "row_order"       ) index_data % row_order       = var
        if ( key == "endian"          ) index_data % endian          = var
        if ( key == "iswater"         ) read( var, * ) index_data % iswater
        if ( key == "islake"          ) read( var, * ) index_data % islake
        if ( key == "isice"           ) read( var, * ) index_data % isice
        if ( key == "isurban"         ) read( var, * ) index_data % isurban
        if ( key == "isoilwater"      ) read( var, * ) index_data % isoilwater
        if ( key == "mminlu"          ) index_data % mminlu          = var
        if ( key == "filename_digits" ) read( var, * ) index_data % filename_digits
      end if
    end do
  end subroutine

  logical function get_kv_pair( line, key, var ) result( found )
    implicit none
    character (len=*), intent( in )    :: line
    character (len=*), intent( inout ) :: key, var
    integer                            :: idx, idx_comment
    ! logical, intent( out )             :: found

    found = .false.

    idx         = index( line, "=" )
    idx_comment = index( line, "#" )

    if ( idx == 0 ) return

    if ( idx_comment /= 0 .and. idx_comment < idx ) then
      ! We have a comment before our = and that is not good
      write( *, * ) "Key-value parse error, comment before = "
      return
    end if

    if ( idx_comment == 0 ) idx_comment = len( line )

    ! We are fine to parse
    key = trim(line(1:idx-1))
    var = trim(line(idx+1:idx_comment))
    found = .true.

  end function get_kv_pair

end module source_data_module

program compute_gwdo
  use netcdf
  use nc_helpers
  use source_data_module
  use gridinfo_module

  implicit none
  character (len=128), parameter  :: geo_em_prefix = "geo_em.d"
  character (len=128)             :: geo_em_file, tmp
  integer                         :: i, j, k, l
  integer                         ::  ierr, ncid, ndims, dim1, dim2, dim3
  integer, dimension(nf90_max_var_dims) :: dimids
  integer                         :: start_i, end_i, start_j, end_j, start_k, end_k
  integer                         :: mapfacxid, mapfacyid
  integer                         :: latvarid, lonvarid
  integer                         :: convarid, varvarid
  integer, dimension(4)           :: olvarid, oavarid

  ! I will probably need more
  real, dimension(:,:,:), allocatable :: con_data
  real, dimension(:,:,:), allocatable :: lats, lons

  real                                :: cur_lat, cur_lon
  type( source_index_raw )            :: index_data


  

  call get_grid_params()

  write( *, * ) "Found geogrid   : ", n_domains
  write( *, * ) "Using data from : ", geog_data_path

  write( tmp, "(a)" ) trim(geog_data_path) // "topo_gmted2010_30s/"
  call read_index_file( tmp, index_data )

  write( *, * ) "Source data dx / dy : ", index_data % dx, ", ", index_data % dy

  do i = 1, n_domains
    write( geo_em_file, "(A8,I0.2,A3)" ) trim( geo_em_prefix ), i, ".nc"
    write( *, * ) "Processing ", geo_em_file
    ierr = nf90_open( geo_em_file, NF90_WRITE, ncid )
    call nf90_check_error( ierr )

    ierr = nf90_inq_varid( ncid, "XLAT_M", latvarid )
    call nf90_check_error( ierr )

    ierr = nf90_inq_varid( ncid, "XLONG_M", lonvarid )
    call nf90_check_error( ierr )

    ierr = nf90_inq_varid( ncid, "MAPFAC_MX", mapfacxid )
    call nf90_check_error( ierr )

    ierr = nf90_inq_varid( ncid, "MAPFAC_MY", mapfacyid )
    call nf90_check_error( ierr )

    ierr = nf90_inquire_variable( ncid, lonvarid, ndims = ndims, dimids = dimids )
    call nf90_check_error( ierr )

    write( *, * ) "Variable CLONG has ", ndims, " dims"

    ierr = nf90_inq_varid( ncid, "CON", convarid )
    call nf90_check_error( ierr )

    ierr = nf90_inq_varid( ncid, "VAR", varvarid )
    call nf90_check_error( ierr )

    do j = 1, 4
      write( tmp, "(A2,I1)" ) "OL", j
      ierr = nf90_inq_varid( ncid, tmp, olvarid(j) )
      write( tmp, "(A2,I1)" ) "OA", j
      ierr = nf90_inq_varid( ncid, tmp, oavarid(j) )
    end do

    ierr = nf90_inquire_dimension( ncid, dimids(1), len = dim1 )
    ierr = nf90_inquire_dimension( ncid, dimids(2), len = dim2 )
    ierr = nf90_inquire_dimension( ncid, dimids(3), len = dim3 )

    write( *, * ) "Dimensions sizes are ", dim1, ", ", dim2, ", ", dim3

    allocate( con_data( dim1, dim2, dim3 ) )
    allocate( lats( dim1, dim2, dim3 ) )
    allocate( lons( dim1, dim2, dim3 ) )

    ierr = nf90_get_var( ncid, latvarid, lats )
    ierr = nf90_get_var( ncid, lonvarid, lons )

    ! Just stuff for now
    do j = 1, dim1
      do k = 1, dim2
        do l = 1, dim3
          con_data(j,k,l) = 3.1415926535
        end do
      end do
    end do

    ! Just stuff for now
    do j = 1, dim1
      do k = 1, dim2
        do l = 1, dim3
          cur_lat = lats(j,k,l)
          cur_lon = lons(j,k,l)
          ! write( *, * ) "Current LL : ", cur_lat, ", ", cur_lon
        end do
      end do
    end do

    ierr = nf90_put_var( ncid, convarid, con_data )
    call nf90_check_error( ierr )


    ierr = nf90_close( ncid )
    call nf90_check_error( ierr )

  end do

  return
end program compute_gwdo

