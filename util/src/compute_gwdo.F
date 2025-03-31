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
    character (len=256):: source_path      = "required"
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

    write( index_data % source_path, "(a)" ) trim(source_path)
    write( index_path, "(a)" ) trim(source_path) // 'index'
    write( *, * ) "Reading 'index' file at ", index_data % source_path

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
      write( *, * ) 'Could not open ', trim(index_data % source_path)
    end if

    do
      read( funit, '(a)', iostat=iostatus ) line_buffer
      if ( iostatus /= 0 ) then
        if ( iostatus == iostat_end ) exit
      end if

      ! Process line iff a key-value pair was found
      kv_exist = get_kv_pair( line_buffer, key, var )
      if ( kv_exist ) then
        ! consider multiline else if even if not neater
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
    key = trim(adjustl(line(1:idx-1)))
    var = trim(adjustl(line(idx+1:idx_comment)))
    found = .true.

  end function get_kv_pair

  integer function get_proj_code( index_data ) result( proj_code )
    use misc_definitions_module
    implicit none
    type( source_index_raw ), intent( in ) :: index_data
    if      ( index_data % projection == "lambert"      ) then; proj_code = PROJ_LC
    else if ( index_data % projection == "polar_wgs84"  ) then; proj_code = PROJ_PS_WGS84
    else if ( index_data % projection == "albers_nad83" ) then; proj_code = PROJ_ALBERS_NAD83
    else if ( index_data % projection == "polar"        ) then; proj_code = PROJ_PS
    else if ( index_data % projection == "mercator"     ) then; proj_code = PROJ_MERC
    else if ( index_data % projection == "regular_ll"   ) then; proj_code = PROJ_LATLON
    else; write( *, * ) "ERROR: Unknown projection '", index_data % projection, "'"
    end if
  end function get_proj_code


  ! Taken from the souce_data_module of geogrid, but simplified to take stateful index data 
  subroutine get_tile_fname(index_data, lat, lon, tile_fname, istatus)

    use llxy_module
    use gridinfo_module

    implicit none

    type( source_index_raw ), intent( in ) :: index_data
    real, intent(in) :: lat, lon
    character (len=*), intent(out) :: tile_fname
    integer, intent(out) :: istatus

    ! Local variables
    integer :: current_domain, idx
    real :: rx, ry

    istatus = 0
    write(tile_fname, '(a)') 'TILE.OUTSIDE.DOMAIN'

    ! do idx=1,num_entries
    !   if ((index(source_fieldname(idx),trim(fieldname)) /= 0) .and. &
    !       (len_trim(source_fieldname(idx)) == len_trim(fieldname))) then
    !     if (ilevel == source_priority(idx)) then
    !       istatus = 0
    !       exit
    !     end if
    !   end if
    ! end do

    ! if (istatus /= 0) return

    current_domain = iget_selected_domain()
    call select_domain(SOURCE_PROJ)
    call lltoxy(lat, lon, rx, ry, M) 
    call select_domain(current_domain)

    !      rx = real(source_tile_x(idx)) * real(floor((rx-0.5*source_dx(idx))/ real(source_tile_x(idx)))) + 1.0
    !      ry = real(source_tile_y(idx)) * real(floor((ry-0.5*source_dy(idx))/ real(source_tile_y(idx)))) + 1.0
    rx = real( index_data % tile_x ) * real(floor((rx-0.5) / real( index_data % tile_x ))) + 1.0
    ry = real( index_data % tile_y ) * real(floor((ry-0.5) / real( index_data % tile_y ))) + 1.0

    if (rx > 0. .and. ry > 0.) then
      select case(index_data % filename_digits)
        case (5)
          write(tile_fname, '(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim( index_data % source_path ), &
          nint(rx),'-',nint(rx)+index_data % tile_x -1,'.',nint(ry),'-',nint(ry)+index_data % tile_y -1
        case (6)
          write(tile_fname, '(a,i6.6,a1,i6.6,a1,i6.6,a1,i6.6)') trim( index_data % source_path ), &
          nint(rx),'-',nint(rx)+index_data % tile_x -1,'.',nint(ry),'-',nint(ry)+index_data % tile_y -1
        case default
          call mprintf(.true., ERROR, 'In GEOGRID.TBL, for index file of data at '// &
          'entry %i, filename_digits must be either 5 or 6.', i1=idx)
          istatus = 1
          return
      end select
    end if

  end subroutine get_tile_fname
end module source_data_module

program compute_gwdo
  use netcdf
  use nc_helpers
  use source_data_module
  use gridinfo_module
  use llxy_module

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

  ! For now set the source data projection here
  call push_source_projection( get_proj_code( index_data ), index_data % stdlon, &
                               index_data % truelat1, index_data % truelat2,     &
                               index_data % dx, index_data % dy,                 &
                               index_data % dy, index_data % dx,                 &
                               index_data % known_x, index_data % known_y,       &
                               index_data % known_lat, index_data % known_lon )

  

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

    ! Find correct file and location within file to grab


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
          call get_tile_fname( index_data, cur_lat, cur_lon, tmp, ierr )
          if ( j == 10 .and. k == 10 ) then
            write( *, * ) "Filename for [", i, ", ", j, ", ", k, "] at LL [", cur_lat, ", ", cur_lon, "] :", tmp 
          end if
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

