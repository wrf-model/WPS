module format_module
  implicit none
  type minfo
     integer :: igrid
     integer :: nx
     integer :: ny
     real :: truelat1
     real :: truelat2
     real :: lov
     real :: lat1
     real :: lon1
     real :: dx
     real :: dy
  end type minfo
  type outblock
     integer :: iformat = 3
     character(len=24) :: hdate
     real :: xfcst
     character(len=9) :: field
     character(len=25) :: Units
     character(len=46) :: Desc
     real :: level
     integer :: nx
     integer :: ny
     type(minfo) :: map
     real, pointer, dimension(:,:) :: data
     
  end type outblock
contains
!
!============================================================================
!
  subroutine build_block(block, hdate, xfcst, field, units, desc, xlvl, map, data, &
       idim, jdim)
    implicit none
    type(outblock) :: block
    character(len=*) :: hdate, field, units, desc
    real :: xlvl, xfcst
    type(minfo) :: map
    integer :: idim, jdim
    real, dimension(idim, jdim) :: data
    
    block%hdate = trim(hdate)
    block%xfcst = xfcst
    block%field = trim(field)
    block%units = trim(units)
    block%desc  = trim(desc)
    block%level = xlvl
    block%map   = map
    allocate(block%data(idim,jdim))
    block%data  = data
  end subroutine build_block
!
!============================================================================
!
  subroutine write_block(block)
    implicit none
    type(outblock) :: block
    integer :: iunit
    write (iunit) block%iformat
    write (iunit) block%hdate, block%xfcst, block%field, block%units, block%Desc, &
                  block%level, block%map%nx, block%map%ny, block%map%igrid
    if (block%map%igrid.eq.3) then ! lamcon
       write (iunit) block%map%lat1, block%map%lon1, block%map%dx, block%map%dy,&
                     block%map%lov, block%map%truelat1, block%map%truelat2
    elseif (block%map%igrid.eq.0)then ! lat/lon
       write (iunit) block%map%lat1, block%map%lon1, block%map%dy, block%map%dx
    endif
    write (iunit) block%data
  end subroutine write_block
!
!============================================================================
!
  subroutine read_block(iunit, block)
    implicit none
    integer :: iunit
    type(outblock) :: block
    read (iunit) block%iformat
    read (iunit) block%hdate, block%xfcst, block%field, block%units, block%Desc, &
                 block%level, block%map%nx, block%map%ny, block%map%igrid
    if (block%map%igrid.eq.3) then ! lamcon
       read (iunit) block%map%lat1, block%map%lon1, block%map%dx, block%map%dy,&
                    block%map%lov, block%map%truelat1, block%map%truelat2
    elseif (block%map%igrid.eq.0)then ! lat/lon
       read (iunit) block%map%lat1, block%map%lon1, block%map%dy, block%map%dx
    endif
    read (iunit) block%data
  end subroutine read_block
end module format_module

