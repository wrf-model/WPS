module storage_module
  use gridinfo
  integer, parameter :: NAMLEN = 9
  character (len=NAMLEN) :: name
  character (len=NAMLEN) :: blank = '                                                  '
  type data
     character (LEN=NAMLEN) :: id
     type (mapinfo) :: data_map
     real, dimension(:,:), pointer :: data_array
     type(data), pointer :: next_data
  end type data
  type level_node
     integer :: p_level
     type(data), pointer :: datalev
     type(level_node), pointer :: next_level
  end type level_node

  type(level_node), pointer :: base

contains

  subroutine clear_storage

    type(data), pointer :: cdata, ldata
    type(level_node), pointer :: current, llevel
    logical :: first_call = .TRUE.

!  print*, 'CLEAR_STORAGE'

    if (first_call) then
       nullify(base)
       allocate(base)
       base%p_level = 300000
       nullify(base%next_level)
       nullify(base%datalev)
       first_call = .FALSE.
    else

       levloop : do

          ! Bop down to the deepest level, saving a pointer to the next highest level.
          current => base
          nullify(llevel)
          LLOOP : do while (associated(current%next_level))
             llevel => current
             current => current%next_level
          enddo LLOOP
          if (current%p_level .eq. 300000) exit levloop


          fieldloop : do
             ! Now bop over to the lowest field, saving a pointer to the next higher field.
             cdata => current%datalev
             nullify(ldata)
             CLOOP : do while (associated(cdata%next_data))
                ldata => cdata
                cdata => cdata%next_data
             enddo CLOOP
             if (associated(ldata)) then 
                ! This is not the last field for this level
                deallocate(cdata%data_array)
                nullify(cdata%data_array)
                nullify(cdata%next_data)  
                deallocate(cdata)
                nullify(cdata)
                deallocate(ldata%next_data)
                nullify(ldata%next_data)
                cdata => ldata
                deallocate(ldata)
                nullify(ldata)
                cycle fieldloop
             else
                ! This is the last field for this level
                deallocate(cdata%data_array)
                nullify(cdata%data_array)
                nullify(cdata%next_data)
                deallocate(cdata)
                nullify(cdata)
                nullify(current%next_level)
                deallocate(current%datalev)
                nullify(current%datalev)
                deallocate(current)
                nullify(current)
                deallocate(llevel%next_level)
                nullify(llevel%next_level)
                exit fieldloop
             endif
          enddo fieldloop
          if (llevel%p_level.eq.300000) exit levloop
       enddo levloop

       deallocate(base)
       nullify(base)
       allocate(base)
       nullify(base%next_level)
       nullify(base%datalev)
       base%p_level = 300000

    endif


  end subroutine clear_storage

  subroutine put_storage(pres, namein, scr2d, ix, jx)
    implicit none

! Input:
    integer :: pres
    character(LEN=*) :: namein
    integer :: ix, jx
    real, dimension(ix,jx) :: scr2d

    type(level_node), pointer :: current
    type(data), pointer  :: cdata

    name = namein//blank

    current => base

    LEVLLOOP : do

       if (current%p_level .eq. pres) then
          cdata => current%datalev
          DATALOOP : do
             if (cdata%id.eq.name) then
                print*, 'Overwriting ', name, ' at level ', pres
                exit LEVLLOOP
             endif
             if (associated(cdata%next_data)) then
                cdata => cdata%next_data
             else
                allocate(cdata%next_data)
                cdata => cdata%next_data
                nullify(cdata%next_data)
                exit LEVLLOOP
             endif
          enddo DATALOOP

       endif
       if (associated(current%next_level)) then
          current => current%next_level
       else
          allocate(current%next_level)
          current => current%next_level
          nullify(current%next_level)
          allocate(current%datalev)
          cdata => current%datalev
          nullify(cdata%next_data)
          exit LEVLLOOP
       endif
    enddo LEVLLOOP

    current%p_level = pres
    cdata%data_map = map
    cdata%id = name
    allocate (cdata%data_array(ix,jx))
    cdata%data_array = scr2d

  end subroutine put_storage

  subroutine get_storage(pres, namein, scr2d, ix, jx)
    type(level_node), pointer :: current
    type(data), pointer :: cdata
    integer :: pres
    character (len=*) :: namein
    real, dimension(ix,jx) :: scr2d
    name = namein//blank
    current => base

    do while (associated(current))
       if (current%p_level.eq.pres) then
          cdata => current%datalev
          do while (associated(cdata))
             if (cdata%id.eq.name) then
                if ((ix.ne.cdata%data_map%nx).or.(jx.ne.cdata%data_map%ny)) then
                   print*, 'Storage dimensions don''t match input dimensions:'
                   print*, 'Storage dimensions = ', cdata%data_map%nx, cdata%data_map%ny
                   print*, 'input dimensions = ', ix, jx
                   print*, 'Subroutine GET_STORAGE'
                     stop
                endif
                scr2d = cdata%data_array
                map = cdata%data_map
                return
             endif
             cdata => cdata%next_data
          enddo
          print*, 'Could not find ', name, ' at level ', current%p_level, pres
          print*, 'Subroutine GET_STORAGE'
            stop
       endif
       current => current%next_level
    enddo
    print*, 'Could not find ', pres
    print*, 'Subroutine GET_STORAGE'
    stop

  end subroutine get_storage
  subroutine get_dims(pres, namein)
    type(level_node), pointer :: current
    type(data), pointer :: cdata
    integer :: pres
    character (len=*) :: namein
    name = namein//blank
    current => base

    do while (associated(current))
       if (current%p_level.eq.pres) then
          cdata => current%datalev
          do while (associated(cdata))
             if (cdata%id.eq.name) then
                map = cdata%data_map
                return
             endif
             cdata => cdata%next_data
          enddo
          print*, 'Could not find ', name, ' at level ', current%p_level, pres
          print*, 'Subroutine GET_STORAGE'
          stop
       endif
       current => current%next_level
    enddo
    print*, 'Could not find ', pres
    print*, 'Subroutine GET_DIMS'
    stop

  end subroutine get_dims

  subroutine print_storage
    type(level_node), pointer :: current
    type(data), pointer :: cdata
    print*, 'PRINT_STORAGE-------------------------------------------------------------'
    print*
    current => base

    do while (associated(current))
       print*, 'p_level = ', current%p_level
       cdata => current%datalev

       do while (associated(cdata))
          print*, '     data: ', cdata%id, &
               cdata%data_array(1,1)
          cdata => cdata%next_data
       enddo

       current => current%next_level

    enddo
    print*
    print*, 'DONE PRINT STORAGE --------------------------------------------------------'
  end subroutine print_storage

  subroutine get_plvls(plvl, maxlvl, nlvl)
    implicit none
    integer maxlvl, nlvl
    real, dimension(maxlvl) :: plvl
    type(level_node), pointer :: current
    integer nn

    current => base
    nlvl = 0
    plvl = -999.
    CURLOOP : do while (associated(current))
       if (current%p_level .ne. 300000) then
          nlvl = nlvl + 1
          LEVLOOP : do nn = 1, nlvl
             if (current%p_level.gt.plvl(nn)) then
                plvl(nn+1:maxlvl) = plvl(nn:maxlvl-1)
                plvl(nn) = float(current%p_level)
                exit LEVLOOP
             endif
          enddo LEVLOOP
       endif
       current => current%next_level
    enddo CURLOOP

  end subroutine get_plvls

  logical function is_there(pres, namein)
    type(level_node), pointer :: current
    type(data), pointer :: cdata
    integer :: pres
    character (len=*) :: namein

    name = namein//blank

    current => base

    do while (associated(current))
       if (current%p_level.eq.pres) then
          cdata => current%datalev
          do while (associated(cdata))
             if (cdata%id.eq.name) then
                is_there = .TRUE.
                return
             endif
             cdata => cdata%next_data
          enddo
          is_there = .FALSE.
          return
       endif
       current => current%next_level
    enddo
    is_there = .FALSE.
  end function is_there

end module storage_module
