! Print information about a grib file.
!  Usage: "gribscan [-v] [-V] filename"
! 
program gribscan
  use module_grib
  interface
     subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2,&
          a3, h3, i3, l3, hlast)
       integer :: err
       character(len=*) , optional :: a1, a2, a3
       character(len=*), optional :: h1, h2, h3
       integer , optional :: i1, i2, i3
       logical, optional :: l1, l2, l3
       character(len=*), optional :: hlast
     end subroutine parse_args
  end interface

  character(len=80) :: flnm
  character(len=30) :: hopt
  real, allocatable, dimension(:) :: datarray
  integer :: ierr, igherr
  integer :: cc
  character(len=100) :: fmt = '(I4,1X, &
       & I3,1x, &
       & I4, &
       & 2(1x,I4),2x,I4.4,2("-",I2.2),"_",I2.2,":",&
       & I2.2, 1x, "+", i2.2)'
  logical :: ivb = .FALSE.
  logical :: idb = .FALSE.
  integer :: year

  flnm = ' '
  call parse_args(ierr, a1='v', l1=ivb, a2='V', l2=idb, hlast=flnm)
  if (ierr.ne.0) then
     call getarg(0, hopt)
     write(*,'(//,"Usage: ", A, " [-v] [-V] file",/)') trim(hopt)
     write(*,'("     -v   : Print more information about the GRIB records")')
     write(*,'("     -V   : Print way too much information about the GRIB&
          & records")')
     write(*,'("     file : GRIB file to read"//)')
      stop
!    stop
  endif

  if (idb) ivb = .TRUE.

  call copen(idum, munit, flnm, 1, ierr, 1)

  if (.not. ivb) then
     write(*,'(50("-"))')
     write(*,'(" rec GRIB Lvl  Lvl  Lvl         Time      Fcst")')
     write(*,'(" Num Code Code one  two                   hour")')
     write(*,'(50("-"))')
  endif 

  irec = 0
  call gribget(munit, ierr)
  do while (ierr.eq.0) 
     irec = irec + 1
     call gribheader(igherr)
     if (igherr /= 0) then
        call deallogrib
        call gribget(munit, ierr)
        cycle
     endif

     if (ivb) then
        call gribprint(0)
        call gribprint(1)
        call gribprint(2)
        call gribprint(3)
        call gribprint(4)
        write(*,'(//,70("*"))')
        if (idb) then
           if (sec2(4).eq.50) then
              ndat = (infogrid(1)+1)*(infogrid(2)+1)
           else
              ndat = (infogrid(1)*infogrid(2))
           endif
           allocate(datarray(ndat))
           call gribdata(datarray, ndat)
           print*, 'Datarray = ', Datarray
           deallocate(datarray)
        endif
     else
        CC = sec1(22)
        year = (cc-1)*100 + sec1(11)
        write(*,FMT) irec, sec1(7), sec1(8:10), year,sec1(12:15),sec1(17)
     endif

     call deallogrib

     call gribget(munit, ierr)
  enddo
  if (ierr.eq.1) write(*,'(/,"***** End-Of-File on C unit ", I3,/)') munit
  call cclose( munit, 0, ierr)

end program gribscan

subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2, a3, h3, i3, l3, &
     hlast)
  integer :: err
  character(len=*) , optional :: a1, a2, a3
  character(len=*), optional :: h1, h2, h3
  integer , optional :: i1, i2, i3
  logical, optional :: l1, l2, l3
  character(len=*), optional :: hlast

  character(len=100) :: hold
  integer :: ioff = 0

  if (present(hlast)) then
     ioff = -1
  endif

  err = 0

  narg = iargc()
  numarg = narg + ioff

  i = 1
  LOOP : do while ( i <= numarg)

     ierr = 1
     if (present(i1)) then
        call checkiarg(i, a1, i1, ierr)
     elseif (present(h1)) then
        call checkharg(i, a1, h1, ierr)
     elseif (present(l1)) then
        call checklarg(i, a1, l1, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i2)) then
        call checkiarg(i, a2, i2, ierr)
     elseif (present(h2)) then
        call checkharg(i, a2, h2, ierr)
     elseif (present(l2)) then
        call checklarg(i, a2, l2, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i3)) then
        call checkiarg(i, a3, i3, ierr)
     elseif (present(h3)) then
        call checkharg(i, a3, h3, ierr)
     elseif (present(l3)) then
        call checklarg(i, a3, l3, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     err = 1
     call getarg(1, hold)
     write(*, '("arg = ", A)') trim(hold)

     exit LOOP

  enddo LOOP

  if (present(hlast)) then
     if (narg.eq.0) then
        err = 1
     else
        call getarg(narg, hlast)
     endif
  endif

contains
  subroutine checkiarg(c, a, i, ierr)
    integer :: c
    character(len=*) :: a
    integer :: i

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       read(hold, *) i
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       read(hold, *) i
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkiarg
  subroutine checkharg(c, a, h, ierr)
    integer :: c
    character(len=*) :: a
    character(len=*) :: h

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       h = trim(hold)
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       h = trim(hold)
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkharg

  subroutine checklarg(c, a, l, ierr)
    integer :: c
    character(len=*) :: a
    logical :: l

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)
    if ('-'//a.eq.trim(hold)) then
       l = .TRUE.
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checklarg

end subroutine parse_args


