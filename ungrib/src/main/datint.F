subroutine datint(fuldates, nful, hstart, ntimes, interval, out_format)
!                                                                             !
!*****************************************************************************!
!                                                                             !
!   interpolate missing data in time
!    out_format: requested output format
!                                                                             !
!*****************************************************************************!
  use gridinfo
  use storage_module
  implicit none
  integer :: nful
  integer :: interval
  character(len=*), dimension(nful) :: fuldates
  character(len=*) :: hstart
  integer :: ntimes

  character(len=24) :: hdate = "0000-00-00_00:00:00.0000"
  character(len=24) :: hdate_output, jdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: desc
  character(LEN=3)  :: out_format
  real :: xfcst

  real :: level
  real, allocatable, dimension(:,:) :: scr2d, bfr2d
  integer :: iful, intervala, intervalb, ifv
  real :: awt
  integer :: itime

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for seconds.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  end if

  write(*, '(/,10("*"), /, "Subroutine DATINT:",/,2x,  &
       &    "Interpolating 3-d files to fill in any missing data...",/, &
       &    10("*")/)')

  TIMELOOP : do itime = 1, ntimes
     call geth_newdate(hdate(1:19), hstart(1:19), (itime-1)*interval)
     write(*, '(/,2x,"Looking for data at time ", A19)') hdate(1:datelen)//"      "
     do iful = 1, nful
        if (fuldates(iful).eq.hdate) then
           write(*, '(/, 10x, "Found file:      FILE:", A19)') hdate(1:datelen)//"      "
           cycle TIMELOOP
        else if ((fuldates(iful).lt.hdate) .and. &
             (fuldates(iful+1).gt.hdate) )then

           write(*,'(/, 10x,"Found surrounding files:      FILE:",& 
                &  A19,2x,"FILE:",A19)') fuldates(iful)(1:datelen)//"      ", &
                fuldates(iful+1)(1:datelen)//"      "
           write(*, '(10x, "Interpolating to create file:  FILE:", A19,/)') &
              &  hdate(1:datelen)//"      "
           call geth_idts(hdate(1:19), fuldates(iful)(1:19), intervalA)
           write(*,'(15x, "A Time Difference = ", F6.2, " hours.")') &
                float(intervalA) / 3600.
           call geth_idts(fuldates(iful+1)(1:19), hdate(1:19), intervalB)
           write(*,'(15x, "B Time Difference = ", F6.2, " hours.")') &
                float(intervalB) / 3600.
           AWT = 1. - (float(intervalA)/float(intervalA+intervalB))

           open(10, file='FILE:'//fuldates(iful)(1:datelen), form='unformatted', &
                status='old')
           call clear_storage
           READLOOP1 : do
	      if ( out_format(1:2) .eq. 'SI' ) then
              read(10, end=44) ifv
              read (10) jdate, xfcst, map%source, field, units, desc, level, map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
	      else if ( out_format(1:2) .eq. 'WP' ) then
              read(10, end=44) ifv
              read (10) jdate, xfcst, map%source, field, units, desc, level, map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
	      else if ( out_format(1:2) .eq. 'MM' ) then
              read(10, end=44) ifv
              read(10) jdate, xfcst, field, units, desc, level,&
                   map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              elseif (map%igrid.eq.0)then ! lat/lon
                 read (10) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid.eq.1)then ! Mercator
                 read (10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              else
                 write(*,'("Unrecognized map%igrid: ", I20)') map%igrid
                 stop 'DATINT'
              endif
	      else
	        write(6,*) 'unknown out_format'
		stop 'datint'
              endif
              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              call put_storage(nint(level), field, scr2d, map%nx, map%ny)
              deallocate(scr2d)
           enddo READLOOP1
44         close(10)

           open(10, file='FILE:'//fuldates(iful+1)(1:datelen), status='old', &
                form = 'unformatted')
           open(11, file='FILE:'//hdate(1:datelen), status='new', form='unformatted')
           READLOOP2 : do
	      if ( out_format(1:2) .eq. 'SI' ) then
              read (10,END=45) ifv
              read (10) jdate, xfcst, map%source, field, units, desc, level, &
                      map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
              else if ( out_format(1:2) .eq. 'WP' ) then
              read (10,END=45) ifv
              read (10) jdate, xfcst, map%source, field, units, desc, level, &
                      map%nx, map%ny, map%igrid
              if (map%igrid == 0) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid == 1) then
                 read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              elseif (map%igrid == 3) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
              elseif (map%igrid == 5) then
                 read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
              else
                 print*, 'Unrecognized map%igrid: ', map%igrid
                 stop "STOP IN DATINT"
              endif
              else if ( out_format(1:2) .eq. 'MM' ) then
              read(10, end=45) ifv
              read(10) jdate, xfcst, field, units, desc, level,&
                   map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              elseif (map%igrid.eq.0)then ! lat/lon
                 read (10) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid.eq.1)then ! Mercator
                 read (10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              else
                 write(*,'("Unrecognized map%igrid: ", I20)') map%igrid
                 stop 'DATINT'
              endif
              else
                write(6,*) 'unknown out_format'
                stop 'datint'
              endif
              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              if (is_there(nint(level), field)) then
                 allocate(bfr2d(map%nx,map%ny))
                 call get_storage(nint(level), field, bfr2d, map%nx, map%ny)
                 scr2d = bfr2d * (AWT) + scr2d * (1.-AWT)
                 hdate_output = hdate
		 if (out_format(1:2) .eq. 'SI') then
                 write(11) ifv
                 write(11) hdate_output, xfcst, map%source, field, units, desc, &
                      level, map%nx, map%ny, map%igrid
                 if (map%igrid == 0) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid == 1) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 elseif (map%igrid == 3) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1, map%truelat2
                 elseif (map%igrid == 5) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1
                 else
                    print*, 'Unrecognized map%igrid: ', map%igrid
                    stop "STOP IN DATINT"
                 endif
		 else if (out_format(1:2) .eq. 'WP') then
                 write(11) ifv
                 write(11) hdate_output, xfcst, map%source, field, units, desc, &
                      level, map%nx, map%ny, map%igrid
                 if (map%igrid == 0) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid == 1) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 elseif (map%igrid == 3) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1, map%truelat2
                 elseif (map%igrid == 5) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1
                 else
                    print*, 'Unrecognized map%igrid: ', map%igrid
                    stop "STOP IN DATINT"
                 endif
		 else if (out_format(1:2) .eq. 'MM') then
                 write (11) ifv
                 write (11) hdate_output, xfcst, field, units, Desc, level,&
                   map%nx, map%ny, map%igrid
                 if (map%igrid.eq.3) then ! lamcon
                   write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
                 elseif (map%igrid.eq.5) then ! Polar Stereographic
                   write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
                 elseif (map%igrid.eq.0)then ! lat/lon
                   write (11) map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid.eq.1)then ! Mercator
                   write (11) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 else
                   write(*,'("Unrecognized map%igrid: ", I20)') map%igrid
                   stop 'DATINT'
                 endif
                 endif
                 write(11) scr2d
              else
                 print*, 'hdate = ', hdate
                 print*, 'Problem:  ', fuldates
                 print*, 'Field = ', field
                 stop
              endif
              deallocate(scr2d, bfr2d)
           enddo READLOOP2
45         close(10)
           close(11)
           cycle TIMELOOP
        endif
     enddo

     print*, 'Data not found: ', hdate
     stop
     
  enddo TIMELOOP

  write(*, '(/,10("*"), /, "End Subroutine DATINT.",/,  10("*")/)')

end subroutine datint
