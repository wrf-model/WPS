!*****************************************************************************!
! Subroutine RD_GRIB1                                                         !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB file.  Based on the information in   !
!    the GRIB header and the user-defined Vtable, decide whether the field in !
!    the GRIB record is one to process or to skip.  If the field is one we    !
!    want to keep, extract the data from the GRIB record, and pass the data   !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       IUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB     !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the     !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIBFLNM: File name to open, if it is not already open.               !
!       IUARR   : Array to hold UNIX File descriptors retured from a C open   !
!                 statement.  If the value of IUARR(IUNIT) is zero, then the  !
!                 file GRIBFLNM must be opened, and the value of IUARR(IUNIT) !
!                 becomes the UNIX File descriptor to read from.              !
!       DEBUG_LEVEL Integer for various amounts of printout.                    !
!                                                                             !
!    Output:                                                                  !
!       LEVEL    : The pressure-level (Pa) of the field to process.           !
!       FIELD    : The field name of the field to process.  NULL is returned  !
!                  if we do not want to process the field we read.            !
!       HDATE    : The 19-character date of the field to process.             !
!       IERR     : Error flag: 0 - no error on read from GRIB file.           !
!                              1 - Hit the end of the GRIB file.              !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
! Externals                                                                   !
!     Module TABLE                                                            !
!     Module GRIDINFO                                                         !
!     Subroutine COPEN                                                        !
!     Subroutine DEALLOGRIB                                                   !
!     Subroutine GRIBGET                                                      !
!     Subroutine GRIBHEADER                                                   !
!     Subroutine GET_SEC1                                                     !
!     Subroutine GET_SEC2                                                     !
!     Subroutine GET_GRIDINFO                                                 !
!     Subroutine BUILD_HDATE                                                  !
!     Subroutine GETH_NEWDATE                                                 !
!     Subroutine GRIBDATA                                                     !
!                                                                             !
! Side Effects                                                                !
!     File GRIBFLNM is opened, as necessary                                   !
!                                                                             !
!     Variable MAP from module GRIDINFO is filled in.                         !
!                                                                             !
!     Numerous side effects from the GRIB-processing routines.                !
!                                                                             !
! Kevin W. Manning                                                            !
! NCAR/MMM                                                                    !
! Summer, 1998, and continuing                                                !
! SDG                                                                         !
!                                                                             !
!*****************************************************************************!
SUBROUTINE rd_grib1(IUNIT, gribflnm, level, field, hdate,  &
     ierr, iuarr, debug_level)
  use table
  use gridinfo
  use datarray
  use module_debug

  implicit none

  integer :: debug_level
  integer :: iunit ! Array number in IUARR assigned to the C read pointer.
  integer, dimension(100) :: KSEC1
  integer, dimension(10) :: KSEC2 
  integer, dimension(40) :: infogrid
  real, dimension(40) :: ginfo
!
!-----------------------------------------------------------------------
  integer :: iparm, ktype
  logical :: lopen

  integer :: icenter, iprocess, iscan, ii, isb
  integer year, month, day, hour, minute, second, icc, iyy
  integer :: fcst
  real :: level
  character(LEN=*) :: field
  character(LEN=132) :: gribflnm
  character(LEN=8) :: tmp8
  integer, dimension(255) :: iuarr
  integer :: ierr, iostat, nunit
  integer :: i, lvl2, lvl1
  character(LEN=19) :: hdate
  integer :: igherr

  ierr = 0

! If the file GRIBFLNM has not been opened, then IUARR(IUNIT) should be Zero.
! In this case, open the file GRIBFLNM, and store the UNIX File descriptor
! in to IUARR(IUNIT).  This way, we will know what UNIX File descriptor to use
! next time we call this RD_GRIB subroutine.
!
  if (iuarr(iunit).eq.0) then
     if (debug_level.gt.0) then
        call copen(iunit, nunit, gribflnm, 1, ierr,  1)
     else
        call copen(iunit, nunit, gribflnm, 1, ierr, -1)
     endif
     if (ierr.ne.0) then
        call deallogrib
        ierr = 2
        return
     endif
     iuarr(iunit) = nunit
  endif

! Read a single GRIB record, but do no unpacking now:

  call gribget(iuarr(iunit), ierr)

  if (ierr.ne.0) then
     call mprintf(.true.,DEBUG,"RD_GRIB1 gribget read error, ierr = %i",i1=ierr)
     call deallogrib
     return
  endif
!
! Unpack the header information:
!
  call gribheader(debug_level,igherr)
  if (igherr /= 0) then
     field = "NULL"
     call deallogrib
     return
  endif
!
! Copy header information to arrays KSEC1, KSEC2, INFOGRID, and GRIDINFO
!
  call get_sec1(ksec1)
  call get_sec2(ksec2)
  call get_gridinfo(infogrid, ginfo)

  icenter = KSEC1(3)        ! Indicator of the source (center) of the data.
  iprocess = KSEC1(4)       ! Indicator of model (or whatever) which generated the data.

  if (icenter.eq.7) then
    if (iprocess.eq.83 .or. iprocess.eq.84) then
      map%source = 'NCEP NAM Model'
    elseif (iprocess.eq.81) then
      map%source = 'NCEP GFS Model'
    elseif (iprocess.eq.96) then
      map%source = 'NCEP GFS Model'
    elseif (iprocess.eq.109) then
      map%source = 'NCEP RTMA'
    elseif (iprocess.eq.105) then
      map%source = 'NCEP RUC Model'
    elseif (iprocess.eq.140) then
      map%source = 'NCEP NARR'
    elseif (iprocess.eq.44) then
      map%source = 'NCEP SST Analysis'
    else
      map%source = 'unknown model from NCEP'
    end if
!  grid numbers only set for NCEP and AFWA models
    write(tmp8,'("GRID ",i3)') KSEC1(5)
    map%source(25:32) = tmp8
  else if (icenter .eq. 57) then
    if (iprocess .eq. 87) then
      map%source = 'AFWA AGRMET'
    else
      map%source = 'AFWA'
    endif
    write(tmp8,'("GRID ",i3)') KSEC1(5)
    map%source(25:32) = tmp8
  else if (icenter .eq. 98) then
      map%source = 'ECMWF'
  else
    map%source = 'unknown model and orig center'
  end if

  IPARM=KSEC1(7)            ! Indicator of parameter
  KTYPE=KSEC1(8)            ! Indicator of type of level

!   print *,' IPARM, KTYPE, KSEC1(9)', iparm,ktype,ksec1(9)

  IF(KTYPE.EQ.1) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.100) THEN
     LVL1=FLOAT(KSEC1(9))  * 100.
     LVL2=-99
  ELSEIF(KTYPE.EQ.101) THEN
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.102) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.103) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.105) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.107) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.109) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.111) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.112) THEN ! Layer between two depths below surface
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ELSEIF(KTYPE.EQ.113) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.115) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.117) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.119) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.125) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.160) THEN
     LVL1=KSEC1(9)
     LVL2=-99
  ELSEIF(KTYPE.EQ.200) THEN
     LVL1=0
     LVL2=-99
  ELSEIF(KTYPE.EQ.201) THEN
     LVL1=0
     LVL2=-99
  ELSE
     LVL1=KSEC1(9)
     LVL2=KSEC1(10)
  ENDIF

! Check to see that the combination of iparm, ktype, lvl1, and lvl2
! match what has been requested in the Vtable.  If not, set the field
! name to NULL, meaning that we do not want to process this one.

  field = 'NULL'
  do i = 1, maxvar
     if (gcode(i).eq.iparm) then
        if (lcode(i).eq.ktype) then
           if ((level1(i).eq.lvl1) .or. (level1(i) == splatcode) ) then
              if (level2(i).eq.lvl2) then
                 field=namvar(i)
                 if (ktype.eq.100) then ! Pressure-level
                    level=lvl1
                 elseif (ktype.eq.102) then
                    level=201300.
                 elseif ((ktype.eq.116.and.lvl1.le.50.and.lvl2.eq.0) .or. &
                      (ktype.eq.105).or.(ktype.eq.1) .or. &
                      (ktype.eq.111).or.(ktype.eq.112) ) then
                    ! level=200100.
                    level = float(200000+iprty(i))
!-- tanya's change for non-isobaric levels
                 elseif (ktype.eq.109) then
                    level = lvl1
!       print *,'LEVEL=',level
                 endif
              endif
           endif
        endif
     endif
  enddo

  if (field .eq. 'NULL') then
     call deallogrib
     return
  endif

  if ((field.eq.'WEASD').or.(field.eq.'SNOW')) then
     level = level + ksec1(19)+1
  endif

! Build the 19-character date string, based on GRIB header date and time
! information, including forecast time information:

  ICC=KSEC1(22)             ! CENTURY OF THE DATA
  IYY=KSEC1(11)             ! (TWO-DIGIT) YEAR OF THE DATA
  MONTH=KSEC1(12)           ! MONTH OF THE DATA
  DAY=KSEC1(13)             ! DAY OF THE DATA
  HOUR=KSEC1(14)            ! HOUR OF THE DATA
  MINUTE=KSEC1(15)          ! MINUTE OF THE DATA
  SECOND=0
  if (ksec1(19) == 3) then
     FCST = (KSEC1(17) + KSEC1(18))/2
!  TEMPORARY AFWA FIX
!  elseif (ksec1(19) == 4 .or. ksec1(19) == 5) then
   elseif (ksec1(19) == 4 .or. ksec1(19) == 5 .or. ksec1(19) == 7) then
     FCST = KSEC1(18)
  else
     FCST = KSEC1(17)
  endif

  if (IYY.EQ.00) then
     YEAR = ICC*100
  else
     YEAR = (ICC-1)*100 + IYY
  endif

  hdate(1:19) = '                   '
  call build_hdate(hdate,year,month,day,hour,minute,second)

  call geth_newdate(hdate,hdate,3600*fcst)

! Store information about the grid on which the data is. 
! This stuff gets stored in the MAP variable, as defined in module GRIDINFO

  map%startloc = 'SWCORNER'
  map%grid_wind = .true.
! NCEP's grib1 messages say they use 6367.47, but they really use 6371.229. Hardcode it.
  map%r_earth = 6371.229   

  if (ksec2(4).eq.0) then ! Lat/Lon grid
     map%igrid = 0
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%dx = ginfo(8)
     map%dy = ginfo(9)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)
     write(tmp8,'(b8.8)') infogrid(5)
     if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
     if (icenter .eq. 7 .and. KSEC1(5) .eq. 173 ) then  ! correction for ncep grid 173
       map%lat1 = 89.958333
       map%lon1 = 0.041667
       map%dx = 0.083333333 * sign(1.0,map%dx)
       map%dy = 0.083333333 * sign(1.0,map%dy)
     endif

!    print *, "CE map stuff", map%igrid, map%nx, map%ny, map%dx, &
!    map%dy, map%lat1, map%lon1

  elseif (ksec2(4).eq.3) then ! Lambert Conformal Grid
     map%igrid = 3
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%lov = ginfo(6)
     map%truelat1 = ginfo(11)
     map%truelat2 = ginfo(12)
     map%dx = ginfo(7)
     map%dy = ginfo(8)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)
     write(tmp8,'(b8.8)') infogrid(5)
     if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
!    if (tmp8(2:2) .eq. '0') map%r_earth = 6367.47
         
  elseif(ksec2(4).eq.4) then ! Gaussian Grid
     map%igrid = 4
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%dx = ginfo(8)
!    map%dy = ginfo(19)
     map%dy = real (infogrid(9))
     map%lon1 = ginfo(4)
     map%lat1 = ginfo(3)
     write(tmp8,'(b8.8)') infogrid(5)
     if (tmp8(5:5) .eq. '0') map%grid_wind = .false.

  elseif (ksec2(4).eq.5) then ! Polar-Stereographic Grid.
     map%igrid = 5
     map%nx = infogrid(1)
     map%ny = infogrid(2)
     map%lov = ginfo(6)
     map%truelat1 = 60.
     map%truelat2 = 91.
     map%dx = ginfo(7)
     map%dy = ginfo(8)
     map%lat1 = ginfo(3)
     map%lon1 = ginfo(4)
     write(tmp8,'(b8.8)') infogrid(5)
     if (tmp8(5:5) .eq. '0') map%grid_wind = .false.

  else
     print*, 'Unknown ksec2(4): ', ksec2(4)
  endif

111  format(' igrid      : ', i3, /, &
          ' nx, ny     : ', 2I4, /, &
          ' truelat1, 2: ', 2F10.4, /, &
          ' Center Lon : ', F10.4, /, &
          ' LatLon(1,1): ', 2F10.4, /, &
          ' DX, DY     : ', F10.4, F10.4)

! Special for NCEP/NCAR Reanalysis Project:
!      Throw out PSFC on lat/lon grid (save gaussian version)
  if ((icenter.eq.7).and.(iprocess.eq.80)) then   ! Careful! This combination may refer 
                                                      ! to other products as well.
     if ((field.eq.'PSFC').and.(ksec2(4).eq.0)) then
        field='NULL'
        call deallogrib
        return
     endif
  endif

  if (allocated(rdatarray)) deallocate(rdatarray)
  allocate(rdatarray(map%nx * map%ny))

! Unpack the 2D slab from the GRIB record, and put it in array rdatarray
  call gribdata(rdatarray,map%nx*map%ny)
! Deallocate a couple of arrays that may have been allocated by the 
! GRIB decoding routines.

  call deallogrib

END subroutine rd_grib1
